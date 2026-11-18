#!/usr/bin/env python3
"""
Reference side of 010-verify-clumpy.r: runs the reference CLUMPY (Mazy's Python clumpy, as
forked in mmyrte/clumpy) on inputs written by the R step, and writes back what evoland has to
reproduce. Three subcommands, one per check:

  usam    MuST/GART pivot test per anterior class (uSAM), recording the exact uniforms drawn,
          so that the R step can replay them through evoland's allocator.
  patch   clumpy's single-patch grower (Patcher.allocate) on a battery of scenarios, in four
          variants: as shipped, with the connectedness (hollow-fill) step switched off, and
          each of those with the elongation moments taken about the centroid of the patch
          including the candidate cell (Mazy 2022, eqs. 3.I.6-3.I.8).
  areas   GaussianPatcher's patch-area draws and the number of cells a patch then gets.

The clumpy code is used verbatim; only the random draws of patch area and elongation are
replaced by the values each scenario prescribes (as in mmyrte/clumpy scripts/run_allocation.py).

Usage (from the repository root):
    python 2026-09-paper-figures/010-verify-clumpy/clumpy_reference.py usam  IN_DIR OUT_DIR
    python 2026-09-paper-figures/010-verify-clumpy/clumpy_reference.py patch IN_DIR OUT_DIR
    python 2026-09-paper-figures/010-verify-clumpy/clumpy_reference.py areas OUT_DIR MEAN SD N SEED
"""

import csv
import json
import sys
import types
import warnings
from pathlib import Path

import numpy as np

warnings.filterwarnings("ignore", category=SyntaxWarning)

from clumpy.allocation._gart import generalized_allocation_rejection_test as gart  # noqa: E402
from clumpy.layer._land_use_layer import LandUseLayer  # noqa: E402
from clumpy.layer._layer import Layer  # noqa: E402
from clumpy.patch._gaussian_patcher import GaussianPatcher  # noqa: E402
from clumpy.patch import _patcher as patcher_module  # noqa: E402


# --------------------------------------------------------------------------------------------
# uSAM: MuST (clumpy's GART) per anterior class
# --------------------------------------------------------------------------------------------


def run_usam(in_dir: Path, out_dir: Path) -> None:
    """
    in_dir holds landscape.csv (one row per cell in row-major order: cell, lulc; lulc -1 for
    no data), transitions.csv (id_trans, from, to) and potentials.csv (id_trans, cell, value;
    cells 1-based) and seed.txt. For each anterior class in ascending order, GART draws one
    uniform per cell of the class (cells in ascending order) and assigns the final class. The
    uniforms are recorded in the order they are drawn (uniforms.csv), and the posterior map is
    written to posterior.csv.
    """
    land = np.loadtxt(in_dir / "landscape.csv", delimiter=",", skiprows=1, dtype=np.int64)
    lulc = land[:, 1]
    trans = np.loadtxt(in_dir / "transitions.csv", delimiter=",", skiprows=1, dtype=np.int64, ndmin=2)
    pot = np.loadtxt(in_dir / "potentials.csv", delimiter=",", skiprows=1, ndmin=2)
    seed = int((in_dir / "seed.txt").read_text().strip())

    n = lulc.size
    # dense per-transition potentials, 0 where a transition has none
    dense = {int(t): np.zeros(n) for t in trans[:, 0]}
    for id_trans, cell, value in pot:
        dense[int(id_trans)][int(cell) - 1] = value

    posterior = lulc.copy()
    stream = []
    n_fallthrough = 0
    for k, u in enumerate(sorted(set(int(x) for x in trans[:, 1]))):
        rows = trans[trans[:, 1] == u]
        rows = rows[np.argsort(rows[:, 0])]  # transitions in id_trans order, as evoland
        J = np.flatnonzero(lulc == u)
        if J.size == 0:
            continue
        P = np.column_stack([dense[int(t)][J] for t in rows[:, 0]])
        P = np.column_stack([P, 1.0 - P.sum(axis=1)])  # the stay column
        list_v = [int(v) for v in rows[:, 2]] + [u]
        seed_u = seed + k
        # the uniforms GART draws for this seed (it seeds the global generator itself)
        np.random.seed(seed_u)
        x = np.random.random(J.size)
        np.random.seed(None)
        V = gart(P.copy(), list_v, seed=seed_u)
        # GART leaves 0 where x is not below the last cumulative sum (rounding of the stay
        # column); that cell stays, as in evoland's MuST
        n_fallthrough += int(np.sum(V == 0))
        V[V == 0] = u
        posterior[J] = V
        stream.append(x)

    stream = np.concatenate(stream) if stream else np.zeros(0)
    np.savetxt(out_dir / "uniforms.csv", stream, fmt="%.17g")
    with open(out_dir / "posterior.csv", "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["cell", "lulc"])
        for i, v in enumerate(posterior, start=1):
            w.writerow([i, int(v)])
    (out_dir / "fallthrough.txt").write_text(str(n_fallthrough))


# --------------------------------------------------------------------------------------------
# uPAM: the single-patch grower
# --------------------------------------------------------------------------------------------


def _allocate_thesis_moments(self, lul, lul_origin, j, proba_layer):
    """
    clumpy's Patcher.allocate, copied line for line, except for the second-order moments of
    the candidate patch. clumpy takes them about the centroid of the current patch (xc, yc,
    without the candidate); Mazy (2022, eqs. 3.I.6-3.I.8) defines them about the centroid of
    the patch whose elongation is computed, i.e. including the candidate. Lines changed are
    marked CHANGED.
    """
    structures = patcher_module.structures
    _convolve = patcher_module._convolve

    n_neighbors_to_fill = (
        structures[self.neighbors_structure].sum() - 1 - self.nb_of_missing_to_fill
    )
    J_allocated = [j]
    if lul.flat[j] != self.initial_state:
        return 0, J_allocated
    area, eccentricity = self.sample(n=1)
    while len(J_allocated) < area:
        x_allocated, y_allocated = lul.unravel_index(J_allocated)
        box_shape = [
            x_allocated.max() - x_allocated.min() + 3,
            y_allocated.max() - y_allocated.min() + 3,
        ]
        x_offset = x_allocated.min() - 1
        y_offset = y_allocated.min() - 1
        if x_allocated.min() == 0:
            x_offset += 1
            box_shape[0] -= 1
        if y_allocated.min() == 0:
            y_offset += 1
            box_shape[1] -= 1
        if x_allocated.max() == lul.shape[0] - 1:
            box_shape[0] -= 1
        if y_allocated.max() == lul.shape[1] - 1:
            box_shape[1] -= 1
        box_shape = tuple(box_shape)
        A = np.zeros(box_shape)
        A[x_allocated - x_offset, y_allocated - y_offset] = 1
        B = _convolve(A, structures[self.neighbors_structure])
        x_neighbors_box, y_neighbors_box = np.where(B * (1 - A) > 0)
        j_neighbors_box = np.ravel_multi_index([x_neighbors_box, y_neighbors_box], box_shape)
        x_neighbors = x_neighbors_box + x_offset
        y_neighbors = y_neighbors_box + y_offset
        j_neighbors = lul.ravel_index(x_neighbors, y_neighbors)
        vi_neighbors = lul_origin.flat[j_neighbors]
        vf_neighbors = lul.flat[j_neighbors]
        if self.avoid_aggregation and (
            np.sum((vi_neighbors == self.initial_state) * (vf_neighbors == self.final_state)) > 0
        ):
            return 0, J_allocated
        id_j_neighbors_to_keep = np.arange(j_neighbors.size)[
            (vi_neighbors == self.initial_state) * (vf_neighbors == self.initial_state)
        ]
        if id_j_neighbors_to_keep.size == 0:
            return 0, J_allocated
        j_neighbors_box = j_neighbors_box[id_j_neighbors_to_keep]
        b_neighbors = B.flat[j_neighbors_box]
        j_neighbors = j_neighbors[id_j_neighbors_to_keep]
        x_neighbors = x_neighbors[id_j_neighbors_to_keep]
        y_neighbors = y_neighbors[id_j_neighbors_to_keep]
        j_hollows = j_neighbors[b_neighbors >= n_neighbors_to_fill]
        if j_hollows.size > 0:
            J_allocated.append(np.random.choice(j_hollows))
            continue
        if self.equi_neighbors_proba:
            P = np.ones(j_neighbors.size)
        else:
            P = proba_layer.flat[j_neighbors]
        if np.isclose(P.sum(), 0):
            if self.proceed_even_if_no_probability:
                P.fill(1)
            else:
                return 0, J_allocated
        m = len(J_allocated) + 1
        xc = (np.sum(x_allocated) + x_neighbors) / m  # CHANGED: centroid with the candidate
        yc = (np.sum(y_allocated) + y_neighbors) / m  # CHANGED
        dx = x_allocated[:, None] - xc[None, :]  # CHANGED: patch cells about that centroid
        dy = y_allocated[:, None] - yc[None, :]  # CHANGED
        mu_20 = (np.sum(dx**2, axis=0) + (x_neighbors - xc) ** 2) / m  # CHANGED
        mu_02 = (np.sum(dy**2, axis=0) + (y_neighbors - yc) ** 2) / m  # CHANGED
        mu_11 = (np.sum(dx * dy, axis=0) + (x_neighbors - xc) * (y_neighbors - yc)) / m  # CHANGED
        delta = np.power(mu_20 - mu_02, 2) + 4 * np.power(mu_11, 2)
        e = 1 - np.sqrt((mu_20 + mu_02 - np.sqrt(delta)) / (mu_20 + mu_02 + np.sqrt(delta)))
        de = np.abs(eccentricity - e)
        P /= de
        J_allocated.append(j_neighbors[np.argmax(P)])
    if self.avoid_aggregation:
        last_neighbors = lul.get_neighbors_id(
            j=J_allocated[-1], neighbors_structure=self.neighbors_structure
        )
        vi_neighbors = lul_origin.flat[last_neighbors]
        vf_neighbors = lul.flat[last_neighbors]
        if np.any((vi_neighbors == self.initial_state) & (vf_neighbors == self.final_state)):
            return (0, J_allocated)
    lul.flat[J_allocated] = self.final_state
    return (len(J_allocated), J_allocated)


def _grow(sc, land, ant, proba, hollow_fill: bool, thesis_moments: bool, seed: int):
    patcher = GaussianPatcher(
        area_mean=float(sc["area"]),
        area_cov=0.0,
        eccentricity=float(sc["elongation"]),
        neighbors_structure="rook",
        avoid_aggregation=bool(sc["avoid_aggregation"]),
        # clumpy's default: a neighbour with 3 of its 4 rook neighbours in the patch is a
        # hollow and is filled first, at random. With -1 the threshold is 5, which no rook
        # neighbour can reach, so the step never fires.
        nb_of_missing_to_fill=1 if hollow_fill else -1,
        proceed_even_if_no_probability=True,
        equi_neighbors_proba=False,
    )
    patcher.initial_state = int(sc["from"])
    patcher.final_state = int(sc["to"])
    area = float(sc["area"])
    elong = float(sc["elongation"])
    # the scenario fixes the patch's area and elongation (no random draw)
    patcher._sample = types.MethodType(lambda self, n: (np.full(n, area), np.full(n, elong)), patcher)
    if thesis_moments:
        patcher.allocate = types.MethodType(_allocate_thesis_moments, patcher)
    lul = LandUseLayer(land.copy())
    lul_origin = LandUseLayer(ant.copy())
    proba_layer = Layer(proba.copy(), label="proba")
    np.random.seed(seed)  # only the hollow-fill step draws
    with np.errstate(divide="ignore", invalid="ignore"):
        s, J = patcher.allocate(
            lul=lul, lul_origin=lul_origin, j=int(sc["pivot"]) - 1, proba_layer=proba_layer
        )
    cells = sorted(int(c) + 1 for c in J) if s > 0 else []
    return int(s), cells


def run_patch(in_dir: Path, out_dir: Path) -> None:
    """
    in_dir/scenarios.json: a list of scenarios, each with nrow, ncol, the current map `land`,
    the anterior map `ant`, the potentials `proba` (row-major vectors), the 1-based `pivot`,
    the patch `area` and `elongation`, `from`, `to` and `avoid_aggregation`. Writes one row
    per scenario and variant: whether the patch succeeded and its cells (1-based, sorted).
    """
    scenarios = json.loads((in_dir / "scenarios.json").read_text())
    variants = {
        "clumpy": (True, False),
        "clumpy, no hollow-fill": (False, False),
        "clumpy, thesis moments": (True, True),
        "clumpy, no hollow-fill, thesis moments": (False, True),
    }
    with open(out_dir / "patches.csv", "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["scenario", "variant", "size", "cells"])
        for sc in scenarios:
            shape = (int(sc["nrow"]), int(sc["ncol"]))
            land = np.array(sc["land"], dtype=np.int64).reshape(shape)
            ant = np.array(sc["ant"], dtype=np.int64).reshape(shape)
            proba = np.array(sc["proba"], dtype=np.float64).reshape(shape)
            for name, (hollow, thesis) in variants.items():
                s, cells = _grow(sc, land, ant, proba, hollow, thesis, seed=int(sc["id"]))
                w.writerow([sc["id"], name, s, " ".join(map(str, cells))])


def _exact_outcomes(sc, max_states=200000):
    """
    Every patch the grower can produce if ties are resolved in exact arithmetic and each tie
    may go either way: the grower of `_allocate_thesis_moments` without hollow-fill, but with
    integer moments. Two candidates tie when they have the same potential and the same
    elongation; the elongation depends on the moments only through D / T^2 (T the trace and D
    the squared eigenvalue gap of the moment matrix, both integers once scaled by m^2), so
    ties are decided on exact fractions. Returns a set of frozensets of 1-based cells (the
    empty frozenset for a failed patch), or None if the search is cut off.
    """
    from decimal import Decimal, getcontext
    from fractions import Fraction

    getcontext().prec = 60
    nrow, ncol = int(sc["nrow"]), int(sc["ncol"])
    land = np.array(sc["land"], dtype=np.int64)
    ant = np.array(sc["ant"], dtype=np.int64)
    proba = np.array(sc["proba"], dtype=np.float64)
    u, v = int(sc["from"]), int(sc["to"])
    area = int(sc["area"])
    target = Decimal(repr(float(sc["elongation"])))
    avoid = bool(sc["avoid_aggregation"])
    pivot = int(sc["pivot"]) - 1

    def nbrs(j):
        r, c = divmod(j, ncol)
        out = []
        if r > 0:
            out.append(j - ncol)
        if c > 0:
            out.append(j - 1)
        if c < ncol - 1:
            out.append(j + 1)
        if r < nrow - 1:
            out.append(j + ncol)
        return out

    def foreign(j):
        return ant[j] == u and land[j] == v

    if land[pivot] != u:
        return {frozenset()}
    outcomes = set()
    seen = set()
    stack = [((pivot,), pivot)]
    while stack:
        patch, last = stack.pop()
        key = (frozenset(patch), last)
        if key in seen:
            continue
        seen.add(key)
        if len(seen) > max_states:
            return None
        if len(patch) >= area:
            if avoid and any(foreign(x) for x in nbrs(last)):
                outcomes.add(frozenset())
            else:
                outcomes.add(frozenset(c + 1 for c in patch))
            continue
        in_patch = set(patch)
        border = sorted({x for p in patch for x in nbrs(p) if x not in in_patch})
        if avoid and any(foreign(b) for b in border):
            outcomes.add(frozenset())
            continue
        cand = [b for b in border if ant[b] == u and land[b] == u]
        if not cand:
            # no neighbour of the source class left: a failure with aggregation avoidance (as
            # in clumpy and the thesis); without it, evoland keeps the patch grown so far (one
            # of the documented differences, kept apart in the summary)
            outcomes.add(frozenset() if avoid else frozenset(c + 1 for c in patch))
            continue
        m = len(patch) + 1
        xs = [p // ncol for p in patch]
        ys = [p % ncol for p in patch]
        groups = {}
        for b in cand:
            bx, by = divmod(b, ncol)
            sx, sy = sum(xs) + bx, sum(ys) + by
            sxx = sum(x * x for x in xs) + bx * bx
            syy = sum(y * y for y in ys) + by * by
            sxy = sum(x * y for x, y in zip(xs, ys)) + bx * by
            a = m * sxx - sx * sx
            d = m * syy - sy * sy
            c = m * sxy - sx * sy
            T = a + d
            D = (a - d) ** 2 + 4 * c * c
            ratio = Fraction(D, T * T)
            sqD = Decimal(D).sqrt()
            e = 1 - ((Decimal(T) - sqD) / (Decimal(T) + sqD)).sqrt()
            de = abs(target - e)
            p = Decimal(repr(float(proba[b])))
            score = p / de
            groups.setdefault((p, ratio), [score, []])[1].append(b)
        best = max(g[0] for g in groups.values())
        for score, members in groups.values():
            if score == best:
                for b in members:
                    stack.append((patch + (b,), b))
    return outcomes


def run_ties(in_dir: Path, out_dir: Path) -> None:
    """
    For the scenarios in in_dir/scenarios.json listed in in_dir/evoland.csv (scenario, cells:
    evoland's patch, 1-based, space-separated), whether evoland's patch is among the patches
    Mazy's grower can produce under exact-arithmetic ties (`_exact_outcomes`).
    """
    scenarios = {int(sc["id"]): sc for sc in json.loads((in_dir / "scenarios.json").read_text())}
    with open(in_dir / "evoland.csv") as f:
        rows = list(csv.DictReader(f))
    with open(out_dir / "ties.csv", "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["scenario", "n_outcomes", "evoland_reachable"])
        for row in rows:
            sc = scenarios[int(row["scenario"])]
            evo = frozenset(int(c) for c in row["cells"].split()) if row["cells"] else frozenset()
            outcomes = _exact_outcomes(sc)
            if outcomes is None:
                w.writerow([row["scenario"], "NA", "NA"])
            else:
                w.writerow([row["scenario"], len(outcomes), evo in outcomes])


# --------------------------------------------------------------------------------------------
# Patch areas
# --------------------------------------------------------------------------------------------


def run_areas(out_dir: Path, mean: float, sd: float, n: int, seed: int) -> None:
    """
    GaussianPatcher's area draws (normal, clamped to >= 1) and the number of cells
    Patcher.allocate then grows: it adds cells while the patch is smaller than the drawn
    (real-valued) area, i.e. ceil(area) cells.
    """
    patcher = GaussianPatcher(area_mean=mean, area_cov=sd)
    np.random.seed(seed)
    areas, _ = patcher.sample(n)
    cells = np.ceil(areas).astype(int)
    np.savetxt(out_dir / "areas.csv", np.column_stack([areas, cells]), fmt=["%.10g", "%d"],
               delimiter=",", header="area,cells", comments="")


if __name__ == "__main__":
    cmd = sys.argv[1]
    if cmd == "usam":
        run_usam(Path(sys.argv[2]), Path(sys.argv[3]))
    elif cmd == "patch":
        run_patch(Path(sys.argv[2]), Path(sys.argv[3]))
    elif cmd == "ties":
        run_ties(Path(sys.argv[2]), Path(sys.argv[3]))
    elif cmd == "areas":
        run_areas(Path(sys.argv[2]), float(sys.argv[3]), float(sys.argv[4]), int(sys.argv[5]),
                  int(sys.argv[6]))
    else:
        raise SystemExit(f"unknown subcommand {cmd}")
