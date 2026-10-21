"""CLUinPy on the PIE benchmark: CLUMondo-style allocation of 1991 -> 1999 in one step, with the
observed 1999 class totals as demand, on one of two suitability surfaces.

Usage (from the repository root, with CLUinPy's src/ and src/suitability/ on PYTHONPATH):

    # as shipped: CLUinPy's own logistic suitability on the 1991 map, all conversions allowed
    python 2026-09-model-comparison/040-alloc-cluinpy/run_pie.py <cluinpy_repo> <n> native

    # the crossing: an estimator's class probability map (band k = class k), only the viable
    # conversions allowed
    python 2026-09-model-comparison/040-alloc-cluinpy/run_pie.py <cluinpy_repo> <n> crossing \
        <suitability.tif> <maps_dir> <seed_base>

Realisation i draws Python's random module from seed_base + i. Writes <maps_dir>/rNN.tif and
appends to outputs/timings-cluinpy.csv.
"""

import argparse
import csv
import glob
import os
import random
import runpy
import shutil
import sys
import time

import numpy as np
import pandas as pd
import rasterio

PIE_DIR = "2026-09-model-comparison"
# PIE_SCALE = k > 1: the tiled grids of 080-scaling, see common.r
_SCALE = os.environ.get("PIE_SCALE", "")
_SUFFIX = f"-k{_SCALE}" if _SCALE else ""
DATA_DIR = os.path.join(PIE_DIR, "data" + _SUFFIX)
OUT_DIR = os.path.join(PIE_DIR, "outputs" + _SUFFIX)
WORK_ROOT = os.path.abspath(os.path.join(OUT_DIR, "cluinpy-work"))
WORK_DIR = WORK_ROOT  # per invocation, set in main()
TIMINGS = os.path.join(OUT_DIR, "timings-cluinpy.csv")
VIABLE_ONLY = False  # set in main()
NO_DATA = -9999
CLASSES = ["Forest", "Built", "Other"]  # PIE codes 1, 2, 3 -> CLUinPy 0, 1, 2
START_YEAR, END_YEAR = 1999, 1999


def record_timing(stage, seconds, note=""):
    new = not os.path.exists(TIMINGS)
    with open(TIMINGS, "a", newline="") as f:
        w = csv.writer(f, quoting=csv.QUOTE_NONNUMERIC)
        if new:
            w.writerow(["tool", "stage", "seconds", "note", "host", "timestamp"])
        w.writerow(["cluinpy", stage, round(seconds, 2), note, os.uname().nodename,
                    time.strftime("%Y-%m-%dT%H:%M:%S")])


def write_like(template, path, array, dtype, nodata, count=1):
    with rasterio.open(template) as src:
        profile = src.profile
    profile.update(dtype=dtype, nodata=nodata, count=count, compress="deflate")
    with rasterio.open(path, "w", **profile) as dst:
        if count == 1:
            dst.write(array.astype(dtype), 1)
        else:
            dst.write(array.astype(dtype))


def prepare_inputs():
    """Reclassify land use, align predictors' no-data value, write the tables."""
    lu_1991_path = os.path.join(DATA_DIR, "lu_1991.tif")
    with rasterio.open(lu_1991_path) as src:
        lu = src.read(1, masked=True)
    land = np.where(lu.mask, NO_DATA, lu.filled(1).astype("int16") - 1).astype("int16")
    land_path = os.path.join(WORK_DIR, "land_1991.tif")
    write_like(lu_1991_path, land_path, land, "int16", NO_DATA)

    env_dir = os.path.join(WORK_DIR, "env")
    os.makedirs(env_dir, exist_ok=True)
    for i in (1, 2, 3):
        with rasterio.open(os.path.join(DATA_DIR, f"ef_00{i}.tif")) as src:
            ef = src.read(1, masked=True)
        write_like(lu_1991_path, os.path.join(env_dir, f"ef_00{i}.tif"),
                   ef.filled(NO_DATA), "float32", NO_DATA)

    region = np.where(land == NO_DATA, 1, 0).astype("int16")
    region_path = os.path.join(WORK_DIR, "region.tif")
    write_like(lu_1991_path, region_path, region, "int16", NO_DATA)

    # one service per class: the class area. Demand is the observed 1999 total of each class,
    # allocated in one step from the 1991 map (CLUinPy's "year" 1999 is the only step)
    totals = pd.read_csv(os.path.join(OUT_DIR, "demand_class_totals.csv"))
    t99 = totals[totals.year == 1999].sort_values("id_lulc").N.to_numpy(float)
    demand = pd.DataFrame([t99], columns=[f"{c}_area" for c in CLASSES])
    demand.to_excel(os.path.join(WORK_DIR, "demand.xlsx"), index=False)

    identity = pd.DataFrame(np.eye(3), columns=[f"{c}_area" for c in CLASSES])
    identity.insert(0, "class", CLASSES)
    identity.to_excel(os.path.join(WORK_DIR, "lus_matrix.xlsx"), index=False)
    identity.to_excel(os.path.join(WORK_DIR, "lus_conv.xlsx"), index=False)

    # conversions allowed: all, or the transitions viable in 1985 -> 1991 (and persistence)
    allow_matrix = np.ones((3, 3), dtype=int)
    if VIABLE_ONLY:
        viable = pd.read_csv(os.path.join(OUT_DIR, "alloc_params_viable.csv"))
        allow_matrix = np.eye(3, dtype=int)
        for a, p in zip(viable.id_lulc_anterior, viable.id_lulc_posterior):
            allow_matrix[a - 1, p - 1] = 1
    allow = pd.DataFrame(allow_matrix, columns=CLASSES)
    allow.insert(0, "class", CLASSES)
    allow.to_excel(os.path.join(WORK_DIR, "allow.xlsx"), index=False)
    return land_path, region_path, env_dir


def fit_suitability(land_path, env_dir):
    """CLUinPy's own suitability module, logistic regression, as in its tutorial."""
    from suitability.main import suitability
    from suitability.sampling import sample_per_class
    from suitability.io_utils import find_files

    land = rasterio.open(land_path).read(1)
    sample_list = sample_per_class(land, NO_DATA, "fraction", 0.1, 100, 500)
    out = os.path.join(WORK_DIR, "suitability_out")
    suitability(classification=land_path,
                env_vars=find_files(env_dir, ".tif", ".tif"),
                mode="logistic",
                out_path=out,
                n_samples_corr=1000,
                vif_threshold=5,
                min_distance=3,
                test_fraction=0.3,
                random_state=12,
                sample_size_list=sample_list,
                no_data_value=NO_DATA,
                predict_outputs=True)
    stacks = sorted(glob.glob(os.path.join(out, "*", "suitability_stack.tif")))
    return stacks[-1]


def external_suitability(path, land_path):
    """An estimator's class probabilities as CLUinPy's suitability stack: band k = class k,
    no-data where the land map has none."""
    land = rasterio.open(land_path).read(1)
    with rasterio.open(path) as src:
        suit = src.read(masked=True).filled(np.nan).astype("float32")
    suit[:, land == NO_DATA] = NO_DATA
    suit[np.isnan(suit)] = 0.0
    out = os.path.join(WORK_DIR, "suitability_stack.tif")
    write_like(land_path, out, suit, "float32", NO_DATA, count=suit.shape[0])
    return out


def write_config(path, land_path, suit_path, region_path, out_dir, tolerance):
    lines = {
        "land_array": land_path,
        "suit_array": suit_path,
        "region_array": region_path,
        # as in the CLUinPy tutorial for comparable classes: no neighbourhood for forest and
        # other, 0.3 for built-up land; inertia rises from other to built-up land
        "neigh_weights": "0,0.3,0",
        "start_year": START_YEAR,
        "end_year": END_YEAR,
        "demand": os.path.join(WORK_DIR, "demand.xlsx"),
        "dem_weights": "1,1,1",
        "lus_conv": os.path.join(WORK_DIR, "lus_conv.xlsx"),
        "lus_matrix_path": os.path.join(WORK_DIR, "lus_matrix.xlsx"),
        "allow": os.path.join(WORK_DIR, "allow.xlsx"),
        # % of demand; the tutorial uses 2, which leaves ~700 built cells unallocated here
        "max_diff_allow": tolerance,
        "totdiff_allow": tolerance,
        "max_iter": 30000,  # the crossing's class probabilities need far more iterations, see 040-alloc-cluinpy.r
        "out_dir": out_dir,
        "crs": "EPSG:26986",
        "dtype": "int16",
        "no_data_out": NO_DATA,
        "conv_res": "0.8,1,0.6",
        "out_year": END_YEAR,
        "no_data_value": NO_DATA,
    }
    with open(path, "w") as f:
        f.write("\n".join(f"--{k}={v}" for k, v in lines.items()))


def main():
    global WORK_DIR, VIABLE_ONLY
    parser = argparse.ArgumentParser()
    parser.add_argument("cluinpy_repo")
    parser.add_argument("n_realisations", type=int)
    parser.add_argument("mode", choices=["native", "crossing"])
    parser.add_argument("suitability", nargs="?")
    parser.add_argument("maps_dir", nargs="?", default=os.path.join(OUT_DIR, "maps", "cluinpy"))
    parser.add_argument("seed_base", nargs="?", type=int, default=10000 + 8000 + 600)
    args = parser.parse_args()
    native = args.mode == "native"
    VIABLE_ONLY = not native
    tag = "as shipped" if native else os.path.basename(args.maps_dir)

    WORK_DIR = os.path.join(WORK_ROOT, tag.replace(" ", "-"))
    shutil.rmtree(WORK_DIR, ignore_errors=True)
    os.makedirs(WORK_DIR)
    os.makedirs(args.maps_dir, exist_ok=True)

    land_path, region_path, env_dir = prepare_inputs()
    if native:
        t0 = time.time()
        suit_path = fit_suitability(land_path, env_dir)
        record_timing("fit_suitability", time.time() - t0)
        shutil.copy(suit_path, os.path.join(OUT_DIR, "maps", "cluinpy-suitability.tif"))
    else:
        suit_path = external_suitability(args.suitability, land_path)

    for i in range(1, args.n_realisations + 1):
        out_dir = os.path.join(WORK_DIR, f"run_{i:02d}")
        os.makedirs(out_dir)
        config = os.path.join(out_dir, "config.txt")
        # 0.1 % as shipped; 0.5 % in the crossing, where the binned Weights of Evidence surface
        # (many cells tied) oscillates around 0.25 % and does not reach 0.1 %
        write_config(config, land_path, suit_path, region_path, out_dir, 0.1 if native else 0.5)
        # the allocation draws its demand-adjustment "speed" from Python's random module
        random.seed(args.seed_base + i)
        sys.argv = ["run_CLUinPy", "--config", config]
        t0 = time.time()
        runpy.run_module("scripts.run_CLUinPy", run_name="__main__")
        record_timing("allocate", time.time() - t0, f"{tag} realisation={i}")
        result = glob.glob(os.path.join(out_dir, "**", f"cov{END_YEAR}*.tif"), recursive=True)
        if not result or "error" in os.path.basename(result[0]):
            raise RuntimeError(f"CLUinPy did not converge for {tag} realisation {i}: {result}")
        with rasterio.open(result[0]) as src:
            cov = src.read(1)
        lu = np.where(cov == NO_DATA, 255, cov + 1).astype("uint8")
        write_like(os.path.join(DATA_DIR, "lu_1991.tif"), os.path.join(args.maps_dir, f"r{i:02d}.tif"),
                   lu, "uint8", 255)


if __name__ == "__main__":
    main()
