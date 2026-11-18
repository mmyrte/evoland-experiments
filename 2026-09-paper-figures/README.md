# Paper figures (2026-09) — mock-ups for the evoland-plus model description paper

**Status:** active. Figures for the paper in
[mmyrte/evoland-plus-paper](https://github.com/mmyrte/evoland-plus-paper), built on small
synthetic cases so they run in minutes. [`TODO.md`](TODO.md) holds the open work.

Outputs are written to `figures/` and copied by hand to `evoland-plus-paper/figures/`. A second
Overleaf project on this repo would remove the copy step.

**Running:** `000-synthetic-process.r` defines the synthetic process and the scores, and the
`010-*` steps source it. The `010` steps are independent (own databases, own outputs), so they
share one stage; `020` scores the maps of `010-fig2-ensembles` with the further validation
metrics, and the `030` steps only draw:

```bash
./execute-all.sh --workers 3 '2026-09-paper-figures/0*'
```

renders `000`, the `010` steps in parallel, then `020` and `030`.
`010-learner-comparison` additionally runs its cases on its own PSOCK cluster. The two
`010-verify-*` steps (see [Verification](#verification-the-allocators-against-their-reference-implementations))
load evoland.plus from a source checkout (`EVOLAND_PLUS_SRC`, default `../evoland.plus`)
rather than the pinned `evoland` package, because they test the version the paper describes.

**Plotting elsewhere** (e.g. on macOS, for its PDF rendering): the expensive step
`010-fig2-ensembles` writes its maps and scores to `results/fig2/` (Parquet, LFS-tracked, and
CSV). `020` reads only those files, and the `030` steps read only those files and `020`'s output,
so with a clone of the repository (`git lfs pull`) and data.table, arrow, ggplot2 and patchwork,
`Rscript 2026-09-paper-figures/030-fig2-plot.r` redraws Fig. 2 without evoland, Dinamica, lulcc
or CLUinPy. `020` also needs Rcpp and a C++ compiler (`validation-metrics.r` at the repository
root).

| Step | Figure | What it shows |
| --- | --- | --- |
| [`000-synthetic-process.r`](000-synthetic-process.r) | — | Synthetic process and scores, sourced by the `010` steps |
| [`010-fig2-ensembles.r`](010-fig2-ensembles.r) | `results/fig2/` | Allocates and scores the ensembles of Fig. 2 on a backcast; writes the maps and scores |
| [`010-skill-attribution.r`](010-skill-attribution.r) | `figures/skill-attribution.csv` | Where the skill is lost: potentials or allocation |
| [`010-learner-comparison.r`](010-learner-comparison.r) | `figures/learner-comparison.{csv,pdf}` | Which learner, feature set and number of calibration periods close the estimation gap |
| [`010-verify-clumpy.r`](010-verify-clumpy.r) | `results/verification/clumpy-*.csv` | Verification: evoland's uSAM and uPAM grower against the reference clumpy (Python), via [`010-verify-clumpy/`](010-verify-clumpy/) |
| [`010-verify-greedy-lulcc.r`](010-verify-greedy-lulcc.r) | `results/verification/greedy-lulcc*.csv` | Verification: evoland's greedy allocator against lulcc's Ordered model |
| [`020-verify-placement.r`](020-verify-placement.r) | `results/verification/placement-*.csv` | Verification on Fig. 2's maps: does each allocator place change where its potentials say (Kolmogorov-Smirnov, quantity, cell-wise frequencies) |
| [`020-validation-metrics.r`](020-validation-metrics.r) | `results/fig2/metrics-*.csv`, `toc.csv` | Every map of Fig. 2 scored with the validation metrics in common use (below) |
| [`030-fig2-plot.r`](030-fig2-plot.r) | Figs. 2a and 2b, `figures/fig2a-fom.pdf`, `figures/fig2b-ensembles.pdf` | (2a) conventional validation: observed map, one realisation scored cell by cell, its figure of merit; (2b) why ensembles: change frequency, FoM of every realisation with each allocator's Brier skill, where change is placed |
| [`030-metric-relations-plot.r`](030-metric-relations-plot.r) | `figures/metric-relations.pdf`, `figures/metric-agreement.pdf` | How the metrics relate to each other and to allocation bias |

## Verification: the allocators against their reference implementations

GMD asks for verification (the code does what the method says) apart from evaluation (the model
is fit for purpose). Three steps build that evidence for the allocators the paper discusses. All
run in under two minutes. Results from the last run, on evoland.plus `develop` at `d1d7794`, clumpy
0.1.0 (mmyrte/clumpy, package code as pinned in `pyproject.toml`; the comparison scripts of the
fork's branch `claude/upbeat-mayer-2ik3ux` are the starting point), lulcc 1.0.4, R 4.6.2.

### uSAM: bit-identical to clumpy, end to end

[`010-verify-clumpy.r`](010-verify-clumpy.r) builds 20 random 60 × 60 landscapes (four classes,
a block without data, two anterior classes with two transitions each, random potentials), runs
clumpy's pivot test (`generalized_allocation_rejection_test`) per anterior class, records the
uniforms numpy draws, and replays them into evoland through R's user-supplied random number
generator ([`010-verify-clumpy/replay_rng.c`](010-verify-clumpy/replay_rng.c)). evoland allocates
through its allocator specification, `alloc_clumpy_spec()$one_period(ctx)`, the code
`allocate()` runs, with the context built in memory. evoland itself is not modified.

| cases | cells | changed | cells differing | uniforms drawn by clumpy / used by evoland |
| --- | --- | --- | --- | --- |
| 20 | 69 600 | 5 028 | **0** | 52 021 / 52 021 |

The earlier check on the fork matched `must_cpp` alone; this one covers the R layer (sparse
potentials, grid cells, method selection) and the C++ allocator as well, and shows that evoland
consumes exactly one uniform per eligible cell, in clumpy's order.

### uPAM: the patch grower, with the known differences isolated

uPAM is uSAM's pivot test, a patch grower and an iteration over the shrinking pool (Mazy 2022,
Fig. 3.2). The iteration cannot be replayed against clumpy: clumpy's iterative allocator
(`clumpy.allocation.Unbiased.allocate`) does not run (it refers to the undefined names
`features`, `tm`, `mask`), and it iterates differently from the thesis (it re-estimates the
potentials between passes and does not stop a transition at its quota). evoland follows the
thesis. The grower is checked directly: 2000 random 16 × 16 scenarios with a prescribed patch area
(1–16 cells) and elongation (0.31, 0.57, 0.83), other patches of the same step nearby, half with
aggregation avoidance; a fifth with equal potentials everywhere.

Share of scenarios where clumpy's patch is evoland's, cell by cell (`results/verification/clumpy-patch-summary.csv`):

| clumpy variant | avoidance off | avoidance on |
| --- | --- | --- |
| as shipped | 0.575 | 0.727 |
| without the connectedness ("hollow-fill") step | 0.618 | 0.763 |
| with the thesis' elongation moments | 0.771 | 0.864 |
| both | 0.893 | 0.937 |

The differences are those of the re-implementation, documented on the fork
(`scripts/comparison/PIXEL_PERFECT_PLAN.md`):

1. **Connectedness step.** Mazy's step (iv)–(v) adds a neighbour that is almost enclosed by the
   patch first, at random. evoland does not implement it.
2. **Elongation moments.** Mazy (2022, eqs. 3.I.6–3.I.12) takes a patch's second moments about
   its own centroid. To score a candidate, clumpy takes the moments of patch-plus-candidate about
   the centroid of the patch *without* the candidate. evoland follows the thesis. The variant
   "thesis moments" ([`_allocate_thesis_moments`](010-verify-clumpy/clumpy_reference.py)) is
   clumpy's function with only those lines changed.
3. **No neighbour left.** Without aggregation avoidance, clumpy fails a patch that runs out of
   neighbours of its class; evoland keeps what it has grown (8 of 1000 scenarios).
4. **Ties.** Candidates that tie in exact arithmetic (mirror images with equal potential) get
   elongations that differ in the last bits, differently in the two codes, so round-off decides
   the tie. Mazy's algorithm leaves ties open.

With distinct potentials and aggregation avoidance on, clumpy with (1) and (2) aligned gives
evoland's patch in **800 of 800** scenarios. The decisive check covers all 2000: an exact
enumeration (integer moments, exact fractions) of every patch Mazy's grower can produce when each
exact tie may go either way finds evoland's patch among them in **every scenario** (up to 63
possible patches per scenario when all potentials are equal; `clumpy-patch-ties.csv`).

**Patch areas.** clumpy's `GaussianPatcher` grows a patch while it is smaller than the drawn
real-valued area, so it gets `ceiling(area)` cells, half a cell more than the prescribed mean on
average. evoland rounds to the nearest integer (`clumpy-areas.csv`):

| mean, sd | clumpy mean cells | evoland mean cells |
| --- | --- | --- |
| 3, 1 | 3.51 | 3.00 |
| 10, 1 | 10.51 | 10.00 |
| 10, 3 | 10.52 | 10.00 |

### Greedy: identical to lulcc's Ordered model where they are the same procedure

[`010-verify-greedy-lulcc.r`](010-verify-greedy-lulcc.r): lulcc's Ordered model fills class
*totals* in a fixed class order; evoland's greedy allocator fills *transition counts*. They are
the same procedure when each growing class has one source class. Ten random 50 × 50 landscapes
per case, the same potentials (lulcc gets them as class probabilities), lulcc both through its
core (`lulcc:::.ordered`) and its full interface (`OrderedModel`, `allocate`, with GLMs fixed to
return the probabilities, as in `2026-09-model-comparison`):

| case | maps identical (evoland ordered / joint) | cells differing |
| --- | --- | --- |
| one transition (1 → 2) | 10 / 10 | 0 of 694 changed |
| two disjoint transitions | 10 / 10 | 0 of 1346 |
| ties (potentials rounded to 0.01) | 10 / 10 | 0 of 1346 |
| two sources (1 → 2, 3 → 2) | 0 / 0 | 24 % of changed cells differ (by design) |
| swap (1 → 2, 2 → 1) | 0 / 0 | 65 % (lulcc sees net change only) |

lulcc's full interface gives the same maps as its core in all identical cases. Ties at the
quota cut-off (14–35 tied cells in a typical landscape) are taken in cell order by both.

### Placement on Fig. 2: Kolmogorov-Smirnov against the potentials

[`020-verify-placement.r`](020-verify-placement.r) reads `results/fig2/` only. Every allocator
gets the same adjusted potentials p; an unbiased one changes cell j by transition k with
probability p_jk, so the changed cells' percentiles of p (within each transition, as in panel (e))
follow the distribution F0(x) = share of the potential mass up to percentile x. The KS distance D
of each map to F0 is tested against D's null distribution simulated from exactly that model
(independent multinomial draws per cell: 2000 single maps, 400 pooled ensembles of 100).

| allocator | median D per map | maps rejected at 5 % | D of the pooled ensemble | p |
| --- | --- | --- | --- | --- |
| CLUMPY (uSAM) | 0.029 | **0.05** | 0.002 | **0.79** |
| uPAM, merging allowed | 0.063 | 0.79 | 0.049 | < 0.003 |
| uPAM | 0.118 | 1.00 | 0.109 | < 0.003 |
| Dinamica | 0.151 | 1.00 | 0.139 | < 0.003 |
| CLUMondo | 0.465 | 1.00 | 0.465 | < 0.003 |
| rank-and-fill | 0.530 | (one map) | | < 0.001 |
| CLUE-S | 0.741 | 1.00 | 0.741 | < 0.003 |
| null: median, 95 % quantile | 0.029, 0.049 | | 0.003, 0.005 | |

(p < 0.003 is the resolution of 400 simulated ensembles.) uSAM is rejected at the nominal rate
and its pooled 57 000 changed cells are indistinguishable from F0; every other allocator is
rejected. Two further checks of the ensembles against p:

- **Quantity per transition** (mean over 100 members against the sum of p): uSAM z = −1.7 to
  +1.0; uPAM within ±0.5 (quota); Dinamica fixes the count; CLUE-S and CLUMondo miss by 54–180
  standard errors (they allocate net class totals).
- **Cell-wise frequencies** (Pearson's X² over 6687 cells and their three outcomes, df 13 151):
  uSAM z = **−0.64**; uPAM merging 26, Dinamica 48, uPAM 78, CLUE-S 348, CLUMondo 7997.

Against the true probabilities q (panel (e) as drawn; evaluation, not verification), CLUMPY's
maps are rejected in 11 % of cases and its pooled ensemble with p < 0.003: the ensemble
reproduces the potentials, and the potentials carry the estimation error.

**A caveat on the observed map.** Against q, the observed change has D = 0.013, below every one
of 2000 simulated draws of the true process (null median 0.031; also below all of 300 draws in a
separate check). The landscape seed is fixed (`set.seed(1337)`), so it is either chance (roughly
1 in 10⁴ for a KS statistic this small) or an unintended consequence of tuning the case over many
iterations. It does not affect the verification above, which does not use the observed map, but
the paper should not read the observed curve's closeness to the expectation as evidence. The open
item "replicate over several landscape seeds" (TODO) would settle it.

## Figure 2: ensembles vs. single maps

**uPAM in Fig. 2b** is CLUMPY's patch allocator on the patch sizes estimated in period 1 -> 2,
with merging allowed (`avoid_aggregation = FALSE`). With clumpy's default (`TRUE`), a patch that
would merge with an existing patch fails and is drawn elsewhere, which keeps change away from the
target class, exactly where this neighbourhood-driven process makes it most likely: Brier skill
+0.146 instead of +0.155 (uSAM +0.164), 25 % of change in the top 5 % of true probabilities
instead of 30 % (uSAM 34 %, observed 36 %). `010` still computes both ensembles; the plot shows
the one with merging allowed. The remaining gap to uSAM comes from growing patches into less
probable neighbours.

The domain is a synthetic 90 × 90 grid (map panels: the 30 × 30 block with the most observed
change), set up like the evoland-plus vignette
`stochastic-allocation-sensitivity.qmd`, with these differences:

- **Structured initial map:** urban land clusters where accessibility is high, there is a
  small immutable lake, forest sits on the better sites and arable land takes the rest.
- **Known change process:** land use changes through a stated logistic process
  (`000-synthetic-process.r`), so the model has a signal to learn. The vignette's noise
  increments have none.
- **Estimator:** a random forest (ranger, 500 trees, `min.node.size = 50`, all available
  predictors), the best setting in `010-learner-comparison` without logistic regression's home
  advantage. An earlier version also showed a logistic-regression ensemble; its points and
  curves overlapped the random forest's so much that the panels were hard to read, so it was
  dropped.
- **Allocation:** single-cell (uSAM). The process changes cells independently; the estimated
  1.1–1.2-cell "patches" are clusters induced by its neighbourhood terms, and allocating with
  them (uPAM) moves change onto less probable neighbours (see below).

The backcast works as follows:

- **Calibration** uses the transition from period 1 to 2 only. Period 3 is flagged as
  extrapolated, so model fitting and allocation parameters never see it.
- **Held-out observation:** the observed period 3 is stored in its own run, which is a sibling
  of the ensemble runs, so the ensemble cannot read it.
- **Demand** is the observed 2 → 3 quantity of each viable transition. The figure is about
  *where* change is placed, not how much.
- **Ensembles:** the same random-forest potentials, 100 realisations each of four allocators,
  every run registered in `runs_t` with its own seed:
  - CLUMPY (uSAM, evoland);
  - Dinamica EGO's expander and patcher, through evoland's `alloc_dinamica()`;
  - CLUE-S (lulcc) and CLUMondo (CLUinPy), which allocate class totals: they get the class
    probability map of the potentials and the observed class totals of period 3, as in
    `2026-09-model-comparison/` (lulcc through a GLM with coefficients fixed at 0 and 1;
    CLUinPy through `010-fig2-ensembles/cluinpy_alloc.py`, with the lake left out as no data,
    because CLUMondo cannot hold a fixed class at its demand). CLUE-S's absolute tolerances are
    scaled to the domain (5 cells).

  Needs Dinamica EGO, lulcc and the repository's uv environment (`uv sync`); about 18 min,
  most of it Dinamica.
- **Deterministic:** the same demand is also allocated by rank-and-fill on the adjusted
  potentials, the kind of allocator SEALS uses.

Panels, on a 3 × 2 grid with a legend each:
- **(a)** observed land use in period 3 (all cells), with the cells changed since period 2
  outlined
- **(b)** one CLUMPY realisation, the draw with the median figure of merit, coloured by
  outcome (hit / miss / false alarm / wrong hit)
- **(c)** change frequency across the CLUMPY ensemble, with the observed change outlined
- **(d)** FoM of every realisation, one row per allocator, with each ensemble's expectation
  (from mean counts), the deterministic map, and the two nulls (no change, random placement),
  labelled in the panel
- **(e)** where change is placed: the cumulative distribution of changed cells over the
  percentile of their true probability, for the observed change, the median realisation of
  each allocator (band: CLUMPY's 5–95 %) and the deterministic map; the diagonal is random
  placement

### What the numbers say (last render)

All four transitions are viable; none of the 573 observed changes on the 6687 forest and arable
cells is unmodelled.

Random-forest potentials, 100 realisations per allocator:

| allocator | FoM median (5–95 %) | FoM ensemble | Brier skill, ensemble (fair) | Brier skill, single map (median) | change in top 5 % |
| --- | --- | --- | --- | --- | --- |
| CLUMPY | 0.138 (0.123–0.152) | 0.137 | 0.164 | −0.62 | 0.339 |
| Dinamica | 0.166 (0.148–0.178) | 0.165 | 0.138 | −0.53 | 0.428 |
| CLUE-S | 0.161 (no spread) | 0.161 | −0.034 | −0.034 | 0.957 |
| CLUMondo | 0.165 (no spread) | 0.165 | −0.180 | −0.181 | 0.706 |
| rank-and-fill (deterministic) | 0.245 | | −0.30 | | 0.660 |

Adjusted potentials: Brier skill 0.160. Observed change: 0.361 in the top 5 %.

References: FoM of random allocation 0.026; Brier skill of the true probabilities 0.191
(multiclass Brier score over the cells that can change).

**Skill reference.** Skill is measured against the *uniform forecast*, which knows the quantity
of change but not its location: every cell of a land-use class gets the same probability for each
transition, the transition's demand divided by the class's area. It is a forecast, not a map.
Do not confuse it with the *random-allocation null* of the FoM, which is a map with the right
quantity of change placed at random; the uniform forecast is that map's cell-wise expectation.
The Brier skill score BSS = 1 − BS / BS_uniform is 0 for a forecast no better than the uniform
one, 1 for a perfect one, and negative for one worse than it. (Forecast verification calls this
kind of reference "climatology".)

Panel (e) ranks every changed cell by its true transition probability, as a percentile among
the cells that could make the same transition:

| changed cells | share in top 5 % | median percentile |
| --- | --- | --- |
| expected under the true probabilities | 0.361 | — |
| observed | 0.361 | 0.909 |
| CLUMPY realisations (median) | 0.339 | 0.902 |
| Dinamica realisations (median) | 0.428 | 0.937 |
| CLUMondo realisations (median) | 0.706 | 0.974 |
| CLUE-S realisations (median) | 0.957 | 0.984 |
| rank-and-fill (deterministic) | 0.660 | 0.965 |

What the figure argues:

1. **FoM is the wrong yardstick for a stochastic process.** It ranks the allocators by how much
   they concentrate change: rank-and-fill highest, then CLUMondo, Dinamica and CLUE-S, CLUMPY
   lowest. Panel (e) shows that observed change does not concentrate: only a third of it falls
   in the top 5 %, exactly what the true probabilities predict. CLUMPY's realisations reproduce
   this; Dinamica is slightly biased towards the top (43 %); rank-and-fill, CLUMondo and CLUE-S
   put two thirds to almost all of their change there.
2. **The unbiased ensemble loses nothing.** The CLUMPY ensemble frequency is as good a
   probabilistic forecast as the potentials it samples from (skill 0.164 vs. 0.160), Dinamica's
   is close (0.138). CLUE-S and CLUMondo are near-deterministic: their 100 realisations barely
   differ, so their ensembles are no better than single maps and score below the uniform
   forecast, as does rank-and-fill.
3. **Patches matter.** In the earlier two-learner version, uPAM on the estimated patches made the
   logistic-regression ensemble put only 28 % of its change in the top 5 % (observed: 36 %), and
   its ensemble lost skill against its potentials (0.166 vs. 0.183). Patch parameters estimated
   from a neighbourhood-driven process are not neutral: they describe clustering that the
   neighbourhood terms already produce.

## Validation metrics: which ones reward allocation bias?

[`020-validation-metrics.r`](020-validation-metrics.r) scores every map of Fig. 2 (400
realisations, rank-and-fill) with the metrics in common use for land-change models, all
implemented in [`validation-metrics.r`](../validation-metrics.r) at the repository root (tested in
`validation-metrics-tests.r`, which also checks the similarity of differences against evoland's
`calc_transition_similarity()`):

- **Figure of merit** (Pontius et al. 2008), as in Fig. 2.
- **Kappa Simulation** (KSim; van Vliet et al. 2011): Cohen's kappa with the expected agreement of
  a random allocation of each transition within its initial class, so persistence does not
  inflate it. It supersedes Cohen's kappa and Pontius' Kappa_location/Kappa_histo for change,
  which are therefore not computed. KSim = KTransition × KTransloc: the quantity of each
  transition, and its location.
- **Fuzzy Kappa Simulation** (FKS; van Vliet, Hagen-Zanker et al. 2013): KSim with location
  tolerance. A cell's agreement is the two-way fuzzy membership (Hagen 2003) between the maps of
  transitions; the expected agreement follows Hagen-Zanker (2009), stratified by initial class.
  Exponential decay with halving distance 2 cells, radius 2 and 4 cells. With a 1 × 1 window it
  equals KSim (tested). evoland's `calc_transition_similarity()` is *not* a fuzzy kappa: it is
  the similarity of differences below, with no chance correction.
- **Similarity of differences** (Hagen 2003, as in Dinamica EGO): fuzzy agreement of the change of
  each transition, minimum of the two directions, 11-cell window; averaged over the transitions
  with their observed change as weights.
- **TOC AUC** (Total Operating Characteristic, Pontius & Si 2014; its normalised area equals the
  ROC AUC), per transition within its initial class: for a single map a one-threshold curve, for
  an ensemble its frequencies.
- **Quantity and allocation disagreement** (Pontius & Millones 2011) and the multiclass **Brier
  score**, as before.
- Not computed: the multi-resolution figure of merit (common, but it does not summarise an
  ensemble in one panel).

Medians over each allocator's realisations (rank-and-fill: its one map); the last column is the
share of the simulated change in the top 5 % of the true probabilities (observed: 0.361):

| allocator | FoM | KSim | KTransloc | FKS (r = 4) | SoD | TOC AUC | alloc. disagreement | Brier | top 5 % |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| CLUMPY | 0.138 | 0.191 | 0.195 | 0.313 | 0.470 | 0.594 | 0.104 | 0.256 | 0.339 |
| Dinamica | 0.166 | 0.235 | 0.235 | 0.380 | 0.491 | 0.616 | 0.100 | 0.242 | 0.428 |
| CLUE-S | 0.161 | 0.251 | 0.529 | 0.305 | 0.243 | 0.586 | 0.047 | 0.164 | 0.957 |
| CLUMondo | 0.165 | 0.281 | 0.452 | 0.323 | 0.270 | 0.601 | 0.075 | 0.187 | 0.706 |
| rank-and-fill | 0.245 | 0.350 | 0.350 | 0.453 | 0.476 | 0.674 | 0.085 | 0.206 | 0.660 |

As forecasts (TOC AUC per transition; Brier skill against the uniform forecast, fair for the
ensembles): CLUMPY ensemble 0.779, 0.164; Dinamica ensemble 0.781, 0.138; CLUE-S 0.586, −0.034;
CLUMondo 0.601, −0.180; adjusted potentials 0.789, 0.160; true probabilities 0.819, 0.191.

Figures: `figures/metric-relations.pdf` (each metric against the share in the top 5 %, one dot
per map) and `figures/metric-agreement.pdf` (rank correlations between the metrics; the
ensembles as forecasts). What they show:

1. **The kappas inherit the figure of merit's bias.** KSim and FKS rank the transition
   allocators exactly as FoM does: rank-and-fill > Dinamica > CLUMPY. Within the
   transition allocators every single-map location metric (FoM, KSim, FKS, TOC AUC) rises with
   the share of change on the most probable cells, past the observed 36 %. The location
   tolerance of FKS does not help: near misses of a concentrated map are still concentrated.
2. **Class-total allocators split the metrics.** CLUE-S and CLUMondo make less change than
   observed (no swaps between forest and arable), so their KTransloc, allocation disagreement
   and single-map Brier score look best, while the similarity of differences, which takes the
   minimum over both directions, punishes the missing change hard (0.24–0.27 against 0.47–0.49).
   Which allocator "wins" depends on the metric, not on how faithfully it reproduces the process.
3. **Only the ensemble-level scores reward an unbiased allocator.** As probabilistic forecasts,
   the CLUMPY and Dinamica ensembles reach the TOC AUC and Brier skill of the potentials they
   sample from (0.78 vs. 0.79; 0.164 vs. 0.160), and the near-deterministic CLUE-S and CLUMondo
   ensembles fall back to the level of their single maps. No single-map metric recovers this.

## Skill attribution: potentials or allocation?

[`010-skill-attribution.r`](010-skill-attribution.r) uses the known process to separate the
two. It crosses **potentials** (estimated by ranger, as in Fig. 2, vs. the oracle true
probabilities `q`, restricted to the viable transitions) with **allocation** (none, CLUMPY uSAM
on single cells, CLUMPY uPAM with the estimated patches), at 30 × 30 and 90 × 90 cells, with 100
members per ensemble. Because `q` is known, the expected multiclass Brier score of a forecast `p`
splits into a distance to the truth, mean of sum_k (p_k − q_k)^2, and an irreducible term,
mean of sum_k q_k (1 − q_k). The distance no longer depends on the single observed period 3.

| quantity (distance to truth, expected skill vs. the uniform forecast) | 30 × 30 | 90 × 90 |
| --- | --- | --- |
| uniform forecast | 0.021, 0 | 0.027, 0 |
| estimated potentials (adjusted) | 0.0125, 0.050 | 0.0137, 0.087 |
| ensemble on estimated potentials, uSAM | 0.0124, 0.051 | 0.0138, 0.087 |
| ensemble on estimated potentials, uPAM | 0.0148, 0.036 | 0.0140, 0.085 |
| oracle potentials (adjusted) | 0.0040, 0.100 | 0.0004, 0.177 |
| ensemble on oracle potentials, uSAM | 0.0040, 0.100 | 0.0003, 0.177 |
| ensemble on oracle potentials, uPAM | 0.0063, 0.087 | 0.0030, 0.159 |
| truth, all four transitions (ceiling) | 0, 0.124 | 0, 0.179 |

Viable transitions: 2 at 30 × 30 (7 of 56 observed changes unmodelled), all 4 at 90 × 90 (573
changes, none unmodelled).

What it shows:

1. **Allocation loses nothing with uSAM.** The ensemble frequency reproduces the potentials it
   samples from, at both sizes and for both potential sources. This verifies that the C++ uSAM is
   unbiased. uPAM adds a small, consistent loss (distance +0.002–0.003) because it places change
   in patches while the synthetic process changes single cells.
2. **The loss is in estimation.** The estimated potentials capture only 40 % (30 × 30) and 49 %
   (90 × 90) of the attainable skill. Their distance to the truth does not shrink with nine times
   the data (0.0125 → 0.0137), so a larger domain alone does not fix it. Suspects: a single
   calibration period, ranger's poorly calibrated probabilities for rare events, and the
   predictor set (top 4 by rpart importance; distance-band neighbourhood shares that only
   approximate the process's 5 × 5 window).
3. **The ceiling is low.** Even the true probabilities only reach a skill of 0.12–0.18 over
   the uniform forecast, because the events are rare (per-cell probabilities of a few percent). A larger
   domain does not raise the ceiling, but it makes realised scores track expected ones (90 × 90:
   realised and expected skill agree to about 0.02; 30 × 30: up to 0.03 apart on 0.05).
   Caveat: each size uses one landscape, so size and landscape are confounded; the higher
   ceiling at 90 × 90 reflects that landscape, not the size.
4. **FoM tells another story.** The FoM spread narrows with size (estimated, uSAM: 0.05–0.13 at
   30 × 30, 0.10–0.13 at 90 × 90), and uSAM scores higher than uPAM throughout.

## Learner comparison: the learner closes the gap

[`010-learner-comparison.r`](010-learner-comparison.r) crosses seven learners with three feature
sets, one vs. two calibration periods (paired: same target transition), 30 × 30 vs. 90 × 90 and
three landscape seeds. It scores the adjusted potentials only, since `010-skill-attribution` showed that uSAM
passes them through unchanged. Run: 12 min on 3 workers, no failed configurations; two runs on
the current pin are bit-identical (the stochastic learners moved by up to 0.07 per configuration,
0.02 in the means below, against the previous pin `08aa38e`). Output:
`figures/learner-comparison.{csv,pdf}`.

Share of attainable skill (1 = as good as the true probabilities, 0 = the uniform forecast), mean over
three seeds, one calibration period:

| learner | 30 × 30, rpart top 4 | 30 × 30, all available | 90 × 90, rpart top 4 | 90 × 90, all available | 90 × 90, process features |
| --- | --- | --- | --- | --- | --- |
| log_reg | 0.76 | 0.48 | 0.95 | 0.95 | 0.97 |
| ranger_large_leaves | 0.61 | 0.69 | 0.75 | 0.82 | 0.84 |
| xgboost | −0.02 | −0.07 | 0.76 | 0.76 | 0.80 |
| cv_glmnet | 0.33 | 0.19 | 0.73 | 0.75 | 0.74 |
| ranger (Fig. 2 setting) | 0.27 | 0.49 | 0.57 | 0.72 | 0.65 |
| naive_bayes | 0.24 | 0.14 | 0.41 | 0.36 | 0.48 |
| featureless | −0.01 | −0.01 | 0.00 | 0.00 | 0.00 |
| oracle, viable transitions | 0.92 | | 0.99 | | |

What it shows:

1. **The learner is the main factor.** Logistic regression reaches 95–97 % of the attainable skill
   at 90 × 90, even with only the predictors a modeller has. Ranger as configured for Fig. 2
   reaches 57 %; the same forest with larger leaves (`min.node.size = 50`) reaches 75–84 %. The
   gap is in calibration: small leaves give overconfident probabilities, and a proper score
   punishes that.
2. **More data helps the flexible learners, not the fixed one.** From 30 × 30 to 90 × 90, xgboost
   goes from no skill to 0.76–0.80, and cv_glmnet and ranger roughly double. A second calibration
   period helps at 30 × 30 (more data again) and little at 90 × 90. The seeds agree at 90 × 90
   (ranger: 0.57–0.58; log_reg: 0.92–0.97). At 30 × 30 they do not (ranger: 0.07–0.46).
3. **The feature set matters little** once the learner is right. Process features gain at most a
   few points; rpart's top 4 hurt ranger at 90 × 90.
4. **Naive Bayes, the closest to Dinamica's weights of evidence, stays poor** (0.36–0.49 at
   90 × 90). Its raw probabilities are badly calibrated (distance to truth 0.10–0.35 before
   adjustment, 0.016 after). The adjustment to demand rescues it only partly.
5. **Caveat: log_reg has home advantage.** The synthetic process is logistic, so a logistic
   regression is close to correctly specified. On real data, the flexible learners may win. The
   general lesson is calibration and enough data, not "use a GLM".

For Fig. 2, a 90 × 90 domain with log_reg (or ranger with larger leaves, to avoid the home
advantage) puts the estimates within a few percent of the truth. Then the figure shows what
allocation and ensembles do, rather than estimation error. The ceiling stays low (skill ~0.18
over the uniform forecast), as the events are rare.
