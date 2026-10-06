# Model comparison (2026-09) — the Plum Island Ecosystems benchmark

**Status:** active. The pipeline runs end to end in about 30 minutes on 4 cores. The first
results are below; [`TODO.md`](TODO.md) holds the open work.

Two purposes, served by one worked example:

1. **Illustrate the use of evoland-plus** end to end on a tractable case, in the manner of the
   package vignettes but as a complete small study rather than a feature tour.
2. **Compare against other land-use-change modelling tools** on the same data, the same
   calibration and validation periods, and the same metrics, so the comparison says something
   about the methods rather than about the inputs. This is one section of the model description
   paper, not the paper itself.

## The case: Plum Island Ecosystems (PIE)

The PIE data ship with [lulcc](https://github.com/simonmoulds/lulcc) and were the worked example
of its GMD paper (Moulds et al. 2015, <https://doi.org/10.5194/gmd-8-3215-2015>):

- land use in **1985, 1991, 1999**, three classes (forest, built, other), 434 × 497 cells of
  ~100 m, of which 113 563 lie in the study area;
- three explanatory factors: elevation, slope, distance to built land in 1985.

It is small enough that every tool runs in seconds to minutes, it is published with an open
tool, and an open-source comparator has already used it, which makes it common ground rather
than evoland's home turf. It replaces the earlier plan of a Swiss Arealstatistik extent; scaling
is now a separate question (see TODO).

## Protocol

The protocol is a **backcast**:

- **Calibrate** on 1985 → 1991.
- **Allocate** 1991 → 1999.
- **Validate** against the observed 1999 map, which no model or allocator can read: in evoland
  it is stored in its own run, a sibling of every simulation run.

What is held fixed across tools:

- **Inputs.** The GeoTIFFs written by `000-pie-data.r`. The grid is relabelled to exactly
  100 m, with no resampling, and set to EPSG:26986.
- **Demand.** The observed 1991 → 1999 quantities. Transition-based tools (evoland,
  Dinamica) get the counts of the five transitions that were viable in 1985 → 1991. Class-total
  tools (lulcc, CLUinPy) get the class totals of 1999, interpolated annually. The comparison is
  therefore about *where* change goes. The 8 cells of built → forest are unmodelled everywhere,
  since that transition never occurred in 1985 → 1991.
- **Patch parameters and expander fractions.** For the patch-based allocators (CLUMPY,
  Dinamica), these are evoland's estimates on 1985 → 1991.
- **Replicates.** 20 realisations per estimator × allocator pairing. The seeds are registered
  in `runs_t`, or set in the drivers.

### Estimator × allocator matrix

| estimator ↓ / allocator → | CLUMPY (evoland) | Dinamica Expander/Patcher | greedy (evoland) | CLUE-S (lulcc) | Ordered (lulcc) | CLUMondo (CLUinPy) |
| --- | --- | --- | --- | --- | --- | --- |
| logistic regression (evoland, mlr3) | 020 | 020 | 020 | | | |
| random forest (evoland, ranger) | 020 | 020 | 020 | | | |
| Weights of Evidence (Dinamica) | 040 | 030 | | | | |
| GLM class suitability (lulcc) | 040 | | | 030 | 030 | |
| logistic class suitability (CLUinPy) | 040 | | | | | 030 |

The numbers in the matrix are the pipeline steps that produce each pairing. The diagonal
pairings are each tool's own; the rest are crossings. The crossings with CLUMPY feed the other
tools' surfaces into evoland's `trans_pot_t`. For the class-suitability models (CLUE paradigm),
the potential of a transition i → k is the suitability of class k at a cell of class i.

## Pipeline

Run with `./execute-all.sh --workers 3 '2026-09-model-comparison/0*.r'` from the repo root.
Steps sharing a number are independent.

| Step | What it does |
| --- | --- |
| [`000-pie-data.r`](000-pie-data.r) | Writes `data/` (git-ignored) from `lulcc::pie` |
| [`010-evoland-calibrate.r`](010-evoland-calibrate.r) | Builds the evoland DB: domain, predictors (factors plus neighbourhood counts within 150 m and 150–500 m), held-out run, demand, allocation parameters, logistic regression and random forest; exports the potentials |
| [`020-alloc-evoland-clumpy.r`](020-alloc-evoland-clumpy.r) | CLUMPY (uPAM) on both estimators' potentials |
| [`020-alloc-evoland-dinamica.r`](020-alloc-evoland-dinamica.r) | Dinamica via `alloc_dinamica()` on the same potentials |
| [`020-alloc-evoland-greedy.r`](020-alloc-evoland-greedy.r) | evoland's deterministic rank-and-fill allocator (`alloc_greedy()`, ethzplus/evoland-plus#65) on the same potentials, one map per estimator |
| [`030-dinamica-native.r`](030-dinamica-native.r) | Standalone Dinamica: [`calibrate.ego`](030-dinamica-native/calibrate.ego) (Weights of Evidence ranges, coefficients and correlations for the three factors plus distances to classes) and [`simulate.ego`](030-dinamica-native/simulate.ego) (WoE probability maps and the `AllocateTransitions` submodel) |
| [`030-lulcc.r`](030-lulcc.r) | lulcc 1.0.4 as in its GMD demo: GLMs on the 1991 map, CLUE-S and Ordered allocation in annual steps |
| [`030-cluinpy.r`](030-cluinpy.r) | Calls [`030-cluinpy/run_pie.py`](030-cluinpy/run_pie.py): CLUinPy's logistic suitability module and CLUMondo allocation with one area service per class |
| [`040-cross-clumpy.r`](040-cross-clumpy.r) | CLUMPY on the WoE, lulcc and CLUinPy surfaces |
| [`050-compare.r`](050-compare.r) | Imports all maps as runs and computes: figure of merit (with random-allocation null), quantity/allocation disagreement, gross change, fuzzy similarity, ANOVA decomposition, timings. Writes `figures/` |
| [`051-ensemble-scores.r`](051-ensemble-scores.r) | Each ensemble as a probabilistic forecast: multi-category (fair) Brier score of per-cell class frequencies, skill against random allocation |
| [`060-fig3-matrix.r`](060-fig3-matrix.r) | Paper Fig. 3 (`figures/fig3-pie-matrix.pdf`): FoM/null over the matrix, ANOVA shares |
| [`070d-lulcc-moulds2015.r`](070d-lulcc-moulds2015.r) | Diagnostic: lulcc's GMD demo as published (1985 → 1999), reproducing Moulds et al. (2015) Fig. 8 |
| [`080-scaling/`](080-scaling/) | Scaling benchmark: `run.sh` runs each tool's step on PIE tiled k × k (0.1–4.1 M cells) under `/usr/bin/time -v`; `collect.r` tabulates and plots (`figures/scaling*.{csv,pdf}`) |

## Environment

The steps were developed in a Claude Code cloud container (Ubuntu 24.04, R 4.6.1, 4 cores,
15 GiB) against evoland-plus at the tip of the stacked fix branches
`claude/gifted-lamport-41q8gv-0{1..8}-*` (see TODO; the steps rely on 01, 03 and 07, on 06 to
replay, and on 08 for the larger scales).

- **Dinamica EGO 8.11.2.** Extracted from `ghcr.io/mmyrte/evoland:latest` instead of
  downloading the AppImage:
  `docker create --name x ghcr.io/mmyrte/evoland:latest; docker cp x:/opt/dinamica /opt/dinamica`.
  Then install the bundled R package (`/opt/dinamica/usr/bin/Data/R/Dinamica_1.0.8.tar.gz`,
  which needs `RcppProgress`) and set the variables from the `install-dinamica` vignette.
  **Do not** set `DINAMICA_EGO_8_TEMP_DIR` in `~/.Renviron`: Dinamica hands the R processes it
  spawns a per-run temp dir through that variable, and an `.Renviron` override breaks every
  `CalculateRExpression` with "Failed to send message, session is closed".
- **lulcc 1.0.4** from apt (`r-cran-lulcc`, c2d4u). It also needs `gsubfn` and `caret`, which
  it only *suggests* but calls in `ExpVarRasterList()` and `glmModels()`.
- **CLUinPy** (<https://github.com/nomisthomsen/CLUinPy>, commit `7d18dfe`). Its dependencies go
  into a venv with access to the system GDAL bindings:
  `uv venv --python /usr/bin/python3.12 --system-site-packages`, then
  `uv pip install numpy pandas scikit-learn statsmodels scipy rasterio geopandas xgboost joblib numba openpyxl`.
  Point `CLUINPY_REPO` and `CLUINPY_PYTHON` at the clone and the venv's python.
- `PIE_N_REALISATIONS` (default 20) lowers the replicate count for quick tests.

## First results (20 realisations each)

Figure of merit (FoM) is the ratio of the ensemble-mean counts. The null is the FoM expected
from placing the same quantities at random within the anterior class; persistence scores 0.
There are 4 702 observed changed cells.

| estimator × allocator | FoM | sd | FoM / null | allocation disagreement | gross change (cells) | fuzzy sim. forest→built | fair Brier skill |
| --- | --- | --- | --- | --- | --- | --- | --- |
| random forest × greedy | 0.043 | — | 2.14 | 0.075 | 4 748 | 0.231 | −0.899 |
| logistic regression × greedy | 0.038 | — | 1.92 | 0.076 | 4 748 | 0.151 | −0.915 |
| random forest × Dinamica | 0.036 | 0.0026 | 1.80 | 0.077 | 4 743 | 0.227 | −0.077 |
| logistic regression × Dinamica | 0.034 | 0.0028 | 1.70 | 0.077 | 4 743 | 0.200 | −0.099 |
| GLM suitability (lulcc) × CLUE-S | 0.033 | 0.0000 | 1.53 | 0.181 | 17 829 | 0.205 | −3.568 |
| logistic suitability (CLUinPy) × CLUMondo | 0.033 | 0.0000 | 1.86 | 0.065 | 3 248 | 0.151 | −0.637 |
| Weights of Evidence × Dinamica | 0.029 | 0.0024 | 1.49 | 0.078 | 4 743 | 0.210 | −0.089 |
| random forest × CLUMPY | 0.029 | 0.0023 | 1.47 | 0.078 | 4 754 | 0.220 | −0.005 |
| GLM suitability (lulcc) × Ordered | 0.029 | 0.0004 | 1.42 | 0.071 | 3 901 | 0.210 | −0.720 |
| logistic regression × CLUMPY | 0.026 | 0.0017 | 1.31 | 0.078 | 4 752 | 0.208 | 0.001 |
| GLM suitability (lulcc) × CLUMPY | 0.025 | 0.0018 | 1.25 | 0.079 | 4 754 | 0.208 | −0.020 |
| Weights of Evidence × CLUMPY | 0.025 | 0.0019 | 1.24 | 0.079 | 4 753 | 0.204 | 0.000 |
| logistic suitability (CLUinPy) × CLUMPY | 0.024 | 0.0024 | 1.22 | 0.079 | 4 755 | 0.212 | −0.013 |

Full tables are in [`figures/`](figures/) (`pie-*.csv`). The figures are
`figures/pie-figure-of-merit.pdf` and `figures/pie-outcome-maps.pdf`.

What these numbers say, so far:

1. **Every tool beats random allocation, none by much.** FoM is 0.024–0.035 against a null of
   ~0.020. PIE is a hard case for location: ~4 % of cells change, much of it scattered. Read
   the results as relative statements, not as skill. (`070d-lulcc-moulds2015.r` reproduces the
   multi-resolution FoM of Moulds et al. 2015, Fig. 8, within ~0.02; their run spans 1985 → 1999.)
2. **The allocator matters more than the estimator.** In the fully crossed block (logistic
   regression, random forest, WoE × CLUMPY, Dinamica), a two-way ANOVA on per-run FoM gives
   49 % of the variance to the allocator, 24 % to the estimator, 2 % to their interaction and
   25 % to the replicates. (The shares move by a few points between replays, because the
   Dinamica realisations are not seeded: 47–51 % allocator, 23–24 % estimator over four runs.) The last share is the stochastic allocation that single-map tools
   hide.
3. **Dinamica scores above CLUMPY on the same potentials, consistently.** The gain is +0.005
   to +0.008 FoM for every estimator. This is expected rather than a defect of CLUMPY: Dinamica's
   Expander/Patcher prune to the highest-probability cells (`pruneFactor`; thesis ch. 3.8),
   which approaches greedy allocation. A single-map FoM rewards the mode. CLUMPY samples in
   proportion to the potentials, which gives an unbiased ensemble at the cost of single-map FoM.
   This is the same effect as the deterministic stand-in in `2026-09-paper-figures` Fig. 2.
   **The ensemble-level score reverses the ranking** (`051-ensemble-scores.r`). It scores each
   ensemble's per-cell class frequencies as a probabilistic forecast, with the fair Brier score
   (Ferro 2014) and skill relative to random allocation of the same quantities. Scores, best
   first:

   | pairing | fair Brier skill |
   | --- | --- |
   | CLUMPY ensembles | −0.020 to +0.001 |
   | Dinamica ensembles | −0.077 to −0.099 |
   | greedy (deterministic, evoland) | −0.90, −0.92 |
   | CLUMondo (deterministic) | −0.64 |
   | Ordered (deterministic) | −0.72 |
   | CLUE-S (deterministic) | −3.6 |

   Two readings:
   - **Dinamica.** It wins on FoM by concentrating change, not by placing it more probably.
   - **PIE overall.** On this case even the best potentials are only about as good a
     probabilistic forecast as the quantities alone; the location signal in three static
     factors plus neighbourhood is weak.

   The random forest has the highest FoM but a slightly worse Brier score than the logistic
   regression: calibration matters for an unbiased sampler.
   **The greedy allocator completes the picture** (`020-alloc-evoland-greedy.r`). On the same
   potentials, evoland's deterministic rank-and-fill reaches the highest FoM of all pairings
   (0.038 and 0.043; 1.9 and 2.1 × random), and nearly the worst ensemble score (fair Brier
   skill −0.90): FoM rises with greediness (CLUMPY < Dinamica < greedy) while the probabilistic
   score falls in the same order.
4. **Estimators.** The random forest is best under both allocators. evoland's logistic
   regression beats Dinamica's Weights of Evidence under both allocators. The two models get
   the same information except that evoland uses neighbourhood counts where WoE uses distance
   maps; WoE additionally assumes conditional independence between variables. The class-
   suitability surfaces (lulcc, CLUinPy) are the weakest *transition* estimators when CLUMPY
   allocates them, as expected: they describe where a class is, not where it appears.
5. **CLUE-family behaviour is different in kind, and FoM alone hides it.**
   - **CLUE-S (lulcc defaults: elasticity 0.2, all conversions allowed).** It matches the class
     totals but changes 17 829 cells, 3.8× the observed change. That includes 2 614 cells of
     built → forest, against 8 observed. FoM rewards the extra hits that so much change buys,
     so CLUE-S ranks second by FoM while having by far the worst allocation disagreement.
   - **CLUMondo (CLUinPy) and Ordered (lulcc).** They do the opposite: they produce only net
     change (3 248 and 3 901 cells). They are deterministic in practice (sd 0); CLUinPy's random
     seed only changes the convergence speed. This is why "estimator × allocator" is a cleaner
     framing than tool-vs-tool: the CLUE family answers a different question (class totals) than
     the transition-based tools.
6. **Cost is not a differentiator on this case** (but see Scaling below for larger domains).

   | step | median time | note |
   | --- | --- | --- |
   | Dinamica native allocation | 0.9 s | single-threaded, see Replay check |
   | Dinamica WoE calibration | 4.8 s | single-threaded |
   | CLUMPY allocation | 2.1 s | |
   | CLUinPy allocation | 5.7 s | 8 annual steps |
   | lulcc CLUE-S allocation | 4.2 s | 8 annual steps |
   | lulcc Ordered allocation | 2.3 s | 8 annual steps |
   | evoland → Dinamica allocation | 5.2 s | including writing the inputs and an R round trip inside Dinamica |
   | random forest fit | 41 s | all 5 transitions |
   | random forest prediction | 27 s | all 5 transitions |

   Scaling to millions of cells is where the tools may separate; PIE does not test it.

## Scaling (080-scaling)

**Set-up.**
- **Domain.** PIE tiled k × k with mirroring (k = 1, 2, 3, 4, 6): 0.11, 0.45, 1.0, 1.8 and
  4.1 M cells. k = 6 is about the size of the Swiss 100 m grid; tiling keeps class shares and
  patch statistics.
- **What runs.** One realisation per tool and the logistic regression only, each step its own
  process.
- **Machine.** 4 cores, 16 GiB, R's vector heap capped at 12 GiB (`R_MAX_VSIZE`).
- **Outputs.** `figures/scaling-steps.csv`, `figures/scaling-stages.csv`, `figures/scaling.pdf`.

| step | 0.11 M | 0.45 M | 1.0 M | 1.8 M | 4.1 M cells |
| --- | --- | --- | --- | --- | --- |
| evoland calibration (set-up, neighbours, fit, predict) | 45 s, 2.0 GB | 106 s, 6.7 GB | 204 s, 10.1 GB | **out of memory** (R heap 12 GB) | **OOM-killed** (14 GB) |
| ↳ with streamed neighbours (ethzplus/evoland-plus#66) | | | 198 s, 3.7 GB | 290 s, 5.6 GB | 599 s, 9.8 GB |
| evoland CLUMPY allocation | 10 s, 0.6 GB | 13 s, 0.9 GB | 24 s, 1.5 GB | 34 s, 2.4 GB | 61 s, 4.0 GB |
| evoland Dinamica allocation | 25 s, 0.9 GB | 17 s, 1.4 GB | 30 s, 2.7 GB | 43 s, 4.4 GB | 79 s, 9.5 GB |
| Dinamica standalone (WoE calibration + allocation) | 11 s | 18 s | 34 s | 44 s | 88 s, **0.28 GB** |
| lulcc (GLM, CLUE-S, Ordered) | 30 s, 0.8 GB | 60 s, 1.2 GB | 103 s, 1.9 GB | 658 s, 3.4 GB | 1 572 s, 6.6 GB |
| CLUinPy (suitability, CLUMondo, 8 years) | 81 s, 0.4 GB | 66 s, 0.5 GB | 85 s, 0.6 GB | 172 s, 0.7 GB | 396 s, 1.3 GB |

Memory is the peak of the largest process. For standalone Dinamica that is the R driver up to
1 M cells; `DinamicaConsole` itself, measured directly at 4.1 M cells, peaks at 150 MB
(calibration) and 280 MB (allocation).

**Where the time goes.** Per-stage timings, in `scaling-stages.csv`:

- **evoland.** Roughly linear in time. At 1 M cells: neighbours 80 s, logistic fit 36 s,
  prediction 36 s, CLUMPY allocation 10 s, patch statistics 3.5 s.
- **Dinamica.** Weights-of-Evidence calibration dominates (64 s of 76 s at 4.1 M, run
  single-threaded because of the race in the probability map). In parallel, the calibration
  takes 23 s instead of 70 s. Calibration is deterministic, so only `probabilities.ego` and
  `simulate.ego` need to run single-threaded.
- **CLUinPy.** Linear; the 8 annual allocation steps dominate.
- **lulcc.** The Ordered model is linear (24 s at 1.8 M). CLUE-S jumps from 14 s (1.0 M) to
  514 s (1.8 M), and at 1.8 M every one of the 8 annual steps hit `max.iter` without converging.
  Its tolerance (`max.diff = 50` cells) and update step (`diff × scale.f`) are absolute cell
  counts. The demo's values therefore get stricter and more oscillatory as the domain grows.
  (Its "average difference" criterion also sums *signed* differences, so it is nearly always
  met.) lulcc's GLM fit is cheap; its memory grows to 6.6 GB at 4.1 M (raster package, in RAM).

**evoland: what limits it.**

1. **The neighbourhood table, not DuckLake — fixed.** `set_neighbors()` used to build every
   (cell, neighbour) pair within `max_distance` as one R data.table (79 pairs per cell at 500 m:
   17 M rows at k = 1, ~615 M at k = 6), then key, `cut()` and commit it. Now
   (ethzplus/evoland-plus#66) the C++ hash-index search hands chunks of complete neighbourhoods
   to a callback that commits them into DuckLake as it goes, so memory is bounded by the chunk.
   At 1 M cells the calibration peak fell from 10.1 to 3.7 GB, and 1.8 M and 4.1 M cells now run
   on 16 GB (4.1 M: 10 min calibration, 1 min CLUMPY allocation). The relational table stays,
   with it the independence from a regular tiling (hexagons, irregular tracts), which a ring
   convolution would have given up. At 4.1 M the remaining peak (9.8 GB) is before the
   neighbour step, in this pipeline's own ingestion of the full rectangle into R, which the
   pruning below addresses.
2. **`coords_t` covers the whole rectangle.** PIE's study area is 53 % of its bounding box.
   `010` now restricts `coords_t` to cells with land use after ingestion, as
   `2026-05-ssp-ch/010-ingest-lulc-data.qmd` does (the scaling numbers above predate that).
3. **DuckLake.** Every catalog-resolving call costs a fixed 20–70 ms: `get_read_expr()`, the
   table bindings, `.has_predictions()`. A plain DuckDB query costs 1 ms. On PIE that is 75 % of a
   prediction (11 s, of which the model's `predict()` takes 1.7 s), but the cost does not grow with
   the domain. The data-bound work in DuckDB (the `pred_data_wide_v()` PIVOT, the joins) scales
   linearly here. Two side effects:
   - every realisation stores a full map (`lulc_data_t` held 25 M rows after the benchmark);
   - every commit makes a snapshot (283 after the benchmark).
4. Rasterising tables (`tabular_to_raster()`) is not a bottleneck: 0.17 s per call at 113 k
   cells once terra is loaded. (A cell-index rewrite was tried and was slower, so it was dropped.)

## Reproducibility in different environments

The argument here doesn't run along Mazy/Longaretti's statistical argument that the other
allocation routines are biased; it is about usefulness. What running each tool headless on PIE
actually took:

- **CLUE family.** The CLUE family takes in absolute class suitability, not transition
  potential: confirmed for lulcc and CLUinPy (see the matrix above). This is a different
  modelling paradigm.
  - **lulcc.** Installs from CRAN or apt, but depends on retired packages (`raster`, `sp`).
    It *suggests* packages that its constructors require (`gsubfn`, `caret`). Its demo
    parameters produce the excessive gross change shown above.
  - **CLUinPy.** Configured by a text file plus four Excel workbooks with positional
    conventions (classes 0..N−1, band order = class order, first column = labels). It does not
    pin dependencies, and needs system GDAL bindings. Its documented convergence tolerance
    (2 %) left ~700 built cells unallocated here, so this benchmark uses 0.1 %. Its only
    randomness is unseeded `random.random()`, so exact replay requires seeding from the
    outside, as `run_pie.py` does.
  - **lulcc's Ordered model** (Fuchs et al. 2013, `lulcc:::.ordered`) is greedy rank-and-fill
    with competition settled by a fixed class priority (`order = c(2, 1, 3)`: built, forest,
    other). Each class in turn:
    - **if its demand rises**, it ranks every cell not yet in the class by the class's
      suitability and takes the top n;
    - **if its demand falls**, it releases its n least suitable cells.

    Claimed cells are removed from later classes, which settles the competition. There are no
    transitions: built land is taken from forest or other alike, purely by built suitability.
    `stochastic = TRUE` thins the ranked candidates by a Bernoulli(suitability) draw before taking
    the top n. High-suitability cells survive almost always, so it stays nearly deterministic
    (FoM sd 0.0004 here). evoland now has the same allocator, on transitions instead of classes:
    `alloc_greedy(arbitration = "ordered", order = ...)`, next to a `"joint"` mode that ranks all
    transitions together by adjusted potential (ethzplus/evoland-plus#65).
  - DynaCLUE: C++ code that is hard to compile; not attempted.
- **Dinamica EGO.**
  - **Running it.** It runs fully headless from the console. The `.ego` scripts are
    plain-text, diffable artifacts (see `030-dinamica-native/`). However:
    - the script syntax is documented only on dinamicaego.com/csr.ufmg.br (unreachable from
      the cloud container, so the WoE skeleton syntax was found by probing the parser);
    - R integration relies on a file-queue IPC that is sensitive to environment variables;
    - the allocator is biased by pruning (thesis ch. 3.8);
    - the licence is restrictive and the code is not reachable.

    The Dinamica virtual machine is a very advanced piece of technology, but it does not have
    the community effects of an R or Python package.
  - **Exact replay.** Only single-threaded and with `-predefined-seed` (see Replay check:
    WoE probability maps differ between parallel runs, and without `-predefined-seed` every run
    draws a fresh seed).
  - **Auditability.** Medium to high.
- **IDRISI TerrSet liberaGIS LCM.** Not attempted. It is GUI-driven and Windows-only, and its
  licence forbids reverse engineering; Mazy et al. have reproduced the LCM logic. It is useful
  only as a widely cited black-box comparator.
- **CLUMPY.** Python scripting; exact replay is realistic if dependencies and RNG state are
  pinned. It serves as the experimental reference implementation, and evoland includes its
  allocator.
- **R lulcc.** R scripting; exact replay is realistic with package lockfiles and seed control.
  It is the strongest statistical and validation workbench of the open tools, but unmaintained.

## Replay check

The whole pipeline was run twice from scratch (`execute-all.sh`, fresh database), and the
per-run FoM of the two runs compared:

- **Bit-identical:**
  - evoland logistic regression × CLUMPY;
  - lulcc CLUE-S and Ordered;
  - CLUinPy (seeded through Python's `random`);
  - CLUMPY on the lulcc and CLUinPy surfaces.
- **Different, by design:** every Dinamica allocation. Dinamica draws a fresh seed per run
  unless `-predefined-seed` is given, and that seeds every run identically, so an ensemble of
  distinct but replayable realisations is not available from the console.
- **Different, by mistake, now fixed:**
  - **Random forest.** `set.seed()` did not make it replay, and neither did ranger's own
    `seed`. The cause was in evoland: `fit_full_models()` read its training rows in whatever
    order DuckDB returned them, and a seeded random forest fits a different model on reordered
    rows. Fixed upstream (stacked branch 06); with that and the learner seed, the potentials of
    two runs of `010` are bit-identical.
  - **Weights of Evidence × CLUMPY.** It differed for two reasons:
    1. *Corroded probabilities.* The WoE probabilities were saved after `AllocateTransitions`,
       which depletes ("corrodes") the probability map in place, so the crossing was fed one
       realisation's leftover probabilities. They now come from `probabilities.ego`.
    2. *A parallel race in Dinamica.* `CalcWOfEProbabilityMap` in Dinamica 8.11.2 is **not
       deterministic when run in parallel**: between identical runs, the probabilities of the
       forest transitions differed in 300–1 300 cells, some turning NA. Calibration and distance
       maps were identical, and single-threaded runs (`-processors=1
       -disable-parallel-functors -disable-parallel-map-load`) are bit-identical. The
       standalone Dinamica steps now run single-threaded. (Worth reporting to the Dinamica team,
       and worth a sentence in the paper: Dinamica's default settings do not replay.)

## Publication venue

Go with Geoscientific Model Development:
- follow <https://www.geoscientific-model-development.net/submission.html> and
  <https://www.geoscientific-model-development.net/about/manuscript_types.html>;
- take inspiration at least from Moulds' lulcc paper: <https://doi.org/10.5194/gmd-8-3215-2015>.
  This benchmark is now the same case as that paper, which makes the comparison direct;
- check for other land use change models at
  <https://gmd.copernicus.org/model_description_paper.html> (most seem to relate to land surface
  modelling at roughly continental or global scales).

Keep Environmental Modelling & Software as backup; if two options are equally desirable for GMD,
EMS should tip the scales in favour of either one of them.
- Follow <https://www.sciencedirect.com/journal/environmental-modelling-and-software/publish/guide-for-authors>.
- Take structural inspiration from papers like iCLUE (<https://doi.org/10.1016/j.envsoft.2018.07.010>)
  or CLUinPy (<https://doi.org/10.1016/j.envsoft.2026.106917>).
