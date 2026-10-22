# Model comparison (2026-09) — the Plum Island Ecosystems benchmark

**Status:** active. The pipeline runs end to end in about 45 minutes on 4 cores. Results are
below; [`TODO.md`](TODO.md) holds the open work.

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
- **Demand.** The observed 1991 → 1999 quantities, prescribed, and allocated in one step by
  every tool. Transition-based tools (evoland, Dinamica) get the counts of the five transitions
  that were viable in 1985 → 1991. Class-total tools (lulcc, CLUinPy) get the observed class
  totals of 1999. The comparison is
  therefore about *where* change goes. The 8 cells of built → forest are unmodelled everywhere,
  since that transition never occurred in 1985 → 1991.
- **Patch parameters and expander fractions.** For the patch-based allocators (CLUMPY,
  Dinamica), these are evoland's estimates on 1985 → 1991.
- **Replicates.** 20 realisations per estimator × allocator pairing. The seeds are registered
  in `runs_t`, or set in the drivers.

### Estimator × allocator crossing

Every estimator is paired with every allocator: 5 × 6 pairings, 20 realisations each (one for
the deterministic greedy allocator).

| | estimator | from |
| --- | --- | --- |
| rows | logistic regression | evoland, `mlr3::lrn("classif.log_reg")` |
| | random forest | evoland, `classif.ranger` (500 trees, min node 50) |
| | naive Bayes | evoland, `classif.naive_bayes` (e1071, Gaussian) |
| | Weights of Evidence | Dinamica EGO, `015-dinamica-woe/` |
| | Bayes-eKDE | clumpy (mmyrte/clumpy): kernel densities (`ekde`) per final class, Bayes' rule; `015-clumpy-kde/` |

| | allocator | from | allocates |
| --- | --- | --- | --- |
| columns | CLUMPY (uPAM) | evoland | transitions, patches, one step |
| | Dinamica Expander/Patcher | Dinamica EGO, driven by evoland's `alloc_dinamica()` | transitions, patches, one step |
| | greedy rank-and-fill | evoland `alloc_greedy()` | transitions, cells, one step |
| | CLUE-S | lulcc | class totals, one step |
| | Ordered | lulcc | class totals, one step |
| | CLUMondo | CLUinPy | class totals, one step |

How the decoupling works:

- **One surface per estimator.** evoland's learners write transition potentials into their run
  (`010`). Dinamica's Weights of Evidence probability maps are written into run 3000 the same
  way (`020`). The three evoland allocators read every estimator alike.
- **Class-total allocators** (CLUE-S, Ordered, CLUMondo) take a suitability per class and
  cell. They get each estimator's *class probability map*: from a cell of class i, the
  adjusted potential of i → k for k ≠ i (scaled to the demanded rate and row-closed by
  `adjusted_trans_pot_v()`), and one minus their sum for staying in i (`020`). lulcc gets
  these through its own model interface: the explanatory variable of class k is the logit of
  its probability, and its model a binomial GLM fixed at coefficients (0, 1), so lulcc's
  `predict()` returns the estimator's probabilities exactly. CLUinPy reads them as its
  suitability stack.
- **Conversions.** In the crossing, only the transitions viable in 1985 → 1991 are allowed
  for every allocator (lulcc's rules matrix, CLUinPy's allow matrix).
- **Each tool as shipped** stays as a reference outside the crossing: lulcc's GLMs and
  CLUinPy's logistic regression are class-presence models on the three factors (the CLUE
  paradigm, and the same model family as evoland's logistic regression), with all conversions
  allowed; standalone Dinamica allocates its WoE probabilities with its own `simulate.ego`.
  Earlier versions of this benchmark had these as separate estimator rows; under different
  names they are the same logistic regression, which says nothing about the estimator.
  Like evoland's learners, they are fitted on every cell, not on the samples their demos draw
  (lulcc 10 % with the rest held out for ROC; CLUinPy 10 % per class capped at 500 cells, with
  30 % of those held out). The comparison is between paradigms, presence on one map against
  transitions between two, not between sample sizes; `070d` shows separately that lulcc as
  published reproduces. (A refit on all cells barely changes lulcc's ranking of cells:
  Spearman ≥ 0.995 for every class against 10 different 10 % samples.)
- **What stays with the estimator.** Weights of Evidence uses Dinamica's distance-to-class maps
  where evoland's learners use neighbourhood counts; naive Bayes is the same model family as
  Weights of Evidence (additive log-odds per predictor, conditional independence), with a
  Gaussian density in place of binned ranges, on evoland's predictors. The naive Bayes – WoE
  pair separates the model from Dinamica's feature engineering.
- **What stays with the allocator.** The allocators' own
  parameters (patch parameters for CLUMPY and Dinamica, evoland's estimates; elasticities and
  iteration settings for CLUE-S; resistance and neighbourhood weights for CLUMondo), and
  demand as transition counts or class totals (the same observed 1999 quantities).

Run ids (`common.r`): estimator 1000 (logistic regression), 2000 (random forest), 3000 (WoE),
4000 (naive Bayes), 5000 (Bayes-eKDE); realisation `estimator + allocator offset + member`, with offsets CLUMPY 100,
Dinamica 200, greedy 300, CLUE-S 400, Ordered 500, CLUMondo 600. As shipped: lulcc 7401…/7501…,
CLUinPy 8601…, standalone Dinamica 3701….

## Pipeline

Run with `./execute-all.sh --workers 3 '2026-09-model-comparison/0*.r'` from the repo root.
Steps sharing a number are independent.

| Step | What it does |
| --- | --- |
| [`000-pie-data.r`](000-pie-data.r) | Writes `data/` (git-ignored) from `lulcc::pie`, and the demand tables |
| [`010-evoland-calibrate.r`](010-evoland-calibrate.r) | Builds the evoland DB: domain, predictors (factors plus neighbourhood counts within 150 m and 150–500 m), held-out run, demand, allocation parameters, every run of the crossing; fits the logistic regression, random forest and naive Bayes and predicts their potentials |
| [`015-clumpy-kde.r`](015-clumpy-kde.r) | clumpy's Bayes-eKDE estimator on evoland's predictors, through [`015-clumpy-kde/fit_predict.py`](015-clumpy-kde/fit_predict.py); potentials into run 5000 |
| [`015-dinamica-woe.r`](015-dinamica-woe.r) | Standalone Dinamica: [`calibrate.ego`](015-dinamica-woe/calibrate.ego) (Weights of Evidence on the three factors plus distances to classes), [`probabilities.ego`](015-dinamica-woe/probabilities.ego) (the WoE estimator's probability maps) and, as shipped, [`simulate.ego`](015-dinamica-woe/simulate.ego) (`AllocateTransitions`) |
| [`020-potentials.r`](020-potentials.r) | WoE probabilities into run 3000's `trans_pot_t`; every estimator's class probability map for the class-total allocators |
| [`030-alloc-evoland-clumpy.r`](030-alloc-evoland-clumpy.r) | CLUMPY (uPAM) on every estimator |
| [`030-alloc-evoland-dinamica.r`](030-alloc-evoland-dinamica.r) | Dinamica via `alloc_dinamica()` on every estimator |
| [`030-alloc-evoland-greedy.r`](030-alloc-evoland-greedy.r) | evoland's deterministic rank-and-fill (`alloc_greedy()`, joint ranking) on every estimator |
| [`040-alloc-lulcc.r`](040-alloc-lulcc.r) | lulcc 1.0.4: CLUE-S and Ordered on every estimator's class probabilities; as shipped, its GLMs on the 1991 map |
| [`040-alloc-cluinpy.r`](040-alloc-cluinpy.r) | Calls [`040-alloc-cluinpy/run_pie.py`](040-alloc-cluinpy/run_pie.py): CLUMondo on every estimator's class probabilities; as shipped, CLUinPy's logistic suitability module |
| [`050-compare.r`](050-compare.r) | Imports the maps made outside the DB as runs and computes: figure of merit (with random-allocation null), quantity/allocation disagreement and simulated transitions (`lulc_crosstab_v()`), fuzzy similarity, ANOVA decomposition, timings. Writes `figures/` |
| [`051-ensemble-scores.r`](051-ensemble-scores.r) | Each ensemble as a probabilistic forecast: multi-category (fair) Brier score of per-cell class frequencies, skill against random allocation |
| [`052-check-greedy-ordered.r`](052-check-greedy-ordered.r) | Check: evoland's `alloc_greedy(arbitration = "ordered")` against lulcc's Ordered model, cell by cell |
| [`060-fig3-matrix.r`](060-fig3-matrix.r) | Paper Fig. 3 (`figures/fig3-pie-matrix.pdf`): FoM/null over the crossing and the tools as shipped, ANOVA shares |
| [`070d-lulcc-moulds2015.r`](070d-lulcc-moulds2015.r) | Diagnostic: lulcc's GMD demo as published (1985 → 1999), reproducing Moulds et al. (2015) Fig. 8 |
| [`080-scaling/`](080-scaling/) | Scaling benchmark: `run.sh` runs each step on PIE tiled k × k (0.1–4.1 M cells) under `/usr/bin/time -v`; `collect.r` tabulates and plots (`figures/scaling*.{csv,pdf}`) |

## Environment

The steps were developed in a Claude Code cloud container (Ubuntu 24.04, R 4.6.1, 4 cores,
15 GiB) against evoland-plus `develop` at `08aa38e`, as pinned in `rproject.toml` (it carries
the stacked fixes ethzplus/evoland-plus#59–#66, `lulc_crosstab_v()` and `get_read_expr(id_run =)`).

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
- **CLUinPy** (<https://github.com/nomisthomsen/CLUinPy>, commit `7d18dfe`). Its dependencies are
  the repository's uv environment (`pyproject.toml`): `uv sync` at the repository root makes
  `.venv`, building the GDAL bindings against the system GDAL (needs `libgdal-dev` 3.8.4).
  CLUinPy itself is not an installable package (its `setup.py` packages nothing), so
  `python-env.r` clones it at the pinned commit into `.cache/CLUinPy` on first use. The R steps
  call the project's python (`.venv`, or `UV_PROJECT_ENVIRONMENT`); no environment variables
  are needed.
- **naive Bayes** needs `e1071` (an mlr3learners suggestion).
- **clumpy** (mmyrte/clumpy at `e9f73b9`, with its `ekde` and `hyperclip` extensions) comes
  from the same uv environment; its Cython extensions need NumPy 1.x, so the environment pins
  `numpy<2`.
- `PIE_N_REALISATIONS` (default 20) lowers the replicate count for quick tests;
  `PIE_ESTIMATORS` (default `log_reg,ranger,woe,naive_bayes,kde`) restricts the estimators.

## Results: the 5 × 6 crossing (2026-10-22)

Figure of merit (FoM) is the ratio of the ensemble-mean counts. The null is the FoM expected
from placing the same quantities at random within the anterior class (about 0.020);
persistence scores 0. There are 4 702 observed changed cells. Greedy and Ordered (in the
crossing) are deterministic, one map each; all other ensembles have 20 realisations.

**FoM by pairing** (`figures/pie-fom-per-run.csv`, Fig. 3):

| estimator ↓ / allocator → | CLUMPY | Dinamica | greedy | CLUE-S | Ordered | CLUMondo |
| --- | --- | --- | --- | --- | --- | --- |
| random forest | 0.029 | 0.034 | 0.043 | 0.042 | **0.045** | 0.041 |
| logistic regression | 0.026 | 0.034 | 0.038 | 0.039 | 0.039 | 0.037 |
| Bayes-eKDE (clumpy) | 0.029 | 0.032 | 0.036 | 0.035 | 0.034 | 0.035 |
| naive Bayes | **0.031** | 0.032 | 0.034 | 0.036 | 0.033 | 0.033 |
| Weights of Evidence | 0.025 | 0.030 | 0.030 | 0.029 | 0.030 | 0.027 |

**Each tool as shipped**, for reference: lulcc GLM × CLUE-S 0.032, lulcc GLM × Ordered 0.029,
CLUinPy logistic × CLUMondo 0.034, Dinamica WoE × Dinamica (`simulate.ego`) 0.030.

**Other scores** (`figures/pie-ensemble-summary.csv`, `pie-ensemble-brier.csv`), ranges over the
five estimators:

| allocator | gross change (cells) | allocation disagreement | fair Brier skill |
| --- | --- | --- | --- |
| CLUMPY | 4 753 | 0.077–0.079 | −0.03 to +0.00 |
| Dinamica | 4 743 | 0.077–0.078 | −0.18 to −0.06 |
| greedy | 4 748 | 0.075–0.078 | −0.96 to −0.90 |
| CLUE-S | 3 000–3 130 | 0.062–0.063 | −0.60 to −0.57 |
| Ordered | 3 130–4 010 | 0.063–0.071 | −0.79 to −0.58 |
| CLUMondo | 3 030–3 120 | 0.061–0.064 | −0.63 to −0.58 |
| *lulcc GLM × CLUE-S, as shipped* | *15 883* | *0.166* | *−3.18* |

**Variance of per-run FoM** over the crossing of the stochastic allocators (5 estimators ×
CLUMPY, Dinamica, CLUE-S, CLUMondo; two-way ANOVA): estimator 36 %, allocator 39 %,
interaction 15 %, replicates 9 % (with the four estimators before Bayes-eKDE: 42, 37, 13 and
8 %).

What these numbers say:

1. **Every pairing beats random allocation, none by much**: FoM 0.025–0.045, 1.2–2.5 × the
   null. PIE is hard for location: about 4 % of cells change, much of it scattered.
   (`070d-lulcc-moulds2015.r` reproduces the multi-resolution FoM of Moulds et al. 2015, Fig. 8,
   within ~0.02; their run spans 1985 → 1999.)
2. **With the estimators decoupled, the estimator matters as much as the allocator.** The
   ranking of the estimators is nearly the same under five of the six allocators: random
   forest > logistic regression > Bayes-eKDE ≈ naive Bayes > Weights of Evidence. The
   exception is CLUMPY, under which naive Bayes comes first and logistic regression drops to
   fourth; that is most of the 15 % interaction. A plausible reading, not
   yet checked: CLUMPY samples in proportion to the potentials, so sharper (overconfident)
   probabilities, which naive Bayes tends to produce, concentrate its samples on the likely
   cells, while the rank-based allocators only use the order. (The earlier 3 × 2 block, CLUMPY and Dinamica only, gave
   the allocator 49 % and the estimator 24 %: with only the two patch allocators, the estimators
   looked closer than they are.)
3. **The tools' own estimators were the weak part, not their allocators.** Fed the random
   forest's class probabilities, lulcc's CLUE-S and Ordered and CLUinPy's CLUMondo reach the
   highest FoM of the whole matrix (0.041–0.045), above the transition allocators. As shipped,
   with their class-presence logistic regressions, the same allocators score 0.029–0.034. The
   logistic regression row makes the same point within one model family: evoland's logistic
   regression on transitions, with neighbourhood counts, beats the presence models of lulcc and
   CLUinPy (also logistic regressions, on the three factors alone) under their own allocators
   (0.039 vs. 0.032 for CLUE-S, 0.039 vs. 0.029 for Ordered, 0.037 vs. 0.034 for CLUMondo).
4. **CLUE-S's excess change came from its inputs.** As shipped (presence GLMs, all conversions
   allowed, elasticity 0.2), CLUE-S changes 15 883 cells, 3.4× the observed change, including
   2 410 cells of built → forest. With an estimator's class probabilities and the viable
   transitions, the same allocator with the same parameters changes 3 000–3 130 cells.
5. **Class-total allocators produce net change, and FoM rewards it.** CLUE-S, Ordered and
   CLUMondo convert other and forest to built and little else (gross change mostly 3 000–3 130
   cells, against 4 702 observed): the swaps between forest and other, which every estimator predicts
   poorly, are left out, and so are their false alarms. The transition allocators place every
   viable transition at its observed count and pay for the swaps. The class-total maps also have
   the lowest allocation disagreement (0.062–0.064).
6. **Scored as probabilistic forecasts, the order reverses**, as in the earlier rounds
   (`051-ensemble-scores.r`: fair Brier skill against random allocation of the same quantities).
   CLUMPY ensembles are about as good a forecast as the quantities alone (−0.03 to 0.00), Dinamica
   ensembles slightly worse (−0.18 to −0.06), and the near-deterministic maps far worse: CLUE-S,
   Ordered and CLUMondo −0.79 to −0.57, greedy −0.96 to −0.90, CLUE-S as shipped −3.2. FoM rises
   with greediness (CLUMPY < Dinamica < greedy, class totals) while the probabilistic score
   falls in the same order. The random forest has the highest FoM but a slightly worse Brier
   score than the logistic regression under CLUMPY: calibration matters for an unbiased
   sampler.
7. **Naive Bayes and Weights of Evidence**, the same model family (additive log-odds per
   predictor, conditional independence): naive Bayes on evoland's predictors beats Dinamica's
   Weights of Evidence on its distance maps under every allocator (0.031–0.036 vs. 0.025–0.030).
   The two differ in predictors (neighbourhood counts vs. distances) and in density (Gaussian vs.
   binned ranges); separating those is in TODO.
8. **Dinamica driven by evoland and Dinamica as shipped agree**: Weights of Evidence × Dinamica
   0.0299 through `alloc_dinamica()`, 0.0300 through the standalone `simulate.ego`, with
   identical transition counts. (Both moved by ~0.001 from the previous run: Dinamica draws a
   fresh seed per run, see Replay check.)
10. **clumpy's Bayes-eKDE**, the density-ratio estimator of the reference clumpy, on the same
    nine predictors as the evoland learners: FoM 0.029–0.036, between the logistic regression
    and naive Bayes, and second only to naive Bayes under CLUMPY (0.029, tied with the random
    forest). It is the one non-parametric estimator besides the random forest, and like it, it
    is not calibrated by a likelihood: its fair Brier skill under CLUMPY is −0.03, against
    0.00 for the logistic regression. Under lulcc's Ordered model it shares 92 % of greedy's
    built gains, the most of any estimator (`052`). Fit and prediction take 2.5 min for the
    three anterior classes (ekde in nine dimensions; 47 k cells for forest).
9. **Cost is not a differentiator at PIE's size** (but see Scaling). Median times per
   allocation: CLUMPY 1.7 s, greedy 1.1 s, Dinamica via evoland 5.1 s, Dinamica standalone
   0.4 s, lulcc Ordered 0.9 s, CLUinPy 1.3 s, lulcc CLUE-S 1.5 s. Fits: logistic regression
   5.4 s, naive Bayes 3.4 s, random forest 50 s, Bayes-eKDE 150 s.

### Where the crossing needed care

- **lulcc**: suitability enters as the logit of the class probability, through a GLM with
  coefficients fixed at (0, 1), so lulcc's own `predict()` returns the estimator's
  probability; checked to 1e-5. Its stochastic Ordered mode corrupts the map on small
  suitabilities (TODO, upstream issues), so Ordered runs deterministically in the crossing.
- **CLUinPy**: CLUMondo needs up to ~1 600 demand iterations on these surfaces (limit 30 000),
  and the binned WoE surface oscillates around a 0.25 % demand gap, so the crossing's
  tolerance is 0.5 % (0.1 % as shipped).
- **One step.** Every allocator goes from 1991 to 1999 in one step with the prescribed 1999
  quantities. An earlier version had the class-total tools take 8 annual steps on linearly
  interpolated totals. With static suitabilities that changes little: Ordered gives the same
  maps (ranking by a fixed surface and filling in 8 instalments takes the same cells as
  filling at once), CLUE-S maps differ by ~140 cells, CLUMondo's by its tolerance, and no
  ensemble's FoM moved by more than 0.0013.

## Scaling (080-scaling)

**Set-up.**
- **Domain.** PIE tiled k × k with mirroring (k = 1, 2, 3, 4, 6): 0.11, 0.45, 1.0, 1.8 and
  4.1 M cells. k = 6 is about the size of the Swiss 100 m grid; tiling keeps class shares and
  patch statistics.
- **What runs.** One realisation per tool and the logistic regression only, each step its own
  process.
- **Machine.** 4 cores, 16 GiB, R's vector heap capped at 12 GiB (`R_MAX_VSIZE`).
- **Outputs.** `figures/scaling-steps.csv`, `figures/scaling-stages.csv`, `figures/scaling.pdf`.
- **When.** 2026-10-06, on the stacked evoland branches with streamed neighbours
  (ethzplus/evoland-plus#66, now the default on develop) and `coords_t` pruned, before the
  crossing and the pin to `08aa38e`. To be rerun on the current pin (TODO). Before streaming,
  the calibration ran out of memory from 1.8 M cells (10.1 GB at 1.0 M cells).

| step | 0.11 M | 0.45 M | 1.0 M | 1.8 M | 4.1 M cells |
| --- | --- | --- | --- | --- | --- |
| evoland calibration (set-up, neighbours, fit, predict) | 37 s, 1.5 GB | 92 s, 2.6 GB | 141 s, 3.4 GB | 226 s, 5.3 GB | 424 s, 9.3 GB |
| evoland CLUMPY allocation (same) | 10 s, 0.6 GB | 17 s, 1.1 GB | 22 s, 1.4 GB | 36 s, 2.3 GB | 49 s, 3.3 GB |
| evoland Dinamica allocation (same) | 13 s, 0.7 GB | 22 s, 1.2 GB | 27 s, 2.2 GB | 41 s, 2.8 GB | 56 s, 5.8 GB |
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
   on 16 GB (4.1 M unpruned: 10 min calibration, 1 min CLUMPY allocation). The relational table stays,
   with it the independence from a regular tiling (hexagons, irregular tracts), which a ring
   convolution would have given up.
2. **`coords_t` covers the whole rectangle.** PIE's study area is 53 % of its bounding box.
   `010` now restricts `coords_t` to cells with land use after ingestion, as
   `2026-05-ssp-ch/010-ingest-lulc-data.qmd` does. Together with the streaming this brings 4.1 M
   cells to 7 min and 9.3 GB (neighbours 96 s instead of 177 s unpruned). The remaining peak is
   this pipeline's ingestion of the full rectangle into R before the pruning, i.e. the script,
   not evoland; the results of the benchmark itself are bit-identical with and without pruning.
3. **DuckLake** (ethzplus/evoland-plus#85). Every catalog-resolving call costs a fixed
   20–70 ms: `get_read_expr()`, the table bindings, `.has_predictions()`. A plain DuckDB query
   costs 1 ms. On PIE that is 75 % of a prediction (11 s, of which the model's `predict()` takes
   1.7 s), but the cost does not grow with the domain. The data-bound work in DuckDB (the `pred_data_wide_v()` PIVOT, the joins) scales
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
    It *suggests* packages that its constructors require (`gsubfn`, `caret`). With its own
    GLMs and the demo parameters it produces the excessive gross change above; fed an
    estimator's class probabilities and the viable transitions, it does not (finding 4). Its
    stochastic Ordered mode corrupts maps when suitabilities are small (TODO).
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
    (FoM sd 0.0004 here). evoland has the same procedure on transitions instead of classes:
    `alloc_greedy(arbitration = "ordered", order = ...)`, next to a `"joint"` mode that ranks all
    transitions together by adjusted potential (ethzplus/evoland-plus#65).

    **Check** (`052-check-greedy-ordered.r`, `figures/pie-greedy-ordered-vs-lulcc.csv`): with the
    same estimator and priority (transitions into built, then forest, then other), the two do
    the same where they fill the same demand. They place built land alike: 81–86 % of the cells
    that greedy turns built are also built in lulcc's map. FoM is within 0.002 (logistic
    regression 0.0392 vs. 0.0392, random forest 0.043 vs. 0.045, WoE 0.0298 vs. 0.0297, naive
    Bayes 0.033 vs. 0.033), and 98 % of all cells agree. They differ in the demand they fill:
    greedy changes the 4 748 cells of the observed transitions, lulcc only the net class change
    (3 130–4 010 cells), so it skips the swaps between forest and other.
  - DynaCLUE: not attempted. Its C++ code is hard to compile, which is arguably solvable, but
    it is definitely harder to extend than an R or Python package.
- **Dinamica EGO.**
  - **Running it.** It runs fully headless from the console. The `.ego` scripts are
    plain-text, diffable artifacts (see `015-dinamica-woe/`). However:
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
  It is the strongest statistical and validation workbench of the open tools, but unmaintained,
  and slow on big domains: everything is held in memory through `raster`, and CLUE-S's absolute
  tolerances stop it converging above ~1 M cells (Scaling: 26 min and 6.6 GB at 4.1 M cells).

## Replay check

Checked on the pipeline before the 2026-10-21 restructuring into the 4 × 6 crossing; the new
pipeline has been run once from scratch so far (TODO).

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
    1. *Corroded probabilities.* The script saved the map it had passed *into*
       `AllocateTransitions` (`SaveMap { map = probabilities }`, after the allocation), and got
       it back depleted. Inside `AllocateTransitions`, Expander "corrodes" the probability map
       as it allocates, so that Patcher does not reuse those cells, and hands it on as an
       explicit output, `corrodedProbabilities`. That much is by design and visible in the
       functor's interface. But the input map is changed too: the corrosion happens in place,
       although the script's dataflow treats `probabilities` as a value that the allocation only
       reads. We could not check whether this is documented (the Dinamica wiki at csr.ufmg.br
       answered 503). It is a trap for anyone saving or reusing a probability map in the same
       script. The crossing was fed one realisation's leftover probabilities; they now come
       from `probabilities.ego`, which does no allocation.
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
