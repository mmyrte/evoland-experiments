# TODO — 2026-09-model-comparison

Open work. Design rationale and first results are in [`README.md`](README.md).

## Done (2026-10-02)

- [x] Benchmark case: Plum Island Ecosystems from lulcc, replacing the Arealstatistik plan.
- [x] Protocol: calibrate 1985 → 1991, allocate 1991 → 1999 on observed demand, validate against
      held-out 1999.
- [x] evoland reference run: logistic regression and random forest; CLUMPY and Dinamica
      allocation (`010`, `020`).
- [x] Dinamica standalone, headless: Weights of Evidence calibration and Expander/Patcher
      allocation in two `.ego` scripts (`030-dinamica-native/`).
- [x] lulcc: GLM suitability, CLUE-S and Ordered (`030-lulcc.r`).
- [x] CLUinPy: logistic suitability, CLUMondo allocation (`030-cluinpy*`).
- [x] Crossings: CLUMPY on WoE, lulcc and CLUinPy surfaces (`040`).
- [x] Common metrics, ANOVA decomposition and timings (`050`); ensemble Brier scores (`051`);
      paper Fig. 3 (`060`).

## Next

- [ ] **Render via quarto** (`execute-all.sh`) and commit the HTML reports. So far the steps
      were only run with `Rscript` (which works because they are knitr-spin `.r` files). The
      repo's `.Rprofile` activates rv; the steps were run with `R_PROFILE_USER=/dev/null`
      against a system library.
- [ ] **Bump the evoland pin** in `rproject.toml` once the stacked fix PRs are merged (see
      below). The pipeline needs fixes 01 (square-cell tolerance) and 03 (external
      potentials).
- [x] **Ensemble-level metric next to FoM** (`051-ensemble-scores.r`): fair Brier skill against
      random allocation; reverses the FoM ranking of Dinamica vs. CLUMPY (README finding 3).
      Open: a reliability diagram per ensemble; whether to score only cells of anterior classes
      with viable transitions; more realisations for the Dinamica ensembles.
- [ ] **Multi-resolution agreement.** FoM and masked fuzzy similarity across window sizes
      (1–15 cells).
- [ ] **Nulls beyond random allocation.** A neighbourhood-only model (evoland with only the
      neighbourhood predictors) is cheap; persistence is trivially 0.
- [ ] **Dinamica's own patch parameters.** Native Dinamica practice tunes the Expander/Patcher
      parameters by hand or by `eval_alloc_params_t`-style search. Here they are evoland's
      estimates, to isolate the estimator. A variant with Dinamica's GUI defaults would show how
      much a typical user gets.
- [ ] **Dinamica annual steps.** `simulate.ego` allocates 1991 → 1999 in one step, like evoland.
      Dinamica practice would iterate 8 annual steps with a multi-step matrix
      (`DetermineTransitionMatrix`), recomputing the distance maps. Same for evoland
      (`alloc_*` over yearly periods); decide whether that is worth a variant.
- [ ] **CLUE-S configuration.** The lulcc demo parameters (elasticity 0.2 for all classes, all
      conversions allowed) produce 3.8× the observed gross change, including 2.6 k cells of
      built → forest. Add a variant with built elasticity 1 / no built conversions to see whether
      CLUE-S is merely mis-parametrised by the demo or structurally different. Report the
      default anyway, per the "best-documented configuration" guard rail.
- [ ] **CLUinPy neighbourhood and resistance** values are taken from its tutorial's analogous
      classes; document a sensitivity check or justify.
- [x] **Check against Moulds et al. (2015)** (`070d-lulcc-moulds2015.r`): lulcc's GMD demo as
      published (1985 → 1999, 5 seeds) reproduces the paper's Fig. 8 (forest → built FoM over
      resolutions 2–256) within ~0.02 for both CLUE-S and Ordered. Our lulcc installation and
      set-up are sound. Their native-resolution forest → built FoM (0.078 CLUE-S, 0.066 Ordered)
      is higher than our benchmark's overall FoM because it covers 14 years of change and a single
      transition. Note for the paper: multi-resolution FoM is what lulcc reports.
- [x] **Scaling benchmark** (`080-scaling/`, README "Scaling"): evoland's calibration runs out of
      memory above ~1 M cells because of the neighbour edge list; Dinamica streams (0.3 GB at
      4.1 M); lulcc's CLUE-S stops converging at 1.8 M (absolute tolerances); CLUinPy is linear.
      Open:
      - evoland: grid-native neighbour counts (ring convolution) instead of `neighbors_t`, then
        rerun k = 4, 6 — the main architectural fix for the paper's scaling claim;
      - evoland: restrict `coords_t` to cells with data by default;
      - Dinamica: run `calibrate.ego` in parallel (deterministic), only the probability map and
        allocation single-threaded;
      - lulcc: CLUE-S with tolerances scaled to the domain, to separate parametrisation from
        algorithm;
      - repeat on a bigger machine to see where lulcc and CLUinPy stop.
- [x] **Reproducibility.** The full pipeline was replayed from scratch (README, Replay check).
      Found and fixed: random forest not replaying (evoland fix 06 plus a learner seed); corroded
      WoE probabilities in the crossing.
      Found a Dinamica 8.11.2 parallel race in `CalcWOfEProbabilityMap`. Open:
      - a minimal reproducer for the Dinamica team (`probabilities.ego` run three times with
        default settings shows it);
      - evoland's own `alloc_dinamica()` runs Dinamica with parallel functors; Expander/Patcher
        are random anyway, but check whether `CreateCubeOfProbabilityMaps` or the allocation
        have a similar race;
      - record tool versions in a table for the paper.
- [ ] **TerrSet LCM**: still dropped (Windows, GUI, licence).
- [ ] **Figures for the paper**: `figures/pie-figure-of-merit.pdf` and
      `figures/pie-outcome-maps.pdf` are first drafts in base R. An estimator × allocator heatmap
      of FoM with the ANOVA shares as an inset may be the more compact paper figure.

## Upstream issues found while building this

evoland-plus, each on its own stacked branch off `claude/gifted-lamport-41q8gv` (= `develop` at
`ecebf27`), in this order:

1. `claude/gifted-lamport-41q8gv-01-square-cells`: `compute_alloc_params_single()` compared the
   x and y resolution with `==`. Rasters rebuilt from `coords_t` on a non-integer origin (PIE)
   have resolution `100.00000000000006`, and `create_alloc_params_t()` failed for every
   transition.
2. `…-02-trans-count-message`: `predict_trans_pot()` reported `length(viable_trans)` (the
   column count, 7) as the number of transitions.
3. `…-03-external-trans-pot`: `predict_trans_pot()` demanded fitted models even when every
   potential was already in `trans_pot_t`, which blocked allocating potentials from external
   estimators.
4. `…-04-missing-models-message`: the "No fitted model" error printed a literal
   `{toString(missing_models)}` (`glue_collapse` doesn't interpolate), and didn't name the
   required `select_score`.

5. `…-05-dinamica-temp-dir-docs`: `install-dinamica` vignette warns against setting
   `DINAMICA_EGO_8_TEMP_DIR` in `.Renviron` (breaks Dinamica's R bridge).
6. `…-06-deterministic-training-order`: `fit_full_models()` read training data in DuckDB's
   arbitrary row order, so order-sensitive learners (ranger) did not replay even when seeded.

Other tools:

- lulcc 1.0.4 lists `gsubfn` and `caret` under Suggests but needs them in
  `ExpVarRasterList()` / `glmModels()`.
- Dinamica: an `.Renviron` that sets `DINAMICA_EGO_8_TEMP_DIR` breaks the R bridge (see README,
  Environment); now noted in the vignette (fix 05).
- Dinamica 8.11.2: `CalcWOfEProbabilityMap` is not deterministic in parallel (README, Replay
  check).
- The HTML reports in `html-reports/` were rendered but not committed: the LFS upload is refused
  from the cloud container (403 from lfs.github.com). Render and commit them from a machine with
  LFS push access.

## Housekeeping (repo-wide)

- [ ] Three files in the repo were committed as plain binaries although `.gitattributes` routes
      them through LFS, so a fresh clone shows them as modified:
      `2026-05-ssp-ch/091-change-intensity-2030.png`,
      `2026-05-ssp-ch/graphs/fig-spm8a-ar6-wg1.png`,
      `2026-09-paper-figures/html-reports/010-fig2-ensembles.html`. Fix with
      `git add --renormalize <files>` and commit.
