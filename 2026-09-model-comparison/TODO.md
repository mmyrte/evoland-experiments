# TODO — 2026-09-model-comparison

Open work only. Design rationale and results are in [`README.md`](README.md).

## Pipeline

- [x] **Replay from scratch** on the new pin (`3bc7856`, 2026-11-01, cloud container): 369 of
      370 non-Dinamica maps bit-identical (README, Replay check). Still open: the same in the
      devcontainer (mmyrte/evoland-experiments#3).
- [ ] **Bayes-eKDE × CLUMPY**: one realisation of 20 differed between the two runs (0.0003 FoM);
      check whether the eKDE potentials (`015-clumpy-kde`) replay bit for bit.
- [ ] **Paper:** `main.tex` cites the ANOVA shares 36/39/15/9 %; the rerun gives 40/38/13/9 %,
      moved by Dinamica's seeds alone. Either report a range over reruns, or find a way to give
      each Dinamica member its own replayable seed (`-predefined-seed` gives every run the same).
- [ ] **Render via quarto** (`execute-all.sh`) and commit the HTML reports from a machine with
      LFS push access (the cloud container gets 403 from lfs.github.com).
- [ ] **Rerun the scaling benchmark** (`080-scaling/`) on the current pin, once the evoland
      performance issues below are settled; the README table predates the crossing.
- [ ] **Tool versions** in a table for the paper.

## Analysis

- [x] **Further validation metrics** (`051-validation-metrics.r`): KSim, Fuzzy Kappa Simulation,
      similarity of differences, TOC AUC, per map and per ensemble; ANOVA per metric; appendix
      figures `figures/pie-metric-{relations,agreement}.pdf`.
- [ ] Decide which metric figure goes into the appendix, and whether the per-metric variance
      shares belong in Sect. 5 (they show that "how substitutable are the estimators" depends on
      the metric).

- [ ] **Reliability diagrams** per estimator and per ensemble, to check the reading of the
      naive Bayes × CLUMPY interaction (sharp probabilities) and of the Brier skill (`051`).
      Decide whether to score only cells of anterior classes with viable transitions, and
      whether the Dinamica ensembles need more realisations.
- [ ] **Weights of Evidence on evoland's predictors.** WoE uses Dinamica's distance maps, the
      evoland learners neighbourhood counts; feeding WoE evoland's predictors (as rasters)
      would make the WoE – naive Bayes pair a pure test of binning vs. Gaussian densities.
- [ ] **Ordered, stochastic.** The crossing runs lulcc's Ordered deterministically (see
      upstream issues); a stochastic variant needs lulcc patched.
- [ ] **CLUE-S configuration.** The lulcc demo parameters (elasticity 0.2 for all classes, all
      conversions allowed) overshoot the observed gross change as shipped. A variant with built
      elasticity 1 / no built conversions would show whether CLUE-S is mis-parametrised by the
      demo or structurally different. Report the default anyway.
- [ ] **Elasticity on conditional probabilities.** The crossing's class probabilities already
      condition on the anterior class, so CLUE-S elasticity and CLUMondo conversion resistance
      count persistence a second time. A variant with both set to zero would show how much
      that matters.
- [ ] **CLUinPy neighbourhood and resistance** values are taken from its tutorial's analogous
      classes; document a sensitivity check or justify.
- [ ] **Dinamica's own patch parameters.** Here they are evoland's estimates, to isolate the
      estimator. A variant with Dinamica's GUI defaults would show how much a typical user gets.

## Ideas, not needed for the paper

- **Annual steps.** Everything allocates 1991 → 1999 in one step. Iterating 8 annual steps with
  recomputed neighbourhoods and distance maps (Dinamica's `DetermineTransitionMatrix`, evoland's
  `alloc_*` over yearly periods, CLUE's annual demand) moves towards cellular automata and away
  from the pattern-based approach.

## Upstream

evoland-plus (performance, from the scaling benchmark):

- ethzplus/evoland-plus#85: DuckLake per-call overhead in prediction (batch the catalog work).
- ethzplus/evoland-plus#86: `extract_using_coords_t()` builds point geometries; indexing cells
  takes a fifth of the memory (the 9.3 GB ingestion peak at 4.1 M cells).
- ethzplus/evoland-plus#87: optional stratified training subsample in `fit_full_models()`.
- ethzplus/evoland-plus#88: checkpoint DuckLake after a batch of allocations.
- `figure_of_merit_v()` over ~500 runs takes ~105 s per call, one lineage subquery per run;
  `050` calls it twice (overall, per transition) on the same cells. A single call returning
  both, or a batched lineage resolution (#85), would halve or better.

Other tools, to report:

- lulcc 1.0.4 lists `gsubfn` and `caret` under Suggests but needs them in
  `ExpVarRasterList()` / `glmModels()`.
- lulcc 1.0.4, `OrderedModel` with `stochastic = TRUE`: `.ordered()` thins each class's ranked
  candidates by Bernoulli(suitability) and takes the first n; when the draws cannot supply n
  cells in 1000 tries (small suitabilities), the missing picks become `NA` indices, cells stay
  at -1, and the next step's C code (`total`/`allow`) reads out of range: the output map holds
  class codes in the billions. Reproducer: `040-alloc-lulcc.r` with `ordered_stochastic = TRUE`
  in the crossing (seed 11501). For simonmoulds/lulcc.
- Dinamica 8.11.2: `CalcWOfEProbabilityMap` is not deterministic in parallel (README, Replay
  check). Needs a minimal reproducer for the Dinamica team (`probabilities.ego` run three times
  with default settings shows it). Check whether evoland's `alloc_dinamica()`, which runs
  Dinamica with parallel functors, has a similar race in `CreateCubeOfProbabilityMaps`.
- Dinamica 8.11.2: `AllocateTransitions` (Expander) corrodes its *input* probability map in
  place, so saving that map after the allocation saves the depleted one (README, Replay check).
  Check the documentation once csr.ufmg.br is reachable; report if undocumented.
