# TODO — 2026-09-model-comparison

Open work only. Design rationale and results are in [`README.md`](README.md).

## Pipeline

- [ ] **Replay from scratch** in the devcontainer (mmyrte/evoland-experiments#3) and compare
      the per-run FoM with this branch's; the README's Replay check predates the crossing.
- [ ] **Render via quarto** (`execute-all.sh`) and commit the HTML reports from a machine with
      LFS push access (the cloud container gets 403 from lfs.github.com).
- [ ] **Rerun the scaling benchmark** (`080-scaling/`) on the current pin, once the evoland
      performance issues below are settled; the README table predates the crossing.
- [ ] **Tool versions** in a table for the paper.

## Analysis

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

## Proposed, awaiting a decision

- [ ] **Fig. 2 (ensembles)**: the class-total and greedy allocators next to CLUMPY, with a
      binned realised-vs-estimated probability panel to show their bias.

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
