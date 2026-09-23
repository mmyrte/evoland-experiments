# TODO — 2026-09-model-comparison

Open work. Design rationale is in [`README.md`](README.md). Nothing is built yet.

## Decide before writing any code

- [ ] **Study extent.** One bioregion, one canton, or the ValPar.CH extent. Must be small
      enough that four toolchains can be run and re-run by hand.
- [ ] **Predictor set.** A small set every tool can take, reused from `2026-05-ssp-ch/`
      ingestion. Deliberately not the full SSP-CH set.
- [ ] **Calibration / validation period pair**, out of the four observed Arealstatistik
      periods.
- [ ] **Which comparators are actually in reach.** TerrSet needs a licence and a Windows
      machine; confirm or drop it. Dinamica standalone needs a workable headless setup beyond
      what `alloc_dinamica()` already drives.
- [ ] **Where comparability ends.** Each tool's class schema, neighbourhood definition and
      allocation randomness differ. Write down what is held fixed and what is not before
      running anything, so the result is interpretable.

## Build

- [ ] **`001-setup-db`** — a small `*.evolanddb` on the chosen extent.
- [ ] **`010-`/`020-` ingestion** — LULC history and the agreed predictors, reusing the
      `2026-05-ssp-ch/` steps.
- [ ] **`030-`–`060-` evoland-plus reference run** — neighbours, viable transitions, covariate
      selection, transition models. Written to be readable as an illustration, since that is
      half the point of this sub-project.
- [ ] **`070-alloc-evoland`** — calibrate → allocate → validate with CLUMPY.
- [ ] **`071-alloc-dinamica`** — same models, Dinamica allocation via `alloc_dinamica()`.
- [ ] **`072-dinamica-native`** — Dinamica's Weights-of-Evidence estimator plus its own
      allocator, run standalone.
- [ ] **`073-lulcc`** — lulcc's suitability estimation and CLUE-S allocation.
- [ ] **`074-terrset`** — TerrSet LCM, if it is in reach. Likely a documented manual run with
      the exports checked in rather than a scripted step.
- [ ] **`080-compare`** — common metrics across all runs: figure of merit against the
      random-allocation null, masked fuzzy similarity, quantity vs. allocation disagreement,
      per-transition AUC where available.
- [ ] **`080d-report`** — comparison figures and tables.

## Publication: what the comparison should show

The comparison is meant to support a model-description paper on evoland-plus and the modelling
framework (Geoscientific Model Development and Environmental Modelling & Software are the
natural venues; GMD requires the code and data availability that this repo already aims for).
Candidate analyses are grouped by the claim they support. Pick a subset before building: each
one fixes design choices in the build steps above.

### A. The framework separates what other tools bundle

- [ ] **Estimator × allocator factorial.** Cross {mlr3 learner (GLM, ranger, GRRF), Dinamica
      Weights of Evidence, lulcc's suitability model} with {CLUMPY uSAM/uPAM, Dinamica, CLUE-S}
      wherever the tools allow the pairing. Decompose each outcome metric into estimator,
      allocator, interaction and stochastic (replicate) variance. This is the core figure: it
      says how much of the "tool difference" belongs to each stage, which no single-tool study
      can show.
- [ ] **Estimator quality on its own.** Per-transition ROC/AUC, calibration curves and
      precision–recall on held-out cells, against `classif.featureless`. mlr3 makes swapping
      learners a one-line change; show that, and show where learners disagree.
- [ ] **Quantity vs. location.** Hold the location model fixed and vary only the demand model:
      the LP solver (`trans_rates_lp`), linear extrapolation of observed rates, and a Markov
      chain as in TerrSet/lulcc. Report quantity disagreement in the backcast. This separates
      *how much* from *where*, which the literature often conflates.

### B. Agreement with observed change

- [ ] **Backcast skill against nulls.** Figure of merit, with producer's/user's accuracy split,
      against three nulls: persistence (no change), random allocation within the anterior
      class, and a neighbourhood-only model. A tool is only useful where it beats all three.
- [ ] **Pontius budget of components.** Quantity, exchange, shift and allocation disagreement
      per tool, so a better FoM can be attributed rather than just reported.
- [ ] **Multi-resolution agreement.** FoM and masked fuzzy similarity across window sizes (1–15
      cells). Tools that look different cell by cell often converge at 1 km; where they stop
      converging is the useful statement.
- [ ] **Compounding over horizon.** Single-step (one period pair) vs. chained (1985 → 2018)
      backcasts: how error accumulates per tool. Dynamic neighbour predictors, recomputed each
      period, are an evoland feature that should show here.
- [ ] **Landscape pattern fidelity.** Patch-size distribution, number of patches, edge density
      and elongation of simulated vs. observed new patches (reusing `patch_stats.cpp`), and the
      value of estimating allocation parameters from data rather than using tool defaults.

### C. Uncertainty is a first-class output

- [ ] **Ensembles vs. single maps.** The CLUMPY ensemble as a per-cell change-frequency map with
      its FoM distribution, next to the deterministic tools' single values. Only if this stays
      fair in both directions (see Open questions) does it earn a place.
- [ ] **Structural vs. scenario uncertainty.** Run the tools forward under two or three SSP
      demands from `2026-05-ssp-ch`. Show where tools disagree under the *same* scenario next to
      where scenarios disagree under the *same* tool. If model choice moves the map as much as
      the scenario does, that is a finding for anyone using scenario maps.
- [ ] **Parameter sensitivity.** Response of pattern metrics and FoM to allocation parameters
      (patch size, elongation, expander fraction) via perturbed `alloc_params_t` runs, built on
      the caller side after ethzplus/evoland-plus#53.

### D. Practical utility

- [ ] **Capability matrix.** Open source, language, estimator choice, stochastic allocation,
      ensembles, scenario/run lineage, provenance of inputs, interventions, HPC/concurrency,
      licence cost. Text-only, but reviewers expect it.
- [ ] **Scaling benchmark.** Wall time and peak memory per stage (prediction, allocation) from
      the 40×40 replica to the full 4.1 M-cell grid, on the same machine for all tools, plus
      evoland's parallel writers on DuckLake. Ship the timing harness with the paper.
- [ ] **Reproducibility.** Rebuild the study database from md5-pinned sources and show identical
      outputs under fixed seeds; report what the other tools need to reach the same bar.
- [ ] **Interventions through the interface.** A masked, multiplicative potential adjustment
      (e.g. building zones), per the `2026-05-ssp-ch` recipe, showing the relocation effect
      under fixed demand. It demonstrates the extension point without claiming a policy result.
- [ ] **Downstream consequence (optional).** One simple indicator computed on every tool's
      output, such as an ecosystem-service proxy or habitat fragmentation, to show that the
      differences matter for a decision and not only for a metric.
- [ ] **Spatial transferability (optional).** Calibrate on one bioregion and validate on
      another with spatial resampling in mlr3, if the chosen extent allows it.

### Protocol guard rails

- [ ] Write the comparison protocol down (inputs, periods, metrics, nulls, seeds, what is held
      fixed per tool) before the first run, and keep it in this directory.
- [ ] Record tool versions, licences, settings and hardware from the first run, including the
      Dinamica build (currently 8.11.2 from `ghcr.io/mmyrte/evoland`).
- [ ] Report every tool at its best-documented configuration, not evoland at its tuned best
      against the others' defaults.

## Open questions

- [ ] **How much of the difference is the allocator?** The evoland-plus vs. Dinamica-driven
      pair shares the estimator, and the Dinamica-driven vs. Dinamica-native pair shares the
      allocator. Whether those two crossings are enough to attribute the differences is worth
      settling before the comparison is written up.
- [ ] **Stochastic tools need ensembles, deterministic ones do not.** Decide how to compare a
      CLUMPY ensemble against a single deterministic LCM map without flattering either.
- [ ] **Which journal.** The comparison is meant to support a publication on the framework
      (see above); pick the venue early, since GMD's model-description format fixes the paper's
      structure and its code/data availability requirements.
