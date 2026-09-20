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

## Open questions

- [ ] **How much of the difference is the allocator?** The evoland-plus vs. Dinamica-driven
      pair shares the estimator, and the Dinamica-driven vs. Dinamica-native pair shares the
      allocator. Whether those two crossings are enough to attribute the differences is worth
      settling before the comparison is written up.
- [ ] **Stochastic tools need ensembles, deterministic ones do not.** Decide how to compare a
      CLUMPY ensemble against a single deterministic LCM map without flattering either.
- [ ] **Publication intent.** If this is meant to be citable, the tool versions, licences and
      exact settings need recording from the first run rather than reconstructed later.
