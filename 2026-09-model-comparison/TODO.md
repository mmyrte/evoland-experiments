# TODO — 2026-09-model-comparison

Open work. Design rationale is in [`README.md`](README.md). Nothing is built yet.

## Decide before writing any code

- [ ] **Study extent.** Full SSP-CH extent; if any one of the other tools fails due to memory
      pressure/computation time, that is a direct result (although we can afford to run it on a
      machine with 2TiB of RAM x 128 cores)
- [ ] **Predictor set.** A small set every tool can take, reused from `2026-05-ssp-ch/`
      ingestion. Deliberately not the full SSP-CH set. See README.
- [ ] **Calibration / validation period pair**, out of the four observed Arealstatistik
      periods. See README.
- [ ] **Which comparators are actually in reach.** TerrSet needs a Windows machine and leads to
      point&click adventures; drop it. Dinamica standalone needs a workable headless setup beyond what
      `alloc_dinamica()` already drives.
- [x] **How to compare?** Lead with a comparison of
      usability/robustness/reproducibility/accessibility/auditability criteria, see readme. Then follow
      up with a quantitative comparison where feasible, but again focus on feasibility.

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
      allocator, run standalone but headless.
  - [ ] call via processx,
  - [ ] data from evoland dumped to tifs,
  - [ ] read back into dinamica, use .ego script. see also dump of dinamica functors at `2026-09-model-comparison/072-dinamica/dinamica-functors.txt`.
  - [ ] read output like alloc-dinamica, write to evoland DB for validation.
- [ ] **`073-lulcc`** — lulcc's suitability estimation and CLUE-S allocation. as with dinamica, read back into evoland DB for easy validation/comparison.
- [ ] **`080-compare`** — common metrics across all runs: figure of merit against the
      random-allocation null, masked fuzzy similarity, quantity vs. allocation disagreement,
      per-transition AUC where available.
- [ ] **`080d-report`** — comparison figures and tables.

## Publication: what the comparison should show

The comparison is meant to support a model-description paper on evoland-plus and the modelling
framework (Geoscientific Model Development and Environmental Modelling & Software are the
natural venues; GMD requires the code and data availability that this repo already aims for).

The comparison is not supposed to go out of its way to compare correctness (that is already done by
Mazy in his thesis, https://theses.hal.science/tel-04382012/document). Rather, it is supposed to
show that the results hold up when compared to other modelling environments.

The comparison tries to complete an **estimator x allocator** matrix:

- {mlr3 learner (GLM, ranger, GRRF), Dinamica Weights of Evidence (naive bayes), lulcc's suitability model}
- {CLUMPY uSAM/uPAM, Dinamica, CLUE-S}

wherever the tools allow the pairing. Decompose each outcome metric into estimator, allocator,
interaction and stochastic (replicate) variance. If we manage this, this could be a nice figure: it
says how much of the "tool difference" belongs to each stage.

### How to validate

- [ ] **Backcast skill against nulls.** Figure of merit against three nulls: persistence (no
      change), random allocation within the anterior class, and a neighbourhood-only model. A tool
      is only useful where it beats all three.
- [ ] **Multi-resolution agreement.** FoM and masked fuzzy similarity across window sizes (1–15
      cells). Runs that look different at cellular level converge at coarser resolutions; where they
      converge lies a useful resolution (source?)

### D. Practical utility

- [ ] **Capability matrix.** Open source, language, estimator choice, stochastic allocation,
      ensembles, scenario/run lineage, provenance of inputs, interventions, HPC/concurrency,
      licence cost. Might work better in prose.
- [ ] **Scaling benchmark.** Wall time and peak memory per stage (prediction, allocation) from
      the 40×40 replica to the full 4.1 M-cell grid, on the same machine for all tools, plus
      evoland's parallel writers on DuckLake. Ship the timing harness with the paper.
- [ ] **Reproducibility.** Rebuild the study database from md5-pinned sources and show identical
      outputs under fixed seeds; report what the other tools need to reach the same bar.

### Protocol guard rails

- [ ] Write the comparison protocol down (inputs, periods, metrics, nulls, seeds, what is held
      fixed per tool) before the first run, and keep it in this directory.
- [ ] Record tool versions, licences, settings and hardware from the first run, including the
      Dinamica build (currently 8.11.2 from `ghcr.io/mmyrte/evoland`).
- [ ] Report every tool at its best-documented configuration, not evoland at its tuned best
      against the others' defaults.

## Non-comparison sections: illustrate model use / goodies

- [ ] **Ensembles vs. single maps.** The CLUMPY ensemble as a per-cell change-frequency map with its
      FoM distribution, next to the deterministic tools' single values. This is not infeasible with
      other models, just not built-in.
- [ ] **Estimator quality on its own.** mlr3 makes swapping learners a one-line change; show that,
      and show where learners disagree.
- [ ] The LP solver (`trans_rates_lp`) versus linear extrapolation of observed rates or Markov
      chain as in TerrSet/lulcc.
- [ ] **Landscape pattern analysis.** Efficient patch statistics in `patch_stats.cpp`, like fragstats and patch elongation.
- [ ] Emphasize easy export for ecosystem services / ecological value
