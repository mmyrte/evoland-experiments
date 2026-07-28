# valparish (2025-10) — reference stub

A **ValPar.CH-inspired** evoland-plus setup: Arealstatistik land-use history plus
a set of Swiss biophysical/socio-economic predictors, run through the full
evoland transition-modelling pipeline (setup → predictors → neighbours →
covariate selection → GLM transition models → transition rates → allocation
parameters).

## Status: reference only

This experiment **co-evolved with the early evoland-plus development process** and
is kept purely for reference. It is **not expected to run through against any
specific commit of evoland-plus** — the package API has moved on since.

Its main value now is as a worked example of the _complete_ pipeline (through
allocation), which the active `2026-05-ssp-ch/` experiment has not yet reached.
Several of its ingest scripts were the direct ancestors of the reproducible ones
in `2026-05-ssp-ch/` (see the refactor documented there).

## Pipeline

| Script                     | Step                                                                  |
| -------------------------- | --------------------------------------------------------------------- |
| `0-setup-db.r`             | Create `fullch.evolanddb`, square coords grid, periods                |
| `1-ingest-lulc-data.r`     | Arealstatistik NOAS04 LULC history                                    |
| `2-ingest-preds-*.r`       | Predictors: population, sonBASE noise, STATENT, ValPar local GeoTIFFs |
| `3-neighbors.r`            | Neighbourhood predictors                                              |
| `4-covariate-selection.r`  | Covariance / importance filtering of covariates                       |
| `5-transition-modelling.r` | GLM partial transition models                                         |
| `6-transition-rates.r`     | Observed rates + linear extrapolation to future periods               |
| `7-alloc-params.r`         | Allocation parameters (Dinamica)                                      |

## Open notes (carried forward, not scheduled here)

- [ ] **Bioregions.** Set biogeographic regions
      (`ch.bafu.biogeographische_regionen`). → now `2026-05-ssp-ch` "region ID as
      indicator". (`1-ingest-lulc-data.r:5`)
- [ ] **Deglaciated-area land-use class.** New class based on the glacier
      inventory; interacts with the small-area inclusion threshold. → carried to
      `2026-05-ssp-ch`. (`1-ingest-lulc-data.r:7`)
- [ ] **Arealstatistik 2025 vintage.** Only 1985–2018 selected; AS2025 not yet
      finished at time of writing. (`1-ingest-lulc-data.r:44`)
- [ ] **Population classes** `1_3` vs `1_5` interchangeability check.
      (`2-ingest-preds-pop.r:78`)
- [ ] **Covariate cardinality threshold** (`min_cardinality_abs`) not set to a
      justified value. (`4-covariate-selection.r:6`)
- [ ] **Train/test split** (`sample_frac = 0.3`) not justified. → folded into
      `2026-05-ssp-ch` transition-model validation (MS9 phase 3). (`5-transition-modelling.r:9`)
