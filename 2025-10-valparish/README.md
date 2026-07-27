# valparish (2025-10) — reference stub

A **ValPar.CH-inspired** evoland-plus setup: Arealstatistik land-use history plus
a set of Swiss biophysical/socio-economic predictors, run through the full
evoland transition-modelling pipeline (setup → predictors → neighbours →
covariate selection → GLM transition models → transition rates → allocation
parameters).

## Status: reference only

This experiment **co-evolved with the early evoland-plus development process** and
is kept purely for reference. It is **not expected to run through against any
specific commit of evoland-plus** — the package API has moved on since. Do not
treat it as a maintained pipeline.

Its main value now is as a worked example of the *complete* pipeline (through
allocation), which the active `2026-05-ssp-ch/` experiment has not yet reached.
Several of its ingest scripts were the direct ancestors of the reproducible ones
in `2026-05-ssp-ch/` (see the refactor documented there).

## Pipeline

| Script | Step |
| --- | --- |
| `0-setup-db.r` | Create `fullch.evolanddb`, square coords grid, periods |
| `1-ingest-lulc-data.r` | Arealstatistik NOAS04 LULC history |
| `2-ingest-preds-*.r` | Predictors: population, sonBASE noise, STATENT, ValPar local GeoTIFFs |
| `3-neighbors.r` | Neighbourhood predictors |
| `4-covariate-selection.r` | Covariance / importance filtering of covariates |
| `5-transition-modelling.r` | GLM partial transition models |
| `6-transition-rates.r` | Observed rates + linear extrapolation to future periods |
| `7-alloc-params.r` | Allocation parameters (Dinamica) |

See [`TODO.md`](TODO.md) for the (frozen) outstanding notes.
