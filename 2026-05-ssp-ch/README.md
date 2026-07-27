# SSP-CH (2026-05) — baseline re-implementation

Re-implementation of the SSP-CH scenarios previously developed for
[the SSP-CH scenario project](https://ssp-ch-szenarien.wsl.ch/en/), a
Swiss-specific operationalisation of the SSP scenarios. This is the **baseline**
for milestone **MS9 phase 1/3** and for the `2026-07-ssp-rsofun/` extension.

Previous results: <https://zenodo.org/records/17108008>.

> **Naming note.** This directory is `2026-05-ssp-ch`; some correspondence refers
> to it as `2026-06-ssp-ch`. Treat them as the same experiment. Not renamed to
> avoid breaking the in-script output paths and the `2026-07-ssp-rsofun` cross
> references — flag if a rename is actually wanted.

## What "re-implementation" means here

We keep the **scientific design** of the original SSP-CH work — the SSP
narratives and the **land-use demand curves** in
`NCCS-SSP-scenarios/Tools/Transition_Tables.xlsx` (the original workflow is
orchestrated by `NCCS-SSP-scenarios/Scripts/LULCC_CH_master.R`) — but rebuild the
pipeline on the **new evoland-plus** with:

- **New, reproducible data sources.** Every predictor is fetched from a public
  HTTP(S) source with an md5-verified download, replacing the original's local
  `file://` GeoTIFFs. The full data-source inventory and provenance is in
  [`REFACTOR-valpar-local.md`](REFACTOR-valpar-local.md).
- **A DuckDB-backed `evoland_db`.** State lives in `ssp-ch.evolanddb`; predictors
  are ingested via `db$add_predictor`, keyed by `id_coord`, `id_period`, `id_run`.
- **CH2025 climate** instead of CHELSA for the projected runs, mapping SSP decades
  onto global warming levels — see [`2-ingest-preds-ch2025-todo.md`](2-ingest-preds-ch2025-todo.md).

**Scenario scope.** SSP1/3/4/5 (SSP2 is deliberately excluded: treating it as a
business-as-usual case biases against the point of scenario analysis — spanning a
maximally diverse set of futures).

**Model class.** The transition model here is **purely empirical/statistical**
(static + climatological predictors, no biophysical feedback). The process-based
extension is the separate `2026-07-ssp-rsofun/` experiment.

## Domain & periods

- **Extent:** full Switzerland, EPSG:2056, 100 m grid (~4.1 M hectare cells).
- **Periods:** decadal (`P10Y`); observed 1985–2020 (Arealstatistik), extrapolated
  to 2060.

## Pipeline

| Script | Step | Status |
| --- | --- | --- |
| `0-setup-db.r` | Create `ssp-ch.evolanddb`, coords grid, periods | ✅ |
| `1-ingest-lulc-data.r` | Arealstatistik NOAS04 LULC (1985/97/09/18) | ✅ (AS2025, bioregions, deglaciation open) |
| `2-ingest-preds-dem.r` | DHM25 → elevation/slope/aspect/hillshade | ✅ |
| `2-ingest-preds-envidat-eiv.r` | SPEEDMIND EIV biophysical indicators (CWMs) | ✅ (soil layers to be replaced by rsofun soil) |
| `2-ingest-preds-swisstlm3d.r` | Distance to lakes/rivers/roads | ✅ |
| `2-ingest-preds-pop.r` | Municipal population | ✅ |
| `2-ingest-preds-statent.r` | STATENT employment (FTE by sector) | ✅ |
| `2-ingest-preds-ch2025-1-download.r` | Probe + download CH2025 climate netCDFs | ✅ |
| `2-ingest-preds-ch2025-2-etl.r` | CH2025 → predictors | 🟡 obs only; projected `-gwl` deferred |
| `3-neighbors.r` | Neighbourhood predictors | ✅ |
| `4-covariate-selection.r` | GRRF importance / covariance feature selection | 🟡 thresholds not finalised |
| `999-dump-preds-raster.r` | Debug: dump predictors to raster | ✅ (utility) |
| *(missing)* `5-transition-modelling.r` | mlr3 transition models | ⬜ not started |
| *(missing)* `6-transition-rates.r` | Demand curves (Transition_Tables.xlsx) → rates | ⬜ not started |
| *(missing)* `7-alloc-params.r` | Allocation parameters | ⬜ not started |

The `5`/`6`/`7` steps exist in `2025-10-valparish/` as reference implementations
but have **not** yet been ported here, and the SSP demand curves are **not** yet
wired in. This is the bulk of the remaining MS9-phase-1 work — see
[`TODO.md`](TODO.md).

## Data-source & design reference docs

- [`REFACTOR-valpar-local.md`](REFACTOR-valpar-local.md) — full predictor
  inventory and per-source provenance (EnviDat EIV, DHM25, swissTLM3D, CHELSA,
  CH2025), with URLs and md5sums. Most of the refactor is **done**; treated as
  reference.
- [`2-ingest-preds-ch2025-todo.md`](2-ingest-preds-ch2025-todo.md) — rationale for
  CH2025 (GWL structure) and the missing **bioclimatic indicators** wishlist
  (CHELSA-BIOCLIM+).
- [`2-ingest-preds-ch2025-urls.md`](2-ingest-preds-ch2025-urls.md) — reverse-
  engineered CH2025 atlas URL patterns.

See [`TODO.md`](TODO.md) for the consolidated task list.
