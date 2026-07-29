# SSP-CH (2026-05) — baseline re-implementation

Re-implementation of the SSP-CH scenarios previously developed for [the SSP-CH scenario
project](https://ssp-ch-szenarien.wsl.ch/en/), a Swiss-specific operationalisation of
the SSP scenarios. This is the **baseline** for the `2026-07-ssp-rsofun/` extension.

Previous results: <https://zenodo.org/records/17108008>.

## What "re-implementation" means here

We keep the **scientific design** of the original SSP-CH work — the SSP
narratives and the **land-use demand curves** in
`NCCS-SSP-scenarios/Tools/Transition_Tables.xlsx` (the original workflow is
orchestrated by `NCCS-SSP-scenarios/Scripts/LULCC_CH_master.R`) — but rebuild the
pipeline on the **new evoland-plus** with:

- **New, reproducible data sources.** Every predictor is fetched from a public HTTP(S)
  source with an md5-verified download, replacing the original's local GeoTIFFs of
  partly unclear provenance. The full data-source inventory and provenance is in
  [`REFACTOR-valpar-local.md`](REFACTOR-valpar-local.md).
- **CH2025 climate** instead of CHELSA for the projected runs, mapping SSP decades onto
  global warming levels — see
  [`2-ingest-preds-ch2025-todo.md`](2-ingest-preds-ch2025-todo.md).

**Scenario scope.** SSP0/1/3/4/5

- SSP2 is deliberately excluded: treating it as a business-as-usual case biases against
  the point of scenario analysis — spanning a maximally diverse set of futures)
- SSP0 is a "positive normative visioning" scenario, newly added.
- The original SSP-CH work deliberately kept climatological change and socioeconomic
  pathways orthogonal. We here realise each SSP againsts current climatology and against
  SSP-GWL mappings. SSP0 is mapped to GWL1.5, given that we may have already reached
  that point
  <https://climate.copernicus.eu/copernicus-2025-was-third-hottest-year-record>

**Model class.** The transition model here is **purely empirical/statistical**
(static + climatological + economic predictors, no biophysical feedback). The process-based
extension is the separate `2026-07-ssp-rsofun/` experiment.

## Domain & periods

- **Extent:** full Switzerland, EPSG:2056, 100 m grid (~4.1 M hectare cells).
- **Periods:** decadal (`P10Y`); observed 1985–2020 (Arealstatistik), extrapolated
  to 2060.

## Pipeline

| Script                                 | Step                                                                  | Status                                                                                                                 |
| -------------------------------------- | --------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------- |
| `0-setup-db.r`                         | Create `ssp-ch.evolanddb`, coords grid, periods                       | ✅                                                                                                                     |
| `1-ingest-lulc-data.r`                 | Arealstatistik NOAS04 LULC (1985/97/09/18)                            | ✅ (AS2025, bioregions, deglaciation open)                                                                             |
| `2-ingest-preds-dem.r`                 | DHM25 → elevation/slope/aspect/hillshade                              | ✅                                                                                                                     |
| `2-ingest-preds-envidat-eiv.r`         | SPEEDMIND EIV biophysical indicators (CWMs)                           | ✅ (soil layers to be replaced by rsofun soil)                                                                         |
| `2-ingest-preds-swisstlm3d.r`          | Distance to lakes/rivers/roads                                        | ✅                                                                                                                     |
| `2-ingest-preds-pop.r`                 | Municipal population                                                  | ✅                                                                                                                     |
| `2-ingest-preds-statent.r`             | STATENT employment (FTE by sector)                                    | 🟡 Only ingested historical state, needs to match SSP scenario logic                                                   |
| `2-ingest-preds-ch2025-1-download.r`   | Probe + download CH2025 climate netCDFs                               | ✅                                                                                                                     |
| `2-ingest-preds-ch2025-2-etl.r`        | CH2025 → predictors                                                   | 🟡 obs only; projected `-gwl` deferred                                                                                 |
| `3-neighbors.r`                        | Neighbourhood predictors                                              | ✅ for now only considering land use categories as neighbors                                                           |
| `4-covariate-selection.r`              | GRRF importance / covariance feature selection                        | 🟡 viable transition threshold not set; may need to be split (individual scripts for viable trans + feature selection) |
| _(missing)_ `5-transition-modelling.r` | mlr3 transition models                                                | ⬜ not started                                                                                                         |
| _(missing)_ `6-transition-rates.r`     | Demand curves (Transition_Tables.xlsx) → rates                        | ⬜ not started                                                                                                         |
| _(missing)_ `7-validate-backcasting.r` | Estimate patch params → backcast over param choices → validate (fuzzy sim.) | ⬜ not started                                                                                                         |
| _(missing)_ `8-extrapolate.r`          | Extrapolation: stochastic extrapolation                               | ⬜ not started                                                                                                         |
| _(missing)_ `9-report.r`          | Reporting: figures, tables, maps                               | ⬜ not started                                                                                                         |
| `999-dump-preds-raster.r`              | Debug: dump predictors to raster                                      | ✅ (utility)                                                                                                           |

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
