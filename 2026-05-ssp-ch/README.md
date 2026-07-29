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
  partly unclear provenance. Per-source provenance now lives in each `02-ingest-preds-*`
  step's prose; the reused/replaced/discarded picture is the table below.
- **CH2025 climate** instead of CHELSA for the projected runs, mapping SSP decades onto
  global warming levels — see `02-ingest-preds-ch2025-1-download.qmd` ("Why CH2025").

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

Steps are Quarto documents (see the top-level README "Conventions"): `NN-` = core
step, `NNd-` = optional diagnostic that renders a verification report.

| Step | Purpose | Status |
| --- | --- | --- |
| `00-setup-db.qmd` | Create `ssp-ch.evolanddb`, coords grid, periods | ✅ |
| `01-ingest-lulc-data.qmd` | Arealstatistik NOAS04 LULC (1985/97/09/18) | ✅ (AS2025, bioregions, deglaciation open) |
| `02-ingest-preds-dem.qmd` | DHM25 → elevation/slope/aspect | ✅ (hillshade discarded) |
| `02-ingest-preds-envidat-eiv.qmd` | SPEEDMIND EIV biophysical indicators (CWMs) | ✅ (soil layers to be replaced by rsofun soil) |
| `02-ingest-preds-swisstlm3d.qmd` | Distance to lakes/rivers/roads | ✅ |
| `02-ingest-preds-statent.qmd` | STATENT employment (FTE by sector) | 🟡 historical only; needs SSP scenario logic |
| `02-ingest-preds-ch2025-1-download.qmd` | Probe + download CH2025 climate netCDFs | ✅ |
| `02-ingest-preds-ch2025-2-etl.qmd` | CH2025 → predictors | 🟡 obs only; projected `-gwl` deferred |
| `02d-ingest-preds-ch2025-check.qmd` | _diag:_ precip-raster sanity check (was `999-dump-preds-raster`) | ✅ |
| `03-neighbors.qmd` | Neighbourhood predictors | ✅ (land-use categories only) |
| `04-viable-transition-identification.qmd` | Commit viable transitions (`is_viable` threshold) | 🟡 threshold set per `04d`, not finalised |
| `04d-viable-transition-identification.qmd` | _diag:_ observed-transitions plot (justifies the threshold) | ✅ |
| `05-covariate-selection.qmd` | GRRF importance / covariance feature selection | 🟡 GRRF/covariance params not finalised |
| `06-transition-modelling.qmd` | mlr3 transition models | ⬜ not started |
| `07-transition-rates.qmd` | Demand curves (`Transition_Tables.xlsx`) → rates | ⬜ not started |
| `08-validate-backcasting.qmd` | Estimate patch params → backcast over param choices → validate (fuzzy sim.) | ⬜ not started |
| `09-extrapolate.qmd` | Stochastic extrapolation (forward projection) | ⬜ not started |
| `09d-report.qmd` | _diag:_ reporting — figures, tables, maps | ⬜ not started |

`06`/`07` have reference implementations in `2025-10-valparish/` (as GLM); `08`/`09`/`09d`
are new. The SSP demand curves are **not** yet wired in — this is the bulk of the
remaining MS9-phase-1 work. See [`TODO.md`](TODO.md).

## Predictor provenance vs. the original SSP-CH

The predictor set is **not** a straight copy of the original SSP-CH implementation. Its
covariates are catalogued in `NCCS-SSP-scenarios/Tools/Predictor_table.xlsx` (one sheet per
timestep); comparing those sheets against this pipeline gives the reused / replaced /
discarded / added picture below. Note in particular that the original's **future** (SSP)
sheets already drop `Muni_pop`, and its employment predictor is an annual *change* in FTE,
not a level.

| Original predictor(s) | Original source | Here | Notes |
| --- | --- | --- | --- |
| Soil EIVs: pH, nutrients, moisture, moisture variability, aeration, humus | Descombes et al. 2020 (EnviDat) | **Reused** — `02-ingest-preds-envidat-eiv` | Same source; soil layers slated for SSPM (rsofun) replacement. |
| `light_100m` (EIV-L) | Descombes et al. 2020 | **Reused** — same step | — |
| Continentality (EIV-K) | Descombes et al. 2020 | **Discarded** | Redundant with / weaker than CH2025 climate, and likely collinear with the planned bioregions. |
| Elevation, slope, aspect | swissALTI3D 2 m (ValPar local) | **Reused, source replaced** — `02-ingest-preds-dem` (DHM25) | Reproducible HTTP download; swissALTI3D noted as an optional higher-res upgrade. |
| Hillshade | swissALTI3D | **Discarded** | Insolation proxy, redundant with slope/aspect; ray-traced insolation would be the proper form. |
| Distance to lakes / rivers / roads | GWN07 / VECTOR25 / swissTLM3D (ValPar local) | **Reused, source replaced** — `02-ingest-preds-swisstlm3d` | GWN07 / VECTOR25 discontinued → swissTLM3D successor, downloaded directly. |
| `chg_FTE_Sec1/2/3` — annual *change* in FTE per labour-market region | FSO Business Census + STATENT | **Reused, redefined** — `02-ingest-preds-statent` (absolute FTE *levels* per period) | Levels, not change-rates: keeps local signal (e.g. "a farm is here") at the cost of easy extrapolation (see the step's note). |
| Urban neighbourhood matrices (`n9`/`n11` × versions) | Project internal | **Reused, reimplemented** — `03-neighbors` | evoland generic neighbour predictors over distance bands rather than hand-built kernels. |
| `Muni_pop` — municipal population | FSO | **Discarded** | Not used in the original *future* (SSP) sheets either; the ingestion is retained only in `2025-10-valparish/2-ingest-preds-pop.r`. |
| `noise_mean_100m` (sonBASE) | BAFU sonBASE | **Not carried over** | Present in `2025-10-valparish/2-ingest-preds-sonbase.r`; decide whether to re-include (TODO). |
| — (no direct climate predictor in the original suitability set) | — | **Added** — `02-ingest-preds-ch2025-*` | CH2025 temperature/precip/heat/cold/snow/drought indices, SSP→GWL mapped. Heating/cooling degree-days excluded (energy-demand, not suitability). |
| — | — | **Added (planned)** | Bioregions (region ID) and coordinates — see `TODO.md`. |

## Data-source & design provenance

Provenance now lives **in the step documents themselves** (literate `.qmd`), not in separate
notes:

- Per-predictor sources, licences and specs — in each `02-ingest-preds-*.qmd` (e.g. the
  EnviDat EIV licence and observed ranges; the DHM25 / swissTLM3D lineage).
- CH2025 rationale, the SSP→GWL structure, the reverse-engineered API schema, and the
  superseded CHELSA V2.1 predecessor — in `02-ingest-preds-ch2025-1-download.qmd`.
- The missing-bioclimatic-indicators wishlist — appendix of `02-ingest-preds-ch2025-2-etl.qmd`.

See [`TODO.md`](TODO.md) for the consolidated task list.
