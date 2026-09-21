# SSP-CH (2026-05) — baseline re-implementation

Re-implementation of the SSP-CH scenarios developed for [the SSP-CH scenario
project](https://ssp-ch-szenarien.wsl.ch/en/), a Swiss operationalisation of the SSP
scenarios. Previous results: <https://zenodo.org/records/17108008>.

Open work is in [`TODO.md`](TODO.md).

## What "re-implementation" means here

The **scientific design** is kept — the SSP narratives and the **land-use demand** elicited
for them (the original workflow is orchestrated by
`NCCS-SSP-scenarios/Scripts/LULCC_CH_master.R`) — and the pipeline is rebuilt on the new
evoland-plus with:

- **Reproducible data sources.** Every predictor is fetched from a public source with an
  md5-verified download, replacing the original's local GeoTIFFs of partly unclear
  provenance. Per-source provenance and manipulations live in the `020-ingest-preds-*`
  steps.
- **CH2025 climate** instead of CHELSA for the projected runs, mapping SSP decades onto
  global warming levels. See `021-ingest-preds-ch2025-download.qmd`, "Why CH2025".

**Scenario scope.** SSP0/1/3/4/5.

- SSP2 is excluded because it is regularly treated as a business-as-usual case, working
  against the point of scenario analysis, which is to span a maximally diverse set of
  futures.
- SSP0 is a "positive normative visioning" scenario, newly added, mapped to GWL1.5 —
  [we may already be there](https://climate.copernicus.eu/copernicus-august-was-worlds-joint-hottest-month-record-pushing-global-temperatures-back-above).
- The original kept climatological change and socioeconomic pathways orthogonal. Here each SSP
  is realised both against current climatology and against SSP-GWL mappings.

## Where the SSP demand comes from

The per-SSP demand input is sourced from
[`NCCS_simulation_LULC_areas.xlsx`](https://github.com/mmyrte/NCCS-SSP-scenarios/blob/main/Tools/NCCS_simulation_LULC_areas.xlsx):

- It is keyed by SSP0/1/3/4/5 and is what
  `Scripts/Preparation/Simulation_trans_tables_prep.R` reads.
- one row per (scenario × LULC class) with `init_area`, `final_area_2060`,
  `final_area_2100` and a qualitative `chosen_shape` (`Constant change`, `Instant
  growth`, `Instant decline`, `Delayed growth`, `Delayed decline`).

Those are **class-level area targets**, not transition rates. The original converts one into
the other with a linear program,
`Scripts/Functions/lulcc.simulationtransitionratesolver.R` (`lpSolve::lp()`), solving for
per-transition flows subject to observed min/max rate bounds, hard monotonic direction per
class, soft curve-shape constraints and a temporal smoothing term. Step `070` uses the
corrected LP that evoland-plus now carries; see [`notes-lp-solver.md`](notes-lp-solver.md) for
the analysis behind that choice.

## Domain & periods

- **Extent:** Switzerland, EPSG:2056, 100 m grid (~4.1 M hectare cells).
- **Periods:** decadal (`P10Y`); observed 1985–2020 (Arealstatistik), extrapolated forward.
  Eight periods plus the static `id_period 0`; 1–4 observed, 5–8 extrapolated. **Period 8 runs
  2055–2064**, so the 2060 demand targets fall inside it and step `070` treats the 2060 target
  as an interpolation. Decadal steps from 1985 cannot land on 2060.
- **Runs:** `runs_t` identifies scenarios, ordered **base → climate trajectory → SSP**.
  Climate sits above SSP because it is the heavier per-run payload (~2.2 B stored rows this
  way against ~6.2 B with SSP on top). The ordering is a storage decision and deliberately
  does not mirror the narrative; run `description`s still read "SSP3 under …". Set up in
  `001-setup-db.qmd`.

## Pipeline

See the top-level README, "Conventions", for the numbering scheme. "run" below means a
rendered report or an output raster is committed; reports are committed ad-hoc, so a step
marked "written" may have been executed without leaving a trace.

| Step | Purpose | State |
| --- | --- | --- |
| `001-setup-db.qmd` | `ssp-ch.evolanddb`, coords grid, periods, scenario `runs_t` | run |
| `010-ingest-lulc-data.qmd` | Arealstatistik NOAS04 LULC (1985/97/09/18) | run |
| `020-ingest-preds-dem.qmd` | DHM25 → elevation/slope/aspect | run |
| `020-ingest-preds-swisstlm3d.qmd` | Distance to lakes/rivers/roads | run |
| `020-ingest-preds-bioregions.qmd` | BAFU biogeographic regions/subregions (categorical) | run |
| `020-ingest-preds-soil.qmd` | Swiss Soil Property Map sand/clay/OC × 4 depths | run |
| `020-ingest-preds-statent.qmd` | STATENT employment (FTE by sector), historical levels | run |
| `020d-ingest-preds-ch20205-check.qmd` | _diag:_ precip-raster sanity check | written |
| `021-ingest-preds-ch2025-download.qmd` | Probe + download CH2025 climate netCDFs | run |
| `021-ingest-preds-statent-ssp.qmd` | SSP employment projection; **source provenance unknown** | run |
| `022-ingest-preds-ch2025-etl.qmd` | CH2025 `-obs` → predictors at `id_period 0` | run |
| `029-emergency-change-fill.qmd` | Stopgap: overwrite `fill_value` with per-predictor means | run — to be replaced, see TODO |
| `030-neighbors.qmd` | Neighbourhood predictors (land-use categories only) | run |
| `040-viable-transition-identification.qmd` | Commit viable transitions; `min_cardinality_abs = 10000`, provisional | run |
| `040d-viable-transition-identification.qmd` | _diag:_ observed-transitions plot | run |
| `050-covariate-selection.qmd` | GRRF importance selection; `importance_rel_cut = 0.2` | run |
| `051-ingest-preds-ch2025-3-gwl.qmd` | CH2025 `-gwl` projections as per-run/period overrides | written |
| `060-transition-modelling.qmd` | Transition models, selected on AUC | run |
| `060d-transition-modelling.qmd` | _diag:_ held-out ROC + AUC ranking per transition | run |
| `070-transition-rates-solver.qmd` | SSP demand → LP solver → `trans_rates_t` | written |
| `080-validate-backcasting.qmd` | Allocation params → backcast → fuzzy similarity + figure of merit | written |
| `090-extrapolate.qmd` | Stochastic extrapolation per SSP × climate | written |
| `091-stochastic-alloc-2030.qmd` | One period × 20 members × 5 SSPs → change-intensity maps | run, full CH grid |
| `090d-report.qmd` | _diag:_ reporting — figures, tables, maps | not written |

Interventions are not yet implemented at any stage; see [`TODO.md`](TODO.md) § Interventions for
where that is going.

> **Reference implementations.** Check out the package vignettes — `evoland.qmd` for the
  calibrate → rates → allocate chain, and
> `stochastic-allocation-sensitivity.qmd` for the `runs_t` ensemble pattern `080`/`090`
  use.

## Predictor provenance vs. the original SSP-CH

The predictor set is not a straight copy. The original's covariates are catalogued in
[`Predictor_table.xlsx`](https://github.com/mmyrte/NCCS-SSP-scenarios/blob/main/Tools/Predictor_table.xlsx)
(one sheet per timestep); comparing those sheets against this pipeline gives the picture
below. Its **future** (SSP) sheets already drop `Muni_pop`, and its employment predictor
is an annual *change* in FTE rather than a level.

| Original predictor(s) | Original source | Here | Notes |
| --- | --- | --- | --- |
| Soil EIVs: pH, nutrients, moisture, moisture variability, aeration, humus | Descombes et al. 2020 (EnviDat) | **Reused** | Ingested in `2025-10-valparish/020-ingest-preds-envidat-eiv.qmd`; soil layers slated for SSPM replacement. |
| `light_100m` (EIV-L) | Descombes et al. 2020 | **Reused** | Same step. |
| Continentality (EIV-K) | Descombes et al. 2020 | **Discarded** | Weaker than CH2025 climate and likely collinear with bioregions. |
| Elevation, slope, aspect | swissALTI3D 2 m (ValPar local) | **Reused, source replaced** — `020-ingest-preds-dem` (DHM25) | Reproducible HTTP download; swissALTI3D noted as an optional higher-res upgrade. |
| Hillshade | swissALTI3D | **Discarded** | Insolation proxy redundant with slope/aspect; ray-traced insolation would be the proper form. |
| Distance to lakes / rivers / roads | GWN07 / VECTOR25 / swissTLM3D (ValPar local) | **Reused, source replaced** — `020-ingest-preds-swisstlm3d` | GWN07 / VECTOR25 discontinued; swissTLM3D is the successor. |
| `chg_FTE_Sec1/2/3` — annual change in FTE per labour-market region | FSO Business Census + STATENT | **Reused, redefined** — `020-ingest-preds-statent` (absolute FTE levels) | Levels keep local signal ("a farm is here") at the cost of easy extrapolation. |
| Urban neighbourhood matrices (`n9`/`n11` × versions) | Project internal | **Reused, reimplemented** — `030-neighbors` | Generic neighbour predictors over distance bands rather than hand-built kernels. |
| `Muni_pop` — municipal population | FSO | **Discarded** | Unused in the original SSP sheets; ingestion retained only in `2025-10-valparish/020-ingest-preds-pop.r`. |
| `noise_mean_100m` (sonBASE) | BAFU sonBASE | **Not carried over** | Present in `2025-10-valparish/020-ingest-preds-sonbase.r`; re-inclusion undecided. |
| — (no direct climate predictor) | — | **Added** — `021-`/`022-`/`051-ingest-preds-ch2025-*` | Temperature/precip/heat/cold/snow/drought indices, SSP→GWL mapped. Heating/cooling degree-days excluded as energy-demand rather than suitability. |
| — | — | **Added** — `020-ingest-preds-bioregions` | Biogeographic region and subregion as factors. |

## Provenance documentation

Provenance lives in the step documents:

- Per-predictor sources, licences and specs — each `020-ingest-preds-*.qmd`.
- CH2025 rationale, the SSP→GWL structure, the reverse-engineered API schema, and the
  superseded CHELSA V2.1 predecessor — `021-ingest-preds-ch2025-download.qmd`.
- The missing-bioclimatic-indicators wishlist — appendix of `022-ingest-preds-ch2025-etl.qmd`.
