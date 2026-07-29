# TODO — 2026-05-ssp-ch

Task tracker for the SSP-CH baseline (MS9 phase 1, plus the phase-3 backcasting
validation that lives here). Mirrors the pipeline table in [`README.md`](README.md);
refs point at the relevant `.qmd`.

Legend: ⬜ not started · 🟡 in progress / partial · ✅ done

---

## Concluded

- [x] **Adopted the Quarto pipeline + `NNd` diagnostic convention** (see top-level
      README): converted all steps to `.qmd`, two-digit stages, `_quarto.yml`
      (`freeze: auto`), and reworked `execute-all.sh`. Split old `4-covariate-selection`
      into `04-viable-transition-identification` (core) + `04d-…` (diagnostic) +
      `05-covariate-selection` (core). `999-dump-preds-raster` → `02d-ingest-preds-ch2025-check`.
- [x] `00-setup-db.qmd` — `ssp-ch.evolanddb`, full-CH 100 m coords grid, decadal
      periods (1985–2020 observed → 2060 extrapolated).
- [x] `01-ingest-lulc-data.qmd` — Arealstatistik NOAS04 LULC (1985/97/09/18). *(AS2025,
      bioregions, deglaciation still open — see below.)*
- [x] Reproducible predictor ingestion (replaces the ValPar local GeoTIFFs of
      partly unclear provenance; provenance in `REFACTOR-valpar-local.md`):
      `02-ingest-preds-dem.qmd`, `02-ingest-preds-envidat-eiv.qmd`,
      `02-ingest-preds-swisstlm3d.qmd`.
- [x] **Predictor provenance reconciled** against the original SSP-CH
      (`NCCS-SSP-scenarios/Tools/Predictor_table.xlsx`); see the README table.
      Discarded: municipal population (`Muni_pop` — unused in the original SSP
      sheets; the ingestion is dropped, retained only in valparish), hillshade,
      continentality.
- [x] `02-ingest-preds-ch2025-1-download.qmd` — probe + throttled download of all 399
      CH2025 candidate URLs to cache.
- [x] `02-ingest-preds-ch2025-2-etl.qmd` — **observed** CH2025 predictors ingested
      (41 predictors, 1991–2020 → `id_period 0`). *(projected `-gwl` still open.)*
- [x] `03-neighbors.qmd` — neighbourhood predictors. *(currently land-use categories
      only; extending to other predictors is open.)*

---

## Scenario scope & SSP realisation

Decided (README): **SSP0/1/3/4/5**; SSP2 excluded (business-as-usual biases against
spanning a maximally diverse set of futures).

- [ ] **Implement SSP0** ("positive normative visioning", newly added) — bring in
      its interventions (`NCCS-SSP-scenarios/Tools/SSP0_interventions.yml`) and
      demand curves. SSP0 → GWL1.5 (we may already be there).
- [ ] **Realise each SSP against two climate framings** — (a) current climatology
      and (b) the SSP→GWL mapping — keeping socioeconomic and climatological
      pathways orthogonal as in the original work. Encode via `id_run` / `id_period`.

---

## Data ingestion — remaining

### Climate (CH2025)
- [ ] 🟡 **Projected `-gwl` ingestion.** Crosswalk `gwl` → `id_period` per SSP,
      encode quantile × scenario as `id_run`, then decide which indicators feed the
      transition model. (`02-ingest-preds-ch2025-2-etl.qmd`; `2-ingest-preds-ch2025-todo.md`)
- [ ] **SSP5-8.5 late century** (~2071–2100, ~5–6 °C) exceeds GWL3.0 — no CH2025
      aggregate. Decide: cap at GWL3 or drop the tail. (`02-ingest-preds-ch2025-1-download.qmd`)
- [ ] **Bioclimatic indicators.** CH2025 currently lacks CHELSA-BIOCLIM+-style
      bioclim variables; decide which to derive/source. (`02-ingest-preds-ch2025-2-etl.qmd`;
      wishlist table in `2-ingest-preds-ch2025-todo.md`)

### Economic (STATENT)
- [ ] 🟡 **STATENT under SSP logic.** `02-ingest-preds-statent.qmd` currently ingests
      only the historical employment state; it needs to be projected to match each
      SSP scenario's socioeconomic logic.

### Soil
- [ ] **Replace EIV soil layers** (`soil_ph`, `soil_nutrients`, `soil_moisture`,
      `soil_moisture_variability`, `soil_aeration`, `soil_humus`) with the Swiss
      Soil Property Map ingested by `2026-07-ssp-rsofun/2-forcing-soil-1-download.r`.
      (`02-ingest-preds-envidat-eiv.qmd`)

### New predictors
- [ ] **Region ID as indicator.** Ingest biogeographic regions
      (`ch.bafu.biogeographische_regionen`, 2056 shp) as a categorical predictor.
      (`01-ingest-lulc-data.qmd`)
- [ ] **Coordinates as predictors?** Evaluate whether raw E/N (or a smooth basis of
      them) should be added as predictors, weighed against location-identity leakage.
- [ ] **DEM hillshade semantics.** Hillshade was ingested but is probably meant as
      an insolation proxy — reconsider / replace with a proper insolation term.
      (`02-ingest-preds-dem.qmd`)

---

## LULC schema

- [ ] **Arealstatistik 2025.** Add `AS25_72` once the 2025 survey is finalised
      (currently 1985–2018 only). (`01-ingest-lulc-data.qmd`)
- [ ] **Deglaciated-area land-use class.** Introduce (post-SSP-implementation) a new
      class from the glacier inventory; needs disaggregation to represent succession
      on deglaciating areas, and interacts with the small-area inclusion threshold.
      (`01-ingest-lulc-data.qmd`)

---

## Feature selection

The split into viable-transition identification and covariate selection is done; the
parameters remain to be justified.

- [ ] 🟡 **`04-viable-transition-identification.qmd`** — justify the viability threshold
      `min_cardinality_abs` (currently 1000) from the `04d` observed-transitions plot.
- [ ] 🟡 **`05-covariate-selection.qmd`** — decide and document defensible GRRF
      (`gamma`/`num.trees`/`max.depth`) and covariance (`corcut`) parameters.

---

## Transition modelling, validation & extrapolation

`06`/`07` have reference implementations in `2025-10-valparish/` (as GLM); `08`/`09`/`09d`
are new to this experiment. The SSP demand curves are not yet wired in — the bulk of the
remaining MS9-phase-1 work.

- [ ] **`06-transition-modelling.qmd`** — mlr3 transition-potential models (valparish
      used GLM). Decide learner(s) and a **justified train/test split** (valparish
      left `sample_frac = 0.3` unmotivated).
- [ ] **`07-transition-rates.qmd`** — wire in the SSP land-use demand curves from
      `NCCS-SSP-scenarios/Tools/Transition_Tables.xlsx` (per SSP0/1/3/4/5) as the
      future transition-rate targets, replacing valparish's linear extrapolation.
- [ ] **`08-validate-backcasting.qmd`** (MS9 phase 3) — three sub-steps:
    - [ ] (a) **estimate patch/allocation parameters** from historical data (this is
          where allocation-parameter creation lives — no separate `alloc-params` step);
    - [ ] (b) **run the backcasting exercise** over different parameter choices;
    - [ ] (c) **validate** the backcast against observed Arealstatistik periods,
          for now using the **fuzzy similarity** metric available in evoland-plus.
          Define acceptance criteria.
- [ ] **`09-extrapolate.qmd`** — stochastic extrapolation (forward scenario projection
      to 2060 per SSP × climate framing).
- [ ] **`09d-report.qmd`** — diagnostic: reporting figures, tables, maps (human-facing
      outputs; mutates no state).

---

## Housekeeping / follow-ups

- [ ] **Fold the reference `.md` docs into `.qmd` prose** (literate programming), then
      delete them: `REFACTOR-valpar-local.md`, `2-ingest-preds-ch2025-todo.md`,
      `2-ingest-preds-ch2025-urls.md`. (The `fig-spm8a-ar6-wg1.png` asset stays.)
- [ ] **sonBASE noise predictor** — decide whether to re-include `noise_mean_100m`
      (present in `2025-10-valparish/2-ingest-preds-sonbase.r`, not carried to ssp-ch).

---

## Open questions

- [ ] **SSP→GWL / CO₂ mapping.** Finalise the per-SSP, per-period GWL and CO₂
      assignment (SSP0 → GWL1.5 fixed; the SSP5-8.5 tail is unresolved, above).
