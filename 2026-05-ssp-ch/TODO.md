# TODO — 2026-05-ssp-ch

Task tracker for the SSP-CH baseline (MS9 phase 1, plus the phase-3 backcasting
validation that lives here). Mirrors the pipeline table in
[`README.md`](README.md); inline `file:line` refs point at the relevant code.

Legend: ⬜ not started · 🟡 in progress / partial · ✅ done

---

## Concluded

- [x] `0-setup-db.r` — `ssp-ch.evolanddb`, full-CH 100 m coords grid, decadal
      periods (1985–2020 observed → 2060 extrapolated).
- [x] `1-ingest-lulc-data.r` — Arealstatistik NOAS04 LULC (1985/97/09/18). *(AS2025,
      bioregions, deglaciation still open — see below.)*
- [x] Reproducible predictor ingestion (replaces the ValPar local GeoTIFFs of
      partly unclear provenance; provenance in `REFACTOR-valpar-local.md`):
      `2-ingest-preds-dem.r`, `2-ingest-preds-envidat-eiv.r`,
      `2-ingest-preds-swisstlm3d.r`, `2-ingest-preds-pop.r`.
- [x] `2-ingest-preds-ch2025-1-download.r` — probe + throttled download of all 399
      CH2025 candidate URLs to cache.
- [x] `2-ingest-preds-ch2025-2-etl.r` — **observed** CH2025 predictors ingested
      (41 predictors, 1991–2020 → `id_period 0`). *(projected `-gwl` still open.)*
- [x] `3-neighbors.r` — neighbourhood predictors. *(currently land-use categories
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
      transition model. (`2-ingest-preds-ch2025-2-etl.r`; `ch2025-todo.md`)
- [ ] **SSP5-8.5 late century** (~2071–2100, ~5–6 °C) exceeds GWL3.0 — no CH2025
      aggregate. Decide: cap at GWL3 or drop the tail. (`…ch2025-1-download.r:19`)
- [ ] **Bioclimatic indicators.** CH2025 currently lacks CHELSA-BIOCLIM+-style
      bioclim variables; decide which to derive/source. (`…ch2025-2-etl.r:49`;
      wishlist table in `ch2025-todo.md`)

### Economic (STATENT)
- [ ] 🟡 **STATENT under SSP logic.** `2-ingest-preds-statent.r` currently ingests
      only the historical employment state; it needs to be projected to match each
      SSP scenario's socioeconomic logic. (`2-ingest-preds-statent.r`)

### Soil
- [ ] **Replace EIV soil layers** (`soil_ph`, `soil_nutrients`, `soil_moisture`,
      `soil_moisture_variability`, `soil_aeration`, `soil_humus`) with the Swiss
      Soil Property Map ingested by `2026-07-ssp-rsofun/2-forcing-soil-1-download.r`.
      (`2-ingest-preds-envidat-eiv.r:19`)

### New predictors
- [ ] **Region ID as indicator.** Ingest biogeographic regions
      (`ch.bafu.biogeographische_regionen`, 2056 shp) as a categorical predictor.
      (`1-ingest-lulc-data.r:5`)
- [ ] **Coordinates as predictors?** Evaluate whether raw E/N (or a smooth basis of
      them) should be added as predictors, weighed against location-identity leakage.
- [ ] **DEM hillshade semantics.** Hillshade was ingested but is probably meant as
      an insolation proxy — reconsider / replace with a proper insolation term.
      (`2-ingest-preds-dem.r:89`)

---

## LULC schema

- [ ] **Arealstatistik 2025.** Add `AS25_72` once the 2025 survey is finalised
      (currently 1985–2018 only). (`1-ingest-lulc-data.r:44`)
- [ ] **Deglaciated-area land-use class.** Introduce (post-SSP-implementation) a new
      class from the glacier inventory; needs disaggregation to represent succession
      on deglaciating areas, and interacts with the small-area inclusion threshold.
      (`1-ingest-lulc-data.r:7,113`)

---

## Feature selection

- [ ] 🟡 **Finalise `4-covariate-selection.r`.** The viable-transition threshold is
      not set (`min_cardinality_abs`, currently 1000; use the `4-no-obs-trans.svg`
      graph to justify), alongside the GRRF (`gamma`/`num.trees`/`max.depth`) and
      covariance (`corcut`) parameters. May need **splitting** into two scripts —
      viable-transition selection and feature selection. (`4-covariate-selection.r`)

---

## Transition modelling, validation & extrapolation

Steps `5`/`6` have reference implementations in `2025-10-valparish/` (as GLM);
`7`/`8`/`9` are new to this experiment. The SSP demand curves are not yet wired in.
This is the bulk of the remaining MS9-phase-1 work.

- [ ] **`5-transition-modelling.r`** — mlr3 transition-potential models (valparish
      used GLM). Decide learner(s) and a **justified train/test split** (valparish
      left `sample_frac = 0.3` unmotivated).
- [ ] **`6-transition-rates.r`** — wire in the SSP land-use demand curves from
      `NCCS-SSP-scenarios/Tools/Transition_Tables.xlsx` (per SSP0/1/3/4/5) as the
      future transition-rate targets, replacing valparish's linear extrapolation.
- [ ] **`7-validate-backcasting.r`** (MS9 phase 3) — three sub-steps:
    - [ ] (a) **estimate patch/allocation parameters** from historical data (this is
          where allocation-parameter creation lives — no separate `alloc-params` step);
    - [ ] (b) **run the backcasting exercise** over different parameter choices;
    - [ ] (c) **validate** the backcast against observed Arealstatistik periods,
          for now using the **fuzzy similarity** metric available in evoland-plus.
          Define acceptance criteria.
- [ ] **`8-extrapolate.r`** — stochastic extrapolation (forward scenario projection
      to 2060 per SSP × climate framing).
- [ ] **`9-report.r`** — reporting: figures, tables, maps. Optional/diagnostic
      (produces human-facing outputs, does not mutate the DB); a natural candidate
      for the diagnostic-step nomenclature under discussion.

---

## Open questions

- [ ] **SSP→GWL / CO₂ mapping.** Finalise the per-SSP, per-period GWL and CO₂
      assignment (SSP0 → GWL1.5 fixed; the SSP5-8.5 tail is unresolved, above).
