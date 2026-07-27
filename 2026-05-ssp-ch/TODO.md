# TODO — 2026-05-ssp-ch

Task tracker for the SSP-CH baseline (MS9 phase 1/3, plus the phase-3 validation
that lives here). Grouped by pipeline stage; each item notes its origin
(reminder, inline `file:line`, or reference doc).

Legend: ⬜ not started · 🟡 in progress / partial · ✅ done

---

## Concluded

- [x] `0-setup-db.r` — `ssp-ch.evolanddb`, full-CH 100 m coords grid, decadal
      periods (1985–2020 observed → 2060 extrapolated).
- [x] `1-ingest-lulc-data.r` — Arealstatistik NOAS04 LULC ingestion (1985/97/09/18).
- [x] Reproducible predictor ingestion refactor (replaces ValPar `file://`
      sources) — see `REFACTOR-valpar-local.md`:
    - [x] `2-ingest-preds-envidat-eiv.r` (SPEEDMIND EIV, continuous CWMs, md5 verified)
    - [x] `2-ingest-preds-dem.r` (DHM25 → elevation/slope/aspect/hillshade)
    - [x] `2-ingest-preds-swisstlm3d.r` (distance to lakes/rivers/roads)
    - [x] `2-ingest-preds-pop.r`, `2-ingest-preds-statent.r`
- [x] `2-ingest-preds-ch2025-1-download.r` — probe + throttled download of all 399
      CH2025 candidate URLs to cache.
- [x] `2-ingest-preds-ch2025-2-etl.r` — **observed** CH2025 predictors ingested
      (41 predictors, 1991–2020 → `id_period 0`).
- [x] `3-neighbors.r` — neighbourhood predictors.

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

### Soil
- [ ] **Replace EIV soil layers** (`soil_ph`, `soil_nutrients`, `soil_moisture`,
      `soil_moisture_variability`, `soil_aeration`, `soil_humus`) with the Swiss
      Soil Property Map ingested by `2026-07-ssp-rsofun/2-forcing-soil-1-download.r`.
      (`2-ingest-preds-envidat-eiv.r:19`)

### New predictors (from reminders)
- [ ] **Region ID as indicator.** Ingest biogeographic regions
      (`ch.bafu.biogeographische_regionen`, 2056 shp) as a categorical predictor.
      (reminder "ssp-ch: ingestion region ID as indicator"; `1-ingest-lulc-data.r:5`)
- [ ] **Coordinates as predictors?** Evaluate whether raw E/N (or a smooth basis of
      them) should be added as predictors, and whether that is desirable vs. leakage
      of location identity. (reminder "ssp-ch: ingest coordinates as predictors?")
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

- [ ] 🟡 **Finalise covariate selection.** `4-covariate-selection.r` runs GRRF
      importance + covariance filtering but key thresholds are placeholders:
      transition `min_cardinality_abs` (currently 1000, uses the
      `4-no-obs-trans.svg` graph to justify), GRRF `gamma`/`num.trees`/`max.depth`,
      covariance `corcut`. Decide and document defensible values. (reminder
      "ssp-ch: feature selection"; `4-covariate-selection.r`)

---

## Transition modelling & validation  (MS9 phase 1 core + phase 3)

The `5`/`6`/`7` stages exist in `2025-10-valparish/` but are **not yet ported**
here, and the SSP demand curves are not yet wired in.

- [ ] **`5-transition-modelling.r`** — port mlr3 transition modelling to SSP-CH.
      Decide learner(s) and a **justified train/test split** (valparish left
      `sample_frac = 0.3` unmotivated).
- [ ] **`6-transition-rates.r` / demand curves.** Wire in the SSP land-use demand
      curves from `NCCS-SSP-scenarios/Tools/Transition_Tables.xlsx` (per SSP1/3/4/5)
      as the future transition-rate targets, replacing valparish's naive linear
      extrapolation.
- [ ] **`7-alloc-params.r`** — allocation parameters for the SSP runs.
- [ ] **MS9 phase 3 — validate transition models.** Validate the mlr3 models,
      including **backcasting** against observed Arealstatistik periods (predict a
      held-out historical period from earlier ones and score it). Define the metrics
      and acceptance criteria. (reminder "MS9 phase 3/3: validate transition models
      (mlr3 → backcasting)")

---

## Open questions

- [ ] Confirm the naming (`2026-05` vs `2026-06`) — rename the directory or keep
      the alias note in the README.
- [ ] Confirm SSP set (SSP1/3/4/5; SSP2 excluded) and each scenario's CO₂/GWL
      mapping for the projected runs.
