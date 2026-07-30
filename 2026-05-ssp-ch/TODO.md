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
      partly unclear provenance; provenance now in each step's `.qmd` prose):
      `02-ingest-preds-dem.qmd`, `02-ingest-preds-envidat-eiv.qmd`,
      `02-ingest-preds-swisstlm3d.qmd`.
- [x] **Reference `.md` docs folded into the `.qmd` prose and removed**
      (`REFACTOR-valpar-local.md`, `2-ingest-preds-ch2025-todo.md`,
      `2-ingest-preds-ch2025-urls.md`); provenance/rationale now lives with the code.
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
- [x] **Bumped the evoland-plus pin** from `b40175f` to the tip of
      `bugfix/regular-period-lengths`. The old pin predates `alloc_clumpy` (PR #30), so
      `08`/`09` would have required DinamicaConsole; it also predates the `fit_full_models`
      robustness fixes. `rv.lock` still records the old sha — **`rv sync` must regenerate it**
      (the new pin adds `rpart` and `gifski` to suggests).
- [x] **Registered the scenario axis in `runs_t`** (`00-setup-db.qmd`): base `0` → one run per
      SSP → per-SSP climate framings (`current` climatology, `gwl` × q5/q50/q95).
- [x] **Established where the SSP demand actually lives** — `NCCS_simulation_LULC_areas.xlsx`
      (class-area targets + curve shapes, keyed by SSP, SSP0 included), *not*
      `Transition_Tables.xlsx`, whose per-SSP rate blocks are empty. See README.
- [x] **Fixed the `id_period = 0` fallback precedence upstream**
      (evoland-plus `inst/pred_data_wide.sql`, `inst/trans_pred_data.sql`). Both design-matrix
      queries put the period-0 baseline and the period-specific value in one aggregation group
      and resolved them with an unordered `first()`. Demonstrated against DuckDB 1.5.5: with
      scenario rows stored first, `pred_data_wide.sql` silently returned the **baseline**
      instead of the projection. This blocked any per-period scenario predictor.

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
- [x] 🟡 **Projected `-gwl` ingestion written** — `02-ingest-preds-ch2025-3-gwl.qmd`.
      Crosswalk + `id_run` encoding done; **never executed** (no R in the authoring
      environment). Three things still need a decision:
    - [ ] **Ratify the SSP → GWL crosswalk.** Currently provisional, isolated in one
          `rowwiseDT` in that step. See "Open questions" below.
    - [ ] **Decide the ordering.** The full cross product is ~4.4 × 10⁹ rows
          (4.1 M coords × 4 extrapolated periods × 15 scenario arms × 18 yearly indicators),
          which is not storable. The step therefore projects only the climate predictors that
          survived `05-covariate-selection.qmd`, which means it must run **after** `05`
          despite its `02-` number. Either accept that (documented in the step) or renumber
          the pipeline so predictor projection follows selection.
    - [ ] **Seasonal predictors have no projection.** CH2025 publishes `-gwl` aggregates
          `yearly` only, while `-obs` also has DJF/MAM/JJA/SON. Any seasonal predictor that
          survives `05` is silently frozen at its observed baseline under every GWL run.
          Either restrict the climate predictors offered to `05` to `*_yearly`, or accept and
          document the freeze. The step warns when it finds one.
- [x] **SSP5-8.5 late century** — capped at GWL3.0 (CH2025 publishes no higher level),
      consistent with `02-ingest-preds-ch2025-1-download.qmd`. Understates late-century SSP5
      warming; must be stated in reporting.
- [ ] **Bioclimatic indicators.** CH2025 currently lacks CHELSA-BIOCLIM+-style
      bioclim variables; decide which to derive/source. (wishlist in the appendix of
      `02-ingest-preds-ch2025-2-etl.qmd`)

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
      (`01-ingest-lulc-data.qmd`) — *blocked in the authoring environment:* every ingest step
      records a verified `url` + `md5sum`, and `data.geo.admin.ch` is denied by the sandbox
      network policy, so neither can be obtained. Needs a machine with access to that host.
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
- [ ] 🔴 **`05-covariate-selection.qmd` does not run.** Its second chunk calls
      `db$get_pred_filter_score(filter_fun = grrf_filter, num.trees =, max.depth =, gamma =,
      cores =)`. `grrf_filter`, `covariance_filter` and `get_pruned_trans_preds_t` do not
      exist in evoland-plus — verified absent at both the old pin and current main. The live
      API is `get_pred_filter_score(filter = <mlr3filters::Filter>, cluster =)`, which
      **scores but does not prune**; the first chunk assigns the scored table straight back to
      `db$trans_preds_t`, so the covariance filter the prose describes never happens either.
      Rebuild on `FilterImportance$new(learner = LearnerClassifGrrf$new())` plus
      `mlr3filters::FilterFindCorrelation` for the `corcut` role, with an explicit subset
      before commit.
- [ ] 🟡 **Then** decide and document defensible GRRF (`gamma`/`num.trees`/`max.depth`) and
      correlation (`corcut`) parameters.

---

## Transition modelling, validation & extrapolation

`06`/`07` have reference implementations in `2025-10-valparish/` (as GLM); `08`/`09`/`09d`
are new to this experiment. The SSP demand curves are not yet wired in — the bulk of the
remaining MS9-phase-1 work.

- [ ] **`06-transition-modelling.qmd`** — mlr3 transition-potential models (valparish
      used GLM). Decide learner(s) and a **justified train/test split** (valparish
      left `sample_frac = 0.3` unmotivated).
- [ ] **`07-transition-rates.qmd`** — wire in the SSP demand. Source is
      `NCCS-SSP-scenarios/Tools/NCCS_simulation_LULC_areas.xlsx` (class-area targets +
      curve shapes per SSP0/1/3/4/5), **not** `Transition_Tables.xlsx` — see README.
    - [ ] **Port the LP solver** (`lulcc.simulationtransitionratesolver.R`) into
          `evoland-experiments` as a sourceable function. Keep it experiment-local for now;
          upstreaming is tracked in [evoland-plus#32](https://github.com/ethzplus/evoland-plus/issues/32),
          which proposes replacing rate-based `extrapolate_trans_rates()` with an
          absolutes-based solver.
    - [ ] **Resample the demand to decadal steps.** The targets are 5-yearly (2025…2060);
          the pipeline is decadal, and period 8 ends 2064, so the 2060 target is interior.
          The solver takes explicit `Time_steps`/`Step_length`, so this is a matter of
          choosing the decadal target years, not of changing the solver.
    - [ ] **Reconcile the ported solver against the written formulation** (the docx behind
          [evoland-plus#32](https://github.com/ethzplus/evoland-plus/issues/32)). They are
          *not* the same model: the shipped LP works in absolute areas with a hard 99/101 %
          band on final areas and a temporal-smoothing term; the written version works in
          shares and adds quadratic terminal-fit and historic-preference terms, a
          zero-history penalty, hard-forbidden edges, minimax fairness, a ridge term and a
          feasibility precheck LP — and being quadratic it cannot run on `lpSolve::lp()`.
          Decide which is being reproduced before trusting the numbers.
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

- [ ] **sonBASE noise predictor** — decide whether to re-include `noise_mean_100m`
      (present in `2025-10-valparish/2-ingest-preds-sonbase.r`, not carried to ssp-ch).

---

## Open questions

- [ ] **SSP→GWL / CO₂ mapping.** Ratify the per-SSP, per-period GWL assignment. A
      **provisional** crosswalk is now encoded in one `rowwiseDT` in
      `02-ingest-preds-ch2025-3-gwl.qmd`, read off AR6 WG1 SPM.8a against the three levels
      CH2025 publishes:

      | | p5 (2025–34) | p6 (2035–44) | p7 (2045–54) | p8 (2055–64) |
      | --- | --- | --- | --- | --- |
      | SSP0 | 1.5 | 1.5 | 1.5 | 1.5 |
      | SSP1 | 1.5 | 1.5 | 2.0 | 2.0 |
      | SSP3 | 1.5 | 2.0 | 2.0 | 3.0 |
      | SSP4 | 1.5 | 2.0 | 2.0 | 3.0 |
      | SSP5 | 2.0 | 2.0 | 3.0 | 3.0 |

      SSP0 → GWL1.5 is fixed per README. SSP5's last two decades are **capped** at GWL3.0.
      SSP3 and SSP4 are currently identical, which may or may not be intended. CO₂ is not
      yet assigned at all.
- [ ] **Does the pipeline numbering need to change?** Projected climate predictors can only
      be materialised for the *selected* predictor set, so predictor projection now depends
      on `05`. Either keep the `02-…-3-gwl` number with a documented out-of-order run, or
      renumber so projection follows selection.
