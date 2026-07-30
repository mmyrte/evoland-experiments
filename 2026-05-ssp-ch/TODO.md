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
- [x] **Registered the scenario axis in `runs_t`** (`00-setup-db.qmd`), ordered
      **base → climate trajectory → SSP**. Climate sits above SSP because it is the only
      expensive per-run payload (4.1 M cells × 4 periods × N predictors) while the SSP tables
      are far lighter: ~2.2 B stored rows this way versus ~6.2 B with SSP on top, a factor 2.8.
      (Climate alone would suggest a factor 5; projected *employment* is also a per-run,
      per-period family and gets replicated per (SSP × climate) leaf, eating the difference.
      Only `trans_rates_t` and `intrv_meta_t` are genuinely tiny.) It also expresses the
      intended orthogonality —
      trajectories are shared objects several SSPs are realised against, not properties of one
      SSP. The technical ordering deliberately does not match the storytelling; run
      `description`s still read "SSP3 under …".
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
- [x] **Realise each SSP against multiple climate framings** — `current` climatology plus four
      shared CH2025 GWL trajectories, registered in `00-setup-db.qmd`. Orthogonality is now
      structural: trajectories are first-class runs that any SSP can be realised against.
    - [ ] Decide which (SSP × trajectory) pairings are actually allocated. `default_for_ssp`
          records the "own" pairing per SSP, but the full cross is registered and cheap.

---

## Data ingestion — remaining

### Climate (CH2025)
- [x] 🟡 **Projected `-gwl` ingestion written** — `02-ingest-preds-ch2025-3-gwl.qmd`.
      Crosswalk + `id_run` encoding done; **never executed** (no R in the authoring
      environment). Three things still need a decision:
    - [ ] **Ratify the GWL trajectories.** Four provisional schedules (`stable15`, `stab20`,
          `rise30`, `fast30`), isolated in one `rowwiseDT` in that step. See "Open questions".
    - [ ] **Decide the ordering.** Inverting the run hierarchy cut stored climate from 60
          copies to 12, but the full indicator set is still ~1.2 × 10⁹ rows at the q50 default.
          The step therefore projects only the climate predictors that survived
          `05-covariate-selection.qmd`, which means it must run **after** `05` despite its
          `02-` number. Either accept that (documented in the step) or renumber the pipeline so
          predictor projection follows selection.
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
- [ ] 🟡 **STATENT under SSP logic.** `02-ingest-preds-statent.qmd` currently ingests only the
      historical employment state.

      **What the original actually does** (`Scripts/Preparation/Simulation_predictor_prep.R`),
      since this was an open question: *nothing scenario-specific in code*. The SSP signal is
      entirely exogenous — a project-internal table `Data/Preds/NCCS_future_FTE.csv` giving
      **canton × sector × 5 future years** of FTE totals (in thousands) per SSP. The script
      only: joins the 2020 value from the historical data, linearly interpolates to the model
      time steps with `stats::approx()`, rasterises per canton, and differences consecutive
      periods into `chg_FTE_{period}_{Sector}_{SSP}.tif`. So "how is SSP3 different" has no
      answer in the code — it is whatever the elicited CSV says.

      **Now implemented** — `02-ingest-preds-statent-ssp.qmd` (written, logic verified against
      the real CSV; the full step is unrun). `NCCS_future_FTE.csv` was recovered from the
      Zenodo deposit and supplied directly. Method: interpolate each cantonal sectoral
      trajectory to the period mid-years, then scale the observed hectare-level pattern by the
      canton's growth ratio — the input says how much employment a canton has, not where within
      it moves, so the within-canton pattern is held fixed.

      Remaining:
    - [ ] 🔴 **Provenance of the elicited numbers is unknown** and is now a disclosed gap: the
          original labels the file `Data_citation = "Project Internal"`, and we have found no
          method, assumptions, panel or version behind it. Everything the pipeline projects
          about employment inherits that opacity. Either find the documentation or disclose it
          wherever these predictors influence a result.
    - [ ] **`NCCS_future_population.csv`** (canton × SSP × year, millions) exists with the same
          provenance status and is *not* ingested — municipal population was discarded as a
          predictor. Revisit only if a population predictor is reinstated.
    - [ ] **Pin a fetchable source.** The CSV is read from the evoland cache with a pinned md5
          rather than downloaded, because no verifiable direct URL was reachable. Replace with
          a `download_and_verify()` call once one is confirmed.
    - [ ] **Employment does not relocate within a canton** under this method, only expand or
          contract in place. That is what the source supports; the original is coarser still
          (it rasterises the cantonal value directly, discarding the hectare pattern). Decide
          whether that is acceptable for the transition models.

      *(Two incidental defects in the original, in case they matter for interpreting its
      outputs: the 2020 FTE layers are `file.copy`'d identically across all five SSPs, and the
      `terra::rasterize`/`writeRaster` calls in the projection loop are commented out, so the
      shipped script builds only the path table. There is also a live typo — `kanton_data`
      referenced where `canton_data` is defined.)*

### Soil
- [x] **Ingest the Swiss Soil Property Map** — `02-ingest-preds-soil.qmd` (written, unrun).
      sand / clay / OC at 0/30/60/100 cm, area-weighted mean from the native 30 m grid onto the
      100 m grid via `terra::project(method = "average")`. Point extraction would subsample and
      discard ~8/9 of the source. ~6 GB of downloads.
- [ ] 🔴 **This is not the full replacement the TODO assumed.** SSPM as fetched carries no pH
      and no nutrient layer, so retiring all six EIV soil predictors would *lose* `soil_ph` and
      `soil_nutrients` outright. The step therefore ingests SSPM **alongside** the EIVs and
      removes nothing. Decide per predictor after `05` scores them against each other:
    - `soil_humus` → superseded by `soil_oc_*` (OC is the measurement the EIV indicates).
    - `soil_moisture`, `soil_moisture_variability`, `soil_aeration` → only *partly* superseded
      by texture; the real replacement is the WHC that
      `2026-07-ssp-rsofun/2-forcing-soil-2-whc.r` derives by pedotransfer. Until that is
      ingested here, texture is a rawer predictor, not a better one.
    - `soil_ph`, `soil_nutrients` → **no SSPM counterpart fetched**. The record reportedly has
      N and P layers the rsofun step skips; whether they can stand in is untested.
- [ ] **Depth handling.** The four depths are ingested as separate predictors (12 total, where
      the EIVs offered 6) because topsoil governs cultivation and deeper layers govern water
      storage. If that proves unwieldy in `05`, the alternative is a trapezoidal 0–100 cm
      profile mean per property plus the 0 cm value — noted in the step, not implemented.
- [ ] **Optional within-cell heterogeneity.** `derive_heterogeneity = FALSE` in the step would
      add per-property within-hectare sd (a uniform loam and a half-sand/half-clay hectare have
      the same mean but very different value). Speculative; nearly free. Decide whether to enable.

### New predictors
- [x] **Region ID as indicator** — `02-ingest-preds-bioregions.qmd` (written, unrun).
      6 regions + 12 subregions as `data_type = "factor"` predictors. The published checksum is
      a SHA-256 multihash, not md5, so the md5 was computed from the archive after verifying
      its SHA-256 against the STAC entry.
    - [ ] The two are strictly nested and therefore collinear; `05` should retain at most one
          per transition. Confirm the correlation filter actually does that for factors.
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
- [x] **Analysed the solver integration** — see [`notes-lp-solver.md`](notes-lp-solver.md).
      The shipped LP was run on the real demand numbers (all five SSPs solve in <1 s with
      `lpSolve`). Headline conclusions:
    - The "replace rates with absolutes" premise is **already true of the shipped solver** —
      every decision variable is an area, every constraint an area balance; rates appear only
      as coefficients in the soft bound rows and in the final unit conversion. The rate-based
      thing worth replacing is `extrapolate_trans_rates()`, and absolutes beat it because a
      *coupled solver* beats *21 independent `lm()` fits*, not because of units.
    - Absolutes vs shares is **provably the same LP** up to a scalar. Shares' one real win is
      grid-portability (the demand is on a 4,129,078-cell grid this pipeline will not
      reproduce); rates are effectively mandatory at the allocator interface. Suggested
      layering: shares stored, absolute cells in the solver, rate + count on output.
    - The mass-conservation argument in
      [evoland-plus#32](https://github.com/ethzplus/evoland-plus/issues/32) **does not survive
      measurement**: running the `lm()` path end-to-end gives −0.0000 % area drift and no
      outflow row above 1. Conservation is structural in the simulator. The real deficiency is
      that `extrapolate_trans_rates()` has *no input for a scenario target* and lands 10–19 %
      off every SSP.
- [ ] 🔴 **Three defects in the shipped solver**, found by running it. Two independently
      verified here by reading the source:
    - `build_diff_row()` is called as `build_diff_row(l_i, t_i, t_i + 1, ratio)`, so
      `t2 - 1 == t1` and its fourth assignment **overwrites its first**. The shape row is not
      what its comments say, the whole `chosen_shape` mechanism is dead code, and the term
      dominates the objective ~3,700× — meaning the LP was effectively not minimising
      rate-bound violations. Fixing one line drops the objective from ~305 to ~0.05 and cuts
      out-of-bounds churn by up to 35 %.
    - `Step_length` is `5`, a scalar, at the shipped call site
      (`Simulation_trans_tables_prep.R:14,171`) but is indexed `Step_length[t_i + 1]` — `NA`
      from the first iteration onward. `lpSolve::lp()` swallows the `NA` and returns a
      different answer with `status = 0`.
    - `r_max == 0` yields `x − devUpper ≤ 0` rather than `x = 0`, so "forbidden" edges are
      merely cheap: 62k–289k cells/scenario flow along `static → arable`,
      `closed_forest → static`, `glacier → grassland`.
    - [ ] **Decide the replication stance.** If the published Zenodo outputs were produced by
          this revision, faithful replication means reproducing the bugs. Recommendation in
          the notes: implement both behind a `shipped_bugs` flag, diff once, then use the
          fixed version downstream.
    - [ ] `chosen_shape` is inert for a *second*, independent reason that survives the fix — a
          straight line satisfies all five shapes with equality. Making shapes bite would
          therefore **change** the replication target; that is a call for the original
          elicitation's author.
- [ ] **Build `trans_rate_reachability()` first.** A pure-LP implementation of the docx's
      `compute_final_bounds` (~60 lines, needs no targets) found **24 of 50 SSP × class targets
      unreachable** under observed transition bounds — several by 4–5×, glacier by 1.81× in all
      five scenarios — with 42–63 % of solved flow outside the historic envelope. This is a
      *result*, not a diagnostic, and it means #32's "fail loudly on infeasible targets" would
      abort every scenario: the soft bounds are load-bearing and the precheck's job is to
      quantify, not gate.
    - [ ] ⚠️ **Re-run before quoting.** Those numbers use the original study's 21-edge
          calibration table as a stand-in, because no `ssp-ch.evolanddb` exists yet. This
          pipeline's `trans_meta_t` (`min_cardinality_abs = 1000`, `static` excluded as
          anterior) is a different rule; a broader viable set would widen the bands and could
          change the counts substantially.
- [ ] **`07-transition-rates.qmd`** — wire in the SSP demand. Source is
      `NCCS-SSP-scenarios/Tools/NCCS_simulation_LULC_areas.xlsx` (class-area targets +
      curve shapes per SSP0/1/3/4/5), **not** `Transition_Tables.xlsx` — see README.
    - [ ] **Port the LP solver** to `2026-05-ssp-ch/R/trans-rate-solver.R` as three functions
          (`trans_rate_bounds` / `trans_rate_reachability` / `solve_trans_rates`); add
          `lpSolve` to `rproject.toml`. Keep it experiment-local for now;
          upstreaming is tracked in [evoland-plus#32](https://github.com/ethzplus/evoland-plus/issues/32),
          which proposes replacing rate-based `extrapolate_trans_rates()` with an
          absolutes-based solver.
    - [ ] **Resample the demand to decadal steps.** The targets are 5-yearly (2025…2060);
          the pipeline is decadal, and period 8 ends 2064, so the 2060 target is interior.
          The solver takes explicit `Time_steps`/`Step_length`, so this is a matter of
          choosing the decadal target years, not of changing the solver.
    - [ ] **Four docx ideas are worth porting and are all LP-representable** — the precheck,
          hard-forbidden edges, an L1 terminal-fit term (the ±1 % band is currently a *bias*:
          every class parks on a band edge) and a minimax fairness bound. **No QP dependency
          needed.**
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

      | Trajectory | p5 (2025–34) | p6 (2035–44) | p7 (2045–54) | p8 (2055–64) |
      | --- | --- | --- | --- | --- |
      | `stable15` (default SSP0) | 1.5 | 1.5 | 1.5 | 1.5 |
      | `stab20` (default SSP1) | 1.5 | 1.5 | 2.0 | 2.0 |
      | `rise30` (default SSP3, SSP4) | 1.5 | 2.0 | 2.0 | 3.0 |
      | `fast30` (default SSP5) | 2.0 | 2.0 | 3.0 | 3.0 |

      SSP0 → GWL1.5 is fixed per README. `fast30`'s last two decades are **capped** at GWL3.0.
      SSP3 and SSP4 currently share `rise30`, which may or may not be intended — if they should
      differ, a fifth trajectory is needed. CO₂ is not yet assigned at all.
- [ ] **Does the pipeline numbering need to change?** Projected climate predictors can only
      be materialised for the *selected* predictor set, so predictor projection now depends
      on `05`. Either keep the `02-…-3-gwl` number with a documented out-of-order run, or
      renumber so projection follows selection.
