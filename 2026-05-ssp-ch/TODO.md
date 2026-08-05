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
- [x] **`05-covariate-selection.qmd` rebuilt on the live API.** It previously called
      `get_pred_filter_score(filter_fun = grrf_filter, ...)`; `grrf_filter`,
      `covariance_filter` and `get_pruned_trans_preds_t` do not exist in evoland-plus at either
      the old pin or main. The live API **scores but does not prune**, and the old first chunk
      assigned the scored table straight back, so it committed the *unpruned* cross product and
      the correlation stage never ran at all. Now two explicit stages:
      `flt("find_correlation")` → `FilterImportance$new(learner = LearnerClassifGrrf$new())`,
      with subsetting and a coverage check before commit.
- [x] **Dropped the correlation pre-filter.** An interim revision ran `FilterFindCorrelation`
      before GRRF. It is hard to defend: the threshold is arbitrary, *which* member of a
      correlated pair survives is decided by feature order rather than usefulness, and being
      target-blind it can drop the predictor carrying the signal. GRRF already handles
      redundancy target-awarely. One defensible mechanism beats two stacked heuristics.
      *(Noted for whenever it is reconsidered: `FilterFindCorrelation` is `integer`/`numeric`
      only, so a `factor` predictor makes it raise "unsupported feature types", which
      `pred_filter_worker()` converts into an all-`NA` score for the whole transition — a
      silent hole rather than a crash. It also scores ≈ `1 − max|r|`, so `corcut` translates to
      keeping `score > 1 - corcut`.)*
- [x] **`05` now refuses to run until the cut is chosen.** `importance_rel_cut` ships as
      `NA_real_` and the parameter chunk `stop()`s with instructions pointing at `05d`. There is
      no default to fall back on silently.
- [ ] 🟡 **Read `importance_rel_cut` off `05d-covariate-selection.qmd`.** The tempting
      parameter-free cut (`importance > 0`, on the assumption that GRRF zeroes uninformative
      predictors) **does not work** — reproducing evoland's `LearnerClassifGrrf` training path
      on a synthetic 3-signal / 13-noise task gives only 3 exact zeros out of 16 at every
      `gamma` tried, so `importance > 0` would retain 10 pure-noise predictors. What GRRF does
      give is a strongly *bimodal* distribution (signal 0.85–1.00, surviving noise 0.16–0.20),
      so the cut is defensible but its position is data-dependent. `05d` plots the distribution
      and tabulates survival at candidate cuts; `05` currently carries a provisional 0.5.
- [ ] 🟡 **Sensitivity-check the remaining parameters.** `corcut = 0.7` has a literature default
      (Dormann et al. 2013); `grrf_gamma` / `num.trees` / `max.depth` are reasoned but not yet
      justified against this data. Re-run across a small grid and report how much the retained
      set moves.
- [ ] **Runtime.** `regularization.factor` disables ranger's internal threading
      ("Parallelization deactivated"), so the per-transition `mirai` cluster is the only
      parallelism in `05`. Size `n_workers` accordingly.

---

## Transition modelling, validation & extrapolation

`06`/`07` have reference implementations in `2025-10-valparish/` (as GLM); `08`/`09`/`09d`
are new to this experiment. The SSP demand curves are not yet wired in — the bulk of the
remaining MS9-phase-1 work.

- [x] **`06-transition-modelling.qmd`** — written, unrun. `classif.ranger` selected on
      `classif.auc`, with a `classif.featureless` baseline fitted alongside so the forest's AUC
      is interpretable. AUC is the right criterion because `adjusted_trans_pot_v()`
      column-scales the stored probabilities — a monotone transform — so only the *ranking* of
      cells matters, and AUC is insensitive to the class imbalance of rare transitions. Includes
      the viability/model reconciliation the stochastic vignette requires.
- [x] **`06d-transition-modelling.qmd`** — held-out ROC per transition plus the AUC ranking.
      Computes the curves directly from the stored `PredictionClassif` rather than via
      `db$get_crossval_plots()`, which calls `autoplot()` with no `type` and so yields the
      default bar plot, with no way to pass `type = "roc"` through. Verified: the curve
      reproduces mlr3's `classif.auc` to 1e-5.
- [ ] **Justify the train/test split.** `sample_frac = 0.7` is evoland's default and what both
      vignettes use — a convention, not a justified choice (valparish left `0.3` unmotivated).
      A learning curve of AUC against subsample size for a few representative transitions would
      settle it, and would also reveal whether rare transitions are sample-starved.
- [ ] **Revisit the learner** once the baseline comparison has run. ranger is a deliberate
      first pass; if many transitions sit near AUC 0.5 the question is the predictor set, not
      the learner.
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
- [x] **`07-transition-rates-1-solver.qmd`** and **`07-transition-rates-2-legacy.qmd`** — written,
      unrun. Demand factored into `R/ssp-demand.R`, sourced by both so they solve against
      identical targets. Uses `evoland-plus@claude/linear-program-implementation-2wf1m7`.
- [x] **Targets are used unscaled.** The demand was elicited on 4,129,078 cells and this grid has
      4,129,079 — one cell — so normalising to shares and multiplying back out would change
      nothing while obscuring the provenance of the numbers. `ssp_demand_targets()` *checks* the
      grid size (0.1 % tolerance) and errors with rescaling instructions rather than doing it;
      `07-1` carries the note for anyone re-implementing at a coarser resolution.
- [x] **Evaluated that branch against these requirements.** All 38 of its tests pass, and it was
      exercised on the real demand: bounds from observed history, reachability precheck, coupled
      solve, `trans_rates_t` write for several runs, and — answering the open question in #32 —
      `trans_rate_areas()` recovers the solved trajectory from the rate table alone, verified to
      1.3 × 10⁻⁹ cells. No separate trajectory table is needed.
- [ ] 🔴 **Two of its defaults do not survive this data**; both are passed explicitly in `07-1`,
      and both would be better fixed upstream:
    - `trans_rate_bounds()` infers `step_years` and `stopifnot`s that the extrapolated periods
      are equal, but calendar decades are not: `create_periods_t("P10Y", …)` yields intervals of
      10.001369 / 9.998631 / 10.001369 / 9.998631 years, differing only by leap-day placement, so
      the check fires. Wants a tolerance.
    - `max_reachability_ratio = 10` aborts **every** scenario — glacier alone asks 11.2–12.4×.
      Confirms the point already made in #32: the precheck must quantify, not gate. Note it has
      to be lifted to `Inf`, not merely raised: `static` is absorbing so its ratio is `Inf`, and
      a trial value of 50 still aborted SSP0/SSP3/SSP4. Restore a finite gate once the `static`
      question below is settled.
- [x] **`static` no longer excluded as an anterior class** (`04`). It had been a pure sink, which
      made SSP0/SSP3/SSP4 miss target by 34,000–67,000 cells — static overshoots because its
      inflow cannot be shed, and another class undershoots to compensate under mass balance.
      The original does not treat it as a sink either:
      `Scripts/Preparation/Transition_identification.R` retains `Static → Shrubland` and
      `Static → Static`, re-adding them *after* the inclusion-threshold subset so they bypass the
      frequency filter, alongside hand-picked `Urban → Int_AG/Shrubland` and
      `Closed_Forest → Shrubland/Static/Grassland`. `lulcc.listbylulc.R:18` carries a
      commented-out exclusion of `Static` as an initial class. We apply `min_cardinality_abs`
      uniformly instead of curating by name.
    - [ ] **Re-measure after `04` runs.** Whether this actually relieves the target misses depends
          on which static-initial transitions clear the threshold. If `max_target_err` does not
          fall substantially, the remedy has to move to the demand side.
    - [ ] **Restore a finite `max_reachability_ratio`** in both `07` steps once the static ratios
          come back finite. It is at `Inf` only because `static` made them `Inf`.
- [ ] 🔴 **The `static` area targets are an elicitation defect, and we are knowingly keeping
      them.** `static` aggregates infrastructure, water, rock and scree. Some members genuinely
      convert (rock/scree revegetating to shrubland — the edge the original kept); most cannot.
      Assigning the class a per-scenario 2060 area target treats it as a land use whose extent is
      a policy outcome, when it is an aggregation artefact standing in for unstated assumptions
      about deglaciation, reservoirs and sealing. That the original's own model could not deliver
      SSP3's static target is the symptom. Retained for now to stay close to the replication;
      **not** endorsed. Proper fixes, both out of scope for MS9 phase 1:
    - [ ] Disaggregate `static` into convertible and non-convertible members — the deglaciation
          TODO already requires this.
    - [ ] Re-elicit demand against classes that can carry a target.
    - [ ] Until then, treat any reported `static` trajectory as an artefact and do not interpret
          it. Say so wherever it appears in `09d`.
- [ ] **Re-run the numbers against the real `trans_meta_t`.** Everything above used the original
      study's 21-edge calibration set as a stand-in, because no `ssp-ch.evolanddb` exists yet.
      This pipeline's viable set differs and will move the reachability bands, the target misses
      and the leakage figures.
- [ ] **Decide what the legacy runs are worth.** The committable legacy trajectories differ from
      the corrected ones by **≤ 7 cells, and 0 at the horizon**, in every scenario — because once
      non-viable edges are forbidden in both (required to commit at all), the shape mechanism is
      inert either way and the smoothing weight does not bite over four steps. The original's
      defects did not distort its trajectories; their whole practical effect was to route
      40–65 % of flow onto edges that do not exist, which its own allocator then dropped. The
      legacy runs are therefore cheap evidence for that claim rather than a different answer.
- [ ] **Neither the hard terminal band nor soft forbidden edges can be reproduced**, and the step
      says so. The original reached its 99–101 % band by leaking through forbidden edges; forbid
      the leakage and the band is infeasible (`lpSolve` status 2), so `07-2` substitutes the soft
      terminal fit. Defect 3's leakage is measured by a second, uncommitted solve —
      `trans_rates_from_solution()` refuses to persist a solution containing non-viable flow, and
      is right to, since those transitions have no `trans_pot_t` rows.
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
