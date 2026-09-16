# TODO — 2026-05-ssp-ch

Task tracker for the SSP-CH baseline (MS9 phase 1, plus the phase-3 backcasting
validation that lives here). Mirrors the pipeline table in [`README.md`](README.md);
refs point at the relevant `.qmd`.

Legend: ⬜ not started · 🟡 in progress / partial · ✅ done

---

## Handover — read this first

The full pipeline up to 070 has been run. `080` and `090` are now **written but never run
against the real database** — only against a 40×40-cell synthetic replica of this pipeline's
structure, which is enough to prove the API calls and the metrics work but says nothing about
runtime or results. `090d` is still not written.

What they contain, and the parameters to look at first:

- **`080-validate-backcasting.qmd`** — observed rates → fitted models → estimated allocation
  parameters → backcast periods 2→3→4, then both requested validations: fuzzy similarity of
  differences per transition, and cross-tabulation (hard per replicate, soft over the replicate
  ensemble) reduced to a figure of merit against a closed-form random-allocation null.
  `n_perturbations` × `n_replicates` is the cost driver — it ships at 3 × 3 = 12 member runs,
  each re-predicting transition potentials over the full grid for every allocated period.
  **Nobody has measured what one of those costs on 4.1 M cells.** Start at 1 × 2 and grow.
- **`090-extrapolate.qmd`** — allocates the 070 rates forward per (SSP × climate framing ×
  replicate), subset via `ssp_subset` / `climate_subset` to SSP1+SSP3 under `current` as asked.
  Verifies realised areas against `trans_rate_areas()`, which is the under-delivery check 070
  defers to it.

Both steps are **interventions-free** by design (first iteration). Every place an intervention
would attach carries a comment saying what it would take; see "Interventions" below for the two
that need an upstream change before they are possible at all.

Neither step branches Monte-Carlo members *per allocation time step*, as the earlier handover
suggested: the replicates branch at the root and chain, because a 40-year backcast is testing
the compounding trajectory. The one-step-ahead variant needs no new machinery and the recipe is
in `080`'s prose — worth adding when per-step skill becomes the question.

**Environment.** R 4.6.1 on Ubuntu 24.04 with r2u; the working toolchain used for the
component checks was data.table 1.18.4 (`rowwiseDT` needs ≥ 1.15), terra 1.9.34, mlr3 1.7.1,
lpSolve 5.6.23, GDAL 3.8.4 / GEOS 3.12.1 / PROJ 9.4.0. One environment trap worth knowing:
duckdb resolves its extension cache per driver object, and it fetches extensions over **http**,
which some proxies refuse — if `evoland_db$new()` fails with a 403 on `spatial.duckdb_extension`,
pre-place the extension under `$DUCKDB_R_HOME/extensions/<version>/<platform>/`.

---

## Concluded

- [x] **Adopted the Quarto pipeline + `NNd` diagnostic convention** (see top-level
      README): converted all steps to `.qmd`, two-digit stages, `_quarto.yml`
      (`freeze: auto`), and reworked `execute-all.sh`. Split old `4-covariate-selection`
      into `04-viable-transition-identification` (core) + `04d-…` (diagnostic) +
      `05-covariate-selection` (core). `999-dump-preds-raster` → `02d-ingest-preds-ch2025-check`.
- [x] `00-setup-db.qmd` — `ssp-ch.evolanddb`, full-CH 100 m coords grid, decadal
      periods (1985–2020 observed → 2060 extrapolated).
- [x] `01-ingest-lulc-data.qmd` — Arealstatistik NOAS04 LULC (1985/97/09/18). _(AS2025,
      bioregions, deglaciation still open — see below.)_
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
      (41 predictors, 1991–2020 → `id_period 0`). _(projected `-gwl` still open.)_
- [x] `03-neighbors.qmd` — neighbourhood predictors. _(currently land-use categories
      only; extending to other predictors is open.)_
- [x] **Bumped the evoland-plus pin** from `b40175f` to the tip of
      `bugfix/regular-period-lengths`. The old pin predates `alloc_clumpy` (PR #30), so
      `08`/`09` would have required DinamicaConsole; it also predates the `fit_full_models`
      robustness fixes. `rv.lock` still records the old sha — **`rv sync` must regenerate it**
      (the new pin adds `rpart` and `gifski` to suggests).
- [x] **Registered the scenario axis in `runs_t`** (`00-setup-db.qmd`), ordered
      **base → climate trajectory → SSP**. Climate sits above SSP because it is the only
      expensive per-run payload (4.1 M cells × 4 periods × N predictors) while the SSP tables
      are far lighter: ~2.2 B stored rows this way versus ~6.2 B with SSP on top, a factor 2.8.
      (Climate alone would suggest a factor 5; projected _employment_ is also a per-run,
      per-period family and gets replicated per (SSP × climate) leaf, eating the difference.
      Only `trans_rates_t` and `intrv_meta_t` are genuinely tiny.) It also expresses the
      intended orthogonality —
      trajectories are shared objects several SSPs are realised against, not properties of one
      SSP. The technical ordering deliberately does not match the storytelling; run
      `description`s still read "SSP3 under …".
- [x] **Established where the SSP demand actually lives** — `NCCS_simulation_LULC_areas.xlsx`
      (class-area targets + curve shapes, keyed by SSP, SSP0 included), _not_
      `Transition_Tables.xlsx`, whose per-SSP rate blocks are empty. See README.
- [x] **Fixed the `id_period = 0` fallback precedence upstream** —
      [evoland-plus#41](https://github.com/ethzplus/evoland-plus/pull/41), touching
      `inst/pred_data_wide.sql` and `inst/trans_pred_data.sql`. Both design-matrix
      queries put the period-0 baseline and the period-specific value in one aggregation group
      and resolved them with an unordered `first()`. Demonstrated against DuckDB 1.5.5: with
      scenario rows stored first, `pred_data_wide.sql` silently returned the **baseline**
      instead of the projection. This blocked any per-period scenario predictor.
      Precedence is decided **per slice**, not per coordinate: if any coordinate carries a
      period-specific value for a predictor, that whole slice is used and coordinates it does
      not cover come back `NA` rather than silently reverting to the baseline. Covered by a
      test in `inst/tinytest/test_db_evoland.R` (verified to fail against the pre-fix SQL).

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

- [ ] NAs introduced at country borders, unclear how. for now averaged for fill_value
- [x] 🟡 **Projected `-gwl` ingestion written** — `02-ingest-preds-ch2025-3-gwl.qmd`.
      Crosswalk + `id_run` encoding done; **never executed** (no R in the authoring
      environment). Three things still need a decision:
  - [x] **Ratify the GWL trajectories.** Four provisional schedules (`stable15`, `stab20`,
        `rise30`, `fast30`), isolated in one `rowwiseDT` in that step. See "Open questions".
  - [ ] **Decide the ordering.** Inverting the run hierarchy cut stored climate from 60
        copies to 12, but the full indicator set is still ~1.2 × 10⁹ rows at the q50 default.
        The step therefore projects only the climate predictors that survived
        `05-covariate-selection.qmd`, which means it must run **after** `05` despite its
        `02-` number. Either accept that (documented in the step) or renumber the pipeline so
        predictor projection follows selection.
  - [x] **Seasonal predictors have no projection.** CH2025 publishes `-gwl` aggregates
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

- [x] 🟡 **STATENT under SSP logic.** `02-ingest-preds-statent.qmd` currently ingests only the
      historical employment state.
- [x] 🔴 **Provenance of the elicited numbers is unknown** and is now a disclosed gap: the
      original labels the file `Data_citation = "Project Internal"`, and we have found no
      method, assumptions, panel or version behind it. Everything the pipeline projects
      about employment inherits that opacity. Either find the documentation or disclose it
      wherever these predictors influence a result.
- [x] **`NCCS_future_population.csv`** (canton × SSP × year, millions) exists with the same
      provenance status and is _not_ ingested — municipal population was discarded as a
      predictor. Revisit only if a population predictor is reinstated.
- [x] **Pin a fetchable source.** The CSV is read from the evoland cache with a pinned md5
      rather than downloaded, because no verifiable direct URL was reachable. Replace with
      a `download_and_verify()` call once one is confirmed.
- [x] **Employment does not relocate within a canton** under this method, only expand or
      contract in place. That is what the source supports; the original is coarser still
      (it rasterises the cantonal value directly, discarding the hectare pattern). Decide
      whether that is acceptable for the transition models.

### Soil

- [x] **Ingest the Swiss Soil Property Map** — `02-ingest-preds-soil.qmd` (written, unrun).
      sand / clay / OC at 0/30/60/100 cm, area-weighted mean from the native 30 m grid onto the
      100 m grid via `terra::project(method = "average")`. Point extraction would subsample and
      discard ~8/9 of the source. ~6 GB of downloads.
- [x] 🔴 **This is not the full replacement the TODO assumed.** SSPM as fetched carries no pH
      and no nutrient layer, so retiring all six EIV soil predictors would _lose_ `soil_ph` and
      `soil_nutrients` outright. The step therefore ingests SSPM **alongside** the EIVs and
      removes nothing. Decide per predictor after `05` scores them against each other:
  - `soil_humus` → superseded by `soil_oc_*` (OC is the measurement the EIV indicates).
  - `soil_moisture`, `soil_moisture_variability`, `soil_aeration` → only _partly_ superseded
    by texture; the real replacement is the WHC that
    `2026-07-ssp-rsofun/2-forcing-soil-2-whc.r` derives by pedotransfer. Until that is
    ingested here, texture is a rawer predictor, not a better one.
  - `soil_ph`, `soil_nutrients` → **no SSPM counterpart fetched**. The record reportedly has
    N and P layers the rsofun step skips; whether they can stand in is untested.
- [x] **Depth handling.** The four depths are ingested as separate predictors (12 total, where
      the EIVs offered 6) because topsoil governs cultivation and deeper layers govern water
      storage. If that proves unwieldy in `05`, the alternative is a trapezoidal 0–100 cm
      profile mean per property plus the 0 cm value — noted in the step, not implemented. - [ ] future handling - eliminate collinearity at feature selection
- [x] **Optional within-cell heterogeneity.** `derive_heterogeneity = FALSE` in the step would
      add per-property within-hectare sd (a uniform loam and a half-sand/half-clay hectare have
      the same mean but very different value). Speculative; nearly free. Decide whether to enable. _let's not do this, probably only makes sense for sub-hectare DEM_.

### New predictors

- [x] **Region ID as indicator** — `02-ingest-preds-bioregions.qmd` (written, unrun).
      6 regions + 12 subregions as `data_type = "factor"` predictors. The published checksum is
      a SHA-256 multihash, not md5, so the md5 was computed from the archive after verifying
      its SHA-256 against the STAC entry.
  - [ ] The two are strictly nested and therefore collinear; `05` should retain at most one
        per transition. The correlation pre-filter that would have handled this was dropped
        (see "Feature selection"), and would not have worked on factors anyway, so this now
        rests entirely on GRRF's regularisation. Check the retained sets after `05` runs.
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

- [x] 🟡 **`04-viable-transition-identification.qmd`** — justify the viability threshold
      `min_cardinality_abs` (currently 1000) from the `04d` observed-transitions plot.
- [x] **`05-covariate-selection.qmd` rebuilt on the live API.** It previously called
      `get_pred_filter_score(filter_fun = grrf_filter, ...)`; `grrf_filter`,
      `covariance_filter` and `get_pruned_trans_preds_t` do not exist in evoland-plus at either
      the old pin or main. The live API **scores but does not prune**, and the old first chunk
      assigned the scored table straight back, so it committed the _unpruned_ cross product and
      the correlation stage never ran at all. Now a single explicit stage —
      `FilterImportance$new(learner = LearnerClassifGrrf$new())` — with subsetting and a
      coverage check before commit.
- [x] **Dropped the correlation pre-filter.** An interim revision ran `FilterFindCorrelation`
      before GRRF. It is hard to defend: the threshold is arbitrary, _which_ member of a
      correlated pair survives is decided by feature order rather than usefulness, and being
      target-blind it can drop the predictor carrying the signal. GRRF already handles
      redundancy target-awarely. One defensible mechanism beats two stacked heuristics.
      _(Noted for whenever it is reconsidered: `FilterFindCorrelation` is `integer`/`numeric`
      only, so a `factor` predictor makes it raise "unsupported feature types", which
      `pred_filter_worker()` converts into an all-`NA` score for the whole transition — a
      silent hole rather than a crash. It also scores ≈ `1 − max|r|`, so `corcut` translates to
      keeping `score > 1 - corcut`.)_
- [x] **`05` now refuses to run until the cut is chosen.** `importance_rel_cut` ships as
      `NA_real_` and the parameter chunk `stop()`s with instructions pointing at `05d`. There is
      no default to fall back on silently.
- [x] 🟡 **Read `importance_rel_cut` off `05d-covariate-selection.qmd`.** The tempting
      parameter-free cut (`importance > 0`, on the assumption that GRRF zeroes uninformative
      predictors) **does not work** — reproducing evoland's `LearnerClassifGrrf` training path
      on a synthetic 3-signal / 13-noise task gives only 3 exact zeros out of 16 at every
      `gamma` tried, so `importance > 0` would retain 10 pure-noise predictors. What GRRF does
      give is a strongly _bimodal_ distribution (signal 0.85–1.00, surviving noise 0.16–0.20),
      so the cut is defensible but its position is data-dependent. `05d` plots the distribution
      and tabulates survival at candidate cuts; `05` ships `NA_real_` and refuses to run until
      the cut is chosen from real data. **This is the one hard stop in the pipeline.**
- [ ] 🟡 **Sensitivity-check the remaining parameters.** With the correlation filter gone, the
      free parameters in `05` are `grrf_gamma`, `num.trees` and `max.depth` — reasoned, but not
      yet justified against this data — plus `importance_rel_cut` above. Re-run across a small
      grid and report how much the retained set moves.
- [x] **Runtime.** `regularization.factor` disables ranger's internal threading
      ("Parallelization deactivated"), so the per-transition `mirai` cluster is the only
      parallelism in `05`. Size `n_workers` accordingly.

---

## Transition modelling

`06`/`07` have reference implementations in `2025-10-valparish/` (as GLM); `08`/`09`/`09d`
are new to this experiment. The SSP demand curves are not yet wired in — the bulk of the
remaining MS9-phase-1 work.

- [x] **`06-transition-modelling.qmd`** — written, unrun. `classif.ranger` selected on
      `classif.auc`, with a `classif.featureless` baseline fitted alongside so the forest's AUC
      is interpretable. AUC is the right criterion because `adjusted_trans_pot_v()`
      column-scales the stored probabilities — a monotone transform — so only the _ranking_ of
      cells matters, and AUC is insensitive to the class imbalance of rare transitions. Includes
      the viability/model reconciliation the stochastic vignette requires.
- [x] **`06d-transition-modelling.qmd`** — held-out ROC per transition plus the AUC ranking.
      Computes the curves directly from the stored `PredictionClassif` rather than via
      `db$get_crossval_plots()`, which calls `autoplot()` with no `type` and so yields the
      default bar plot, with no way to pass `type = "roc"` through. Verified: the curve
      reproduces mlr3's `classif.auc` to 1e-5.
- [ ] **Justify the train/test split.** `sample_frac = 0.7` is evoland's default and what both
      vignettes use, not a justified choice.
      A learning curve of AUC against subsample size for a few representative transitions would
      settle it, and would also reveal whether rare transitions are sample-starved.
- [x] **Revisit the learner** once the baseline comparison has run. ranger is a deliberate
      first pass; if many transitions sit near AUC 0.5 the question is the predictor set, not
      the learner.

## transition rates

- [x] **Analysed the solver integration** — see [`notes-lp-solver.md`](notes-lp-solver.md).
      The shipped LP was run on the real demand numbers (all five SSPs solve in <1 s with
      `lpSolve`). Headline conclusions:
  - The "replace rates with absolutes" premise is **already true of the shipped solver** —
    every decision variable is an area, every constraint an area balance; rates appear only
    as coefficients in the soft bound rows and in the final unit conversion. The rate-based
    thing worth replacing is `extrapolate_trans_rates()`, and absolutes beat it because a
    _coupled solver_ beats _21 independent `lm()` fits_, not because of units.
  - Absolutes vs shares is **provably the same LP** up to a scalar. Shares' one real win is
    grid-portability (the demand is on a 4,129,078-cell grid this pipeline will not
    reproduce); rates are effectively mandatory at the allocator interface. Suggested
    layering: shares stored, absolute cells in the solver, rate + count on output.
  - The mass-conservation argument in
    [evoland-plus#32](https://github.com/ethzplus/evoland-plus/issues/32) **does not survive
    measurement**: running the `lm()` path end-to-end gives −0.0000 % area drift and no
    outflow row above 1. Conservation is structural in the simulator. The real deficiency is
    that `extrapolate_trans_rates()` has _no input for a scenario target_ and lands 10–19 %
    off every SSP.
- [x] **`trans_rate_reachability()` exists upstream** (added on the LP branch, now pinned) and
      `07-1` runs it before any solve, treating the table as a result rather than a gate. The
      prototype behind that decision — a pure-LP implementation of the docx's
      `compute_final_bounds` (~60 lines, needs no targets) — found **24 of 50 SSP × class targets
      unreachable** under observed transition bounds — several by 4–5×, glacier by 1.81× in all
      five scenarios — with 42–63 % of solved flow outside the historic envelope. This is a
      _result_, not a diagnostic, and it means #32's "fail loudly on infeasible targets" would
      abort every scenario: the soft bounds are load-bearing and the precheck's job is to
      quantify, not gate.
- [x] **`07-transition-rates-1-solver.qmd`** and **`07-transition-rates-2-legacy.qmd`** — written,
      unrun. Demand factored into `R/ssp-demand.R`, sourced by both so they solve against
      identical targets. Both use the upstream LP solver (evoland-plus#32, on `develop` and so
      carried by the pinned commit); `07-2` asserts `07-1` has run before it starts.
- [x] **Targets are used unscaled.** The demand was elicited on 4,129,078 cells and this grid has
      4,129,079 — one cell — so normalising to shares and multiplying back out would change
      nothing while obscuring the provenance of the numbers. `ssp_demand_targets()` _checks_ the
      grid size (0.1 % tolerance) and errors with rescaling instructions rather than doing it;
      `07-1` carries the note for anyone re-implementing at a coarser resolution.
- [x] **Evaluated that branch against these requirements.** All 38 of its tests pass, and it was
      exercised on the real demand: bounds from observed history, reachability precheck, coupled
      solve, `trans_rates_t` write for several runs, and — answering the open question in #32 —
      `trans_rate_areas()` recovers the solved trajectory from the rate table alone, verified to
      1.3 × 10⁻⁹ cells. No separate trajectory table is needed.
- [x] **`static` no longer excluded as an anterior class** (`04`). It had been a pure sink, which
      made SSP0/SSP3/SSP4 miss target by 34,000–67,000 cells — static overshoots because its
      inflow cannot be shed, and another class undershoots to compensate under mass balance.
      The original does not treat it as a sink either:
      `Scripts/Preparation/Transition_identification.R` retains `Static → Shrubland` and
      `Static → Static`, re-adding them _after_ the inclusion-threshold subset so they bypass the
      frequency filter, alongside hand-picked `Urban → Int_AG/Shrubland` and
      `Closed_Forest → Shrubland/Static/Grassland`. `lulcc.listbylulc.R:18` carries a
      commented-out exclusion of `Static` as an initial class. We apply `min_cardinality_abs`
      uniformly instead of curating by name.
  - [ ] **Re-measure after `04` runs.** Whether this actually relieves the target misses depends
        on which static-initial transitions clear the threshold. If `max_target_err` does not
        fall substantially, the remedy has to move to the demand side.
  - [x] **Restore a finite `max_reachability_ratio`** in both `07` steps once the static ratios
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

## validation via backcasting

- [x] 🟡 **`080-validate-backcasting.qmd`** — written, unrun on the real DB. All three sub-steps:
  - [x] (a) **estimate patch/allocation parameters** — `create_alloc_params_t()`, one parent run
        per parameter set, replicates inheriting through the run lineage.
  - [x] (b) **run the backcast** over the parameter sets × stochastic replicates.
  - [x] (c) **validate** — fuzzy similarity of differences, cross-tabulation, figure of merit,
        acceptance criteria.
- [ ] **Size the run.** The parameters ship at values chosen for legibility, not measurement:
      `n_perturbations = 3`, `n_replicates = 3`, `fuzzy_window = 11`. Measure one member run
      first.
- [ ] 🔴 **Only `frac_expander` is perturbed.** `create_alloc_params_t()` jitters that one
      column, so the sweep says nothing about sensitivity to `mean_patch_size` or
      `patch_elongation`. If patch geometry turns out to matter, the perturbation has to be
      widened upstream.
- [ ] **Fuzzy similarity is nearly uninformative as evoland reports it.** `calc_transition_
      similarity()` binarises the change maps with `NA -> 0` and averages the similarity surface
      over the *whole* raster, so the agreeing background dominates and every transition and
      parameter set scores near 1 (0.94–0.96 on the synthetic replica). `080` therefore also
      reports `similarity_change`, the same surface masked to cells that changed in either map,
      and selects on the figure of merit instead. Consider pushing the masked variant upstream —
      it is the quantity Dinamica's own validation reads.
- [ ] **Decide whether the acceptance criteria are the right ones.** `080` proposes three:
      quantity fidelity (< 5 % shortfall on transitions above 1000 cells), allocation skill
      (ensemble FoM ≥ 2× the random-within-class null), and per-transition honesty (transitions
      at chance may not be shown as maps in `090d`). The first two are gates in code; the third
      is a reporting rule that nothing enforces.
- [ ] **Fold this back into `eval_alloc_params_t()`.** The upstream helper does exactly this
      loop but calls `alloc_dinamica()` (so it needs DinamicaConsole), ignores the `runs_t`
      hierarchy, runs one realisation per parameter set and reports only the unmasked
      similarity. A CLUMPY backend plus an `n_replicates` argument would collapse most of `080`
      back into one call.

## extrapolation

- [x] 🟡 **`090-extrapolate.qmd`** — written, unrun on the real DB. Forward projection per
      (SSP × climate framing × replicate), with the realised-vs-solved area check.
- [ ] **Widen the subset.** Ships at SSP1 + SSP3 under `current`. The full cross registered in
      `001` is 25 leaves; which (SSP × trajectory) pairings are actually worth allocating is
      still open (see "Scenario scope").
- [x] 🟡 **`091-stochastic-alloc-2030.qmd`** — the smallest useful forward experiment: the first
      extrapolated period only, 20 stochastic members for each of the five SSPs under `current`,
      gathered into a per-cell change intensity (share of members in which the cell leaves its
      last observed class) and plotted as five comparable maps plus a between-scenario range
      layer. Uses the unperturbed `080` parameter set, written onto the scenario runs rather than
      run `0`. Written, unrun on the real DB.
  - [ ] **The five panels are only as different as `070`'s rate vectors.** With one climate
        framing and one anterior state, the SSP demand is the sole scenario-varying input at
        this horizon. If the maps come out near-identical, that is a finding about the demand,
        not a bug — but check `070`'s solved rates before concluding anything.
  - [ ] **Single-period allocation is not the same test as `090`.** No compounding, so the
        intensity maps say where change goes *first*, not where a scenario ends up.
- [ ] **`090d-report.qmd`** — diagnostic: reporting figures, tables, maps (human-facing
      outputs; mutates no state). Should carry at least the change-frequency map over the
      replicate ensemble (per the evoland stochastic-allocation vignette) and the realised-vs-
      demanded trajectory per class.

## Interventions

Not implemented anywhere; `080`/`090` carry comments at each attachment point. The three stages
in `NCCS-SSP-scenarios/Tools/SSP*_interventions.yml` differ sharply in what they cost us:

- [ ] **Pre-allocation** (patch geometry). Already reachable: write a modified `alloc_params_t`
      row set onto the SSP run instead of inheriting run 0's. Two conversions needed —
      `Param_adjust_type: Relative` vs `Absolute`, and `Patch_Isometry` (a Dinamica parameter)
      back through `isometry_from_elongation()`, which is not injective over its flat segments.
      Note also that the YAML's numbers were tuned against the *original's* parameter estimates
      (see `Spatial_intervention_updates.txt`, where SSP1's 0.20 patcher target was cut to 0.15
      because the estimates were already close), so they do not transfer unexamined to ours.
- [ ] 🔴 **Allocation** (masked probability adjustment). **Not possible without an upstream
      change.** The evoland analogue is editing `trans_pot_t` between prediction and allocation,
      but `alloc_clumpy_one_period()` calls `predict_trans_pot()` unconditionally as its first
      act and that write overwrites any edit — `use_parent_trans_pot` only redirects which run
      is predicted for. Needs a `skip_prediction` flag or a hook. Note too that
      `adjusted_trans_pot_v()` rescales each transition's potentials to match the target rate,
      so raising potentials inside a mask **moves** change rather than adding it.
- [ ] **Post-allocation** (direct map edit). Trivial: rewrite `lulc_data_t` for the run and
      period after allocation and before the next period is allocated.
- [ ] **Masks have no reproducible source.** `intrv_meta_t` / `intrv_masks_t` exist upstream and
      nothing reads them yet; before they can be filled, the YAML's ValPar-local paths
      (`Data/Spat_prob_perturb_layers/Bulding_zones/BZ_raster.grd`, municipality typology, …)
      need the same treatment the `02x` steps gave the predictors.
- [ ] **Conservation interventions are a port, not a translation.** SSP1's
      `Conservation_expansion_and_preservation` is a spatial optimisation over
      `Ca_expansion_target` / `Ca_prioritization` / `Ca_patch_preference` / `Ca_expansion_rate`,
      implemented in `Scripts/Functions/identify_CAs_by_target_and_configuration.R` and friends.
      Scope separately.

## Upstream (evoland-plus) asks arising from 080/090/091

- [x] ~~**`db$alloc_clumpy(seed = ...)` does not work.**~~ Resolved upstream in `abc8bca`: the
      argument was removed rather than implemented, so `set.seed()` before the call is now the
      sanctioned route. It is exact — all CLUMPY randomness goes through R's RNG
      (`src/alloc_clumpy.cpp`). All three steps do this.
- [x] ~~**Expose `use_parent_trans_pot` on the multi-period `alloc_clumpy()`.**~~ Done in
      `ca4d681`, together with `force_predict_trans_pot`. `091` is built on it: 100 member runs
      share 5 predictions, because `.has_predictions()` resolves through the run lineage and the
      write goes to the parent.
- [ ] 🔴 **`alloc_clumpy()` upserts neighbour predictors after every period, including the
      last.** `upsert_new_neighbors()` recomputes the neighbourhood predictors for the period
      just allocated and upserts them into `pred_data_t` — which, unlike `lulc_data_t`, is
      **not** partitioned by `id_run`, so every upsert rewrites the whole predictor table. For a
      single-period ensemble that work is entirely wasted (nothing consumes period *n+1*
      neighbours) and it is what would make `091` unrunnable at 100 members. `091` therefore
      calls `evoland:::alloc_clumpy_one_period()` and commits `lulc_data_t` itself. Wanted: an
      `update_neighbors` argument, or skipping the upsert after the last period of the requested
      sequence. Partitioning `pred_data_t` by `id_run` would help independently.
- [ ] **`trans_pot_t` is written per run and period** and is the largest thing `080` stores. If
      disk is tight, member runs need pruning between evaluations. `091` sidesteps this via
      `use_parent_trans_pot`; `080`'s chained replicates cannot, past the first period.
- [ ] **`terra::panel()` / `plot()` need `type = "continuous"`** for an ensemble-share layer.
      With `n_members + 1` distinct values terra falls back to a categorical legend, printing
      full-precision fractions as class labels and — at small member counts — failing to shade
      the panels at all. Not an evoland issue, but it will bite anyone plotting these maps.

---

## Open questions

| Trajectory                    | p5 (2025–34) | p6 (2035–44) | p7 (2045–54) | p8 (2055–64) |
| ----------------------------- | ------------ | ------------ | ------------ | ------------ |
| `stable15` (default SSP0)     | 1.5          | 1.5          | 1.5          | 1.5          |
| `stab20` (default SSP1)       | 1.5          | 1.5          | 2.0          | 2.0          |
| `rise30` (default SSP3, SSP4) | 1.5          | 2.0          | 2.0          | 3.0          |
| `fast30` (default SSP5)       | 2.0          | 2.0          | 3.0          | 3.0          |

- [ ] **SSP→GWL / CO₂ mapping.** Ratify the per-SSP, per-period GWL assignment. A
      **provisional** crosswalk is now encoded in one `rowwiseDT` in
      `02-ingest-preds-ch2025-3-gwl.qmd`, read off AR6 WG1 SPM.8a against the three levels
      CH2025 publishes:
      SSP0 → GWL1.5 is fixed per README. `fast30`'s last two decades are **capped** at GWL3.0.
      SSP3 and SSP4 currently share `rise30`, which may or may not be intended — if they should
      differ, a fifth trajectory is needed. CO₂ is not yet assigned at all.

- [ ] **Does the pipeline numbering need to change?** Projected climate predictors can only
      be materialised for the _selected_ predictor set, so predictor projection now depends
      on `05`. Either keep the `02-…-3-gwl` number with a documented out-of-order run, or
      renumber so projection follows selection.
