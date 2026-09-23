# TODO — 2026-05-ssp-ch

Open work only. Settled decisions live in [`README.md`](README.md) and in the step documents;
completed tasks are in git history. Refs point at the relevant `.qmd`.

## State

`001`–`060` have run against the real database. `070` (rates) and `080`/`090` (backcast,
extrapolation) are written and have only ever run against a 40×40-cell synthetic replica, which
proves the API calls and metrics but says nothing about runtime or results. `091` has run on
the full grid. `090d` is not written.

**Next up:** run `070` for real, then size `080` before letting it loose — `n_perturbations ×
n_replicates` ships at 3 × 3 = 12 member runs, each re-predicting transition potentials over
4.1 M cells for every allocated period, and nobody has measured one.

**Environment.** R 4.6.1 on Ubuntu 24.04 with r2u. Working toolchain: data.table 1.18.4
(`rowwiseDT` needs ≥ 1.15), terra 1.9.34, mlr3 1.7.1, lpSolve 5.6.23, GDAL 3.8.4 / GEOS 3.12.1 /
PROJ 9.4.0. duckdb resolves its extension cache per driver object and fetches extensions over
http, which some proxies refuse; if `evoland_db$new()` fails with a 403 on
`spatial.duckdb_extension`, pre-place the extension under
`$DUCKDB_R_HOME/extensions/<version>/<platform>/`.

---

## Blocking

- [ ] 🔴 **Replace the emergency NA fill with real interpolation.**
      `029-emergency-change-fill.qmd` overwrites `pred_meta_t$fill_value` with each predictor's
      mean (and 0 for `soil_*`). The NAs it papers over come from the ingestion steps producing
      no value at `coords_t` cells outside the source coverage, mostly along the national
      border. The fix belongs in ingestion: interpolate or extrapolate to the full grid at
      ingest time, per source. A mean fill stays defensible for genuinely sparse scenario
      predictors, so keep `fill_value` as a mechanism and stop using it as a mask for missing
      coverage. Retire `029` once ingestion covers the grid.
      (`022-ingest-preds-ch2025-etl.qmd`, `020-ingest-preds-*.qmd`)
- [ ] **Lower `min_cardinality_abs`.** At 10000 the viable-transition set may be too thin for a
      clean rate solve; the code carries the note to decrease it until the solver is happy.
      Settle it against the `040d` observed-transitions plot and the `070` reachability output.
      (`040-viable-transition-identification.qmd:80`)

---

## Scenario realisation

- [ ] **Decide which (SSP × trajectory) pairings are allocated.** `default_for_ssp` records the
      "own" pairing per SSP; the full 25-leaf cross is registered and cheap to store but not
      cheap to allocate. (`001-setup-db.qmd`, `090-extrapolate.qmd`)
- [ ] **Ratify the SSP→GWL crosswalk.** Provisional, encoded in one `rowwiseDT` in
      `051-ingest-preds-ch2025-3-gwl.qmd`, read off AR6 WG1 SPM.8a against the three levels
      CH2025 publishes. SSP3 and SSP4 currently share `rise30`; if they should differ, a fifth
      trajectory is needed. CO₂ is not assigned at all.

  | Trajectory                    | p5 (2025–34) | p6 (2035–44) | p7 (2045–54) | p8 (2055–64) |
  | ----------------------------- | ------------ | ------------ | ------------ | ------------ |
  | `stable15` (default SSP0)     | 1.5          | 1.5          | 1.5          | 1.5          |
  | `stab20` (default SSP1)       | 1.5          | 1.5          | 2.0          | 2.0          |
  | `rise30` (default SSP3, SSP4) | 1.5          | 2.0          | 2.0          | 3.0          |
  | `fast30` (default SSP5)       | 2.0          | 2.0          | 3.0          | 3.0          |

---

## Data ingestion

### Climate (CH2025)

- [ ] **Run `051-ingest-preds-ch2025-3-gwl.qmd`.** Written, never executed. It projects only the
      climate predictors that survived `050`, which is why it runs after `050` despite the ingest
      slug.
- [ ] **Verify every ingested predictor has a future counterpart.** `022` already filters the
      inventory to `time_of_year == "yearly"`, because the seasonal `-obs` aggregates
      (DJF/MAM/JJA/SON) have no `-gwl` equivalent and would freeze at their observed baseline
      under every GWL run. The step carries a code TODO to make that check general rather than
      one hard-coded filter. (`022-ingest-preds-ch2025-etl.qmd:270`)
- [ ] **Bioclimatic indicators.** CH2025 lacks CHELSA-BIOCLIM+-style variables; decide which to
      derive or source. (wishlist in the appendix of `022-ingest-preds-ch2025-etl.qmd`)

### Economic (STATENT)

- [ ] 🔴 **Disclose the provenance gap.** The original labels the elicited employment file
      `Data_citation = "Project Internal"`; no method, assumptions, panel or version has been
      found. Everything the pipeline projects about employment inherits that. Either find the
      documentation or state the gap wherever these predictors influence a result.
- [ ] **Pin a fetchable source.** The CSV is read from the evoland cache with a pinned md5
      because no verifiable direct URL was reachable. Replace with `download_and_verify()` once
      one is confirmed. (`021-ingest-preds-statent-ssp.qmd`)

### Soil

- [ ] 🔴 **Decide what happens to the EnviDat EIV predictors.** They are not in this pipeline.
      The step was moved out in `d78838d` and lives at
      `2025-10-valparish/020-ingest-preds-envidat-eiv.qmd`, where it still opens
      `ssp-ch.evolanddb` but is outside this pipeline's numbering and has not been run against
      it; the `050` scores confirm no EIV predictor is in `pred_meta_t`. The SSPM ingest was
      written on the assumption that the two would sit side by side and be scored against each
      other, which never happened. Against the six EIV soil layers plus `light_100m`:
  - `soil_humus` → covered by `soil_oc_*`, which measures directly what the EIV indicates.
  - `soil_moisture`, `soil_moisture_variability`, `soil_aeration` → partly covered by texture,
    and only through a pedotransfer function. The real replacement is the WHC that
    `2026-07-ssp-rsofun/011-forcing-soil-whc.r` derives, which is not ingested here, so texture
    is currently a rawer predictor than the EIVs were.
  - `soil_ph`, `soil_nutrients`, `light_100m` → no substitute in the pipeline. The SSPM record
    reportedly has N and P layers that nothing fetches; whether they can stand in is untested.
  - Either move the EIV step back in as `020-ingest-preds-envidat-eiv.qmd` and let `050` decide,
    or record the loss of pH, nutrients and light as a deliberate reduction of the predictor set.
    `020-ingest-preds-soil.qmd` §"This is only a partial replacement" needs the same correction.
- [ ] **Eliminate depth collinearity at feature selection.** The four depths are ingested as
      separate predictors (12 in total) because topsoil governs cultivation and deeper layers
      govern water storage. If that proves unwieldy, the alternative is a trapezoidal 0–100 cm
      profile mean per property plus the 0 cm value.

### Other predictors

- [ ] **Bioregion / subregion collinearity.** The two are strictly nested. `050` should retain at
      most one per transition, and with the correlation pre-filter dropped this rests entirely on
      GRRF's regularisation. Check the retained sets.
- [ ] **Insolation.** `020-ingest-preds-dem.qmd` ingests elevation, slope and aspect only. The
      original's hillshade was discarded as a weak insolation proxy; decide whether a
      ray-traced insolation predictor is worth adding in its place.
- [ ] **sonBASE noise.** Decide whether to re-include; the ingestion exists in
      `2025-10-valparish/020-ingest-preds-sonbase.r`.

---

## LULC schema

- [ ] **Arealstatistik 2025.** Add `AS25_72` once the survey is finalised (currently 1985–2018).
      (`010-ingest-lulc-data.qmd`)
- [ ] **Deglaciated-area land-use class.** A new class from the glacier inventory, needing
      disaggregation to represent succession on deglaciating areas, interacting with the
      small-area inclusion threshold. (`010-ingest-lulc-data.qmd`)

---

## Feature selection

- [ ] **Sensitivity-check the parameters.** `grrf_gamma`, `num.trees`, `max.depth` and
      `importance_rel_cut` (currently 0.2) are reasoned but not justified against this data.
      Re-run across a small grid and report how far the retained set moves.
      (`050-covariate-selection.qmd`)

Note for whenever the correlation pre-filter is reconsidered: `FilterFindCorrelation` is
`integer`/`numeric` only, so a `factor` predictor makes it raise "unsupported feature types",
which `pred_filter_worker()` turns into an all-`NA` score for the whole transition — a silent
hole rather than a crash. It scores ≈ `1 − max|r|`, so `corcut` translates to keeping
`score > 1 - corcut`. `regularization.factor` disables ranger's internal threading, so the
per-transition `mirai` cluster is the only parallelism in `050`.

---

## Transition modelling

- [ ] **Justify the train/test split.** `sample_frac = 0.7` is evoland's default and what both
      vignettes use. A learning curve of AUC against subsample size for a few representative
      transitions would settle it, and would reveal whether rare transitions are sample-starved.
      (`060-transition-modelling.qmd`)
- [ ] **Revisit the learner** against the `classif.featureless` baseline. ranger is a deliberate
      first pass; if many transitions sit near AUC 0.5 the question is the predictor set.

---

## Transition rates

- [ ] **Run `070` against the real database.** Written, unrun. The under-delivery check is
      deferred to `090`, which verifies realised areas against `trans_rate_areas()`.
- [ ] **Re-measure target misses after `040` settles.** `static` is no longer excluded as an
      anterior class, which should relieve the 34,000–67,000-cell misses on SSP0/SSP3/SSP4. If
      `max_target_err` does not fall substantially, the remedy moves to the demand side.
- [ ] **Restore a finite `max_reachability_ratio`** in `070` once the static ratios come back
      finite. It is at `Inf` only because `static` made them `Inf`.
- [ ] 🔴 **The `static` area targets are an elicitation defect, knowingly kept.** `static`
      aggregates infrastructure, water, rock and scree. Some members genuinely convert
      (rock/scree revegetating to shrubland); most cannot. Giving the class a per-scenario 2060
      area target treats it as a land use whose extent is a policy outcome, when it is an
      aggregation artefact standing in for unstated assumptions about deglaciation, reservoirs
      and sealing. The original's own model could not deliver SSP3's static target. Retained to
      stay close to the replication, not endorsed. Proper fixes:
  - [ ] Disaggregate `static` into convertible and non-convertible members — the deglaciation
        item above already requires this.
  - [ ] Re-elicit demand against classes that can carry a target.
- [ ] **Housekeeping:** `070`'s prose still points at `R/ssp-demand.R` and a
      `07-transition-rates-2-legacy.qmd` companion, both removed when the demand table was
      inlined. Fix the references, and decide whether the legacy-behaviour comparison run is
      still wanted.

Reachability is reported and not enforced. `trans_rate_reachability()` found 24 of 50
SSP × class targets unreachable under observed transition bounds, several by 4–5×, glacier by
1.81× in all five scenarios, with 42–63 % of solved flow outside the historic envelope. The soft
bounds are load-bearing, so [#32](https://github.com/ethzplus/evoland-plus/issues/32)'s
"fail loudly on infeasible targets" would abort every scenario.

---

## Validation via backcasting

- [ ] **Size the run.** `n_perturbations = 3`, `n_replicates = 3`, `fuzzy_window = 11` were
      chosen for legibility. Measure one member run first; start at 1 × 2 and grow.
      (`080-validate-backcasting.qmd`)
- [ ] 🔴 **Only `frac_expander` is perturbed.** `create_alloc_params_t()` jitters that one
      column, so the sweep says nothing about sensitivity to `mean_patch_size` or
      `patch_elongation` — and CLUMPY ignores `frac_expander`, so under CLUMPY the perturbed
      sets do not differ at all. ethzplus/evoland-plus#52 adds a per-column `sd`.
- [ ] **Fuzzy similarity is nearly uninformative as evoland reports it.**
      `calc_transition_similarity()` binarises the change maps with `NA -> 0` and averages the
      similarity surface over the whole raster, so the agreeing background dominates and
      everything scores near 1 (0.94–0.96 on the synthetic replica). `080` also reports
      `similarity_change`, the same surface masked to cells that changed in either map, and
      selects on the figure of merit. Consider pushing the masked variant upstream; it is what
      Dinamica's own validation reads.
- [ ] **Decide whether the acceptance criteria are right.** `080` proposes quantity fidelity
      (< 5 % shortfall on transitions above 1000 cells), allocation skill (ensemble FoM ≥ 2× the
      random-within-class null) and per-transition honesty (transitions at chance may not be
      shown as maps in `090d`). The first two are gates in code; the third is a reporting rule
      nothing enforces.
- [ ] **Fold this back into `eval_alloc_params_t()`.** The upstream helper runs the same loop but
      calls `alloc_dinamica()` (needing DinamicaConsole), ignores the `runs_t` hierarchy, runs
      one realisation per parameter set and reports only the unmasked similarity. A CLUMPY
      backend plus an `n_replicates` argument would collapse most of `080` into one call.

Replicates branch at the root and chain, because a 40-year backcast tests the compounding
trajectory. A one-step-ahead variant needs no new machinery; the recipe is in `080`'s prose.

---

## Extrapolation & reporting

- [ ] **Widen the `090` subset.** Ships at SSP1 + SSP3 under `current`, via `ssp_subset` /
      `climate_subset`. Depends on the pairing decision above.
- [ ] **Read `091`'s five panels against `070`'s solved rates.** With one climate framing and one
      anterior state, the SSP demand is the only scenario-varying input at this horizon. If the
      maps come out near-identical, that is a finding about the demand. Single-period allocation
      also does not compound, so the intensity maps say where change goes first, not where a
      scenario ends up.
- [ ] **Write `090d-report.qmd`.** Human-facing figures, tables and maps; mutates no state. At
      minimum the change-frequency map over the replicate ensemble and the realised-vs-demanded
      trajectory per class.
- [ ] **Reporting obligations to carry into `090d`:** SSP5-8.5 late century is capped at GWL3.0
      because CH2025 publishes no higher level, which understates late-century SSP5 warming; the
      employment provenance gap above; and any transition sitting at chance.

---

## Interventions

Not implemented at any stage. The three intervention stages in
`NCCS-SSP-scenarios/Tools/SSP*_interventions.yml` are the reference for what is wanted.

**Direction (settled).** No intervention infrastructure upstream: `intrv_meta_t` /
`intrv_masks_t` are to be removed from evoland-plus, and interventions are built here, per
period, on the exported single-period allocators (ethzplus/evoland-plus#49):

1. `db$predict_trans_pot(p, …)` under the scenario run;
2. read the run's `trans_pot_t` for `p`, edit, write it back — the **whole** slice per
   `(id_trans, id_period_post)`, since a run inherits its ancestors' potentials slice-wise and
   a partial write hides the rest of the parent's slice;
3. `alloc_clumpy_one_period()` / `alloc_dinamica_one_period()` reuse the edited potentials
   (they only predict when none exist) and return the map without committing it;
4. edit the map (post-allocation), `db$commit(…, "lulc_data_t")`, then
   `db$upsert_new_neighbors(p)` — in that order, or the next period predicts from stale
   neighbour predictors.

This depends on #49's `id_run` key on `trans_pot_t`; before it, one run's writes overwrote
every other run's potentials for the same transition, period and cell.

**On rescaling.** `adjusted_trans_pot_v()` rescales each transition's potentials to its target
rate, so a masked adjustment relocates change rather than adding it. That is the right
behaviour, not a loss: the fitted models' base rates are artefacts of the training data, and
rescaling re-estimates the intercept against the scenario demand (a prior-shift correction; CLUE-S
does the same with its per-class iteration variables). It keeps quantity (`trans_rates_t`) and
location (`trans_pot_t`) separate. The original's non-rescaled "Absolute" values were not
meaningful: "set to 0.1" is a 100× boost for a transition whose potentials sit near 0.001 and
almost nothing for one near 0.2. Under Dinamica this left quantity intact (the transition
matrix fixes it) but made the maps arbitrary scores; under CLUMPY's uSAM it would change the
quantity too. Two consequences:
- Express adjustments as **multiplicative factors** (odds ratios, "k× as likely inside the
  mask"), which pass through the rescaling unchanged and state the scenario assumption plainly;
  zero is an exact exclusion. This is the mask as a predictor with an imposed rather than
  estimated coefficient. The original's `Relative` method (percentile-mean gaps, thresholds,
  sign rules) has no such reading and is not worth reproducing.
- An intervention meant to change *how much* land changes belongs in the demand (`070`), not in
  the potentials.

The linear scaling is itself an approximation to a logit-scale intercept shift; the two agree
for rare transitions and diverge only where potentials are high.

- [ ] **Decide whether post-hoc probability adjustment is the right mechanism at all.**
      The adjustments are harder to defend than the rescaling. Better: have the transition
      models take the constraint into account — protected areas as a predictor, for instance —
      so the intervention is part of the estimate. Custom mlr3 learners that admit such manual
      constraints are one route.
- [ ] **Pre-allocation: drop `Spatial_zoning` (proposed).** It is the only pre-allocation
      intervention in the YAMLs: `Perc_patcher` for transitions into Urban, 0 in SSP0/3/5, 0.15
      in SSP1, 0.5 in SSP4. Dinamica-only; CLUMPY has no expander/patcher split, and its
      `avoid_aggregation` only prevents merging with patches of the same time step, so adjacency
      to existing urban comes entirely from the potentials (urban neighbour predictors). The
      calibrated patcher fractions were already ~0.15–0.27 (`Spatial_intervention_updates.txt`),
      so the scenario contrast is small. If it must be kept, express it at the allocation stage:
      scale urban-transition potentials in cells with no urban neighbour in the nearest distance
      class by k (k = 0 ≡ patcher 0); the 0.15 / 0.5 settings would need calibrating.
- [ ] **Allocation-stage interventions** per the recipe above. Masks filtered by current land use
      (`From_lulc_filter`), per-time-step masks, the `Agri_*` marginality quartile and
      `Intervention_ranking` all live here and need only the anterior `lulc_data_t`.
- [ ] **Post-allocation: deterministic glacier transitions.** No YAML defines a
      `Post-allocation` intervention; the one post-allocation step the original ran is
      `Scripts/Dinamica_integration/Dinamica_deterministic_trans.R`, which overwrites
      glacier / non-glacier cells from a per-scenario glacier index after allocation. Model it
      as a map edit in step 4, and keep the area it moves out of the demand the rate solver
      allocates (`070` currently marks deglaciation viable for the bounds only).
- [ ] **Masks have no reproducible source.** The YAML's ValPar-local paths
      (`Data/Spat_prob_perturb_layers/Bulding_zones/BZ_raster.grd`, municipality typology, …)
      need the treatment the `020-` steps gave the predictors.
- [ ] **SSP0 interventions and demand curves.** `NCCS-SSP-scenarios/Tools/SSP0_interventions.yml`
      is not wired in.
- [ ] **Conservation interventions need reimplementing.** SSP1's
      `Conservation_expansion_and_preservation` is a spatial optimisation over
      `Ca_expansion_target` / `Ca_prioritization` / `Ca_patch_preference` / `Ca_expansion_rate`,
      implemented in `Scripts/Functions/identify_CAs_by_target_and_configuration.R` and friends.
      Scope separately.

---

## Upstream asks (evoland-plus)

Open PRs; bump the `evoland` pin in `rproject.toml` / `rv.lock` as they merge. The pin currently
sits on #49's head, which `091` needs.

- [ ] **Intervention interface** — ethzplus/evoland-plus#49: `trans_pot_t` keyed by `id_run`,
      the run-lineage read fix below, and `alloc_clumpy_one_period()` /
      `alloc_dinamica_one_period()` exported with matching signatures. Then remove
      `intrv_meta_t` / `intrv_masks_t` upstream.
- [ ] 🔴 **Run-lineage reads returned every ancestor's rows** for tables without `id_period`
      (`alloc_params_t`, `trans_pot_t`, `trans_models_t`, `trans_preds_t`); fixed in #49. In
      `091`, members' lineage runs through their scenario run to run 0, and both carry
      `alloc_params_t` (from `091` and `090`), so `alloc_params_clumpy_v()` returned each
      transition twice and `alloc_clumpy_one_period()` passed misaligned patch parameters to
      C++. **Re-run `091`** on the new pin, and check `080` if run 0 already held parameters
      from an earlier execution when it ran.
- [ ] **`update_neighbors` argument on `alloc_clumpy()` / `alloc_dinamica()`** — #50 (stacked
      on #49). Skips the neighbour upsert after the last requested period. `pred_data_t` is
      already partitioned by `id_run` and `id_period`.
- [ ] **`prune_trans_pot()`** — #51. Deletes the active run's own `trans_pot_t` rows so `080`'s
      chained replicates can drop potentials once evaluated; disk is freed by `$checkpoint()`
      under the catalog retention settings. Wire into `080` once merged.
- [ ] **Widen `create_alloc_params_t()`'s perturbation** — #52: `sd` per column, covering
      `mean_patch_size`, `patch_size_variance` and `patch_elongation`. Wire into `080` once
      merged (see Validation).
- [ ] **Masked fuzzy similarity** (`similarity_change`) and a CLUMPY backend for
      `eval_alloc_params_t()`; see Validation.
- [ ] **`terra::panel()` / `plot()` need `type = "continuous"`** for an ensemble-share layer.
      With `n_members + 1` distinct values terra falls back to a categorical legend, printing
      full-precision fractions as class labels and, at small member counts, failing to shade the
      panels at all. Not an evoland issue, but it bites anyone plotting these maps.
