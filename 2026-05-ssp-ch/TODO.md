# TODO — 2026-05-ssp-ch

Open work only. Settled decisions live in [`README.md`](README.md) and in the step documents;
completed tasks are in git history. Refs point at the relevant `.qmd`.

## State

`001`–`080` have run using the `parquet_db` database; the `ducklake_db` replacement is
being implemented because `090` backcast and extrapolation need to be able to run in
parallel (have only run part-way through until now). `091` has run on the full grid.
`090d`, reporting on backcast and extrapolation, is not written.

**Next up:** fix ingestion scripts w.r.t. NAs introduced at borders etc. Once that is
accomplished, ensure that parallel writers also work for allocation. Then run backcast
and extrapolation for real.

**Environment for claude code web:**
R 4.6.1 on Ubuntu 24.04 with r2u. Working toolchain: data.table 1.18.4
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
- [ ] **Lower `min_cardinality_abs`.** At 10000 the viable-transition set is too thin
      for a clean and realistic rate solve. Settle it both against the `040d`
      observed-transitions plot and the `070` reachability output.
      (`040-viable-transition-identification.qmd:80`)

---

## Scenario realisation

- [ ] Extend to 2100 to match NCCS-SSP
- [ ] **restrict to default_for_ssp (SSP × trajectory) pairings** (`001-setup-db.qmd`, `090-extrapolate.qmd`)
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
- [ ] add a proper insolation estimate; we don't have cloudiness from the climatologies,
      so we'll have to take the terrain for ray-tracing based insolation; in interaction
      with precipitation, this could represent latent shading effects
- [ ] **Bioclimatic indicators.** CH2025 lacks CHELSA-BIOCLIM+-style variables; decide which to
      derive or source. (wishlist in the appendix of `022-ingest-preds-ch2025-etl.qmd`)

### Economic (STATENT)

- [ ] **Disclose the provenance gap.** The original labels the elicited employment file
      `Data_citation = "Project Internal"`; no method, assumptions, panel or version has been
      found. Ask Ben or Lena.
- [ ] **Pin a fetchable source.** The CSV is read from the evoland cache with a pinned md5
      because no verifiable direct URL was reachable. Replace with `download_and_verify()` once
      one is confirmed. (`021-ingest-preds-statent-ssp.qmd`)

### Soil

- [ ] From SSPM: Add the N and P layers from SSPM
- [ ] From SSPM: Add bulk densities https://zenodo.org/records/18428152
- [ ] From SSPM: Add coarse fragments https://zenodo.org/records/17453999
- [ ] Possibly aggregate across the four depths. They are currently ingested as
      separate predictors (topsoil->cultivation, deeper layers->water storage)

### Other predictors

- [ ] **Bioregion / subregion collinearity.** The two are strictly nested. `050` should retain at
      most one per transition, and with the correlation pre-filter dropped this rests entirely on
      GRRF's regularisation. Check the retained sets.

---

## LULC schema

- [ ] **Arealstatistik 2025.** Add `AS25_72` once the survey is finalised (currently
      1985–2018; as of the 2026-08-31 release, only Genève, Vaud, Fribourg, Jura, Solothurn,
      Aargau and the two Basels are done.) See `010-ingest-lulc-data.qmd`
- [ ] **Deglaciated-area land-use class.** A new class from the glacier inventory, needing
      disaggregation to represent succession on deglaciating areas. May require specific
      handling on the viability logic. (`010-ingest-lulc-data.qmd`)

---

## Feature selection

- [ ] **Sensitivity-check the parameters.** `grrf_gamma`, `num.trees`, `max.depth` and
      `importance_rel_cut` (currently 0.2) are reasoned but not justified against this data.
      Re-run across a small grid and report how far the retained set moves.
      (`050-covariate-selection.qmd`)
- [x] reconsider correlation filter; never reall made sense for mixed data types
- [ ] check whether MRMR is a better option https://mlr3filters.mlr-org.com/reference/mlr_filters_mrmr.html

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

- [ ] model transition rates in two phases: first until 2060, second to 2100
- [ ] **Re-measure target misses after `040` settles.** `static` is no longer excluded as an
      anterior class, which should relieve the 34,000–67,000-cell misses on SSP0/SSP3/SSP4. If
      `max_target_err` does not fall substantially, the remedy moves to the demand side.
- [ ] **Restore a finite `max_reachability_ratio`** in `070` once the static ratios come back
      finite. It is at `Inf` only because `static` made them `Inf`.
- [x] **The `static` area targets are an elicitation defect, knowingly kept.** `static`
      aggregates infrastructure, water, rock and scree. Some members genuinely convert
      (rock/scree revegetating to shrubland); most cannot. Giving the class a
      per-scenario 2060 area target treats it as a land use whose extent is a policy
      outcome, when it is an aggregation artefact standing in for unstated assumptions
      about deglaciation, reservoirs and sealing. The original's own model could not
      deliver SSP3's static target. Retained to stay close to the replication, not
      endorsed. Proper fix would be to re-elicit demand against classes that can carry a
      target. Proposed fix:
  - [ ] Disaggregate `static` into convertible and non-convertible members — the deglaciation
        item above already requires this.
- [ ] **Decide whether the legacy-behaviour comparison run is still wanted.** It was
      `071-transition-rates-legacy.qmd`, removed in `f12d134`; `070` still skips its
      `rate_solver` sibling runs if present.
- [ ] find out how to handle transition rates from deterministic transition (deglaciation)

---

## Validation via backcasting

- [ ] **Size the run.** `n_perturbations = 3`, `n_replicates = 3`, `fuzzy_window = 11` were
      chosen for legibility. Measure one member run first; start at 1 × 2 and grow.
      (`080-validate-backcasting.qmd`)
- [ ] **Only `frac_expander` is perturbed.** `080`'s `perturb_alloc_params()` jitters that one
      column, so the sweep says nothing about sensitivity to `mean_patch_size` or
      `patch_elongation`. Widening it is now a local change to that function.
  - [x] Drop jitter upstream, simply return best estimate; leave the
        jittering/perturbation to whoever is constructing the `alloc_params_t`.
        ethzplus/evoland-plus#53, merged; `080` builds its perturbed sets itself.
- [x] **Decide whether the acceptance criteria are right.** `080` proposes quantity fidelity
      (< 5 % shortfall on transitions above 1000 cells), allocation skill (ensemble FoM ≥ 2× the
      random-within-class null) and per-transition honesty (transitions at chance may not be
      shown as maps in `090d`).


---

## Extrapolation & reporting

- [ ] **Widen the `090` subset.** Ships at SSP1 + SSP3 under `current`, via `ssp_subset` /
      `climate_subset`. Depends on the pairing decision above.
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

ethzplus/evoland-plus#49 (per-run `trans_pot_t`, run-lineage reads, exported single-period
allocators), #50 (`update_neighbors`) and #53 (`create_alloc_params_t()` returns the best
estimate only) and #55 (validation metrics) are merged into `develop`, and the `evoland` pin in
`rproject.toml` / `rv.lock` is on `develop` (ecebf27); `080` is adapted to it.

- [ ] 🔴 **Re-run `080` and `091` on the pinned `develop`** (on the group's infrastructure, as
      part of running the pipeline end to end). The pin is bumped; neither step has run on it
      yet, and `080`'s rewritten evaluation has only been checked on a synthetic database.
      Before #49, run-lineage reads of tables without
      `id_period` returned every ancestor's rows. In `091`, members' lineage runs through their
      scenario run to run 0, and both carry `alloc_params_t` (from `091` and `090`), so
      `alloc_clumpy_one_period()` passed misaligned patch parameters to C++. Also check `080`
      if run 0 already held parameters from an earlier execution when it ran.
- [x] **Fuzzy similarity of differences** — ethzplus/evoland-plus#55, merged. The old
      `calc_transition_similarity()` compared kernel-weighted category shares over the whole
      raster, so the unchanged background dominated and everything scored near 1; `080`'s
      masked `similarity_change` did not fix that. #55 reimplements it after Hagen (2003) /
      Dinamica, and adds `db$figure_of_merit_v()`. `080` now reads `similarity` directly and
      takes its figure of merit, null and per-transition FoM from `figure_of_merit_v()`.
  - [ ] `figure_of_merit_v()` has no class filter, so `080` excludes deglaciation after the
        fact. That is exact only while no glacier cell is simulated to change, which `080`
        asserts. An anterior-class filter upstream would remove the workaround.
- [ ] The upstream `eval_alloc_params_t()` is an initial approach for running all
      id_runs in that table and to validate the results. Since running multiple id_runs
      across one or more id_periods is going to be a common analytic scenario, that eval
      function should be replaced by a more generally useful method accepting an id_run x
      id_period subset to evaluate with either clumpy or dinamica.
- [ ] **Remove `intrv_meta_t` / `intrv_masks_t`** — #54, with a vignette on representing
      policy mechanisms (settlement form, spatial steering, exogenous change). Awaiting review.
- [ ] **`terra::panel()` / `plot()` need `type = "continuous"`** for an ensemble-share layer.
      With `n_members + 1` distinct values terra falls back to a categorical legend, printing
      full-precision fractions as class labels and, at small member counts, failing to shade the
      panels at all. Not an evoland issue, but it bites anyone plotting these maps.
