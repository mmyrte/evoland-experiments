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
- [ ] **Housekeeping:** `070`'s prose still points at `R/ssp-demand.R` and a
      `07-transition-rates-2-legacy.qmd` companion, both removed when the demand table was
      inlined. Fix the references, and decide whether the legacy-behaviour comparison run is
      still wanted.
- [ ] find out how to handle transition rates from deterministic transition (deglaciation)

---

## Validation via backcasting

- [ ] **Size the run.** `n_perturbations = 3`, `n_replicates = 3`, `fuzzy_window = 11` were
      chosen for legibility. Measure one member run first; start at 1 × 2 and grow.
      (`080-validate-backcasting.qmd`)
- [ ] **Only `frac_expander` is perturbed.** `create_alloc_params_t()` jitters that one
      column, so the sweep says nothing about sensitivity to `mean_patch_size` or
      `patch_elongation`.
  - [ ] Drop jitter upstream, simply return best estimate; leave the
        jittering/perturbation to whoever is constructing the `alloc_params_t`.
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

> This section to be rewritten by session_019fog72qNWbuSjiGBWuP3np

Not implemented at any stage. The three intervention stages in
`NCCS-SSP-scenarios/Tools/SSP*_interventions.yml` are the reference for what is wanted.

**Direction.** The intervention infrastructure should not live in the main evoland-plus
package. evoland-plus should expose an **interface** for these manipulations, and ad-hoc
interventions get built on it here. `intrv_meta_t` / `intrv_masks_t` exist upstream and nothing
reads them, so the shape is still open.

- [ ] **Decide whether post-hoc probability adjustment is the right mechanism at all.**
      Manipulating transition potentials after they have been estimated is hard to defend.
      Better: have the transition models take the constraint into account — protected areas as a
      predictor, for instance — so the intervention is part of the estimate. Custom mlr3 learners
      that admit such manual constraints are one route. Quantile remapping is out of scope for
      that approach.
- [ ] **Pre-allocation (patch geometry)** is already reachable: write a modified
      `alloc_params_t` row set onto the SSP run instead of inheriting run 0's. Two conversions
      needed — `Param_adjust_type: Relative` vs `Absolute`, and `Patch_Isometry` (a Dinamica
      parameter) back through `isometry_from_elongation()`, which is not injective over its flat
      segments. The YAML's numbers were tuned against the original's parameter estimates (see
      `Spatial_intervention_updates.txt`, where SSP1's 0.20 patcher target was cut to 0.15), so
      they do not transfer unexamined.
- [ ] **Allocation (masked probability adjustment)** needs an upstream change whichever
      direction is taken. The evoland analogue is editing `trans_pot_t` between prediction and
      allocation, but `alloc_clumpy_one_period()` calls `predict_trans_pot()` unconditionally as
      its first act and that write overwrites any edit; `use_parent_trans_pot` only redirects
      which run is predicted for. Needs a `skip_prediction` flag or a hook. Note also that
      `adjusted_trans_pot_v()` rescales each transition's potentials to match the target rate, so
      raising potentials inside a mask moves change rather than adding it.
- [ ] **Post-allocation (direct map edit)** is trivial: rewrite `lulc_data_t` for the run and
      period after allocation and before the next period is allocated.
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

- [ ] The upstream `eval_alloc_params_t()` is an initial approach for running all
      id_runs in that table and to validate the results. Since running multiple id_runs
      across one or more id_periods is going to be a common analytic scenario, that eval
      function should be replaced by a more generally useful method accepting an id_run x
      id_period subset to evaluate with either clumpy or dinamica.
- [ ] **Fuzzy similarity is nearly uninformative as evoland reports it.**
      `calc_transition_similarity()` binarises the change maps with `NA -> 0` and averages the
      similarity surface over the whole raster, so the agreeing background dominates and
      everything scores near 1 (0.94–0.96 on the synthetic replica). `080` also reports
      `similarity_change`, the same surface masked to cells that changed in either map, and
      selects on the figure of merit.
  - [ ] Push the masked variant upstream; it is what Dinamica's own validation reads.
- [ ] **An intervention interface**, per the section above.
- [ ] 🔴 **`alloc_clumpy()` upserts neighbour predictors after every period, including the
      last.** `upsert_new_neighbors()` recomputes the neighbourhood predictors for the period
      just allocated and upserts them into `pred_data_t`, which is not partitioned by `id_run`,
      so every upsert rewrites the whole predictor table. For a single-period ensemble that work
      is wasted, and it is what would make `091` unrunnable at 100 members; `091` therefore calls
      `evoland:::alloc_clumpy_one_period()` and commits `lulc_data_t` itself. Wanted: an
      `update_neighbors` argument, or skipping the upsert after the last requested period.
      Partitioning `pred_data_t` by `id_run` would help independently.
- [ ] **`trans_pot_t` is written per run and period** and is the largest thing `080` stores. If
      disk is tight, member runs need pruning between evaluations. `091` sidesteps this via
      `use_parent_trans_pot`; `080`'s chained replicates cannot, past the first period.
- [ ] **`terra::panel()` / `plot()` need `type = "continuous"`** for an ensemble-share layer.
      With `n_members + 1` distinct values terra falls back to a categorical legend, printing
      full-precision fractions as class labels and, at small member counts, failing to shade the
      panels at all. Not an evoland issue, but it bites anyone plotting these maps.
