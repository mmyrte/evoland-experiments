# Allocation helpers shared by 080 (backcast), 090 (extrapolation) and 091 (2030 ensemble).
#
# Each worker opens its own connection to the DuckLake, so the functions here can run in mirai
# daemons: DuckLake takes concurrent writers, and every member writes only its own run. Source
# this file in the main process and in every daemon, e.g.
#
#   mirai::daemons(n)
#   mirai::everywhere(source(helper_path), helper_path = helper_path)
#
# Deterministic changes ------------------------------------------------------------------------
#
# `det_change_t` holds land use imposed on the allocated map after allocation: one row per
# (id_run, id_period, id_coord) with the class to impose. It is read through the run lineage
# like every other evoland table, so a scenario run's assignments reach all of its members.
# Glacier change is the only producer so far: observed in the backcast (080), GloGEM
# projections in the extrapolation (052-ingest-glacier-projections -> 090/091).

det_change_table <- "det_change_t"

as_det_change_t <- function(x) {
  x <- data.table::as.data.table(x)[, .(
    id_run = as.integer(id_run),
    id_period = as.integer(id_period),
    id_coord = as.integer(id_coord),
    id_lulc = as.integer(id_lulc)
  )]
  evoland::as_ducklake_db_t(
    x,
    class_name = det_change_table,
    key_cols = c("id_run", "id_period", "id_coord"),
    partition_cols = "id_run"
  )
}

# The deterministic assignments for one period, through the active run's lineage
read_det_change <- function(db, id_period) {
  if (!det_change_table %in% db$list_tables()) {
    return(data.table::data.table(id_coord = integer(0), id_lulc = integer(0)))
  }
  db$get_query(
    "select id_coord, id_lulc from {det_read_expr} where id_period = {id_period}",
    det_read_expr = db$get_read_expr(det_change_table)
  )
}

# Impose `assignments` (id_coord, id_lulc) on an allocated lulc_data_t, by reference
apply_det_change <- function(lulc, assignments) {
  if (nrow(assignments) > 0L) {
    lulc[assignments, on = "id_coord", id_lulc := i.id_lulc]
  }
  invisible(lulc)
}

# Member allocation -----------------------------------------------------------------------------

# Allocate one period for one member run, impose the deterministic change and commit the map.
# The transition potentials must already be in the lineage (predicted onto an ancestor); the
# allocator only predicts when none exist. Returns a one-row summary.
allocate_member_period <- function(
  db,
  id_run,
  id_period_post,
  seed,
  select_score,
  select_maximize,
  update_neighbors = FALSE
) {
  db$id_run <- id_run
  # CLUMPY draws through R's RNG, so seeding here makes the member reproducible
  set.seed(seed)
  started <- Sys.time()
  lulc <- evoland::alloc_clumpy_one_period(
    db = db,
    id_period_post = id_period_post,
    select_score = select_score,
    select_maximize = select_maximize
  )
  det <- read_det_change(db, id_period_post)
  apply_det_change(lulc, det)
  db$commit(lulc, "lulc_data_t", method = "upsert")
  # the next period's prediction reads the neighbour predictors of this one
  if (update_neighbors) {
    db$upsert_new_neighbors(id_period_post)
  }
  data.table::data.table(
    id_run = id_run,
    id_period = id_period_post,
    n_cells = nrow(lulc),
    n_det_change = nrow(det),
    seconds = as.numeric(difftime(Sys.time(), started, units = "secs"))
  )
}

# Worker entry point: one member, one or more consecutive periods. Opens its own connection.
# With `predict = TRUE` the member predicts its own potentials for every period (forced, so a
# re-run never reuses potentials of an earlier, different map, nor compounds interventions);
# with `predict = FALSE` they must already be in its lineage. `interventions` is a list of
# intervention tables keyed by period (see `interventions_for_period()`); it needs `predict`.
allocate_member <- function(
  db_path,
  id_run,
  id_periods,
  seed,
  select_score,
  select_maximize,
  predict = FALSE,
  interventions = NULL
) {
  stopifnot("interventions need predict = TRUE" = predict || is.null(interventions))
  db <- evoland::evoland_db$new(path = db_path, id_run = id_run)
  data.table::rbindlist(lapply(id_periods, function(p) {
    intervention_log <- NULL
    if (predict) {
      db$id_run <- id_run
      db$predict_trans_pot(p, select_score, select_maximize, force = TRUE)
      period_interventions <- interventions[[as.character(p)]]
      if (!is.null(period_interventions) && nrow(period_interventions) > 0L) {
        intervention_log <- apply_allocation_interventions(db, p, period_interventions)
      }
    }
    allocated <- allocate_member_period(
      db = db,
      id_run = id_run,
      id_period_post = p,
      seed = seed + p - min(id_periods), # the registered seed for the first period
      select_score = select_score,
      select_maximize = select_maximize,
      update_neighbors = p != max(id_periods)
    )
    if (is.null(intervention_log) || nrow(intervention_log) == 0L) {
      allocated[, `:=`(n_cells_intervened = 0L, interventions_skipped = "")]
    } else {
      allocated[, `:=`(
        n_cells_intervened = sum(intervention_log$n_cells_edited, na.rm = TRUE),
        # a mask that is not ingested; the warning raised for it stays inside the daemon
        interventions_skipped = paste(
          unique(intervention_log[is.na(n_trans), intervention]),
          collapse = ","
        )
      )]
    }
  }))
}

# Worker entry point: one scenario run predicts its potentials once (optionally applying the
# scenario's allocation interventions), then allocates its members one after another from them.
# Used by 091, which allocates a single period from the observed map for every member.
allocate_scenario_members <- function(
  db_path,
  id_run_scenario,
  members,
  id_period_post,
  select_score,
  select_maximize,
  interventions = NULL
) {
  db <- evoland::evoland_db$new(path = db_path, id_run = id_run_scenario)

  # force: a re-run must not compound interventions onto already-edited potentials
  db$predict_trans_pot(id_period_post, select_score, select_maximize, force = TRUE)
  intervention_log <- if (!is.null(interventions)) {
    apply_allocation_interventions(db, id_period_post, interventions)
  }

  allocated <- data.table::rbindlist(lapply(seq_len(nrow(members)), function(i) {
    allocate_member_period(
      db = db,
      id_run = members[["id_run"]][i],
      id_period_post = id_period_post,
      seed = members[["seed"]][i],
      select_score = select_score,
      select_maximize = select_maximize
    )
  }))
  list(allocated = allocated, interventions = intervention_log)
}
