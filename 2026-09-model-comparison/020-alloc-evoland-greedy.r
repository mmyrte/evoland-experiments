#' ---
#' title: "PIE benchmark: evoland-plus potentials, greedy (rank-and-fill) allocation"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' Allocates the observed 1991 → 1999 demand on each estimator's potentials with evoland's
#' deterministic greedy allocator (`alloc_greedy()`, joint ranking): each transition's demand
#' goes to the cells of highest adjusted potential, every cell changes at most once. This is
#' the allocator of lulcc's Ordered model and SEALS' rank-and-fill, run on evoland's transition
#' potentials, so it differs from the CLUMPY and Dinamica runs in the allocator only. One map
#' per estimator, run `parent + 601`.

#| label: setup
#| output: false
library(evoland)
library(data.table)
source("2026-09-model-comparison/common.r")
db <- evoland_db$new(path = db_path)
reset_timings("evoland-greedy")

#| label: runs
db$id_run <- NULL
parents <- intersect(c(1000L, 2000L), db$runs_t$id_run)
greedy_runs <- data.table(
  id_run = parents + 601L,
  parent_id_run = parents,
  description = paste("greedy (joint) allocation on run", parents)
)
db$commit(
  as_runs_t(rbind(db$runs_t[!id_run %in% greedy_runs$id_run], greedy_runs, fill = TRUE)),
  "runs_t",
  method = "overwrite"
)

#| label: allocate
#| output: false
for (id in greedy_runs$id_run) {
  db$id_run <- id
  timed(
    "evoland-greedy",
    "allocate",
    db$alloc_greedy(
      id_periods = 3L,
      select_score = "no.crossval",
      select_maximize = TRUE,
      arbitration = "joint",
      use_parent_trans_pot = TRUE,
      update_neighbors = FALSE
    ),
    note = paste0("id_run=", id)
  )
}

#| label: fom
db$id_run <- NULL
db$figure_of_merit_v(
  id_period_anterior = 2L,
  id_period_post = 3L,
  id_run_reference = id_run_observed,
  id_run_simulated = greedy_runs$id_run
)
