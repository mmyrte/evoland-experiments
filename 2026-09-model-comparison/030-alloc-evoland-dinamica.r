#' ---
#' title: "PIE benchmark: Dinamica EGO allocation of every estimator's potentials"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' Allocates the observed 1991 → 1999 demand with Dinamica's Expander and Patcher, driven by
#' evoland's `alloc_dinamica()`, on the potentials of each estimator (`010`, `020`), once per
#' registered realisation. For the Weights of Evidence row this is Dinamica's own estimator and
#' allocator, but with the run set up by evoland; `015` runs it as shipped for reference.
#'
#' Every realisation is a child of its estimator's run (`common.r`) and inherits its potentials
#' (`use_parent_trans_pot = TRUE`), the allocation parameters and the demand, so the estimators
#' differ only in their potentials; the realisations differ only in the allocator's randomness.

#| label: setup
#| output: false
library(evoland)
library(data.table)
source("2026-09-model-comparison/common.r")
db <- evoland_db$new(path = db_path)
reset_timings("evoland-dinamica")
members <- crossing_members()[allocator == "dinamica"]
members[, .N, by = estimator]

#| label: allocate
#| output: false
for (i in seq_len(nrow(members))) {
  db$id_run <- members$id_run[i]
  set.seed(members$seed[i])
  timed(
    "evoland-dinamica",
    "allocate",
    db$alloc_dinamica(
      id_periods = 3L,
      select_score = "no.crossval",
      select_maximize = TRUE,
      use_parent_trans_pot = TRUE,
      update_neighbors = FALSE
    ),
    note = paste0("id_run=", members$id_run[i])
  )
}

#' # Figure of merit
#'
#' Quick look; `050-compare` tabulates all pairings together.

#| label: fom
db$id_run <- NULL
db$figure_of_merit_v(
  id_period_anterior = 2L,
  id_period_post = 3L,
  id_run_reference = id_run_observed,
  id_run_simulated = members$id_run
)[
  members[, .(id_run, estimator)],
  on = "id_run"
][,
  .(
    fom_mean = mean(figure_of_merit),
    fom_sd = sd(figure_of_merit),
    fom_null = mean(figure_of_merit_null)
  ),
  by = estimator
]
