#' ---
#' title: "PIE benchmark: evoland-plus potentials, Dinamica EGO allocation"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' Allocates the observed 1991 → 1999 demand on the potentials of each estimator calibrated in
#' `010-evoland-calibrate.r`, once per registered realisation. Every realisation is a child
#' of its estimator's run and inherits its potentials (`use_parent_trans_pot = TRUE`), the
#' allocation parameters and the demand; only the allocator's randomness differs.

#| label: setup
#| output: false
library(evoland)
library(data.table)
source("2026-09-model-comparison/common.r")
db <- evoland_db$new(path = db_path)
reset_timings("evoland-dinamica")

#| label: allocate
#| output: false
db$id_run <- NULL
members <- db$runs_t[parent_id_run %in% c(1000L, 2000L) & id_run - parent_id_run > 500L & id_run - parent_id_run <= 500L + n_realisations]
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
#' Quick look; `050-compare` tabulates all tools together.

#| label: fom
db$id_run <- NULL
db$figure_of_merit_v(
  id_period_anterior = 2L,
  id_period_post = 3L,
  id_run_reference = id_run_observed,
  id_run_simulated = members$id_run
)[members[, .(id_run, parent_id_run)], on = "id_run"][,
  .(fom_mean = mean(figure_of_merit), fom_sd = sd(figure_of_merit), fom_null = mean(figure_of_merit_null)),
  by = parent_id_run
]
