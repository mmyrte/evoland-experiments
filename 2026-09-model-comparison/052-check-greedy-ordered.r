#' ---
#' title: "PIE benchmark: evoland's greedy allocator, ordered, against lulcc's Ordered model"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' A check, not part of the crossing: does `alloc_greedy(arbitration = "ordered")` do what
#' lulcc's Ordered model (Fuchs et al. 2013) does? Both rank cells by a potential and fill a
#' demand greedily, with competition settled by a fixed priority: built, forest, other. They
#' differ in what they fill:
#'
#' - **lulcc Ordered** fills *net class totals*: built takes its net gain from any cell not
#'   yet built, by the class probability of built; a shrinking class releases its least
#'   suitable cells. Gross change it does not see.
#' - **evoland greedy, ordered** fills *transition counts*: forest → built takes the observed
#'   number of forest cells, by the potential of forest → built, and so on, transitions into
#'   built first, then into forest, then into other.
#'
#' Fed the same estimator (`020-potentials.r` derives lulcc's class probabilities from the
#' same adjusted potentials), the two should place built land alike and differ where gross
#' change exceeds net change. One deterministic map per estimator, run `estimator + 800 + 1`,
#' compared cell by cell with the crossing's lulcc Ordered map (`estimator + 500 + 1`). Needs
#' `050-compare.r` to have imported the lulcc maps.

#| label: setup
#| output: false
library(evoland)
library(data.table)
source("2026-09-model-comparison/common.r")
db <- evoland_db$new(path = db_path)

checks <-
  estimators[,
    .(
      estimator,
      label,
      id_run_parent = id_run,
      id_run_greedy = id_run + 800L + 1L,
      id_run_lulcc = id_run + allocators[allocator == "ordered", offset] + 1L
    )
  ]

db$id_run <- NULL
runs <- db$runs_t
db$commit(
  as_runs_t(rbind(
    runs[!id_run %in% checks$id_run_greedy],
    checks[, .(
      id_run = id_run_greedy,
      parent_id_run = id_run_parent,
      description = paste(label, "x greedy, ordered (check against lulcc Ordered)"),
      seed = NA_integer_
    )]
  )),
  "runs_t",
  method = "overwrite"
)

#' # Priority
#'
#' lulcc's `order = c(2, 1, 3)` in transitions: those into built first, then into forest, then
#' into other; within a class by anterior class.

#| label: order
db$id_run <- 0L
priority <- db$trans_meta_t[is_viable == TRUE]
priority <-
  priority[
    order(match(id_lulc_posterior, c(2L, 1L, 3L)), id_lulc_anterior)
  ]
priority[, .(id_trans, id_lulc_anterior, id_lulc_posterior)]

#| label: allocate
#| output: false
for (i in seq_len(nrow(checks))) {
  db$id_run <- checks$id_run_greedy[i]
  db$alloc_greedy(
    id_periods = 3L,
    select_score = "no.crossval",
    select_maximize = TRUE,
    arbitration = "ordered",
    order = priority$id_trans,
    use_parent_trans_pot = TRUE,
    update_neighbors = FALSE
  )
}

#' # Comparison
#'
#' Figure of merit of both against the observation, and the agreement of the two 1999 maps:
#' the share of all cells, and of the cells either of them changed, in the same class.

#| label: compare
db$id_run <- NULL
fom <-
  db$figure_of_merit_v(
    id_period_anterior = 2L,
    id_period_post = 3L,
    id_run_reference = id_run_observed,
    id_run_simulated = c(checks$id_run_greedy, checks$id_run_lulcc)
  )[,
    .(id_run, figure_of_merit)
  ]

# a run's 1999 map, read through its lineage
map_1999 <- function(id_run) {
  db$id_run <- id_run
  db$fetch("lulc_data_t", where = "id_period = 3")[, .(id_coord, id_lulc)]
}
db$id_run <- 0L
anterior <- db$fetch("lulc_data_t", where = "id_period = 2")[, .(id_coord, anterior = id_lulc)]
agreement <- rbindlist(lapply(seq_len(nrow(checks)), function(i) {
  maps <-
    anterior[
      map_1999(checks$id_run_greedy[i])[, .(id_coord, greedy = id_lulc)],
      on = "id_coord",
      nomatch = NULL
    ][
      map_1999(checks$id_run_lulcc[i])[, .(id_coord, lulcc = id_lulc)],
      on = "id_coord",
      nomatch = NULL
    ]
  changed <- maps[greedy != anterior | lulcc != anterior]
  data.table(
    estimator = checks$label[i],
    cells_same = maps[, mean(greedy == lulcc)],
    changed_greedy = maps[, sum(greedy != anterior)],
    changed_lulcc = maps[, sum(lulcc != anterior)],
    changed_either = nrow(changed),
    changed_same = changed[, mean(greedy == lulcc)],
    built_gain_same = maps[greedy == 2L & anterior != 2L, mean(lulcc == 2L)]
  )
}))

result <-
  checks[,
    .(estimator = label, id_run_greedy, id_run_lulcc)
  ][
    fom[, .(id_run_greedy = id_run, fom_greedy_ordered = figure_of_merit)],
    on = "id_run_greedy",
    nomatch = NULL
  ][
    fom[, .(id_run_lulcc = id_run, fom_lulcc_ordered = figure_of_merit)],
    on = "id_run_lulcc",
    nomatch = NULL
  ][
    agreement,
    on = "estimator"
  ]
result
fwrite(result, file.path(pie_dir, "figures", "pie-greedy-ordered-vs-lulcc.csv"))
