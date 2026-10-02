#' ---
#' title: "PIE benchmark: other tools' potentials, allocated by CLUMPY"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' Completes the estimator × allocator matrix where the tools allow it. The surfaces the other
#' tools estimate are written into evoland's `trans_pot_t` and allocated by CLUMPY with the same
#' demand and allocation parameters as the evoland runs:
#'
#' - **Weights of Evidence** (run 3000): Dinamica's transition probability maps from
#'   `030-dinamica-native.r`, one per transition, as they are.
#' - **lulcc GLM** (run 4000) and **CLUinPy logistic regression** (run 5000): these estimate
#'   the *suitability of each class*, not of each transition. The potential of a transition
#'   i → k is taken to be the suitability of k, for the cells of class i; CLUMPY scales it to
#'   the demanded rate like any other potential.
#'
#' The realisations are runs `parent + 500 + i`; the tools' own maps are imported as runs
#' `parent + i` in `050-compare.r`.

#| label: setup
#| output: false
library(evoland)
library(data.table)
source("2026-09-model-comparison/common.r")
reset_timings("cross-clumpy")
db <- evoland_db$new(path = db_path)
maps_dir <- file.path(outputs_dir, "maps")

db$id_run <- 0L
viable <- db$trans_meta_t[is_viable == TRUE][order(id_lulc_anterior, id_lulc_posterior)]
viable
anterior <- db$fetch("lulc_data_t", where = "id_run = 0 and id_period = 2")[,
  .(id_coord, id_lulc_anterior = id_lulc)
]

#' # Potentials from the other tools

#| label: potentials
values_at_coords <- function(rast) {
  extract_using_coords_t(rast, db$coords_t)[, .(id_coord, layer = as.character(layer), value)]
}

# Dinamica writes one layer per transition, in the order of the transition set in simulate.ego,
# which is the order of `viable`
woe <- terra::rast(file.path(maps_dir, "dinamica-native", "probabilities.tif"))
# numeric layer names do not survive extraction, hence the prefixes
names(woe) <- paste0("trans_", viable$id_trans)
woe_pot <- values_at_coords(woe)[, .(id_trans = as.integer(sub("trans_", "", layer)), id_coord, value)]

suitability_pot <- function(path) {
  suit <- terra::rast(path)
  names(suit) <- paste0("class_", lulc_classes) # band k is the suitability of class k
  suit_long <- values_at_coords(suit)[, .(
    id_lulc_posterior = as.integer(sub("class_", "", layer)),
    id_coord,
    value
  )]
  viable[, .(id_trans, id_lulc_anterior, id_lulc_posterior)][
    suit_long,
    on = "id_lulc_posterior",
    allow.cartesian = TRUE,
    nomatch = NULL
  ][, .(id_trans, id_lulc_anterior, id_coord, value)]
}

potentials <- list(
  `3000` = woe_pot,
  `4000` = suitability_pot(file.path(maps_dir, "lulcc-suitability.tif")),
  `5000` = suitability_pot(file.path(maps_dir, "cluinpy-suitability.tif"))
)
lapply(potentials, \(p) p[, .(n = .N, min = min(value), max = max(value)), by = id_trans])

#' # Runs

#| label: runs
crossing <- data.table(
  id_run = c(3000L, 4000L, 5000L),
  description = c(
    "Weights of Evidence potentials (Dinamica)",
    "class suitability as potentials (lulcc GLM)",
    "class suitability as potentials (CLUinPy logistic regression)"
  )
)
members <- crossing[,
  .(id_run = id_run + 500L + seq_len(n_realisations), member = seq_len(n_realisations)),
  by = .(parent_id_run = id_run)
][, seed := 10000L + id_run]

db$id_run <- NULL
runs <- rbind(
  db$runs_t[!id_run %in% c(crossing$id_run, members$id_run)],
  crossing[, .(id_run, parent_id_run = 0L, description, seed = NA_integer_)],
  members[, .(id_run, parent_id_run, description = paste("CLUMPY realisation", member), seed)],
  fill = TRUE
)
db$commit(as_runs_t(runs), "runs_t", method = "overwrite")

for (id in crossing$id_run) {
  pot <- potentials[[as.character(id)]][
    viable[, .(id_trans, id_lulc_anterior)],
    on = "id_trans",
    nomatch = NULL
  ][anterior, on = .(id_coord, id_lulc_anterior), nomatch = NULL]
  db$id_run <- id
  db$commit(
    as_trans_pot_t(pot[, .(id_run = id, id_trans, id_period_post = 3L, id_coord, value)]),
    "trans_pot_t",
    method = "upsert"
  )
}

#' # Allocation

#| label: allocate
#| output: false
for (i in seq_len(nrow(members))) {
  db$id_run <- members$id_run[i]
  set.seed(members$seed[i])
  timed(
    "cross-clumpy",
    "allocate",
    db$alloc_clumpy(
      id_periods = 3L,
      select_score = "no.crossval",
      select_maximize = TRUE,
      use_parent_trans_pot = TRUE,
      update_neighbors = FALSE
    ),
    note = paste0("id_run=", members$id_run[i])
  )
}

#| label: fom
db$id_run <- NULL
db$figure_of_merit_v(
  id_period_anterior = 2L,
  id_period_post = 3L,
  id_run_reference = id_run_observed,
  id_run_simulated = members$id_run
)[members[, .(id_run, parent_id_run)], on = "id_run"][,
  .(fom_mean = mean(figure_of_merit), fom_sd = sd(figure_of_merit)),
  by = parent_id_run
]
