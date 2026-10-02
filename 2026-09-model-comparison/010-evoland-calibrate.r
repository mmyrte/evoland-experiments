#' ---
#' title: "PIE benchmark: evoland-plus set-up and calibration"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' Builds the evoland database for the Plum Island Ecosystems benchmark and calibrates two
#' transition-potential estimators on the 1985 → 1991 transition. Allocation of 1991 → 1999
#' follows in the `020-` steps.
#'
#' Backcast design, as in `2026-09-paper-figures/010-fig2-ensembles.r`:
#'
#' - **Calibration** sees 1985 and 1991 only (run 0). 1999 is period 3, flagged as
#'   extrapolated.
#' - **Held-out observation**: the observed 1999 map is stored in run 1, a child of run 0 that
#'   is a sibling of every simulation run, so no model or allocator can read it.
#' - **Demand** is the observed 1991 → 1999 count of each viable transition; every tool gets
#'   the same quantities, so the comparison is about *where* change is placed.

#| label: setup
#| output: false
library(evoland)
library(data.table)
library(terra)
library(mlr3learners)
source("2026-09-model-comparison/common.r")

unlink(db_path, recursive = TRUE)
reset_timings("evoland")
db <- evoland_db$new(path = db_path)

lu <- c(read_lu(1985), read_lu(1991), read_lu(1999))
ef <- terra::rast(file.path(data_dir, paste0("ef_00", 1:3, ".tif")))
names(ef) <- paste0("ef_00", 1:3)

#' # Domain

#| label: domain
db$lulc_meta_t <- create_lulc_meta_t(list(
  forest = list(pretty_name = "Forest", src_classes = 1L),
  built = list(pretty_name = "Built", src_classes = 2L),
  other = list(pretty_name = "Other", src_classes = 3L)
))

db$coords_t <- create_coords_t_square(
  epsg = 26986L,
  extent = terra::ext(lu),
  resolution = terra::res(lu)[1]
)

# Irregular periods (6 and 8 years) are fine for evoland: demand is given as counts below,
# so the period length never enters a rate.
db$periods_t <- as_periods_t(data.table(
  id_period = 0:3,
  start_date = as.Date(c("1991-12-31", "1980-01-01", "1986-01-01", "1992-01-01")),
  end_date = as.Date(c("1991-12-31", "1985-12-31", "1991-12-31", "1999-12-31")),
  is_extrapolated = c(FALSE, FALSE, FALSE, TRUE)
))
db$periods_t

#' # Data

#| label: lulc
lulc_long <- extract_using_coords_t(lu, db$coords_t)[, .(
  id_coord,
  id_period = id_period_by_year[sub("lu_", "", layer)],
  id_lulc = as.integer(value)
)]
lulc_long[, .N, by = .(id_period, id_lulc)][order(id_period, id_lulc)]

db$lulc_data_t <- as_lulc_data_t(lulc_long[
  id_period <= 2L,
  .(id_run = 0L, id_coord, id_period, id_lulc)
])

#' The three explanatory factors of the lulcc case are static (period 0). evoland adds its
#' neighbourhood predictors on top: the number of cells of each class within 150 m (the eight
#' adjacent cells) and between 150 and 500 m.

#| label: predictors
db$pred_meta_t <- create_pred_meta_t(list(
  ef_001 = list(pretty_name = "Elevation", data_type = "float"),
  ef_002 = list(pretty_name = "Slope", data_type = "float"),
  ef_003 = list(pretty_name = "Distance to built land 1985", data_type = "float")
))
db$pred_data_t <- extract_using_coords_t(ef, db$coords_minimal)[, .(
  id_coord,
  id_run = 0L,
  id_period = 0L,
  id_pred = match(as.character(layer), names(ef)),
  value
)] |>
  as_pred_data_t()

timed("evoland", "neighbours", {
  db$set_neighbors(max_distance = 500, distance_breaks = c(0, 150, 500), quiet = TRUE)
  db$generate_neighbor_predictors()
})
db$pred_meta_t

#' # Calibration on 1985 → 1991

#| label: trans-meta
db$trans_meta_t <- create_trans_meta_t(db$trans_v, min_cardinality_abs = 20)
db$trans_meta_t
db$set_full_trans_preds()

#' # Runs
#'
#' Two estimators, each a parent run below run 0: a logistic regression (the estimator of the
#' lulcc and CLUinPy runs) and a random forest. Both see the same predictors.

#| label: runs
estimators <- data.table(
  id_run = c(1000L, 2000L),
  learner = c("log_reg", "ranger"),
  description = c("logistic regression potentials", "random forest potentials")
)
runs <- rbind(
  data.table(
    id_run = c(0L, id_run_observed),
    parent_id_run = c(NA_integer_, 0L),
    description = c("calibration base: observed 1985, 1991", "observed 1999 (validation only)")
  ),
  estimators[, .(id_run, parent_id_run = 0L, description)],
  # the allocation realisations, registered here so that every 020- step can run in parallel
  estimators[,
    .(
      id_run = c(id_run + seq_len(n_realisations), id_run + 500L + seq_len(n_realisations)),
      allocator = rep(c("clumpy", "dinamica"), each = n_realisations),
      member = rep(seq_len(n_realisations), 2)
    ),
    by = .(parent_id_run = id_run, learner)
  ][, .(id_run, parent_id_run, description = paste(learner, allocator, "realisation", member))]
)
runs[, seed := fifelse(id_run > 1000L, 10000L + id_run, NA_integer_)]
db$commit(as_runs_t(runs), "runs_t", method = "overwrite")

db$lulc_data_t <- as_lulc_data_t(lulc_long[
  id_period == 3L,
  .(id_run = id_run_observed, id_coord, id_period, id_lulc)
])

#' # Allocation parameters
#'
#' Patch size, variance and isometry, and the expander fraction, estimated on 1985 → 1991 at
#' run 0 and inherited by every realisation.

#| label: alloc-params
db$id_run <- 0L
db$alloc_params_t <- timed("evoland", "alloc_params", db$create_alloc_params_t())
db$alloc_params_t

#' # Demand: observed quantities 1991 → 1999

#| label: demand
observed_2_3 <- lulc_long[id_period == 2L, .(id_coord, id_lulc_anterior = id_lulc)][
  lulc_long[id_period == 3L, .(id_coord, id_lulc_posterior = id_lulc)],
  on = "id_coord"
]
anterior_totals <- observed_2_3[, .(n_anterior = .N), by = id_lulc_anterior]
observed_counts <- observed_2_3[
  id_lulc_anterior != id_lulc_posterior,
  .(count = .N),
  by = .(id_lulc_anterior, id_lulc_posterior)
]
rates_3 <- db$trans_meta_t[
  is_viable == TRUE,
  .(id_trans, id_lulc_anterior, id_lulc_posterior)
][observed_counts, on = .(id_lulc_anterior, id_lulc_posterior), nomatch = NULL][
  anterior_totals,
  on = "id_lulc_anterior",
  nomatch = NULL
][, .(id_run = 0L, id_period = 3L, id_trans, count, rate = count / n_anterior)]
db$trans_rates_t <- as_trans_rates_t(rates_3)
rates_3

#' Change the evoland model cannot produce, because the transition was not viable in
#' 1985 → 1991:
observed_counts[, sum(count)] - rates_3[, sum(count)]

# demand handed to every external tool: transition counts and class totals in 1999
fwrite(
  observed_counts[order(id_lulc_anterior, id_lulc_posterior)],
  file.path(outputs_dir, "demand_transitions_1991_1999.csv")
)
fwrite(
  lulc_long[, .N, by = .(year = names(id_period_by_year)[id_period], id_lulc)][
    order(year, id_lulc)
  ],
  file.path(outputs_dir, "demand_class_totals.csv")
)

#' # Transition models

#| label: fit
learner_by <- list(
  log_reg = mlr3::lrn("classif.log_reg", predict_type = "prob"),
  ranger = mlr3::lrn(
    "classif.ranger",
    predict_type = "prob",
    num.trees = 500,
    min.node.size = 50
  )
)

trans_preds_base <- db$trans_preds_t
modeled_by <- list()
for (i in seq_len(nrow(estimators))) {
  db$id_run <- estimators$id_run[i]
  learner_id <- estimators$learner[i]
  set.seed(estimators$id_run[i]) # the random forest is stochastic
  trans_models <- timed(
    "evoland",
    paste0("fit_", learner_id),
    db$fit_full_models(
      learner = learner_by[[learner_id]],
      trans_preds = as_trans_preds_t(trans_preds_base[, .(id_run = db$id_run, id_pred, id_trans)])
    )
  )
  modeled_by[[learner_id]] <- unique(trans_models$id_trans[
    !vapply(trans_models$learner_full, is.null, logical(1L))
  ])
  db$trans_models_t <- trans_models
  timed(
    "evoland",
    paste0("predict_", learner_id),
    db$predict_trans_pot(
      id_period_post = 3L,
      select_score = "no.crossval",
      select_maximize = TRUE,
      force = TRUE
    )
  )
}
modeled_by

#' # Transition potentials, exported for the allocator crossing
#'
#' The adjusted potentials of each estimator, written as one GeoTIFF per transition, so that
#' allocators outside evoland can be fed the same surfaces.

#| label: export-potentials
#| fig-asp: 0.6
for (i in seq_len(nrow(estimators))) {
  db$id_run <- estimators$id_run[i]
  adjusted <- db$adjusted_trans_pot_v(3L)
  out <- file.path(outputs_dir, "potentials", estimators$learner[i])
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  trans_labels <- db$trans_meta_t[, .(id_trans, label = paste0(id_lulc_anterior, "_", id_lulc_posterior))]
  pot_rast <- terra::rast(lapply(seq_len(nrow(trans_labels)), function(j) {
    tabular_to_raster(
      adjusted[id_trans == trans_labels$id_trans[j], .(id_coord, value)],
      coords = db$coords_minimal,
      value_col = "value"
    )
  }))
  names(pot_rast) <- trans_labels$label
  for (n in names(pot_rast)) {
    terra::writeRaster(pot_rast[[n]], file.path(out, paste0(n, ".tif")), overwrite = TRUE)
  }
  plot(pot_rast, main = paste(estimators$learner[i], names(pot_rast)))
}
