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

#' As in `2026-05-ssp-ch/010-ingest-lulc-data.qmd`, `coords_t` is restricted to the coordinates
#' that carry land use: the PIE study area covers 53 % of its bounding rectangle, and the rest
#' would only carry empty neighbourhoods and predictors along.

#| label: prune-coords
db$commit(
  x = db$coords_t[id_coord %in% lulc_long[, unique(id_coord)]],
  table_name = "coords_t",
  method = "overwrite"
)
nrow(db$coords_t)

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
#' Four estimators, each a parent run below run 0 (see `common.r`): a logistic regression, a
#' random forest and a naive Bayes classifier fitted here on the same predictors, and Dinamica's
#' Weights of Evidence, whose probability maps `020-potentials.r` writes into run 3000. Every allocator gets every
#' estimator's potentials; the realisations are registered here, each with its seed, so that the
#' allocation steps can run in parallel.

#| label: runs
members <- crossing_members()
runs <- rbind(
  data.table(
    id_run = c(0L, id_run_observed),
    parent_id_run = c(NA_integer_, 0L),
    description = c("calibration base: observed 1985, 1991", "observed 1999 (validation only)"),
    seed = NA_integer_
  ),
  estimators[, .(
    id_run,
    parent_id_run = 0L,
    description = paste(label, "potentials"),
    seed = NA_integer_
  )],
  members[, .(
    id_run,
    parent_id_run = id_run_parent,
    description = paste(estimator_label, "x", allocator_label, "realisation", member),
    seed
  )]
)
db$commit(as_runs_t(runs), "runs_t", method = "overwrite")
members[, .N, by = .(estimator, allocator)]

# the held-out observation, in its own run
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

# the demand every external tool gets is written by 000-pie-data.r from the rasters; the patch
# parameters estimated here are exported for the standalone Dinamica run
fwrite(
  db$alloc_params_t[db$trans_meta_t[is_viable == TRUE], on = "id_trans"][
    order(id_lulc_anterior, id_lulc_posterior),
    .(
      id_lulc_anterior,
      id_lulc_posterior,
      frac_expander,
      mean_patch_size,
      patch_size_variance,
      patch_isometry
    )
  ],
  file.path(outputs_dir, "alloc_params_viable.csv")
)

#' # Transition models

#| label: fit
learner_by <- list(
  log_reg = mlr3::lrn("classif.log_reg", predict_type = "prob"),
  ranger = mlr3::lrn(
    "classif.ranger",
    predict_type = "prob",
    num.trees = 500,
    min.node.size = 50,
    seed = 2000L # set.seed() alone does not make the forest replay
  ),
  # Gaussian naive Bayes: the model family of Weights of Evidence (additive log-odds per
  # predictor, assuming conditional independence), with a normal density per class in place of
  # Dinamica's binned ranges, and on evoland's predictors in place of Dinamica's
  naive_bayes = mlr3::lrn("classif.naive_bayes", predict_type = "prob")
)

# the estimators fitted in evoland; Weights of Evidence comes from Dinamica (015, 020)
evoland_estimators <- estimators[estimator %in% names(learner_by)]
trans_preds_base <- db$trans_preds_t
modeled_by <- list()
for (i in seq_len(nrow(evoland_estimators))) {
  db$id_run <- evoland_estimators$id_run[i]
  learner_id <- evoland_estimators$estimator[i]
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
