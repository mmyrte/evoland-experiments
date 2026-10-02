#' ---
#' title: "Which estimator closes the gap? Learners, feature sets, calibration periods"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' `010-skill-attribution.r` showed that allocation loses no skill (uSAM) and
#' that the estimated transition potentials capture only 40-49 % of the skill
#' the true probabilities attain, with no improvement at nine times the domain.
#' This step looks for the cause in the estimator. It scores the potentials
#' only: the allocator was shown to pass them through unchanged.
#'
#' Factors, all on the same synthetic process:
#'
#' - **learner**: featureless (base rate), logistic regression, lasso
#'   (cv_glmnet), naive Bayes (closest to Dinamica's weights of evidence),
#'   ranger as in Fig. 2, ranger with larger leaves (smoother probabilities),
#'   and xgboost;
#' - **feature set**: what a modeller has, either the top 4 by rpart importance
#'   (as in Fig. 2) or all of it (the drivers, the nuisance field and evoland's
#'   distance-band neighbourhood shares); or the process features, i.e. the
#'   terms of the generating logits: the drivers, the 5 x 5 neighbourhood shares
#'   and the site_quality x (1 - accessibility) interaction. With these, the
#'   logistic regression is correctly specified, so its loss is pure estimation
#'   variance;
#' - **calibration periods**: one or two. The target transition is the same in
#'   both, the extra period is added before it, so the comparison is paired;
#' - **domain size** and **landscape seed**, to separate the two.
#'
#' Scores are those of `010-skill-attribution`: expected multiclass Brier score against the true
#' probabilities, split into the distance to the truth and an irreducible term,
#' over all forest and arable cells (the classes that can change in the true
#' process), so that every configuration is scored on the same cells. Skill is
#' relative to climatology, the observed target rates of all four transitions;
#' `share_of_attainable` is a configuration's skill over the skill of the
#' truth.
#'
#' Learners whose package is missing are skipped with a message. Run time is
#' dominated by model fitting: 3 seeds x 2 sizes x 2 calibration settings x
#' 3 feature sets x 7 learners = 252 fits of up to four transitions each.

#| label: setup
#| output: false
library(evoland)
library(data.table)
library(terra)
library(ggplot2)
library(mlr3learners)

options(synthetic_process.source_only = TRUE)
source("2026-09-paper-figures/000-synthetic-process.r")

options(evoland.ducklake_db_append_warning = FALSE)

grid_sizes <- c(30L, 90L)
landscape_seeds <- c(1337L, 2024L, 4711L)
calibration_periods <- c(1L, 2L)
out_dir <- "2026-09-paper-figures/figures"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

#' # Learners

#| label: learners
learner_specs <- list(
  featureless = list(pkg = NULL, make = function() mlr3::lrn("classif.featureless")),
  log_reg = list(pkg = NULL, make = function() mlr3::lrn("classif.log_reg")),
  cv_glmnet = list(
    pkg = "glmnet",
    make = function() mlr3::lrn("classif.cv_glmnet", alpha = 1)
  ),
  naive_bayes = list(pkg = "e1071", make = function() mlr3::lrn("classif.naive_bayes")),
  ranger = list(
    pkg = "ranger",
    make = function() mlr3::lrn("classif.ranger", num.trees = 200)
  ),
  ranger_large_leaves = list(
    pkg = "ranger",
    make = function() mlr3::lrn("classif.ranger", num.trees = 500, min.node.size = 50)
  ),
  xgboost = list(
    pkg = "xgboost",
    make = function() {
      mlr3::lrn(
        "classif.xgboost",
        nrounds = 200,
        eta = 0.05,
        max_depth = 3,
        subsample = 0.8
      )
    }
  )
)
learner_available <- vapply(
  learner_specs,
  function(x) is.null(x$pkg) || requireNamespace(x$pkg, quietly = TRUE),
  logical(1L)
)
if (!all(learner_available)) {
  message(
    "Skipping learners whose package is not installed: ",
    paste(names(learner_specs)[!learner_available], collapse = ", "),
    ". Install with e.g. `rv add glmnet xgboost e1071`."
  )
}
learner_specs <- learner_specs[learner_available]

#' # One case: domain size x landscape seed x calibration periods

#| label: case
pred_names <- c(
  "accessibility",
  "site_quality",
  "random_nuisance",
  "share_forest_5x5",
  "share_arable_5x5",
  "share_urban_5x5",
  "site_quality_x_inaccessibility"
)
# what a modeller has: drivers and nuisance; evoland's neighbour predictors are added below
available_static <- match(c("accessibility", "site_quality", "random_nuisance"), pred_names)
# the terms of the generating logits
process_preds <- match(
  c(
    "accessibility",
    "site_quality",
    "share_forest_5x5",
    "share_arable_5x5",
    "share_urban_5x5",
    "site_quality_x_inaccessibility"
  ),
  pred_names
)

run_case <- function(n_grid, landscape_seed, n_calib) {
  # the landscape depends on the seed only, so both calibration settings see the same maps
  set.seed(landscape_seed)
  template_rast <- terra::rast(
    crs = "EPSG:2056",
    extent = terra::ext(c(
      xmin = 2697000,
      xmax = 2697000 + n_grid * 100,
      ymin = 1252000,
      ymax = 1252000 + n_grid * 100
    )),
    resolution = 100
  )
  drivers <- make_drivers(template_rast)
  all_maps <- list(make_landscape(template_rast, drivers))
  for (i in 1:3) {
    all_maps[[i + 1L]] <- step_process(all_maps[[i]], drivers)
  }
  # the last map is the target; one or two observed transitions before it
  maps_used <- all_maps[(3L - n_calib):4L]
  target <- length(maps_used)

  db_path <- glue::glue(
    "2026-09-paper-figures/learner-comparison-{n_grid}-{landscape_seed}-{n_calib}.evolanddb"
  )
  unlink(db_path, recursive = TRUE)
  db <- evoland_db$new(path = db_path)
  on.exit(db$id_run <- NULL, add = TRUE)

  db$lulc_meta_t <- create_lulc_meta_t(list(
    forest = list(pretty_name = "Forest"),
    arable = list(pretty_name = "Arable Land"),
    urban = list(pretty_name = "Urban Areas"),
    static = list(pretty_name = "Immutable")
  ))
  db$coords_t <- create_coords_t_square(
    epsg = 2056L,
    extent = terra::ext(template_rast),
    resolution = 100
  )
  db$periods_t <- create_periods_t(
    period_length_str = "P10Y",
    start_observed = "2000-01-01",
    end_observed = sprintf("%d-01-01", 2000L + 10L * n_calib),
    end_extrapolated = sprintf("%d-01-01", 2000L + 10L * (n_calib + 1L))
  )
  periods <- db$periods_t[id_period >= 1L]
  stopifnot(
    "unexpected period layout" = nrow(periods) == target &&
      identical(periods[is_extrapolated == TRUE, id_period], target)
  )

  lulc_long <- extract_using_coords_t(
    terra::rast(maps_used) |> setNames(paste0("id_period=", seq_len(target))),
    db$coords_t
  )[, .(
    id_coord,
    id_period = as.integer(sub(".*[^0-9]", "", layer)),
    id_lulc = value
  )]
  db$lulc_data_t <- as_lulc_data_t(lulc_long[
    id_period < target,
    .(id_run = 0L, id_coord, id_period, id_lulc)
  ])

  # predictors: static drivers in period 0, process neighbourhood shares per anterior period
  db$pred_meta_t <- create_pred_meta_t(
    setNames(
      lapply(pred_names, function(nm) list(description = "synthetic", data_type = "float")),
      pred_names
    )
  )
  stopifnot(
    "predictor ids out of order" = identical(
      db$pred_meta_t[match(pred_names, name), id_pred],
      seq_along(pred_names)
    )
  )
  static_rast <- c(
    drivers$accessibility,
    drivers$site_quality,
    drivers$random_nuisance,
    drivers$site_quality * (1 - drivers$accessibility)
  ) |>
    setNames(c(
      "accessibility",
      "site_quality",
      "random_nuisance",
      "site_quality_x_inaccessibility"
    ))
  static_data <- extract_using_coords_t(static_rast, db$coords_minimal)[,
    .(id_coord, id_period = 0L, id_pred = match(as.character(layer), pred_names), value)
  ]
  dynamic_data <- rbindlist(lapply(seq_len(target - 1L), function(p) {
    shares <- c(
      neighbour_share(maps_used[[p]], 1),
      neighbour_share(maps_used[[p]], 2),
      neighbour_share(maps_used[[p]], 3)
    ) |>
      setNames(c("share_forest_5x5", "share_arable_5x5", "share_urban_5x5"))
    extract_using_coords_t(shares, db$coords_minimal)[,
      .(id_coord, id_period = p, id_pred = match(as.character(layer), pred_names), value)
    ]
  }))
  db$pred_data_t <- as_pred_data_t(
    rbind(static_data, dynamic_data)[, .(id_coord, id_run = 0L, id_period, id_pred, value)]
  )

  db$set_neighbors(max_distance = 1000, distance_breaks = c(0, 300, 1000), quiet = TRUE)
  db$generate_neighbor_predictors()
  neighbour_preds <- setdiff(db$pred_meta_t$id_pred, seq_along(pred_names))
  available_preds <- c(available_static, neighbour_preds)

  db$trans_meta_t <- create_trans_meta_t(
    db$trans_v,
    min_cardinality_abs = 10,
    exclude_anterior = 4
  )
  viable <- db$trans_meta_t[
    is_viable == TRUE,
    .(id_trans, id_lulc_anterior, id_lulc_posterior)
  ]

  # demand: observed target quantities of the viable transitions
  maps <- lulc_long[id_period == target - 1L, .(id_coord, anterior = id_lulc)][
    lulc_long[id_period == target, .(id_coord, observed = id_lulc)],
    on = "id_coord"
  ]
  n_anterior <- maps[, .(n_anterior = .N), by = .(id_lulc_anterior = anterior)]
  observed_rates <- maps[
    anterior != observed,
    .(count = .N),
    by = .(id_lulc_anterior = anterior, id_lulc_posterior = observed)
  ][n_anterior, on = "id_lulc_anterior", nomatch = NULL][, rate := count / n_anterior]
  db$trans_rates_t <- as_trans_rates_t(
    viable[observed_rates, on = .(id_lulc_anterior, id_lulc_posterior), nomatch = NULL][,
      .(id_run = 0L, id_period = target, id_trans, count, rate)
    ]
  )

  # truth for the target transition, and the cells scored: all forest and arable cells
  cell_of <- data.table(
    id_coord = db$coords_minimal$id_coord,
    cell = terra::cellFromXY(template_rast, as.matrix(db$coords_minimal[, .(lon, lat)]))
  )
  truth_all <- true_probs(maps_used[[target - 1L]], drivers)[cell_of, on = "cell", nomatch = NULL]
  eligible <- maps[anterior %in% c(1L, 2L), id_coord]
  classes <- sort(unique(c(maps$anterior, maps$observed)))
  anterior_of <- maps[id_coord %in% eligible, .(id_coord, anterior)]
  bt <- brier_tables(
    maps,
    eligible,
    classes,
    with_persistence(truth_all[, .(id_coord, class, p = q)], anterior_of)[,
      .(id_coord, class, q = p)
    ]
  )
  persist <- function(trans_p) with_persistence(trans_p, anterior_of)
  long_of <- function(pot) {
    pot[viable, on = "id_trans", nomatch = NULL][,
      .(id_coord, class = id_lulc_posterior, p = value)
    ]
  }

  # feature sets: committed per configuration run (prediction reads them through
  # pred_data_wide_v), never for the base run. Lineage reads resolve trans_preds_t per
  # (id_trans, id_pred) slice, so base-run rows would leak into every configuration's set.
  db$id_run <- 0L
  scored <- db$get_pred_filter_score(
    filter = mlr3filters::FilterImportance$new(learner = mlr3::lrn("classif.rpart")),
    trans_preds = as_trans_preds_t(CJ(
      id_run = 0L,
      id_pred = available_preds,
      id_trans = viable$id_trans
    ))
  )
  feature_sets <- list(
    "available, rpart top 4" = scored[order(-importance)][,
      head(.SD, 4L),
      by = id_trans
    ][, .(id_trans, id_pred)],
    "available, all" = CJ(id_trans = viable$id_trans, id_pred = available_preds),
    "process features" = CJ(id_trans = viable$id_trans, id_pred = process_preds)
  )

  configs <- CJ(feature_set = names(feature_sets), learner = names(learner_specs))
  configs[, id_run := .I]
  id_run_oracle <- nrow(configs) + 1L
  db$commit(
    as_runs_t(rbind(
      data.table(id_run = 0L, parent_id_run = NA_integer_, description = "calibration base"),
      configs[, .(
        id_run,
        parent_id_run = 0L,
        description = paste(learner, feature_set, sep = " / ")
      )],
      data.table(
        id_run = id_run_oracle,
        parent_id_run = 0L,
        description = "oracle potentials, viable transitions"
      )
    )),
    "runs_t",
    method = "overwrite"
  )

  # references: the truth (ceiling), the oracle restricted to viable transitions,
  # climatology over all four transitions, persistence
  db$id_run <- id_run_oracle
  db$trans_pot_t <- as_trans_pot_t(
    truth_all[viable, on = .(id_lulc_anterior, class = id_lulc_posterior), nomatch = NULL][,
      .(id_run = id_run_oracle, id_trans, id_period_post = target, id_coord, value = q)
    ]
  )
  references <- rbind(
    cbind(
      learner = "truth, all four transitions",
      stage = "reference",
      score(persist(truth_all[, .(id_coord, class, p = q)]), bt)
    ),
    cbind(
      learner = "oracle, viable transitions",
      stage = "adjusted",
      score(persist(long_of(db$adjusted_trans_pot_v(target))), bt)
    ),
    cbind(
      learner = "climatology",
      stage = "reference",
      score(
        persist(anterior_of[
          observed_rates,
          on = c(anterior = "id_lulc_anterior"),
          allow.cartesian = TRUE,
          nomatch = NULL
        ][, .(id_coord, class = id_lulc_posterior, p = rate)]),
        bt
      )
    ),
    cbind(
      learner = "persistence",
      stage = "reference",
      score(anterior_of[, .(id_coord, class = anterior, p = 1)], bt)
    )
  )
  references[, `:=`(feature_set = NA_character_, status = "ok", fit_seconds = NA_real_)]

  results <- rbindlist(lapply(seq_len(nrow(configs)), function(i) {
    cfg <- configs[i]
    db$id_run <- cfg$id_run
    message(glue::glue(
      "n_grid={n_grid} seed={landscape_seed} n_calib={n_calib}: {cfg$learner} / {cfg$feature_set}"
    ))
    trans_preds <- as_trans_preds_t(feature_sets[[cfg$feature_set]][, .(
      id_run = cfg$id_run,
      id_pred,
      id_trans
    )])
    fit_seconds <- NA_real_
    outcome <- tryCatch(
      {
        # append, not upsert: every column of trans_preds_t is a key column, and
        # evoland's MERGE then has an empty `update set` (DuckDB parser error). Each
        # configuration writes its own id_run slice, so append cannot duplicate rows.
        db$commit(trans_preds, "trans_preds_t", method = "append")
        set.seed(landscape_seed + i)
        fit_seconds <- system.time({
          models <- db$fit_full_models(
            learner = learner_specs[[cfg$learner]]$make(),
            trans_preds = trans_preds
          )
        })[["elapsed"]]
        failed <- models[vapply(learner_full, is.null, logical(1L)), id_trans]
        if (length(failed) > 0L) {
          stop("fit failed for id_trans ", paste(failed, collapse = ", "))
        }
        db$trans_models_t <- models
        db$predict_trans_pot(
          id_period_post = target,
          select_score = "no.crossval",
          select_maximize = TRUE
        )
        raw <- db$trans_pot_t[id_period_post == target]
        rbind(
          cbind(stage = "raw", score(persist(long_of(raw)), bt)),
          cbind(stage = "adjusted", score(persist(long_of(db$adjusted_trans_pot_v(target))), bt))
        )[, status := "ok"]
      },
      error = function(e) {
        data.table(
          stage = "adjusted",
          brier_realised = NA_real_,
          distance_to_truth = NA_real_,
          brier_expected = NA_real_,
          status = conditionMessage(e)
        )
      }
    )
    outcome[, `:=`(
      learner = cfg$learner,
      feature_set = cfg$feature_set,
      fit_seconds = fit_seconds
    )]
  }))

  out <- rbind(results, references, use.names = TRUE)
  clim <- references[learner == "climatology"]
  ceiling <- references[learner == "truth, all four transitions"]
  out[, `:=`(
    n_grid = n_grid,
    landscape_seed = landscape_seed,
    n_calib = n_calib,
    viable_transitions = paste(
      viable[, paste(id_lulc_anterior, id_lulc_posterior, sep = "->")],
      collapse = ", "
    ),
    skill_expected = 1 - brier_expected / clim$brier_expected,
    skill_realised = 1 - brier_realised / clim$brier_realised
  )]
  out[, share_of_attainable := skill_expected / (1 - ceiling$brier_expected / clim$brier_expected)]
  out[]
}

#' # All cases

#' Cases are independent (each has its own database), so they run in parallel
#' on a PSOCK cluster, largest domains first for load balancing. Set the
#' option `learner_comparison.workers` to override the number of workers.

#| label: run
#| output: false
cases <- CJ(n_grid = grid_sizes, landscape_seed = landscape_seeds, n_calib = calibration_periods)
setorder(cases, -n_grid, landscape_seed, n_calib)
n_workers <- getOption(
  "learner_comparison.workers",
  max(1L, min(nrow(cases), parallel::detectCores() - 1L))
)
cl <- parallel::makeCluster(n_workers)
parallel::clusterCall(
  cl,
  function(wd, lib) {
    setwd(wd)
    .libPaths(lib)
    NULL
  },
  getwd(),
  .libPaths()
)
parallel::clusterEvalQ(cl, {
  suppressPackageStartupMessages({
    library(evoland)
    library(data.table)
    library(terra)
    library(mlr3learners)
  })
  # one thread per worker: the workers are the parallelism
  data.table::setDTthreads(1L)
  options(
    synthetic_process.source_only = TRUE,
    evoland.ducklake_db_append_warning = FALSE
  )
  source("2026-09-paper-figures/000-synthetic-process.r")
  NULL
})
parallel::clusterExport(
  cl,
  c("run_case", "learner_specs", "pred_names", "available_static", "process_preds")
)
results <- tryCatch(
  rbindlist(
    parallel::parLapplyLB(
      cl,
      seq_len(nrow(cases)),
      function(i, cases) {
        run_case(cases$n_grid[i], cases$landscape_seed[i], cases$n_calib[i])
      },
      cases = cases
    ),
    use.names = TRUE,
    fill = TRUE
  ),
  finally = parallel::stopCluster(cl)
)
fwrite(results, file.path(out_dir, "learner-comparison.csv"))

failed <- results[!is.na(feature_set) & status != "ok"]
if (nrow(failed) == nrow(results[!is.na(feature_set)])) {
  stop("all configurations failed; first error: ", failed$status[1])
}

#' # Results
#'
#' Failed configurations, if any:

#| label: failures
knitr::kable(unique(results[
  status != "ok",
  .(n_grid, landscape_seed, n_calib, learner, feature_set, status)
]))

#' Share of the attainable skill reached by the adjusted potentials (the
#' allocation-ready ones), mean over landscape seeds. 1 means as good as the
#' true probabilities; 0 means no better than climatology.

#| label: summary
summary_tab <- results[
  stage == "adjusted" & status == "ok",
  .(share = mean(share_of_attainable), distance = mean(distance_to_truth)),
  by = .(n_grid, n_calib, feature_set, learner)
]
knitr::kable(
  dcast(
    summary_tab[!is.na(feature_set)],
    feature_set + learner ~ paste0(n_grid, "x", n_grid, ", ", n_calib, " period(s)"),
    value.var = "share"
  ),
  digits = 2
)

#' References (oracle restricted to the viable transitions, and the truth), mean over seeds:

#| label: references
knitr::kable(
  results[
    is.na(feature_set) & learner != "climatology",
    .(share = mean(share_of_attainable), skill = mean(skill_expected)),
    by = .(n_grid, n_calib, learner)
  ],
  digits = 3
)

#' Raw vs. adjusted potentials: the adjustment rescales each transition to the
#' demand and caps the per-cell sum. A learner with poorly calibrated
#' probabilities gains from it; a well calibrated one should not lose.

#| label: raw-vs-adjusted
knitr::kable(
  dcast(
    results[!is.na(feature_set) & status == "ok"],
    n_grid + n_calib + feature_set + learner ~ stage,
    value.var = "distance_to_truth",
    fun.aggregate = mean
  ),
  digits = 4
)

#| label: plot
#| fig-width: 9
#| fig-height: 6
p <- ggplot(
  results[stage == "adjusted" & status == "ok" & !is.na(feature_set)],
  aes(share_of_attainable, learner, colour = factor(n_calib))
) +
  geom_vline(xintercept = c(0, 1), colour = "grey60", linewidth = 0.3) +
  geom_point(alpha = 0.5, position = position_dodge(width = 0.5)) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 3,
    size = 3,
    position = position_dodge(width = 0.5)
  ) +
  facet_grid(feature_set ~ paste0(n_grid, " x ", n_grid)) +
  scale_colour_manual(values = c("1" = "#0072B2", "2" = "#D55E00"), name = "calibration periods") +
  labs(
    x = "share of attainable skill (adjusted potentials)",
    y = NULL,
    caption = "points: landscape seeds; crosses: mean"
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")
p
ggsave(file.path(out_dir, "learner-comparison.pdf"), p, width = 9, height = 6)
