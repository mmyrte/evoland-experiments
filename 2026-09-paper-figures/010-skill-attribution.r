#' ---
#' title: "Skill attribution: transition potentials or allocation?"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' Figure 2's ensemble has almost no probabilistic skill: its multiclass Brier
#' score barely beats the uniform forecast, which knows the quantity of change
#' but not its location. This step asks where
#' the skill is lost. The
#' landscape is synthetic, so the true per-cell transition probabilities `q` for
#' period 2 -> 3 are known. That allows a factorial experiment:
#'
#' - **potentials**: estimated (ranger, as in Fig. 2) vs. oracle (the true `q`,
#'   restricted to the viable transitions so that the allocator can use them);
#' - **allocation**: none (score the potential surface itself) vs. CLUMPY with
#'   mono-pixel uSAM vs. CLUMPY with the estimated patch parameters (uPAM).
#'
#' Knowing `q` also removes the noise of the single observed outcome. For a
#' forecast `p`, the expected multiclass Brier score over outcomes drawn from
#' `q` is
#'
#' E[BS] = sum_k (p_k - q_k)^2 + sum_k q_k (1 - q_k).
#'
#' The second term is irreducible and the same for every forecast. The first,
#' the *distance to the truth* D, is what a forecast can improve on. So:
#'
#' - estimation loss = D(estimated potentials) - D(oracle potentials);
#' - allocation loss = D(ensemble frequency) - D(the potentials it samples);
#' - attainable skill = expected skill of `q` itself over the uniform forecast
#'   (every cell of a class gets the class's demand rates).
#'
#' The experiment runs at two domain sizes with the same spatial scales, to see
#' whether more training data shrinks the estimation loss. The synthetic
#' process is the one in `010-fig2-ensembles.r`; keep the two in step.

#| label: setup
#| output: false
library(evoland)
library(data.table)
library(terra)
library(ggplot2)
library(mlr3learners)

grid_sizes <- c(30L, 90L)
n_members <- 100L
out_dir <- "2026-09-paper-figures/figures"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

#' # Synthetic process and scores
#'
#' Defined in `000-synthetic-process.r`.

#| label: process
options(synthetic_process.source_only = TRUE)
source("2026-09-paper-figures/000-synthetic-process.r")

#' # One experiment per domain size

#| label: experiment
run_experiment <- function(n_grid, seed = 1337L) {
  set.seed(seed)
  db_path <- glue::glue(
    "2026-09-paper-figures/skill-attribution-{n_grid}.evolanddb"
  )
  unlink(db_path, recursive = TRUE)
  db <- evoland_db$new(path = db_path)

  db$lulc_meta_t <- create_lulc_meta_t(list(
    forest = list(pretty_name = "Forest"),
    arable = list(pretty_name = "Arable Land"),
    urban = list(pretty_name = "Urban Areas"),
    static = list(pretty_name = "Immutable")
  ))
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
  db$coords_t <- create_coords_t_square(
    epsg = 2056L,
    extent = terra::ext(template_rast),
    resolution = 100
  )
  db$periods_t <- create_periods_t(
    period_length_str = "P10Y",
    start_observed = "2000-01-01",
    end_observed = "2010-01-01",
    end_extrapolated = "2020-01-01"
  )

  drivers <- make_drivers(template_rast)
  lulc_1 <- make_landscape(template_rast, drivers)
  lulc_2 <- step_process(lulc_1, drivers)
  lulc_3 <- step_process(lulc_2, drivers)

  lulc_long <- extract_using_coords_t(
    c(lulc_1, lulc_2, lulc_3) |> setNames(paste0("id_period=", 1:3)),
    db$coords_t
  )[, .(
    id_coord,
    id_period = as.integer(sub(".*[^0-9]", "", layer)),
    id_lulc = value
  )]
  db$lulc_data_t <- as_lulc_data_t(lulc_long[
    id_period <= 2L,
    .(id_run = 0L, id_coord, id_period, id_lulc)
  ])

  # raster cell -> id_coord, for the true probabilities
  cell_of <- data.table(
    id_coord = db$coords_minimal$id_coord,
    cell = terra::cellFromXY(
      template_rast,
      as.matrix(db$coords_minimal[, .(lon, lat)])
    )
  )

  # predictors and calibration on 1 -> 2, as in Fig. 2
  db$pred_meta_t <- create_pred_meta_t(list(
    accessibility = list(description = "synthetic driver", data_type = "float"),
    site_quality = list(description = "synthetic driver", data_type = "float"),
    random_nuisance = list(
      description = "synthetic nuisance",
      data_type = "float"
    )
  ))
  db$pred_data_t <- extract_using_coords_t(
    c(drivers$accessibility, drivers$site_quality, drivers$random_nuisance) |>
      setNames(c("accessibility", "site_quality", "random_nuisance")),
    db$coords_minimal
  )[, layer := as.character(layer)][,
    .(
      id_coord,
      id_run = 0L,
      id_period = 0L,
      id_pred = match(
        layer,
        c("accessibility", "site_quality", "random_nuisance")
      ),
      value
    )
  ] |>
    as_pred_data_t()
  db$set_neighbors(
    max_distance = 1000,
    distance_breaks = c(0, 300, 1000),
    quiet = TRUE
  )
  db$generate_neighbor_predictors()

  db$trans_meta_t <- create_trans_meta_t(
    db$trans_v,
    min_cardinality_abs = 10,
    exclude_anterior = 4
  )
  db$set_full_trans_preds()
  trans_pred_scored <- db$get_pred_filter_score(
    filter = mlr3filters::FilterImportance$new(
      learner = mlr3::lrn("classif.rpart")
    )
  )
  db$commit(
    trans_pred_scored[order(-importance)][, head(.SD, 4), by = id_trans],
    "trans_preds_t",
    method = "overwrite"
  )
  trans_models <- db$fit_full_models(
    learner = mlr3::lrn("classif.ranger", num.trees = 200)
  )
  modeled <- unique(trans_models$id_trans[
    !vapply(trans_models$learner_full, is.null, logical(1L))
  ])
  db$trans_models_t <- trans_models
  trans_meta <- db$trans_meta_t
  trans_meta[, is_viable := is_viable & id_trans %in% modeled]
  db$trans_meta_t <- trans_meta
  db$alloc_params_t <- db$create_alloc_params_t()
  viable <- db$trans_meta_t[
    is_viable == TRUE,
    .(id_trans, id_lulc_anterior, id_lulc_posterior)
  ]

  # demand: observed quantities 2 -> 3 of the viable transitions
  maps <- lulc_long[id_period == 2L, .(id_coord, anterior = id_lulc)][
    lulc_long[id_period == 3L, .(id_coord, observed = id_lulc)],
    on = "id_coord"
  ]
  counts <- maps[
    anterior != observed,
    .(count = .N),
    by = .(id_lulc_anterior = anterior, id_lulc_posterior = observed)
  ]
  n_anterior <- maps[, .(n_anterior = .N), by = .(id_lulc_anterior = anterior)]
  rates_3 <- viable[
    counts,
    on = .(id_lulc_anterior, id_lulc_posterior),
    nomatch = NULL
  ][n_anterior, on = "id_lulc_anterior", nomatch = NULL][,
    .(id_run = 0L, id_period = 3L, id_trans, count, rate = count / n_anterior)
  ]
  db$trans_rates_t <- as_trans_rates_t(rates_3)

  # true probabilities for 2 -> 3, given the observed period-2 map
  truth_all <- true_probs(lulc_2, drivers)[cell_of, on = "cell", nomatch = NULL]
  truth_viable <- truth_all[
    viable,
    on = .(id_lulc_anterior, class = id_lulc_posterior),
    nomatch = NULL
  ]

  # runs: one parent per (potentials x allocator) group, members below it
  groups <- CJ(
    potentials = c("estimated", "oracle"),
    allocator = c("uPAM (estimated patches)", "uSAM (single cells)")
  )
  groups[, id_parent := 10L * .I]
  members <- groups[,
    .(id_run = id_parent * 1000L + seq_len(n_members)),
    by = .(id_parent, potentials, allocator)
  ]
  id_run_observed <- 99L
  runs <- rbind(
    data.table(
      id_run = c(0L, id_run_observed),
      parent_id_run = c(NA_integer_, 0L),
      description = c("calibration base", "observed period 3")
    ),
    groups[, .(
      id_run = id_parent,
      parent_id_run = 0L,
      description = paste(potentials, allocator, sep = " x ")
    )],
    members[, .(
      id_run,
      parent_id_run = id_parent,
      description = paste("member of", id_parent)
    )]
  )
  db$commit(as_runs_t(runs), "runs_t", method = "overwrite")
  db$lulc_data_t <- as_lulc_data_t(lulc_long[
    id_period == 3L,
    .(id_run = id_run_observed, id_coord, id_period, id_lulc)
  ])

  # potentials per group parent: estimated ones predicted once, oracle ones injected
  db$id_run <- groups[1, id_parent]
  db$predict_trans_pot(
    id_period_post = 3L,
    select_score = "no.crossval",
    select_maximize = TRUE
  )
  estimated_raw <- db$trans_pot_t[id_period_post == 3L]
  oracle_raw <- truth_viable[, .(
    id_trans,
    id_period_post = 3L,
    id_coord,
    value = q
  )]
  params_base <- db$alloc_params_t
  for (g in seq_len(nrow(groups))) {
    id_g <- groups[g, id_parent]
    raw <- if (groups[g, potentials] == "estimated") {
      estimated_raw
    } else {
      oracle_raw
    }
    if (id_g != groups[1, id_parent]) {
      db$trans_pot_t <- as_trans_pot_t(raw[, .(
        id_run = id_g,
        id_trans,
        id_period_post,
        id_coord,
        value
      )])
    }
    if (startsWith(groups[g, allocator], "uSAM")) {
      db$alloc_params_t <- as_alloc_params_t(copy(params_base)[, `:=`(
        id_run = id_g,
        mean_patch_size = 1,
        patch_size_variance = 0
      )][])
    }
  }

  # adjusted (allocation-ready) potentials per potential source
  adjusted_of <- function(id_g) {
    db$id_run <- id_g
    db$adjusted_trans_pot_v(3L)[
      viable,
      on = "id_trans",
      nomatch = NULL
    ][, .(id_coord, class = id_lulc_posterior, p = value)]
  }
  adjusted <- list(
    estimated = adjusted_of(groups[potentials == "estimated", id_parent][1]),
    oracle = adjusted_of(groups[potentials == "oracle", id_parent][1])
  )

  # allocate the ensembles
  for (r in seq_len(nrow(members))) {
    db$id_run <- members[r, id_run]
    set.seed(seed + members[r, id_run])
    db$alloc_clumpy(
      id_periods = 3,
      select_score = "no.crossval",
      select_maximize = TRUE,
      use_parent_trans_pot = TRUE,
      update_neighbors = FALSE
    )
  }
  db$id_run <- NULL
  simulated <- db$fetch(
    "lulc_data_t",
    where = glue::glue("id_run > 1000 and id_period = 3")
  )[, .(id_run, id_coord, simulated = id_lulc)][
    members[, .(id_run, potentials, allocator)],
    on = "id_run"
  ]

  # scoring
  classes <- sort(unique(c(maps$anterior, maps$observed)))
  eligible <- maps[anterior %in% viable$id_lulc_anterior, id_coord]
  bt <- brier_tables(
    maps,
    eligible,
    classes,
    with_persistence(
      truth_all[, .(id_coord, class, p = q)],
      maps[id_coord %in% eligible, .(id_coord, anterior)]
    )[, .(id_coord, class, q = p)]
  )
  persist <- function(trans_p) with_persistence(trans_p, bt$anterior_of)
  raw_long <- function(raw) {
    raw[viable, on = "id_trans", nomatch = NULL][,
      .(id_coord, class = id_lulc_posterior, p = value)
    ]
  }

  potential_rows <- rbindlist(
    list(
      cbind(
        potentials = "estimated",
        stage = "raw potentials",
        score(persist(raw_long(estimated_raw)), bt)
      ),
      cbind(
        potentials = "estimated",
        stage = "adjusted potentials",
        score(persist(adjusted$estimated), bt)
      ),
      cbind(
        potentials = "oracle",
        stage = "raw potentials",
        score(persist(raw_long(oracle_raw)), bt)
      ),
      cbind(
        potentials = "oracle",
        stage = "adjusted potentials",
        score(persist(adjusted$oracle), bt)
      ),
      cbind(
        potentials = "oracle, all four transitions",
        stage = "truth",
        score(persist(truth_all[, .(id_coord, class, p = q)]), bt)
      ),
      cbind(
        potentials = "none",
        stage = "uniform forecast",
        score(
          persist(
            bt$anterior_of[
              viable[rates_3[, .(id_trans, rate)], on = "id_trans"],
              on = c(anterior = "id_lulc_anterior"),
              allow.cartesian = TRUE,
              nomatch = NULL
            ][, .(id_coord, class = id_lulc_posterior, p = rate)]
          ),
          bt
        )
      ),
      cbind(
        potentials = "none",
        stage = "persistence",
        score(bt$anterior_of[, .(id_coord, class = anterior, p = 1)], bt)
      )
    ),
    use.names = TRUE
  )

  ensemble_rows <- simulated[,
    {
      freq <- .SD[, .(p = .N / n_members), by = .(id_coord, class = simulated)]
      cbind(stage = paste("ensemble,", allocator), score(freq, bt, n_members))
    },
    by = .(potentials, allocator)
  ][, !"allocator"]

  fom_rows <- simulated[
    maps,
    on = "id_coord"
  ][,
    .(fom = fom_counts(anterior, observed, simulated)),
    by = .(potentials, allocator, id_run)
  ][,
    .(
      fom_median = median(fom),
      fom_q05 = quantile(fom, 0.05),
      fom_q95 = quantile(fom, 0.95)
    ),
    by = .(potentials, stage = paste("ensemble,", allocator))
  ]

  scores <- rbind(potential_rows, ensemble_rows, use.names = TRUE)
  ref <- scores[stage == "uniform forecast"]
  scores[, `:=`(
    n_grid = n_grid,
    skill_realised = 1 - brier_realised / ref$brier_realised,
    skill_expected = 1 - brier_expected / ref$brier_expected
  )]
  scores <- fom_rows[scores, on = .(potentials, stage)]

  context <- data.table(
    n_grid = n_grid,
    eligible_cells = length(eligible),
    observed_change = maps[id_coord %in% eligible & anterior != observed, .N],
    unmodelled_change = maps[id_coord %in% eligible & anterior != observed][
      !viable,
      on = c(anterior = "id_lulc_anterior", observed = "id_lulc_posterior")
    ][, .N],
    viable_transitions = paste(
      viable[, paste(id_lulc_anterior, id_lulc_posterior, sep = "->")],
      collapse = ", "
    ),
    mean_patch_size = paste(
      round(params_base$mean_patch_size, 2),
      collapse = ", "
    )
  )
  list(scores = scores, context = context)
}

results <- lapply(grid_sizes, run_experiment)
scores <- rbindlist(lapply(results, `[[`, "scores"), use.names = TRUE)
context <- rbindlist(lapply(results, `[[`, "context"))
fwrite(scores, file.path(out_dir, "skill-attribution.csv"))

#' # Results
#'
#' Context: how much change there is to score, and how much of it the
#' calibrated model cannot produce.

#| label: context
knitr::kable(context)

#' Scores per domain size, over the cells that can change. Brier scores are
#' multiclass (0 to 2, lower is better); ensemble scores carry the fair
#' correction for a finite ensemble. `distance_to_truth` is what a forecast
#' could still improve; `skill_*` is relative to the uniform forecast.

#| label: results
knitr::kable(
  scores[
    order(n_grid, potentials, stage),
    .(
      n_grid,
      potentials,
      stage,
      distance_to_truth,
      brier_expected,
      skill_expected,
      brier_realised,
      skill_realised,
      fom_median,
      fom_q05,
      fom_q95
    )
  ],
  digits = 4
)

#' Decomposition: estimation loss is the distance the estimated potentials add
#' over the oracle ones; allocation loss is the distance an ensemble adds over
#' the adjusted potentials it samples from.

#| label: decomposition
adjusted_d <- scores[
  stage == "adjusted potentials",
  .(n_grid, potentials, d_potentials = distance_to_truth)
]
decomposition <- rbind(
  scores[
    stage == "truth",
    .(n_grid, component = "attainable skill of the truth", value = skill_expected)
  ],
  dcast(adjusted_d, n_grid ~ potentials, value.var = "d_potentials")[,
    .(n_grid, component = "estimation loss", value = estimated - oracle)
  ],
  scores[startsWith(stage, "ensemble")][
    adjusted_d,
    on = .(n_grid, potentials)
  ][, .(
    n_grid,
    component = paste("allocation loss:", potentials, sub("ensemble, ", "", stage)),
    value = distance_to_truth - d_potentials
  )]
)
knitr::kable(decomposition[order(n_grid, component)], digits = 4)
