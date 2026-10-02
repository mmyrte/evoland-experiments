#' ---
#' title: "Figure 2 mock-up: ensembles vs. single maps"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' A backcast on a synthetic 90 × 90 landscape; the map panels show a 30 × 30
#' window of it. Domain and classes follow the
#' evoland-plus vignette `stochastic-allocation-sensitivity.qmd`; unlike the
#' vignette, the initial map is structured (urban clustered where accessible,
#' forest on better sites) and change between periods follows a known process
#' driven by two latent drivers and the neighbourhood, so that there is
#' something to learn. Calibrate on the transition from period 1 to 2, allocate
#' period 3 from the observed period 2 `n_realisations` times with CLUMPY, and
#' validate every realisation against the observed period 3, which calibration
#' never sees.
#'
#' Keeping the backcast honest:
#'
#' - Periods 1 and 2 are observed, period 3 is flagged as extrapolated. Model fitting and
#' allocation-parameter estimation therefore only see the transition 1 -> 2.
#' - The observed period 3 sits in a run of its own (`id_run_observed`), a child of the base run.
#' The ensemble runs are siblings of it and cannot read it; the figure-of-merit
#' view reads it as the reference.
#' - The demand for period 3 is the observed quantity of each transition from 2 to 3, as in the
#' comparison protocol: the figure is about *where* change is placed, not how
#' much.
#'
#' Why 90 × 90 and this learner: `010-skill-attribution.r` showed that the
#' allocator passes the potentials through unchanged, so a weak figure was weak
#' estimation. `010-learner-comparison.r` found that ranger with default leaves
#' on 30 × 30 reaches about a third of the skill the true probabilities attain.
#' Ranger with larger leaves (`min.node.size = 50`) on all available predictors
#' at 90 × 90 reaches about 83 %. Logistic regression does better still, but the
#' synthetic process is logistic, which would give it home advantage.
#'

#| label: setup
#| output: false
set.seed(1337)
library(evoland)
library(data.table)
library(terra)
library(ggplot2)
library(patchwork)
library(mlr3learners)

out_dir <- "2026-09-paper-figures/figures"
db_path <- "2026-09-paper-figures/fig2-ensembles.evolanddb"
unlink(db_path, recursive = TRUE)

n_realisations <- 100L
n_grid <- 90L # domain, cells per side
n_window <- 30L # map panels, cells per side
id_run_observed <- 1000L

#' # Database, domain and synthetic landscape

#| label: create-db
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
  resolution = terra::res(template_rast)[1]
)

# periods 1, 2 observed; period 3 held out as "extrapolated"
db$periods_t <- create_periods_t(
  period_length_str = "P10Y",
  start_observed = "2000-01-01",
  end_observed = "2010-01-01",
  end_extrapolated = "2020-01-01"
)
db$periods_t

#' ## Synthetic process
#'
#' Two latent drivers, a nuisance field, and four transitions whose per-period
#' probabilities depend on the drivers and the 5 × 5 neighbourhood; defined in
#' `000-synthetic-process.r`, which the skill-attribution and learner-comparison steps share.
#'
#' | transition | logit of the per-period probability |
#' |---|---|
#' | arable -> urban | −5.5 + 7 * share_urban + 3 * accessibility |
#' | forest -> urban | −7 + 6 * share_urban + 3 * accessibility |
#' | forest -> arable | −5 + 4 * (1 − site_quality) + 3 * share_arable |
#' | arable -> forest | −6 + 4 * site_quality * (1 − accessibility) + 3 * share_forest |

#| label: synthesize-lulc
#| fig-asp: 0.3
options(synthetic_process.source_only = TRUE)
source("2026-09-paper-figures/000-synthetic-process.r")

drivers <- make_drivers(template_rast)
accessibility <- drivers$accessibility
site_quality <- drivers$site_quality
random_nuisance <- drivers$random_nuisance

initial <- make_landscape(template_rast, drivers)
lulc_2 <- step_process(initial, drivers)
lulc_3 <- step_process(lulc_2, drivers)
synthetic_lulc <- c(initial, lulc_2, lulc_3) |>
  setNames(paste0("id_period=", 1:3))

plot(
  synthetic_lulc,
  nc = 3,
  col = data.frame(
    value = 1:4,
    color = c("#91B690", "#EB9486", "#F3DE8A", "#CBC6D2")
  )
)

lulc_long <- extract_using_coords_t(synthetic_lulc, db$coords_t)[,
  .(
    id_coord,
    id_period = as.integer(sub(".*[^0-9]", "", layer)),
    id_lulc = value
  )
]
lulc_long[, .N, by = .(id_period, id_lulc)][order(id_period, id_lulc)]

db$lulc_data_t <- as_lulc_data_t(lulc_long[
  id_period <= 2L,
  .(id_run = 0L, id_coord, id_period, id_lulc)
])

#' # Predictors

#' The model sees the two drivers, a nuisance field, and evoland's neighbourhood
#' predictors (computed from the land use map), but not the functional form
#' above.

#| label: predictors
db$pred_meta_t <- create_pred_meta_t(list(
  accessibility = list(description = "synthetic driver", data_type = "float"),
  site_quality = list(description = "synthetic driver", data_type = "float"),
  random_nuisance = list(
    description = "synthetic nuisance",
    data_type = "float"
  )
))

db$pred_data_t <- extract_using_coords_t(
  c(accessibility, site_quality, random_nuisance) |>
    setNames(c("accessibility", "site_quality", "random_nuisance")),
  db$coords_minimal
)[, layer := as.character(layer)][,
  .(
    id_coord,
    id_run = 0L,
    id_period = 0L,
    id_pred = fcase(
      layer == "accessibility"   ,
      1L                         ,
      layer == "site_quality"    ,
      2L                         ,
      layer == "random_nuisance" ,
      3L
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

#' # Calibration on 1 -> 2

#| label: calibrate
db$trans_meta_t <- create_trans_meta_t(
  db$trans_v,
  min_cardinality_abs = 10,
  exclude_anterior = 4
)
# all available predictors: in 010-learner-comparison, preselecting rpart's top 4 cost ranger skill
db$set_full_trans_preds()

# larger leaves give better-calibrated probabilities (010-learner-comparison)
trans_models <- db$fit_full_models(
  learner = mlr3::lrn("classif.ranger", num.trees = 500, min.node.size = 50)
)
modeled_trans <- unique(trans_models$id_trans[
  !vapply(trans_models$learner_full, is.null, logical(1L))
])
db$trans_models_t <- trans_models
trans_meta <- db$trans_meta_t
trans_meta[, is_viable := is_viable & id_trans %in% modeled_trans]
db$trans_meta_t <- trans_meta

alloc_params_estimated <- db$create_alloc_params_t()
alloc_params_estimated
# The synthetic process changes cells independently, so single-cell allocation (uSAM) is the
# allocator that matches it. The estimated "patches" (mean 1.1-1.2 cells) are clusters induced
# by the neighbourhood terms of the process. Allocating with them (uPAM) moves change onto
# less probable neighbours: the logistic-regression ensemble then puts 28 % of its change in
# the top 5 % of true probabilities instead of the 36 % a sampler of the truth puts there.
db$alloc_params_t <- as_alloc_params_t(
  copy(alloc_params_estimated)[, `:=`(mean_patch_size = 1, patch_size_variance = 0)][]
)
db$trans_meta_t[is_viable == TRUE]

#' # Demand: observed quantities 2 -> 3

#| label: demand
observed_2_3 <- lulc_long[
  id_period == 2L,
  .(id_coord, id_lulc_anterior = id_lulc)
][
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

# change the model cannot produce: transitions not viable after 1 -> 2
unmodelled_change <- observed_counts[, sum(count)] - rates_3[, sum(count)]
rates_3
unmodelled_change

#' # Runs: the held-out observation and one ensemble per learner
#'
#' Two estimators produce potentials for the same allocator. The random forest
#' (calibrated above, run 0) is the general-purpose choice; the logistic
#' regression is correctly specified for this synthetic process, whose
#' transition probabilities are logistic in the drivers and neighbourhood
#' shares. It therefore shows what the allocation does when the potentials are
#' close to the truth. Each learner's ensemble sits below its own parent run, so
#' its members inherit that learner's potentials and nothing else.

#| label: runs
learners <- data.table(
  learner = c("ranger", "log_reg"),
  label = c("random forest", "logistic regression"),
  id_run_parent = c(0L, 2000L)
)
members <- learners[,
  .(id_run = id_run_parent + seq_len(n_realisations), member = seq_len(n_realisations)),
  by = .(learner, label, id_run_parent)
][, seed := 1000L + .I]

runs <- rbind(
  data.table(
    id_run = c(0L, id_run_observed, 2000L),
    parent_id_run = c(NA_integer_, 0L, 0L),
    description = c(
      "calibration base, random forest potentials",
      "observed period 3 (validation only)",
      "logistic regression potentials"
    ),
    seed = NA_integer_
  ),
  members[, .(
    id_run,
    parent_id_run = id_run_parent,
    description = paste(label, "realisation", member),
    seed
  )]
)
db$commit(as_runs_t(runs), "runs_t", method = "overwrite")

db$lulc_data_t <- as_lulc_data_t(lulc_long[
  id_period == 3L,
  .(id_run = id_run_observed, id_coord, id_period, id_lulc)
])

#' The logistic regression is fitted on the same predictors, under its own run.
#' Its potentials are predicted with `force = TRUE`: otherwise the lineage read
#' would find run 0's potentials and reuse them.

#| label: fit-log-reg
db$id_run <- 2000L
log_reg_models <- db$fit_full_models(
  learner = mlr3::lrn("classif.log_reg"),
  trans_preds = as_trans_preds_t(db$trans_preds_t[, .(id_run = 2000L, id_pred, id_trans)])
)
db$trans_models_t <- log_reg_models
db$predict_trans_pot(
  id_period_post = 3L,
  select_score = "no.crossval",
  select_maximize = TRUE,
  force = TRUE
)

#| label: allocate
#| output: false
for (i in seq_len(nrow(members))) {
  db$id_run <- members$id_run[i]
  set.seed(members$seed[i])
  db$alloc_clumpy(
    id_periods = 3,
    select_score = "no.crossval",
    select_maximize = TRUE,
    use_parent_trans_pot = TRUE,
    update_neighbors = FALSE
  )
}

#' # A deterministic comparator on the same potentials
#'
#' Tools without stochastic allocation return one map. As a stand-in until the
#' real comparators run (`2026-09-model-comparison/`), allocate the same demand
#' deterministically on each learner's adjusted potentials: greedily, highest
#' potential first, one class per cell. This isolates the allocator's
#' contribution; it says nothing about how Dinamica or LCM would score.

#| label: deterministic
adjusted_by <- lapply(setNames(learners$id_run_parent, learners$learner), function(id) {
  db$id_run <- id
  db$adjusted_trans_pot_v(3L)
})

greedy_map <- function(adjusted) {
  candidates <- adjusted[, .(id_coord, id_trans, potential = value)][
    db$trans_meta_t[, .(id_trans, id_lulc_posterior)],
    on = "id_trans",
    nomatch = NULL
  ][rates_3[, .(id_trans, quota = count)], on = "id_trans", nomatch = NULL][
    order(-potential)
  ]
  remaining <- setNames(
    candidates[, quota[1], by = id_trans]$V1,
    candidates[, unique(id_trans)]
  )
  assigned <- new.env()
  keep <- logical(nrow(candidates))
  for (i in seq_len(nrow(candidates))) {
    key <- as.character(candidates$id_coord[i])
    tr <- as.character(candidates$id_trans[i])
    if (remaining[[tr]] > 0 && is.null(assigned[[key]])) {
      assigned[[key]] <- TRUE
      remaining[[tr]] <- remaining[[tr]] - 1L
      keep[i] <- TRUE
    }
  }
  out <- lulc_long[id_period == 2L, .(id_coord, deterministic = id_lulc)]
  out[
    candidates[keep, .(id_coord, id_lulc_posterior)],
    on = "id_coord",
    deterministic := i.id_lulc_posterior
  ]
  out[]
}
deterministic_by <- lapply(adjusted_by, greedy_map)

#' # Validation

#| label: fom
db$id_run <- NULL
fom <- db$figure_of_merit_v(
  id_period_anterior = 2L,
  id_period_post = 3L,
  id_run_reference = id_run_observed,
  id_run_simulated = members$id_run
)[members[, .(id_run, learner, label)], on = "id_run"]

# expected counts of each ensemble: ratio of mean counts, not mean of ratios
fom_ensemble <- fom[,
  lapply(.SD, mean),
  by = .(learner, label),
  .SDcols = c("hits", "wrong_hits", "misses", "false_alarms")
][, figure_of_merit := hits / (hits + wrong_hits + misses + false_alarms)]

fom_counts <- function(anterior, observed, simulated) {
  obs_change <- anterior != observed
  sim_change <- anterior != simulated
  hits <- sum(obs_change & sim_change & simulated == observed)
  wrong <- sum(obs_change & sim_change & simulated != observed)
  misses <- sum(obs_change & !sim_change)
  false_alarms <- sum(!obs_change & sim_change)
  hits / (hits + wrong + misses + false_alarms)
}
maps <- lulc_long[id_period == 2L, .(id_coord, anterior = id_lulc)][
  lulc_long[id_period == 3L, .(id_coord, observed = id_lulc)],
  on = "id_coord"
]
fom_deterministic <- learners[, .(
  learner,
  label,
  figure_of_merit = vapply(
    learner,
    function(lr) {
      maps[deterministic_by[[lr]], on = "id_coord"][,
        fom_counts(anterior, observed, deterministic)
      ]
    },
    numeric(1L)
  )
)]
fom_null <- mean(fom$figure_of_merit_null)

simulated_all <- db$fetch(
  "lulc_data_t",
  where = glue::glue(
    "id_run in ({paste(members$id_run, collapse = ', ')}) and id_period = 3"
  )
)[, .(id_run, id_coord, simulated = id_lulc)][members[, .(id_run, learner)], on = "id_run"]

knitr::kable(
  fom[,
    .(
      fom_median = median(figure_of_merit),
      fom_q05 = quantile(figure_of_merit, 0.05),
      fom_q95 = quantile(figure_of_merit, 0.95)
    ),
    by = .(learner)
  ][fom_ensemble[, .(learner, fom_ensemble = figure_of_merit)], on = "learner"][
    fom_deterministic[, .(learner, fom_deterministic = figure_of_merit)],
    on = "learner"
  ][, fom_random_null := fom_null][],
  digits = 3
)

#' # Probabilistic scores: which class, not just whether
#'
#' The multiclass Brier score (Brier, 1950) scores the full outcome: for each
#' cell, the sum over posterior classes k of (p_k - o_k)^2, where o is the
#' observed class as a one-hot vector. It ranges from 0 to 2. A hard map scores
#' 0 on a correct cell and 2 on any miss, false alarm or wrong hit. Scores are
#' averaged over the cells that can change.
#'
#' Forecasts compared, per learner: the ensemble frequency (with the fair
#' correction for a finite ensemble, Ferro 2014), the adjusted potentials the
#' allocator samples from, the deterministic map and single realisations.
#' References: the true probabilities, climatology (every cell of an anterior
#' class gets that class's demand rates; the reference for the skill score
#' BSS = 1 - BS / BS_climatology) and persistence.

#| label: truth
cell_of <- data.table(
  id_coord = db$coords_minimal$id_coord,
  cell = terra::cellFromXY(template_rast, as.matrix(db$coords_minimal[, .(lon, lat)]))
)
truth <- true_probs(lulc_2, drivers)[cell_of, on = "cell", nomatch = NULL]

#| label: brier-multiclass
viable <- db$trans_meta_t[
  is_viable == TRUE,
  .(id_trans, id_lulc_anterior, id_lulc_posterior)
]
classes <- sort(unique(c(maps$anterior, maps$observed)))
eligible <- maps[anterior %in% viable$id_lulc_anterior, id_coord]
anterior_of <- maps[id_coord %in% eligible, .(id_coord, anterior)]

# a forecast is a long table (id_coord, class, p); classes it omits get p = 0
complete_p <- function(f) {
  f <- f[id_coord %in% eligible, .(p = sum(p)), by = .(id_coord, class)]
  f[CJ(id_coord = eligible, class = classes), on = .(id_coord, class)][
    is.na(p),
    p := 0
  ][]
}
# transition probabilities per cell, with persistence as the remainder
persist <- function(trans_p) {
  stay <- trans_p[, .(leave = sum(p)), by = id_coord][
    anterior_of,
    on = "id_coord"
  ][is.na(leave), leave := 0][, .(id_coord, class = anterior, p = 1 - leave)]
  rbind(trans_p[, .(id_coord, class, p)], stay)
}
observed_p <- complete_p(maps[, .(id_coord, class = observed, p = 1)])[,
  .(id_coord, class, o = p)
]
brier <- function(f, n_members = NA_integer_) {
  d <- complete_p(f)[observed_p, on = .(id_coord, class)]
  cell <- d[,
    .(
      multi = sum((p - o)^2),
      noise = if (is.na(n_members)) 0 else sum(p * (1 - p)) / (n_members - 1)
    ),
    by = id_coord
  ]
  cell[, mean(multi - noise)]
}
potentials_long <- function(adjusted) {
  adjusted[viable, on = "id_trans", nomatch = NULL][,
    .(id_coord, class = id_lulc_posterior, p = value)
  ]
}

scores <- rbindlist(c(
  lapply(learners$learner, function(lr) {
    sims <- simulated_all[learner == lr]
    draws <- sims[, .(bs = brier(.SD[, .(id_coord, class = simulated, p = 1)])), by = id_run]
    data.table(
      learner = lr,
      forecast = c(
        "ensemble frequency (fair)",
        "adjusted potentials",
        "deterministic",
        "single realisation (median)"
      ),
      brier = c(
        brier(
          sims[, .(p = .N / n_realisations), by = .(id_coord, class = simulated)],
          n_realisations
        ),
        brier(persist(potentials_long(adjusted_by[[lr]]))),
        brier(deterministic_by[[lr]][, .(id_coord, class = deterministic, p = 1)]),
        median(draws$bs)
      )
    )
  }),
  list(data.table(
    learner = "reference",
    forecast = c("true probabilities", "climatology", "persistence"),
    brier = c(
      brier(persist(truth[, .(id_coord, class, p = q)])),
      brier(persist(
        anterior_of[
          viable[rates_3[, .(id_trans, rate)], on = "id_trans", nomatch = NULL],
          on = c(anterior = "id_lulc_anterior"),
          allow.cartesian = TRUE,
          nomatch = NULL
        ][, .(id_coord, class = id_lulc_posterior, p = rate)]
      )),
      brier(maps[, .(id_coord, class = anterior, p = 1)])
    )
  ))
))
scores[, skill := 1 - brier / scores[forecast == "climatology", brier]]
knitr::kable(scores, digits = 4)

#' # Where change is placed: the bias of a deterministic map
#'
#' The deterministic map wins on FoM by putting all change on the highest
#' potentials. Observed change does not behave like that: it also happens on
#' cells of moderate probability, in proportion to that probability. For every
#' changed cell, take the percentile of its *true* transition probability among
#' all cells that could make the same transition. The truth is the same for
#' both learners, and percentiles let transitions with different probability
#' scales be pooled. Then compare the distributions of those percentiles for
#' the observed change, the realisations and the greedy maps. Random allocation
#' would give a uniform distribution (the diagonal).
#'
#' An unbiased sampler of the true probabilities reproduces the observed curve
#' in expectation. The logistic-regression ensemble comes close to that because
#' its potentials are nearly the truth here (the model is correctly specified);
#' the random forest's realisations deviate by as much as its potentials do.

#| label: bias
truth_rank <- truth[,
  .(id_coord, rank = frank(q, ties.method = "average") / .N),
  by = .(id_lulc_anterior, class)
]
rank_of_change <- function(changes) {
  truth_rank[
    changes,
    on = .(id_coord, id_lulc_anterior = anterior, class = posterior),
    nomatch = NULL
  ]
}
rank_grid <- seq(0, 1, by = 0.005)
ecdf_on_grid <- function(r) stats::ecdf(r)(rank_grid)

ranks_observed <- rank_of_change(
  maps[anterior != observed, .(id_coord, anterior, posterior = observed)]
)
ranks_deterministic <- rbindlist(
  lapply(learners$learner, function(lr) {
    maps[deterministic_by[[lr]], on = "id_coord"][
      anterior != deterministic,
      .(id_coord, anterior, posterior = deterministic)
    ] |>
      rank_of_change() |>
      cbind(learner = lr)
  })
)
ranks_draws <- rank_of_change(
  simulated_all[maps[, .(id_coord, anterior)], on = "id_coord"][
    anterior != simulated,
    .(id_run, learner, id_coord, anterior, posterior = simulated)
  ]
)

cdf_observed <- ecdf_on_grid(ranks_observed$rank)
cdf_draws <- ranks_draws[,
  .(x = rank_grid, cdf = ecdf_on_grid(rank)),
  by = .(learner, id_run)
]
cdf_band <- cdf_draws[,
  .(lo = quantile(cdf, 0.05), med = median(cdf), hi = quantile(cdf, 0.95)),
  by = .(learner, x)
]
cdf_deterministic <- ranks_deterministic[,
  .(x = rank_grid, cdf = ecdf_on_grid(rank)),
  by = learner
]

bias_tab <- rbind(
  data.table(
    changed_cells = "observed",
    learner = NA_character_,
    share_in_top_5_pct = mean(ranks_observed$rank > 0.95),
    median_percentile = median(ranks_observed$rank)
  ),
  ranks_draws[,
    .(share = mean(rank > 0.95), med = median(rank)),
    by = .(learner, id_run)
  ][,
    .(
      changed_cells = "realisations (median)",
      share_in_top_5_pct = median(share),
      median_percentile = median(med)
    ),
    by = learner
  ],
  ranks_deterministic[,
    .(
      changed_cells = "deterministic",
      share_in_top_5_pct = mean(rank > 0.95),
      median_percentile = median(rank)
    ),
    by = learner
  ],
  use.names = TRUE
)
knitr::kable(bias_tab, digits = 3)

# Kolmogorov-Smirnov distance of each curve to the observed one. Not shown in the figure: the
# distance is not widely known, and the panel shows the bias plainly. Uncomment to report it.
# ks_draws <- cdf_draws[, .(ks = max(abs(cdf - cdf_observed))), by = .(learner, id_run)]
# ks_draws[, .(ks_median = median(ks)), by = learner]
# cdf_deterministic[, .(ks = max(abs(cdf - cdf_observed))), by = learner]

#' # Figure
#'
#' Six cells of a 3 x 2 grid: three maps on top, the FoM strip across two
#' cells and the bias panel in the last one below. Map panels show one
#' n_window x n_window block of the domain, the block with the most observed
#' change; every score, panel (d) and panel (e) use the whole domain. Each panel
#' keeps its own legend.

#| label: figure-data
# a plain data.table: a coords_t subset would carry the class and its print method
coords_xy <- data.table(
  id_coord = db$coords_t$id_coord,
  x = db$coords_t$lon,
  y = db$coords_t$lat
)
coords_xy[,
  block := paste(
    (x - min(x)) %/% (100 * n_window),
    (y - min(y)) %/% (100 * n_window)
  )
]
shown_block <- coords_xy[
  maps[anterior != observed, .(id_coord)],
  on = "id_coord"
][, .N, by = block][order(-N)][1, block]
coords_xy <- coords_xy[block == shown_block, .(id_coord, x, y)]

outcome <- function(anterior, observed, simulated) {
  obs_change <- anterior != observed
  sim_change <- anterior != simulated
  fcase(
    obs_change & sim_change & simulated == observed , "hit"         ,
    obs_change & sim_change                         , "wrong hit"   ,
    obs_change                                      , "miss"        ,
    sim_change                                      , "false alarm" ,
    default = "persistence"
  )
}

# the realisation shown: the random forest draw closest to its ensemble median
fom_ranger <- fom[learner == "ranger"]
shown_run <- fom_ranger[order(abs(figure_of_merit - median(figure_of_merit)))][1, id_run]

lulc_names <- c("1" = "forest", "2" = "arable", "3" = "urban", "4" = "lake (fixed)")
panel_lulc <- maps[, .(id_coord, status = lulc_names[as.character(observed)])]
panel_draw <- maps[
  simulated_all[id_run == shown_run, .(id_coord, simulated)],
  on = "id_coord"
][, .(id_coord, status = outcome(anterior, observed, simulated))]
panel_freq <- simulated_all[learner == "ranger"][
  maps[, .(id_coord, anterior)],
  on = "id_coord"
][, .(p_change = mean(simulated != anterior)), by = id_coord]
observed_change_cells <- maps[anterior != observed, .(id_coord)]

#| label: figure
#| fig-width: 7.2
#| fig-height: 6.6
col_surface <- "#f1f0ed"
# Okabe-Ito: distinguishable under the common colour-vision deficiencies
lulc_cols <- c(
  "forest" = "#009E73",
  "arable" = "#F0E442",
  "urban" = "#D55E00",
  "lake (fixed)" = "#56B4E9"
)
outcome_cols <- c(
  "persistence" = col_surface,
  "hit" = "#2a78d6",
  "miss" = "#eb6834",
  "false alarm" = "#1baf7a",
  "wrong hit" = "#3d3d3d"
)
seq_blue <- c("#fcfcfb", "#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b")
learner_cols <- c("random forest" = "#2a78d6", "logistic regression" = "#7b3fbf")

theme_panel_text <- theme(
  plot.title = element_text(size = 9, face = "bold", hjust = 0, margin = margin(b = 3)),
  plot.subtitle = element_text(size = 7.5, colour = "#555555", margin = margin(b = 4)),
  legend.position = "bottom",
  legend.title = element_blank(),
  legend.key.size = unit(7, "pt"),
  legend.text = element_text(size = 7)
)
theme_map <- theme_void(base_size = 9) +
  theme_panel_text +
  theme(plot.margin = margin(2, 6, 2, 6))
outline <- function() {
  geom_tile(
    data = coords_xy[observed_change_cells, on = "id_coord", nomatch = NULL],
    aes(x, y),
    fill = NA,
    colour = "#1a1a1a",
    linewidth = 0.45,
    width = 70,
    height = 70,
    inherit.aes = FALSE
  )
}

p_a <- ggplot(coords_xy[panel_lulc, on = "id_coord", nomatch = NULL], aes(x, y, fill = status)) +
  geom_tile(colour = "white", linewidth = 0.15) +
  outline() +
  scale_fill_manual(values = lulc_cols, breaks = names(lulc_cols)) +
  coord_equal(expand = FALSE) +
  labs(
    title = "(a) Observed land use, period 3",
    subtitle = sprintf(
      "outline: change since period 2\n%d \u00d7 %d window of the domain",
      n_window,
      n_window
    )
  ) +
  theme_map +
  guides(fill = guide_legend(nrow = 2))

p_b <- ggplot(coords_xy[panel_draw, on = "id_coord", nomatch = NULL], aes(x, y, fill = status)) +
  geom_tile(colour = "white", linewidth = 0.15) +
  scale_fill_manual(
    values = outcome_cols,
    breaks = intersect(names(outcome_cols)[-1], unique(panel_draw$status))
  ) +
  coord_equal(expand = FALSE) +
  labs(
    title = "(b) One realisation",
    subtitle = sprintf(
      "random forest, median draw\nFoM %.2f (whole domain)",
      fom[id_run == shown_run, figure_of_merit]
    )
  ) +
  theme_map +
  guides(fill = guide_legend(nrow = 2))

p_c <- ggplot(coords_xy[panel_freq, on = "id_coord", nomatch = NULL], aes(x, y)) +
  geom_tile(aes(fill = p_change), colour = "white", linewidth = 0.15) +
  outline() +
  scale_fill_gradientn(
    colours = seq_blue,
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    name = "share of realisations",
    guide = guide_colourbar(title.position = "top", title.hjust = 0.5)
  ) +
  coord_equal(expand = FALSE) +
  labs(
    title = "(c) Change frequency",
    subtitle = "random forest ensemble\noutline: observed change"
  ) +
  theme_map +
  theme(
    legend.title = element_text(size = 7),
    legend.key.width = unit(20, "pt"),
    legend.key.height = unit(5, "pt")
  )

# (d): one row per learner, realisations jittered within the row
row_y <- setNames(c(1, 0), learners$label)
fom[, y := row_y[label]]
fom_ensemble[, y := row_y[label]]
fom_deterministic[, y := row_y[label]]
set.seed(7)
p_d <- ggplot(fom, aes(x = figure_of_merit, y = y)) +
  geom_vline(xintercept = 0, colour = "#9a9a9a", linewidth = 0.4) +
  geom_vline(xintercept = fom_null, colour = "#9a9a9a", linewidth = 0.4, linetype = "22") +
  annotate(
    "text",
    x = 0,
    y = 1.55,
    label = " persistence",
    hjust = 0,
    size = 2.4,
    colour = "#555555"
  ) +
  annotate(
    "text",
    x = fom_null,
    y = 1.4,
    label = " random allocation",
    hjust = 0,
    size = 2.4,
    colour = "#555555"
  ) +
  geom_jitter(aes(colour = label), height = 0.18, width = 0, size = 0.9, alpha = 0.6) +
  geom_point(
    data = fom_ensemble,
    aes(shape = "ensemble expectation"),
    size = 2.6,
    fill = "#0d366b",
    colour = "white",
    stroke = 0.7
  ) +
  geom_point(
    data = fom_deterministic,
    aes(shape = "deterministic allocation"),
    size = 2.8,
    fill = "#eb6834",
    colour = "white",
    stroke = 0.7
  ) +
  scale_colour_manual(values = learner_cols, guide = "none") +
  scale_shape_manual(
    values = c("ensemble expectation" = 21, "deterministic allocation" = 23),
    name = NULL
  ) +
  scale_y_continuous(
    breaks = row_y,
    labels = names(row_y),
    limits = c(-0.45, 1.7),
    expand = c(0, 0)
  ) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0.02, 0.06))) +
  labs(
    title = "(d) Figure of merit against the held-out observation",
    subtitle = sprintf("one dot per realisation (n = %d per learner)", n_realisations),
    x = "figure of merit",
    y = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme_panel_text +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "#e6e6e6", linewidth = 0.3),
    axis.text.y = element_text(size = 7.5)
  )

# (e): colour = how change was placed, line type = learner
cdf_lines <- rbind(
  data.table(x = rank_grid, cdf = cdf_observed, what = "observed change", learner = "observed"),
  cdf_band[, .(x, cdf = med, what = "realisations (median, 5-95 %)", learner)],
  cdf_deterministic[, .(x, cdf, what = "deterministic allocation", learner)]
)
cdf_lines[,
  learner := factor(learner, c("observed", learners$learner), c("observed", learners$label))
]
cdf_band[, learner := factor(learner, learners$learner, learners$label)]
what_cols <- c(
  "observed change" = "#1a1a1a",
  "realisations (median, 5-95 %)" = "#2a78d6",
  "deterministic allocation" = "#eb6834"
)
p_e <- ggplot() +
  geom_abline(slope = 1, intercept = 0, colour = "#9a9a9a", linewidth = 0.4, linetype = "22") +
  annotate(
    "text",
    x = 0.6,
    y = 0.66,
    label = "random allocation",
    angle = 45,
    size = 2.3,
    colour = "#555555"
  ) +
  geom_ribbon(
    data = cdf_band,
    aes(x = x, ymin = lo, ymax = hi, group = learner),
    fill = "#cde2fb",
    alpha = 0.8
  ) +
  geom_line(data = cdf_lines, aes(x, cdf, colour = what, linetype = learner), linewidth = 0.55) +
  scale_colour_manual(values = what_cols, breaks = names(what_cols)) +
  scale_linetype_manual(
    values = c("observed" = "solid", "random forest" = "solid", "logistic regression" = "22"),
    breaks = learners$label
  ) +
  scale_x_continuous(labels = function(v) paste0(v * 100, " %"), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "(e) Where change is placed",
    subtitle = "changed cells by their true probability",
    x = "percentile among candidate cells",
    y = "cumulative share of change"
  ) +
  theme_minimal(base_size = 9) +
  theme_panel_text +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#e6e6e6", linewidth = 0.3),
    legend.box = "vertical",
    legend.spacing.y = unit(0, "pt"),
    legend.margin = margin(0, 0, 0, 0)
  ) +
  guides(
    colour = guide_legend(order = 1, ncol = 1),
    linetype = guide_legend(order = 2, nrow = 1, override.aes = list(colour = "#555555"))
  )

fig2 <- p_a +
  p_b +
  p_c +
  p_d +
  p_e +
  plot_layout(design = "ABC\nDDE", heights = c(1, 1.05))
ggsave(
  file.path(out_dir, "fig2-ensembles.pdf"),
  fig2,
  width = 7.2,
  height = 6.6
  # device = cairo_pdf
)
fig2
