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
#' Why 90 × 90 and this learner: `030-skill-attribution.r` showed that the
#' allocator passes the potentials through unchanged, so a weak figure was weak
#' estimation. `031-learner-comparison.r` found that ranger with default leaves
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
#' `000-synthetic-process.r`, which `030` and `031` share.
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
# all available predictors: in 031, preselecting rpart's top 4 cost ranger skill
db$set_full_trans_preds()

# larger leaves give better-calibrated probabilities (031)
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

db$alloc_params_t <- db$create_alloc_params_t()
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

#' # Runs: the held-out observation and the ensemble

#| label: runs
runs <- data.table(
  id_run = c(0L, id_run_observed, seq_len(n_realisations)),
  parent_id_run = c(NA_integer_, 0L, rep(0L, n_realisations)),
  description = c(
    "calibration base",
    "observed period 3 (validation only)",
    paste("CLUMPY realisation", seq_len(n_realisations))
  ),
  seed = c(NA_integer_, NA_integer_, 1000L + seq_len(n_realisations))
)
db$commit(as_runs_t(runs), "runs_t", method = "overwrite")

db$lulc_data_t <- as_lulc_data_t(lulc_long[
  id_period == 3L,
  .(id_run = id_run_observed, id_coord, id_period, id_lulc)
])

#| label: allocate
#| output: false

for (id in seq_len(n_realisations)) {
  db$id_run <- id
  set.seed(runs[id_run == id, seed])
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
#' deterministically on the same adjusted potentials: greedily, highest
#' potential first, one class per cell. This isolates the allocator's
#' contribution; it says nothing about how Dinamica or LCM would score.

#| label: deterministic
db$id_run <- 0L
adjusted <- db$adjusted_trans_pot_v(3L)
str(adjusted)

#| label: deterministic-alloc
pot_col <- intersect(
  c("value", "trans_pot", "potential", "prob"),
  names(adjusted)
)[1]
stopifnot("unknown adjusted potential column" = !is.na(pot_col))

candidates <- adjusted[, .(id_coord, id_trans, potential = get(pot_col))][
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
greedy <- candidates[, {
  keep <- logical(.N)
  for (i in seq_len(.N)) {
    key <- as.character(id_coord[i])
    tr <- as.character(id_trans[i])
    if (remaining[[tr]] > 0 && is.null(assigned[[key]])) {
      assigned[[key]] <- TRUE
      remaining[[tr]] <- remaining[[tr]] - 1L
      keep[i] <- TRUE
    }
  }
  .(id_coord = id_coord[keep], id_lulc = id_lulc_posterior[keep])
}]

deterministic_map <- lulc_long[id_period == 2L, .(id_coord, id_lulc)]
deterministic_map[greedy, on = "id_coord", id_lulc := i.id_lulc]

#' # Validation

#| label: fom
db$id_run <- NULL
fom <- db$figure_of_merit_v(
  id_period_anterior = 2L,
  id_period_post = 3L,
  id_run_reference = id_run_observed,
  id_run_simulated = seq_len(n_realisations)
)

# expected counts of the ensemble: ratio of mean counts, not mean of ratios
fom_ensemble <- fom[,
  lapply(.SD, mean),
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
][deterministic_map[, .(id_coord, deterministic = id_lulc)], on = "id_coord"]
fom_deterministic <- maps[, fom_counts(anterior, observed, deterministic)]

# the ensemble as a probabilistic forecast of change, against single maps (Brier score, lower
# is better); needs the realisations, so fetched here rather than in the figure section
simulated_all <- db$fetch(
  "lulc_data_t",
  where = glue::glue("id_run between 1 and {n_realisations} and id_period = 3")
)[, .(id_run, id_coord, simulated = id_lulc)]
brier <- maps[, .(id_coord, anterior, observed, deterministic)][
  simulated_all[, .(id_run, id_coord, simulated)],
  on = "id_coord"
][,
  .(
    p_ensemble = mean(simulated != anterior),
    y = as.numeric(observed[1] != anterior[1]),
    deterministic = as.numeric(deterministic[1] != anterior[1])
  ),
  by = id_coord
]
brier_draws <- maps[, .(id_coord, anterior, observed)][
  simulated_all,
  on = "id_coord"
][,
  .(
    brier = mean(
      (as.numeric(simulated != anterior) - as.numeric(observed != anterior))^2
    )
  ),
  by = id_run
]

summary_tab <- data.table(
  quantity = c(
    "realisations: median FoM",
    "realisations: 5-95 %",
    "ensemble expected FoM",
    "deterministic greedy",
    "random-allocation null",
    "Brier: ensemble change frequency",
    "Brier: single realisations (median)",
    "Brier: deterministic"
  ),
  value = c(
    sprintf("%.3f", median(fom$figure_of_merit)),
    paste(
      sprintf("%.3f", quantile(fom$figure_of_merit, c(0.05, 0.95))),
      collapse = " to "
    ),
    sprintf("%.3f", fom_ensemble$figure_of_merit),
    sprintf("%.3f", fom_deterministic),
    sprintf("%.3f", mean(fom$figure_of_merit_null)),
    sprintf("%.4f", brier[, mean((p_ensemble - y)^2)]),
    sprintf("%.4f", median(brier_draws$brier)),
    sprintf("%.4f", brier[, mean((deterministic - y)^2)])
  )
)
knitr::kable(summary_tab)

#' # Probabilistic scores: which class, not just whether
#'
#' The Brier scores above only ask *whether* a cell changes. A realisation that
#' turns a forest cell into arable land where it actually became urban counts as
#' correct. The multiclass Brier score (Brier, 1950) scores the full outcome:
#' for each cell, the sum over posterior classes k of (p_k - o_k)^2, where o is
#' the observed class as a one-hot vector. It ranges from 0 to 2. A hard map
#' scores 0 on a correct cell and 2 on any miss, false alarm or wrong hit, so
#' for single maps it is twice the share of cells that are wrong.
#'
#' Scores are averaged over the cells that can change: those whose anterior
#' class has at least one viable transition. Averaging over the lake and over
#' classes the model holds fixed would only dilute the differences.
#'
#' Forecasts compared:
#'
#' - **ensemble**: the share of realisations that end in each class; also its
#'   *fair* version (Ferro, 2014), which removes the penalty a finite ensemble
#'   pays for sampling noise, sum_k p_k (1 - p_k) / (M - 1);
#' - **adjusted potentials**: the per-cell transition probabilities the
#'   allocator samples from, with persistence as the remainder. This is the
#'   probability map any tool with a potential surface already offers;
#' - **climatology**: every cell of an anterior class gets that class's demand
#'   rates. It is the probabilistic counterpart of the random-allocation null,
#'   and the reference for the skill score BSS = 1 - BS / BS_climatology;
#' - **persistence**, the **deterministic** map, and **single realisations**.

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
one_hot <- function(col) maps[, .(id_coord, class = get(col), p = 1)]
# transition probabilities per cell, with persistence as the remainder
with_persistence <- function(trans_p) {
  stay <- trans_p[, .(leave = sum(p)), by = id_coord][
    anterior_of,
    on = "id_coord"
  ][is.na(leave), leave := 0][, .(id_coord, class = anterior, p = 1 - leave)]
  rbind(trans_p, stay)
}

observed_p <- complete_p(one_hot("observed"))[, .(id_coord, class, o = p)]

brier <- function(f, n_members = NA_integer_) {
  d <- complete_p(f)[observed_p, on = .(id_coord, class)][
    anterior_of,
    on = "id_coord"
  ]
  cell <- d[,
    .(
      multi = sum((p - o)^2),
      # probability of leaving the anterior class vs. whether it left
      change = (sum(p[class != anterior]) - sum(o[class != anterior]))^2,
      noise = if (is.na(n_members)) 0 else sum(p * (1 - p)) / (n_members - 1)
    ),
    by = id_coord
  ]
  cell[, .(
    multiclass = mean(multi),
    multiclass_fair = mean(multi - noise),
    change_only = mean(change)
  )]
}

forecasts <- list(
  ensemble = simulated_all[,
    .(p = .N / n_realisations),
    by = .(id_coord, class = simulated)
  ],
  potentials = with_persistence(
    adjusted[viable, on = "id_trans", nomatch = NULL][,
      .(id_coord, class = id_lulc_posterior, p = value)
    ]
  ),
  climatology = with_persistence(
    anterior_of[
      viable[rates_3[, .(id_trans, rate)], on = "id_trans", nomatch = NULL],
      on = c(anterior = "id_lulc_anterior"),
      allow.cartesian = TRUE,
      nomatch = NULL
    ][, .(id_coord, class = id_lulc_posterior, p = rate)]
  ),
  persistence = one_hot("anterior"),
  deterministic = one_hot("deterministic")
)
scores <- rbindlist(
  lapply(names(forecasts), function(nm) {
    brier(forecasts[[nm]], if (nm == "ensemble") n_realisations else NA)
  }),
  idcol = "forecast"
)[, forecast := names(forecasts)[forecast]]

draw_scores <- rbindlist(lapply(seq_len(n_realisations), function(r) {
  brier(simulated_all[id_run == r, .(id_coord, class = simulated, p = 1)])
}))
scores <- rbind(
  scores,
  draw_scores[, lapply(.SD, median)][, forecast := "single realisation (median)"],
  draw_scores[, lapply(.SD, quantile, 0.05)][, forecast := "single realisation (5 %)"],
  draw_scores[, lapply(.SD, quantile, 0.95)][, forecast := "single realisation (95 %)"]
)
clim <- scores[forecast == "climatology"]
scores[, `:=`(
  bss_multiclass = 1 - multiclass / clim$multiclass,
  bss_change_only = 1 - change_only / clim$change_only
)]
knitr::kable(
  scores[, .(
    forecast,
    multiclass,
    multiclass_fair,
    bss_multiclass,
    change_only,
    bss_change_only
  )],
  digits = 4
)

#' Context for the scores: how many cells are scored, how much observed change
#' falls on them, and how much of it the model cannot produce at all.

#| label: brier-context
observed_change <- maps[id_coord %in% eligible & anterior != observed]
data.table(
  eligible_cells = length(eligible),
  observed_change = nrow(observed_change),
  of_which_unmodelled = observed_change[
    !viable,
    on = c(anterior = "id_lulc_anterior", observed = "id_lulc_posterior")
  ][, .N],
  viable_transitions = nrow(viable)
)

#' # Figure
#'
#' Map panels are coloured by the figure-of-merit outcome rather than land-use
#' class: the figure is about where change goes, and the outcome classes are
#' what the metric counts.

#| label: figure-data
simulated <- maps[, .(id_coord, anterior, observed)][
  simulated_all,
  on = "id_coord"
]

coords_xy <- db$coords_t[, .(id_coord, x = lon, y = lat)]

# map panels show one n_window x n_window block of the domain: the block with the
# most observed change, so that the panels have something to show; every score
# and panel (d) use the whole domain
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
    obs_change & sim_change & simulated == observed ,
    "hit"                                           ,
    obs_change & sim_change                         ,
    "wrong hit"                                     ,
    obs_change                                      ,
    "miss"                                          ,
    sim_change                                      ,
    "false alarm"                                   ,
    default = "persistence"
  )
}

# the realisation shown is the one closest to the ensemble median
shown_run <- fom[order(abs(figure_of_merit - median(figure_of_merit)))][
  1,
  id_run
]

panel_obs <- maps[,
  .(
    id_coord,
    status = fifelse(anterior != observed, "observed change", "persistence")
  )
]
panel_draw <- simulated[
  id_run == shown_run,
  .(id_coord, status = outcome(anterior, observed, simulated))
]
panel_freq <- simulated[,
  .(p_change = mean(simulated != anterior)),
  by = id_coord
]
observed_change_cells <- maps[anterior != observed, .(id_coord)]

#| label: figure
#| fig-width: 7.2
#| fig-height: 5.6
col_surface <- "#f1f0ed"
outcome_cols <- c(
  "persistence" = col_surface,
  "observed change" = "#184f95",
  "hit" = "#2a78d6",
  "miss" = "#eb6834",
  "false alarm" = "#1baf7a",
  "wrong hit" = "#3d3d3d"
)
seq_blue <- c("#fcfcfb", "#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b")

theme_map <- theme_void(base_size = 9) +
  theme(
    plot.title = element_text(
      size = 9,
      face = "bold",
      hjust = 0,
      margin = margin(b = 3)
    ),
    plot.subtitle = element_text(
      size = 8,
      colour = "#555555",
      margin = margin(b = 4)
    ),
    plot.margin = margin(2, 8, 2, 8),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.size = unit(8, "pt"),
    legend.text = element_text(size = 7.5)
  )

map_panel <- function(d, title, subtitle = NULL) {
  ggplot(coords_xy[d, on = "id_coord", nomatch = NULL], aes(x, y, fill = status)) +
    geom_tile(colour = "white", linewidth = 0.15) +
    scale_fill_manual(
      values = outcome_cols,
      breaks = intersect(names(outcome_cols)[-1], unique(d$status))
    ) +
    coord_equal(expand = FALSE) +
    labs(title = title, subtitle = subtitle) +
    theme_map
}

p_a <- map_panel(
  panel_obs,
  "(a) Observed change",
  sprintf("period 2 to 3, held out; %d \u00d7 %d window", n_window, n_window)
)
p_b <- map_panel(
  panel_draw,
  "(b) One realisation",
  sprintf("median draw, FoM = %.2f (whole domain)", fom[id_run == shown_run, figure_of_merit])
)
p_c <- ggplot(coords_xy[panel_freq, on = "id_coord", nomatch = NULL], aes(x, y)) +
  geom_tile(aes(fill = p_change), colour = "white", linewidth = 0.15) +
  geom_tile(
    data = coords_xy[observed_change_cells, on = "id_coord", nomatch = NULL],
    fill = NA,
    colour = "#1a1a1a",
    linewidth = 0.45,
    width = 70,
    height = 70
  ) +
  scale_fill_gradientn(
    colours = seq_blue,
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    name = "share of realisations",
    guide = guide_colourbar(title.position = "top", title.hjust = 0.5)
  ) +
  coord_equal(expand = FALSE) +
  labs(title = "(c) Change frequency", subtitle = "outline: observed change") +
  theme_map +
  theme(
    legend.title = element_text(size = 7.5),
    legend.key.width = unit(22, "pt"),
    legend.key.height = unit(6, "pt")
  )

fom_null <- mean(fom$figure_of_merit_null)
refs <- data.table(
  label = c(
    "persistence",
    "random allocation",
    "deterministic, same potentials",
    "ensemble expected"
  ),
  value = c(0, fom_null, fom_deterministic, fom_ensemble$figure_of_merit)
)
p_d <- ggplot(fom, aes(x = figure_of_merit, y = 0)) +
  geom_vline(xintercept = 0, colour = "#9a9a9a", linewidth = 0.4) +
  geom_vline(
    xintercept = fom_null,
    colour = "#9a9a9a",
    linewidth = 0.4,
    linetype = "22"
  ) +
  geom_violin(fill = "#cde2fb", colour = NA, width = 0.7) +
  geom_jitter(
    height = 0.18,
    width = 0,
    size = 1.1,
    colour = "#2a78d6",
    alpha = 0.7
  ) +
  geom_point(
    data = refs[3],
    aes(x = value, y = 0),
    shape = 23,
    size = 3,
    fill = "#eb6834",
    colour = "white",
    stroke = 0.8
  ) +
  geom_point(
    data = refs[4],
    aes(x = value, y = 0),
    shape = 21,
    size = 3,
    fill = "#0d366b",
    colour = "white",
    stroke = 0.8
  ) +
  annotate(
    "text",
    x = 0,
    y = 0.66,
    label = " persistence",
    hjust = 0,
    size = 2.5,
    colour = "#555555"
  ) +
  annotate(
    "text",
    x = fom_null,
    y = 0.66,
    label = " random allocation",
    hjust = 0,
    size = 2.5,
    colour = "#555555"
  ) +
  annotate(
    "segment",
    x = refs[4, value],
    xend = refs[4, value],
    y = -0.08,
    yend = -0.36,
    colour = "#0d366b",
    linewidth = 0.3
  ) +
  annotate(
    "text",
    x = refs[4, value],
    y = -0.42,
    label = "ensemble expectation ",
    hjust = 1,
    size = 2.5,
    colour = "#0d366b"
  ) +
  annotate(
    "segment",
    x = refs[3, value],
    xend = refs[3, value],
    y = -0.08,
    yend = -0.5,
    colour = "#eb6834",
    linewidth = 0.3
  ) +
  annotate(
    "text",
    x = refs[3, value],
    y = -0.56,
    label = "deterministic allocation, same potentials ",
    hjust = 1,
    size = 2.5,
    colour = "#b8461c"
  ) +
  scale_y_continuous(limits = c(-0.62, 0.72), breaks = NULL) +
  scale_x_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "(d) Figure of merit against the held-out observation",
    subtitle = sprintf("one dot per realisation (n = %d)", n_realisations),
    x = "figure of merit",
    y = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(size = 9, face = "bold", margin = margin(b = 3)),
    plot.subtitle = element_text(
      size = 8,
      colour = "#555555",
      margin = margin(b = 4)
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "#e6e6e6", linewidth = 0.3)
  )

maps_row <- (p_a | p_b) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
fig2 <- (maps_row | p_c) / p_d + plot_layout(heights = c(1.3, 1), widths = c(2, 1))
ggsave(
  file.path(out_dir, "fig2-ensembles.pdf"),
  fig2,
  width = 7.2,
  height = 5.6,
  device = cairo_pdf
)
# ggsave(file.path(out_dir, "fig2-ensembles.png"), fig2, width = 7.2, height = 5.6, dpi = 200, bg = "white")
fig2
