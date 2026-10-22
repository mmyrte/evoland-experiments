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
#' synthetic process is logistic, which would give it home advantage; an earlier
#' version of this figure showed both learners and was harder to read.
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
source("python-env.r")

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
# less probable neighbours: a logistic-regression ensemble (nearly true potentials) then put
# 28 % of its change in the top 5 % of true probabilities instead of the 36 % a sampler of the
# truth puts there.
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

#' # Runs: the held-out observation and the ensemble
#'
#' The ensemble members sit below the calibration base (run 0), so they inherit
#' its potentials and nothing else. The `learners` table drives everything
#' downstream; a second learner would need its own parent run, models and
#' potentials (predicted with `force = TRUE`, or the lineage read reuses run 0's).

#| label: runs
learners <- data.table(
  learner = "ranger",
  label = "random forest",
  id_run_parent = 0L
)
members <- learners[,
  .(id_run = id_run_parent + seq_len(n_realisations), member = seq_len(n_realisations)),
  by = .(learner, label, id_run_parent)
][, seed := 1000L + .I]

runs <- rbind(
  data.table(
    id_run = c(0L, id_run_observed),
    parent_id_run = c(NA_integer_, 0L),
    description = c(
      "calibration base, random forest potentials",
      "observed period 3 (validation only)"
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

#' # The established allocators on the same potentials
#'
#' The same random-forest potentials and the same quantities, allocated `n_realisations` times by
#' three established allocators besides CLUMPY:
#'
#' - **Dinamica EGO**'s expander and patcher, through evoland's `alloc_dinamica()`, with the same
#'   single-cell allocation parameters as CLUMPY;
#' - **CLUE-S** in lulcc and **CLUMondo** in CLUinPy, which allocate class totals rather than
#'   transitions. They get the class probability map of the potentials, as in
#'   `2026-09-model-comparison/020-potentials.r`: for a cell of class i, the adjusted potential of
#'   i -> k for k != i, and the remainder for staying in i; the lake is fixed. Their demand is the
#'   observed class totals of period 3; only the viable transitions are allowed. lulcc reads the
#'   probabilities through its own model interface (a GLM on the logit with coefficients fixed at
#'   0 and 1, as in `040-alloc-lulcc.r`). CLUE-S's tolerances are absolute cell counts: the GMD
#'   demo's 50 cells on PIE's 113 k cells scale to 5 on this 8 100-cell domain. CLUinPy's
#'   parameters follow `040-alloc-cluinpy.r` (conversion resistance, a neighbourhood weight for
#'   urban land only).
#'
#' Every realisation is imported as a run, so that all allocators are scored by the same code.
#' Needs Dinamica EGO, lulcc and the repository's uv environment (`python-env.r`, which also
#' clones CLUinPy at its pinned commit).

#| label: allocators
allocators <- data.table(
  allocator = c("CLUMPY", "Dinamica", "CLUE-S", "CLUMondo"),
  tool = c("evoland", "Dinamica EGO via evoland", "lulcc", "CLUinPy"),
  offset = c(0L, 2000L, 3000L, 4000L)
)
other_members <- allocators[
  allocator != "CLUMPY",
  .(member = seq_len(n_realisations)),
  by = .(allocator, offset)
][, `:=`(id_run = offset + member, seed = 10000L + offset + member)]
members[, allocator := "CLUMPY"]
db$id_run <- NULL
db$commit(
  as_runs_t(rbind(
    db$runs_t[!id_run %in% other_members$id_run],
    other_members[, .(
      id_run,
      parent_id_run = 0L,
      description = paste("random forest x", allocator, "realisation", member),
      seed
    )]
  )),
  "runs_t",
  method = "overwrite"
)

#| label: alloc-dinamica
#| output: false
for (i in which(other_members$allocator == "Dinamica")) {
  db$id_run <- other_members$id_run[i]
  set.seed(other_members$seed[i])
  db$alloc_dinamica(
    id_periods = 3L,
    select_score = "no.crossval",
    select_maximize = TRUE,
    use_parent_trans_pot = TRUE,
    update_neighbors = FALSE
  )
}

#| label: class-probabilities
viable_trans <- db$trans_meta_t[
  is_viable == TRUE,
  .(id_trans, id_lulc_anterior, id_lulc_posterior)
]
lulc_classes <- 1:4
anterior_2 <- lulc_long[id_period == 2L, .(id_coord, id_lulc_anterior = id_lulc)]
class_p <-
  CJ(id_coord = anterior_2$id_coord, id_lulc = lulc_classes)[
    anterior_2,
    on = "id_coord"
  ][
    adjusted_by[["ranger"]][viable_trans, on = "id_trans", nomatch = NULL],
    on = .(id_coord, id_lulc_anterior, id_lulc = id_lulc_posterior),
    p := i.value
  ][
    is.na(p),
    p := 0
  ]
class_p[, p_change := sum(p[id_lulc != id_lulc_anterior]), by = id_coord]
class_p[id_lulc == id_lulc_anterior, p := pmax(0, 1 - p_change)]
stopifnot(class_p[, abs(sum(p) - 1) < 1e-6, by = id_coord]$V1)
class_p_rast <- terra::rast(lapply(lulc_classes, function(k) {
  tabular_to_raster(
    class_p[id_lulc == k, .(id_coord, value = p)],
    db$coords_minimal,
    value_col = "value"
  )
}))
names(class_p_rast) <- paste0("p_", lulc_classes)
totals_3 <- lulc_long[id_period == 3L, .N, keyby = id_lulc][J(lulc_classes), N]
# conversions allowed: persistence and the viable transitions
allow <- diag(length(lulc_classes))
allow[as.matrix(viable_trans[, .(id_lulc_anterior, id_lulc_posterior)])] <- 1
maps_dir <- file.path(tempdir(), "fig2-allocator-maps")

#| label: alloc-clues
#| output: false
suppressPackageStartupMessages(library(lulcc))
lu_23 <- raster::stack(lapply(2:3, function(t) raster::raster(synthetic_lulc[[t]])))
names(lu_23) <- c("lu_2", "lu_3")
obs_23 <- ObsLulcRasterStack(
  x = lu_23,
  pattern = "lu",
  categories = lulc_classes,
  labels = c("Forest", "Arable", "Urban", "Lake"),
  t = c(0, 1)
)
suit <- raster::stack(raster::calc(raster::stack(class_p_rast), function(p) {
  stats::qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
}))
names(suit) <- paste0("suit_", lulc_classes)
ef_suit <- ExpVarRasterList(x = suit, pattern = "suit")
study_cells <- which(!is.na(raster::values(obs_23[[1]])))
clues_models <- suppressWarnings(glmModels(
  formula = list(Forest ~ suit_1, Arable ~ suit_2, Urban ~ suit_3, Lake ~ suit_4),
  family = binomial,
  data = getPredictiveModelInputData(obs = obs_23, ef = ef_suit, cells = study_cells, t = 0),
  obs = obs_23
))
for (j in seq_along(clues_models@models)) {
  beta <- clues_models@models[[j]]$coefficients
  clues_models@models[[j]]$coefficients <- setNames(c(0, 1), names(beta))
}
dir.create(file.path(maps_dir, "clue-s"), recursive = TRUE, showWarnings = FALSE)
for (i in which(other_members$allocator == "CLUE-S")) {
  set.seed(other_members$seed[i])
  clues <- allocate(CluesModel(
    obs = obs_23,
    ef = ef_suit,
    models = clues_models,
    time = c(0, 1),
    demand = rbind(total(obs_23)$total[1, ], totals_3),
    elas = c(0.2, 0.2, 0.2, 1),
    rules = allow,
    params = list(
      jitter.f = 0.0002,
      scale.f = 0.000001,
      max.iter = 1000,
      max.diff = 5,
      ave.diff = 5
    )
  ))
  raster::writeRaster(
    clues@output[[2]],
    file.path(maps_dir, "clue-s", sprintf("r%02d.tif", other_members$member[i])),
    datatype = "INT1U",
    overwrite = TRUE
  )
}

#| label: alloc-clumondo
#| output: false
cluinpy_work <- file.path(tempdir(), "fig2-cluinpy")
unlink(cluinpy_work, recursive = TRUE)
dir.create(cluinpy_work)
# the lake never changes and CLUMondo cannot hold a fixed class at its demand: it is left out
# (no data) and restored from the period-2 map on import
cluinpy_classes <- 1:3
lulc_2_codes <- terra::ifel(is.na(lulc_2) | lulc_2 == 4, -9999, lulc_2 - 1)
terra::writeRaster(
  lulc_2_codes,
  file.path(cluinpy_work, "land.tif"),
  datatype = "INT2S",
  NAflag = -9999
)
terra::writeRaster(
  terra::ifel(
    is.na(class_p_rast[[cluinpy_classes]]) | lulc_2 == 4,
    -9999,
    class_p_rast[[cluinpy_classes]]
  ),
  file.path(cluinpy_work, "suit.tif"),
  datatype = "FLT4S",
  NAflag = -9999
)
fwrite(as.data.table(t(totals_3[cluinpy_classes])), file.path(cluinpy_work, "demand.csv"))
fwrite(
  as.data.table(allow[cluinpy_classes, cluinpy_classes]),
  file.path(cluinpy_work, "allow.csv"),
  col.names = FALSE
)
fwrite(
  data.table(
    name = c("Forest", "Arable", "Urban"),
    conv_res = c(0.8, 0.6, 1),
    neigh_weight = c(0, 0, 0.3)
  ),
  file.path(cluinpy_work, "params.csv")
)
processx::run(
  project_python(),
  c(
    file.path("2026-09-paper-figures", "010-fig2-ensembles", "cluinpy_alloc.py"),
    cluinpy_repo(),
    cluinpy_work,
    n_realisations,
    allocators[allocator == "CLUMondo", 10000L + offset],
    file.path(maps_dir, "clumondo")
  ),
  echo = FALSE,
  error_on_status = TRUE
)

#| label: import
cells <- terra::cellFromXY(template_rast, as.matrix(db$coords_t[, .(lon, lat)]))
lulc_2_at <- lulc_2[cells][[1]]
for (a in c("CLUE-S", "CLUMondo")) {
  m <- other_members[allocator == a]
  db$commit(
    as_lulc_data_t(rbindlist(lapply(seq_len(nrow(m)), function(i) {
      map <- terra::rast(file.path(maps_dir, tolower(a), sprintf("r%02d.tif", m$member[i])))
      data.table(
        id_run = m$id_run[i],
        id_coord = db$coords_t$id_coord,
        id_period = 3L,
        # cells a tool left out (CLUinPy: the lake) keep their period-2 class
        id_lulc = as.integer(fcoalesce(as.numeric(map[cells][[1]]), as.numeric(lulc_2_at)))
      )[!is.na(id_lulc)]
    }))),
    "lulc_data_t",
    method = "upsert"
  )
}
members <- rbind(
  members,
  other_members[, .(
    learner = "ranger",
    label = "random forest",
    id_run_parent = 0L,
    id_run,
    member,
    seed,
    allocator
  )],
  use.names = TRUE
)
members[, allocator := factor(allocator, levels = allocators$allocator)]
members[, .N, by = allocator]

#' # Validation

#| label: fom
db$id_run <- NULL
fom <- db$figure_of_merit_v(
  id_period_anterior = 2L,
  id_period_post = 3L,
  id_run_reference = id_run_observed,
  id_run_simulated = members$id_run
)[members[, .(id_run, learner, label, allocator)], on = "id_run"]

# expected counts of each ensemble: ratio of mean counts, not mean of ratios
fom_ensemble <- fom[,
  lapply(.SD, mean),
  by = .(allocator),
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
)[, .(id_run, id_coord, simulated = id_lulc)][
  members[, .(id_run, learner, allocator)],
  on = "id_run"
]

knitr::kable(
  fom[,
    .(
      fom_median = median(figure_of_merit),
      fom_q05 = quantile(figure_of_merit, 0.05),
      fom_q95 = quantile(figure_of_merit, 0.95)
    ),
    by = .(allocator)
  ][fom_ensemble[, .(allocator, fom_ensemble = figure_of_merit)], on = "allocator"][,
    `:=`(fom_deterministic = fom_deterministic$figure_of_merit, fom_random_null = fom_null)
  ][],
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
#' References: the true probabilities, persistence, and the uniform forecast.
#' The latter knows the quantity of change but not its location: every cell of
#' an anterior class gets the same probability for each transition, the
#' transition's demand divided by the class's area. It is a forecast, not a map:
#' the cell-wise expectation of the random-allocation map behind the FoM null.
#' It is the reference for the skill score BSS = 1 - BS / BS_uniform: 0 is no better than
#' placing the right quantity at random, 1 is perfect. (Forecast verification
#' calls this kind of reference "climatology".)

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
  lapply(levels(members$allocator), function(a) {
    sims <- simulated_all[allocator == a]
    draws <- sims[, .(bs = brier(.SD[, .(id_coord, class = simulated, p = 1)])), by = id_run]
    data.table(
      learner = a,
      forecast = c("ensemble frequency (fair)", "single realisation (median)"),
      brier = c(
        brier(
          sims[, .(p = .N / n_realisations), by = .(id_coord, class = simulated)],
          n_realisations
        ),
        median(draws$bs)
      )
    )
  }),
  list(data.table(
    learner = "random forest",
    forecast = c("adjusted potentials", "deterministic"),
    brier = c(
      brier(persist(potentials_long(adjusted_by[["ranger"]]))),
      brier(deterministic_by[["ranger"]][, .(id_coord, class = deterministic, p = 1)])
    )
  )),
  list(data.table(
    learner = "reference",
    forecast = c("true probabilities", "uniform forecast", "persistence"),
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
scores[, skill := 1 - brier / scores[forecast == "uniform forecast", brier]]
knitr::kable(scores, digits = 4)

#' # Where change is placed: the bias of a deterministic map
#'
#' The deterministic map wins on FoM by putting all change on the highest
#' potentials. Observed change does not behave like that: it also happens on
#' cells of moderate probability, in proportion to that probability. For every
#' changed cell, take the percentile of its *true* transition probability among
#' all cells that could make the same transition. The truth does not depend
#' on the learner, and percentiles let transitions with different probability
#' scales be pooled. Then compare the distributions of those percentiles for
#' the observed change, the realisations and the greedy maps. Random allocation
#' would give a uniform distribution (the diagonal).
#'
#' An unbiased sampler of the true probabilities reproduces the observed curve
#' in expectation; the realisations deviate from it by as much as the
#' learner's potentials deviate from the truth.

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
    .(id_run, allocator, id_coord, anterior, posterior = simulated)
  ]
)

cdf_observed <- ecdf_on_grid(ranks_observed$rank)
cdf_draws <- ranks_draws[,
  .(x = rank_grid, cdf = ecdf_on_grid(rank)),
  by = .(allocator, id_run)
]
cdf_band <- cdf_draws[,
  .(lo = quantile(cdf, 0.05), med = median(cdf), hi = quantile(cdf, 0.95)),
  by = .(allocator, x)
]
cdf_deterministic <- ranks_deterministic[,
  .(x = rank_grid, cdf = ecdf_on_grid(rank)),
  by = learner
]

bias_tab <- rbind(
  data.table(
    changed_cells = "observed",
    allocator = NA_character_,
    share_in_top_5_pct = mean(ranks_observed$rank > 0.95),
    median_percentile = median(ranks_observed$rank)
  ),
  ranks_draws[,
    .(share = mean(rank > 0.95), med = median(rank)),
    by = .(allocator, id_run)
  ][,
    .(
      changed_cells = "realisations (median)",
      share_in_top_5_pct = median(share),
      median_percentile = median(med)
    ),
    by = allocator
  ],
  ranks_deterministic[,
    .(
      changed_cells = "deterministic",
      allocator = "rank-and-fill",
      share_in_top_5_pct = mean(rank > 0.95),
      median_percentile = median(rank)
    ),
    by = learner
  ][, !"learner"],
  use.names = TRUE
)
knitr::kable(bias_tab, digits = 3)

# Kolmogorov-Smirnov distance of each curve to the observed one. Not shown in the figure: the
# distance is not widely known, and the panel shows the bias plainly. Uncomment to report it.
# ks_draws <- cdf_draws[, .(ks = max(abs(cdf - cdf_observed))), by = .(allocator, id_run)]
# ks_draws[, .(ks_median = median(ks)), by = allocator]
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
fom_ranger <- fom[allocator == "CLUMPY"]
shown_run <- fom_ranger[order(abs(figure_of_merit - median(figure_of_merit)))][1, id_run]

lulc_names <- c("1" = "forest", "2" = "arable", "3" = "urban", "4" = "lake (fixed)")
panel_lulc <- maps[, .(id_coord, status = lulc_names[as.character(observed)])]
panel_draw <- maps[
  simulated_all[id_run == shown_run, .(id_coord, simulated)],
  on = "id_coord"
][, .(id_coord, status = outcome(anterior, observed, simulated))]
panel_freq <- simulated_all[allocator == "CLUMPY"][
  maps[, .(id_coord, anterior)],
  on = "id_coord"
][, .(p_change = mean(simulated != anterior)), by = id_coord]
observed_change_cells <- maps[anterior != observed, .(id_coord)]

#| label: figure
#| fig-width: 7.2
#| fig-height: 7.4
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
    linewidth = 0.8,
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
      "all cells; outline: changed since period 2\n%d \u00d7 %d window of the domain",
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
      "random forest, CLUMPY, median draw\nFoM %.2f (whole domain)",
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
    subtitle = "random forest, CLUMPY ensemble\noutline: observed change"
  ) +
  theme_map +
  theme(
    legend.title = element_text(size = 7),
    legend.key.width = unit(20, "pt"),
    legend.key.height = unit(5, "pt")
  )

# (d): one row per allocator, realisations jittered around it; the deterministic allocation below
allocator_cols <- c(
  "CLUMPY" = "#2a78d6",
  "Dinamica" = "#4a3aa7",
  "CLUE-S" = "#1baf7a",
  "CLUMondo" = "#e87ba4",
  "rank-and-fill" = "#eb6834"
)
row_levels <- c(levels(members$allocator), "rank-and-fill")
fom[, row := factor(as.character(allocator), levels = rev(row_levels))]
fom_ensemble[, row := factor(as.character(allocator), levels = rev(row_levels))]
fom_deterministic[, row := factor("rank-and-fill", levels = rev(row_levels))]
set.seed(7)
p_d <- ggplot(fom, aes(x = figure_of_merit, y = row)) +
  geom_vline(xintercept = 0, colour = "#9a9a9a", linewidth = 0.4) +
  geom_vline(xintercept = fom_null, colour = "#9a9a9a", linewidth = 0.4, linetype = "22") +
  # the nulls, labelled along their lines
  annotate(
    "text",
    x = 0,
    y = length(row_levels) + 0.4,
    label = "null: no change",
    angle = 90,
    hjust = 1,
    vjust = 1.4,
    size = 2.4,
    colour = "#555555"
  ) +
  annotate(
    "text",
    x = fom_null,
    y = length(row_levels) + 0.4,
    label = "null: random placement",
    angle = 90,
    hjust = 1,
    vjust = 1.4,
    size = 2.4,
    colour = "#555555"
  ) +
  geom_jitter(aes(colour = allocator), height = 0.18, width = 0, size = 0.8, alpha = 0.5) +
  geom_point(
    data = fom_ensemble,
    aes(shape = "ensemble expectation"),
    size = 2.6,
    fill = "#1a1a1a",
    colour = "white",
    stroke = 0.6
  ) +
  geom_point(
    data = fom_deterministic,
    aes(shape = "deterministic allocation"),
    size = 3,
    fill = allocator_cols[["rank-and-fill"]],
    colour = "white",
    stroke = 0.7
  ) +
  scale_colour_manual(values = allocator_cols, guide = "none") +
  scale_shape_manual(
    values = c("ensemble expectation" = 21, "deterministic allocation" = 23),
    name = NULL
  ) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1))) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0.02, 0.06))) +
  labs(
    title = "(d) Figure of merit against the held-out observation",
    subtitle = sprintf(
      "random-forest potentials; one dot per realisation (n = %d per allocator)",
      n_realisations
    ),
    x = "figure of merit",
    y = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme_panel_text +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "#e6e6e6", linewidth = 0.3)
  )

# (e): colour = allocator; black = observed change; band = CLUMPY's 5-95 % range
cdf_lines <- rbind(
  data.table(x = rank_grid, cdf = cdf_observed, what = "observed change"),
  cdf_band[, .(x, cdf = med, what = as.character(allocator))],
  cdf_deterministic[, .(x, cdf, what = "rank-and-fill")]
)
what_cols <- c("observed change" = "#1a1a1a", allocator_cols)
cdf_lines[, what := factor(what, levels = names(what_cols))]
p_e <- ggplot() +
  geom_abline(slope = 1, intercept = 0, colour = "#9a9a9a", linewidth = 0.4, linetype = "22") +
  annotate(
    "text",
    x = 0.6,
    y = 0.66,
    label = "random placement",
    angle = 45,
    size = 2.3,
    colour = "#555555"
  ) +
  geom_ribbon(
    data = cdf_band[allocator == "CLUMPY"],
    aes(x = x, ymin = lo, ymax = hi),
    fill = "#cde2fb"
  ) +
  geom_line(data = cdf_lines, aes(x, cdf, colour = what), linewidth = 0.55) +
  scale_colour_manual(values = what_cols, breaks = names(what_cols)) +
  scale_x_continuous(labels = function(v) paste0(v * 100, " %"), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "(e) Where change is placed",
    subtitle = "changed cells by their true probability;\nmedian realisation per allocator",
    x = "percentile among candidate cells",
    y = "cumulative share of change"
  ) +
  theme_minimal(base_size = 9) +
  theme_panel_text +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#e6e6e6", linewidth = 0.3)
  ) +
  guides(colour = guide_legend(ncol = 2))

fig2 <- p_a +
  p_b +
  p_c +
  p_d +
  p_e +
  plot_layout(design = "ABC\nDDE", heights = c(1, 1.25))
ggsave(
  file.path(out_dir, "fig2-ensembles.pdf"),
  fig2,
  width = 7.2,
  height = 7.4
  # device = cairo_pdf
)
fig2
