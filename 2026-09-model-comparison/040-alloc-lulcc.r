#' ---
#' title: "PIE benchmark: lulcc's CLUE-S and Ordered allocation of every estimator's surface"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' lulcc 1.0.4 (CRAN), with the allocation parameters of its GMD demo
#' (`demo("gmd-paper", package = "lulcc")`, Moulds et al. 2015):
#'
#' - **Allocation**: 8 annual steps from 1991, with CLUE-S (Verburg et al. 2002; elasticity
#'   0.2 for every class, the demo's iteration parameters) and the Ordered procedure (Fuchs et
#'   al. 2013; order built, forest, other). CLUE-S jitters suitabilities by a random amount, so
#'   it is repeated with seeds.
#' - **Ordered, deterministic in the crossing.** lulcc's stochastic mode thins each class's
#'   ranked candidates by a Bernoulli(suitability) draw and then takes the first n, retrying up
#'   to 1000 times. That suits presence probabilities (lulcc's GLMs). With the crossing's class
#'   probabilities, a few per cent for any change, the draws often cannot supply n cells; the
#'   missing picks become `NA` indices, cells stay unassigned (-1), and in the next annual step
#'   lulcc's C code reads out of range: the maps came out with class codes in the billions. The
#'   crossing therefore runs Ordered with `stochastic = FALSE`, one map per estimator; the
#'   as-shipped run keeps the demo's stochastic mode.
#' - **Demand**: lulcc's `approxExtrapDemand()` interpolates the class totals linearly between
#'   1991 and 1999 in annual steps; the end point is the observed 1999 total of every class,
#'   the same quantities the transition allocators get as transition counts.
#'
#' **The crossing.** lulcc allocates on a suitability per class and cell, which it gets by
#' predicting a list of fitted models on its explanatory variables. Here the suitability is each
#' estimator's class probability map from `020-potentials.r`. To feed a fixed map through
#' lulcc's own interface, the explanatory variable of class k is the logit of its probability,
#' and the model of class k is a binomial GLM on that variable alone whose coefficients are set
#' to (0, 1): lulcc's `predict()` then returns exactly the estimator's probability. The
#' conversion rules forbid the transitions that are not viable (`010`), as the transition
#' allocators cannot produce them either.
#'
#' **As shipped (reference, runs 7401..., 7501...).** lulcc's own suitability models: one
#' binomial GLM per class, fitted on a 10 % spatially stratified sample of the *1991* map, with
#' the demo's formulas (built on all three factors; forest and other on elevation and slope),
#' and all conversions allowed, as in the demo. This is the CLUE paradigm, where each class
#' *is*, rather than where it *changes to*. Its GLMs are logistic regressions on the factors
#' alone, so the crossing's logistic-regression row replaces it there.
#'
#' The comparison takes the 1999 map of each run. lulcc allocates class totals, not
#' transitions: the transition counts it produces need not match the observed ones.

#| label: setup
#| output: false
suppressPackageStartupMessages(library(lulcc))
library(data.table)
source("2026-09-model-comparison/common.r")
reset_timings("lulcc")

lu <- raster::stack(lapply(c(1991, 1999), \(y) {
  raster::raster(file.path(data_dir, paste0("lu_", y, ".tif")))
}))
names(lu) <- c("lu_1991", "lu_1999")
class_labels <- c("Forest", "Built", "Other")
obs <- ObsLulcRasterStack(
  x = lu,
  pattern = "lu",
  categories = c(1, 2, 3),
  labels = class_labels,
  t = c(0, 8)
)
obs
crossTabulate(obs, times = c(0, 8))

#' # Demand and allocation parameters

#| label: demand
dmd <- approxExtrapDemand(obs = obs, tout = 0:8)
dmd
clues_parms <- list(
  jitter.f = 0.0002,
  scale.f = 0.000001,
  max.iter = 1000,
  max.diff = 50,
  ave.diff = 50
)
elasticity <- c(0.2, 0.2, 0.2)
ordered_order <- c(2, 1, 3)

# conversion rules for the crossing: rows from, columns to; the viable transitions of 010
viable <- fread(file.path(outputs_dir, "alloc_params_viable.csv"))[, .(
  id_lulc_anterior,
  id_lulc_posterior
)]
viable_rules <- diag(3)
viable_rules[as.matrix(viable)] <- 1
viable_rules

write_final <- function(model, dir, i) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  final <- model@output[[raster::nlayers(model@output)]]
  stopifnot(
    "allocation left cells without a class" = all(raster::values(final) %in% c(1, 2, 3, NA))
  )
  raster::writeRaster(
    final,
    file.path(dir, sprintf("r%02d.tif", i)),
    datatype = "INT1U",
    NAflag = 255,
    overwrite = TRUE
  )
}

# CLUE-S and Ordered realisations, seeded; `tag` names the timing notes
allocate_both <- function(ef, models, rules, seeds, dirs, tag, ordered_stochastic) {
  for (i in seq_along(seeds$clues)) {
    set.seed(seeds$clues[i])
    clues_model <- CluesModel(
      obs = obs,
      ef = ef,
      models = models,
      time = 0:8,
      demand = dmd,
      elas = elasticity,
      rules = rules,
      params = clues_parms
    )
    clues_model <- timed(
      "lulcc",
      "allocate_clues",
      allocate(clues_model),
      note = paste0(tag, " realisation=", i)
    )
    write_final(clues_model, dirs$clues, i)
  }
  for (i in seq_along(seeds$ordered)) {
    set.seed(seeds$ordered[i])
    ordered_model <- OrderedModel(
      obs = obs,
      ef = ef,
      models = models,
      time = 0:8,
      demand = dmd,
      rules = rules,
      order = ordered_order
    )
    ordered_model <- timed(
      "lulcc",
      "allocate_ordered",
      allocate(ordered_model, stochastic = ordered_stochastic),
      note = paste0(tag, " realisation=", i)
    )
    write_final(ordered_model, dirs$ordered, i)
  }
}

#' # The crossing: every estimator's class probabilities
#'
#' A GLM on a sample of the logit layers fixes the model structure lulcc expects; its
#' coefficients are then replaced, so the fit itself does not matter.

#| label: crossing
#| output: false
members <- crossing_members()
fixed_models <- function(ef, sample_cells) {
  train <- getPredictiveModelInputData(obs = obs, ef = ef, cells = sample_cells, t = 0)
  forms <- list(Forest ~ suit_1, Built ~ suit_2, Other ~ suit_3)
  models <- suppressWarnings(glmModels(formula = forms, family = binomial, data = train, obs = obs))
  for (j in seq_along(models@models)) {
    beta <- models@models[[j]]$coefficients
    models@models[[j]]$coefficients <- setNames(c(0, 1), names(beta))
  }
  models
}
study_cells <- which(!is.na(raster::values(obs[[1]])))
sample_cells <- study_cells[seq(1, length(study_cells), by = 50)]

for (est in estimators$estimator) {
  prob <- raster::stack(file.path(outputs_dir, "suitability", paste0(est, ".tif")))
  # logit, bounded away from 0 and 1 so that it stays finite
  suit <- raster::stack(raster::calc(prob, function(p) {
    stats::qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  }))
  names(suit) <- paste0("suit_", 1:3)
  ef <- ExpVarRasterList(x = suit, pattern = "suit")
  models <- fixed_models(ef, sample_cells)

  # lulcc's predict() returns the estimator's probabilities, to rounding
  check_cells <- sample_cells[1:500]
  predicted <- predict(models, newdata = as.data.frame(x = ef, cells = check_cells))
  stopifnot(max(abs(predicted - raster::extract(prob, check_cells)[, 1:3])) < 1e-5)

  m <- members[estimator == est]
  allocate_both(
    ef = ef,
    models = models,
    rules = viable_rules,
    seeds = list(clues = m[allocator == "clue-s", seed], ordered = m[allocator == "ordered", seed]),
    dirs = list(
      clues = external_maps_dir("clue-s", est),
      ordered = external_maps_dir("ordered", est)
    ),
    tag = paste0("estimator=", est),
    ordered_stochastic = FALSE
  )
}

#' # As shipped: lulcc's GLM suitability on the 1991 map

#| label: native-fit
ef_stack <- raster::stack(file.path(data_dir, paste0("ef_00", 1:3, ".tif")))
names(ef_stack) <- paste0("ef_00", 1:3)
ef_native <- ExpVarRasterList(x = ef_stack, pattern = "ef")
set.seed(1991)
fit <- timed("lulcc", "fit_glm", {
  part <- partition(x = obs[[1]], size = 0.1, spatial = TRUE)
  train_data <- getPredictiveModelInputData(
    obs = obs,
    ef = ef_native,
    cells = part[["train"]],
    t = 0
  )
  forms <- list(
    Built ~ ef_001 + ef_002 + ef_003,
    Forest ~ ef_001 + ef_002,
    Other ~ ef_001 + ef_002
  )
  glm_models <- glmModels(formula = forms, family = binomial, data = train_data, obs = obs)
  list(part = part, glm_models = glm_models)
})
glm_models <- fit$glm_models
glm_models

#' Discrimination on the held-out 90 % of 1991 cells (lulcc's ROC tooling):

#| label: native-roc
test_data <- getPredictiveModelInputData(
  obs = obs,
  ef = ef_native,
  cells = fit$part[["test"]],
  t = 0
)
PerformanceList(pred = PredictionList(models = glm_models, newdata = test_data), measure = "rch")

#| label: native-allocate
#| output: false
native <- native_pipelines[native %in% c("lulcc-clues", "lulcc-ordered")]
allocate_both(
  ef = ef_native,
  models = glm_models,
  rules = matrix(data = 1, nrow = 3, ncol = 3, byrow = TRUE),
  seeds = list(
    clues = 10000L + native[native == "lulcc-clues", id_run + offset] + seq_len(n_realisations),
    ordered = 10000L + native[native == "lulcc-ordered", id_run + offset] + seq_len(n_realisations)
  ),
  dirs = list(
    clues = file.path(outputs_dir, "maps", "lulcc-clues"),
    ordered = file.path(outputs_dir, "maps", "lulcc-ordered")
  ),
  tag = "as shipped",
  ordered_stochastic = TRUE
)

#' # Result: the transitions of the last realisations

#| label: result
crosstab_last <- function(dir) {
  last <- tail(sort(list.files(dir, pattern = "^r[0-9]+\\.tif$")), 1)
  sim <- raster::raster(file.path(dir, last))
  table(anterior = raster::values(lu[[1]]), simulated = raster::values(sim))
}
for (est in estimators$estimator) {
  print(est)
  print(crosstab_last(external_maps_dir("clue-s", est)))
  print(crosstab_last(external_maps_dir("ordered", est)))
}
crosstab_last(file.path(outputs_dir, "maps", "lulcc-clues"))
crosstab_last(file.path(outputs_dir, "maps", "lulcc-ordered"))
