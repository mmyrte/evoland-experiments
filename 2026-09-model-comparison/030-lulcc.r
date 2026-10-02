#' ---
#' title: "PIE benchmark: lulcc (GLM suitability, CLUE-S and Ordered allocation)"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' lulcc 1.0.4 (CRAN), configured as in the demo that reproduces Moulds et al. (2015)
#' (`demo("gmd-paper", package = "lulcc")`) on the same PIE data, with the backcast protocol
#' of this benchmark:
#'
#' - **Suitability**: one binomial GLM per class, fitted on a 10 % spatially stratified sample
#'   of the *1991* map, with the paper's formulas (built on all three factors; forest and other
#'   on elevation and slope). This is the CLUE paradigm: the models estimate where each class
#'   *is*, not where it *changes to*. Nothing from 1999 enters the fit.
#' - **Demand**: lulcc's `approxExtrapDemand()` interpolates the class totals linearly between
#'   1991 and 1999 in annual steps; the end point is the observed 1999 total of every class,
#'   the same quantities the other tools get as transition counts.
#' - **Allocation**: 8 annual steps from 1991, with CLUE-S (Verburg et al. 2002) and the
#'   Ordered procedure (Fuchs et al. 2013), with the paper's parameters. The Ordered model is
#'   stochastic; CLUE-S jitters suitabilities by a random amount, so both are repeated with
#'   seeds.
#'
#' The comparison takes the 1999 map of each run. Note that lulcc allocates class totals, not
#' transitions: the transition counts it produces need not match the observed ones.

#| label: setup
#| output: false
suppressPackageStartupMessages(library(lulcc))
library(data.table)
source("2026-09-model-comparison/common.r")
reset_timings("lulcc")

lu <- raster::stack(lapply(c(1991, 1999), \(y) raster::raster(file.path(data_dir, paste0("lu_", y, ".tif")))))
names(lu) <- c("lu_1991", "lu_1999")
ef_stack <- raster::stack(file.path(data_dir, paste0("ef_00", 1:3, ".tif")))
names(ef_stack) <- paste0("ef_00", 1:3)

maps_dirs <- file.path(outputs_dir, "maps", c("lulcc-clues", "lulcc-ordered"))
for (d in maps_dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

#' # Inputs

#| label: inputs
obs <- ObsLulcRasterStack(
  x = lu,
  pattern = "lu",
  categories = c(1, 2, 3),
  labels = c("Forest", "Built", "Other"),
  t = c(0, 8)
)
ef <- ExpVarRasterList(x = ef_stack, pattern = "ef")
obs
crossTabulate(obs, times = c(0, 8))

#' # Suitability models on the 1991 map

#| label: fit
set.seed(1991)
fit <- timed("lulcc", "fit_glm", {
  part <- partition(x = obs[[1]], size = 0.1, spatial = TRUE)
  train_data <- getPredictiveModelInputData(obs = obs, ef = ef, cells = part[["train"]], t = 0)
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

#| label: roc
test_data <- getPredictiveModelInputData(obs = obs, ef = ef, cells = fit$part[["test"]], t = 0)
glm_perf <- PerformanceList(pred = PredictionList(models = glm_models, newdata = test_data), measure = "rch")
glm_perf

#| label: suitability-maps
#| fig-asp: 0.35
all_data <- as.data.frame(x = ef, cells = fit$part[["all"]])
probmaps <- predict(object = glm_models, newdata = all_data, data.frame = TRUE)
probmaps <- probmaps[, c("Forest", "Built", "Other")] # band k = class k, read by 040-cross-clumpy
points <- raster::rasterToPoints(obs[[1]], spatial = TRUE)
suitability <- raster::rasterize(
  x = sp::SpatialPointsDataFrame(points, probmaps),
  y = obs[[1]],
  field = names(probmaps)
)
raster::writeRaster(
  suitability,
  file.path(outputs_dir, "maps", "lulcc-suitability.tif"),
  overwrite = TRUE
)
raster::plot(suitability, nc = 3)

#' # Demand

#| label: demand
dmd <- approxExtrapDemand(obs = obs, tout = 0:8)
dmd

#' # Allocation

#| label: allocate
#| output: false
clues_parms <- list(jitter.f = 0.0002, scale.f = 0.000001, max.iter = 1000, max.diff = 50, ave.diff = 50)
write_final <- function(model, dir, i) {
  final <- model@output[[raster::nlayers(model@output)]]
  raster::writeRaster(
    final,
    file.path(dir, sprintf("r%02d.tif", i)),
    datatype = "INT1U",
    NAflag = 255,
    overwrite = TRUE
  )
}
for (i in seq_len(n_realisations)) {
  set.seed(20000 + i)
  clues_model <- CluesModel(
    obs = obs,
    ef = ef,
    models = glm_models,
    time = 0:8,
    demand = dmd,
    elas = c(0.2, 0.2, 0.2),
    rules = matrix(data = 1, nrow = 3, ncol = 3, byrow = TRUE),
    params = clues_parms
  )
  clues_model <- timed("lulcc", "allocate_clues", allocate(clues_model), note = paste0("realisation=", i))
  write_final(clues_model, maps_dirs[1], i)

  set.seed(30000 + i)
  ordered_model <- OrderedModel(
    obs = obs,
    ef = ef,
    models = glm_models,
    time = 0:8,
    demand = dmd,
    order = c(2, 1, 3)
  )
  ordered_model <- timed(
    "lulcc",
    "allocate_ordered",
    allocate(ordered_model, stochastic = TRUE),
    note = paste0("realisation=", i)
  )
  write_final(ordered_model, maps_dirs[2], i)
}

#' # Result of the last realisations

#| label: result
#| fig-asp: 0.35
final_maps <- raster::stack(
  raster::raster(file.path(maps_dirs[1], sprintf("r%02d.tif", n_realisations))),
  raster::raster(file.path(maps_dirs[2], sprintf("r%02d.tif", n_realisations)))
)
names(final_maps) <- c("CLUE-S", "Ordered")
raster::plot(final_maps, col = lulc_colours$color, breaks = 0:3 + 0.5)
table(anterior = raster::values(lu[[1]]), clues = raster::values(final_maps[[1]]))
table(anterior = raster::values(lu[[1]]), ordered = raster::values(final_maps[[2]]))
