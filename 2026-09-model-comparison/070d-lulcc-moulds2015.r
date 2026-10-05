#' ---
#' title: "PIE benchmark: reproducing Moulds et al. (2015)"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' Diagnostic, not part of the benchmark protocol. Runs lulcc's own demo as published with the
#' GMD paper (`demo("gmd-paper", package = "lulcc")`): suitability GLMs fitted on a 10 % sample
#' of the *1985* map, demand interpolated 1985 → 1999 in annual steps, CLUE-S and Ordered
#' allocation over 14 years, validated against 1999 with `ThreeMapComparison`,
#' `AgreementBudget` and `FigureOfMerit` at resolutions of 2^1 … 2^8 cells.
#'
#' The paper shows the forest → built results only graphically (its Figs. 7 and 8); this step
#' prints the numbers behind them. It also reports figure of merit at native resolution with the
#' benchmark's own (evoland) definition, for comparison with `050-compare.r`. The demo is run
#' with several seeds, because the 10 % partition and the Ordered model are random and the paper
#' fixes no seed.

#| label: setup
#| output: false
suppressPackageStartupMessages(library(lulcc))
library(data.table)
source("2026-09-model-comparison/common.r")
data("pie", package = "lulcc")
# the shipped PROJ string (with +towgs84) no longer compares equal to the CRS current PROJ
# derives for lulcc's own output rasters ("different crs" in stack()); it is NAD83 /
# Massachusetts Mainland, so the equivalent EPSG code is set. Cell values are untouched.
for (n in names(pie)) raster::crs(pie[[n]]) <- "EPSG:26986"
seeds <- 1:5
# the paper's resolutions (2^1 ... 2^8 cells) plus the native one
factors <- c(1, 2^(1:8))

#| label: run
run_demo <- function(seed) {
  set.seed(seed)
  obs <- ObsLulcRasterStack(
    x = pie,
    pattern = "lu",
    categories = c(1, 2, 3),
    labels = c("Forest", "Built", "Other"),
    t = c(0, 6, 14)
  )
  ef <- ExpVarRasterList(x = pie, pattern = "ef")
  part <- partition(x = obs[[1]], size = 0.1, spatial = TRUE)
  train_data <- getPredictiveModelInputData(obs = obs, ef = ef, cells = part[["train"]])
  forms <- list(
    Built ~ ef_001 + ef_002 + ef_003,
    Forest ~ ef_001 + ef_002,
    Other ~ ef_001 + ef_002
  )
  glm_models <- suppressWarnings(glmModels(formula = forms, family = binomial, data = train_data, obs = obs))
  dmd <- approxExtrapDemand(obs = obs, tout = 0:14)
  clues <- allocate(CluesModel(
    obs = obs,
    ef = ef,
    models = glm_models,
    time = 0:14,
    demand = dmd,
    elas = c(0.2, 0.2, 0.2),
    rules = matrix(data = 1, nrow = 3, ncol = 3, byrow = TRUE),
    params = list(jitter.f = 0.0002, scale.f = 0.000001, max.iter = 1000, max.diff = 50, ave.diff = 50)
  ))
  ordered <- allocate(
    OrderedModel(obs = obs, ef = ef, models = glm_models, time = 0:14, demand = dmd, order = c(2, 1, 3)),
    stochastic = TRUE
  )
  rbindlist(lapply(list(`CLUE-S` = clues, Ordered = ordered), function(model) {
    tabs <- ThreeMapComparison(x = model, factors = factors, timestep = 14)
    fom <- FigureOfMerit(x = tabs)
    # forest -> built at each resolution, and overall
    data.table(
      factor = factors,
      fom_forest_built = vapply(fom@transition, function(m) m[1, 2], numeric(1)),
      fom_overall = unlist(fom@overall)
    )
  }), idcol = "model")[, seed := seed][]
}
results <- rbindlist(lapply(seeds, run_demo))

#| label: summary
summary_dt <- results[,
  .(
    fom_forest_built = mean(fom_forest_built),
    fom_forest_built_sd = sd(fom_forest_built),
    fom_overall = mean(fom_overall)
  ),
  by = .(model, factor)
]
summary_dt
fwrite(results, file.path(pie_dir, "figures", "moulds2015-reproduction.csv"))
