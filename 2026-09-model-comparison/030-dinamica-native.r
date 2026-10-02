#' ---
#' title: "PIE benchmark: Dinamica EGO standalone (Weights of Evidence + Expander/Patcher)"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' Dinamica EGO used the way its own documentation calibrates a land use change model,
#' headless through `DinamicaConsole`:
#'
#' 1. [`calibrate.ego`](030-dinamica-native/calibrate.ego): Weights of Evidence ranges and
#'    coefficients for each transition on 1985 → 1991. The variables are the three lulcc
#'    explanatory factors and the distance to each other class, which Dinamica recomputes from
#'    the anterior map in every step. Weights of Evidence is a naive Bayes model: each variable
#'    contributes an additive log-odds term per range, assuming conditional independence.
#' 2. [`simulate.ego`](030-dinamica-native/simulate.ego): probability maps from the weights
#'    on the 1991 map, then the observed 1991 → 1999 net rates allocated by Expander and
#'    Patcher (the `AllocateTransitions` submodel shipped with Dinamica, the same one evoland's
#'    `alloc_dinamica()` drives), in a single step.
#'    [`probabilities.ego`](030-dinamica-native/probabilities.ego) writes the probability maps
#'    alone, for the crossing in `040`: Expander and Patcher deplete the map in place, so a map
#'    saved after allocation is not the Weights of Evidence probability.
#'
#' Held fixed with the evoland runs: the viable transitions, the demand, and the patch
#' parameters and expander fractions, which are evoland's estimates on 1985 → 1991. Native
#' Dinamica practice sets these by hand, so taking the estimates isolates the estimator (Weights
#' of Evidence vs. evoland's learners) from the allocator (Dinamica, in both cases).

#| label: setup
#| output: false
library(evoland)
library(data.table)
source("2026-09-model-comparison/common.r")
reset_timings("dinamica-native")

model_dir <- normalizePath(file.path(pie_dir, "030-dinamica-native"))
work_dir <- file.path(normalizePath(outputs_dir), "dinamica-native-work")
maps_dir <- file.path(outputs_dir, "maps", "dinamica-native")
unlink(work_dir, recursive = TRUE)
dir.create(work_dir, recursive = TRUE)
dir.create(maps_dir, recursive = TRUE, showWarnings = FALSE)

file.copy(
  file.path(data_dir, c(paste0("lu_", c(1985, 1991), ".tif"), paste0("ef_00", 1:3, ".tif"))),
  work_dir
)
file.copy(file.path(model_dir, c("calibrate.ego", "probabilities.ego", "simulate.ego")), work_dir)

# Dinamica 8.11.2's CalcWOfEProbabilityMap is not deterministic when run in parallel: between
# identical runs, the probabilities of the forest transitions differed in 300-1300 cells (some
# turning NA), while calibration and distance maps were identical. Single-threaded runs are
# bit-identical, so every script here runs single-threaded.
run_dinamica_serial <- function(script) {
  exec_dinamica(
    file.path(work_dir, script),
    disable_parallel = TRUE,
    additional_args = c("-processors=1", "-disable-parallel-functors", "-disable-parallel-map-load")
  )
}

#' # Tables: demand and patch parameters

#| label: tables
db <- evoland_db$new(path = db_path)
db$id_run <- 0L
viable <- db$trans_meta_t[is_viable == TRUE][order(id_lulc_anterior, id_lulc_posterior)]
demand <- fread(file.path(outputs_dir, "demand_transitions_1991_1999.csv"))
n_anterior <- fread(file.path(outputs_dir, "demand_class_totals.csv"))[year == 1991]

trans_rates <- viable[, .(id_lulc_anterior, id_lulc_posterior)][
  demand,
  on = .(id_lulc_anterior, id_lulc_posterior),
  nomatch = NULL
][n_anterior, on = .(id_lulc_anterior = id_lulc), nomatch = NULL][
  order(id_lulc_anterior, id_lulc_posterior),
  .(`From*` = id_lulc_anterior, `To*` = id_lulc_posterior, Rate = count / N)
]
alloc_params <- db$alloc_params_t[viable, on = "id_trans"][order(
  id_lulc_anterior,
  id_lulc_posterior
)]
expansion <- alloc_params[, .(
  `From*` = id_lulc_anterior,
  `To*` = id_lulc_posterior,
  Frac_expander = pmax(1e-6, pmin(1 - 1e-6, frac_expander))
)]
patcher <- alloc_params[, .(
  `From*` = id_lulc_anterior,
  `To*` = id_lulc_posterior,
  Mean_Patch_Size = mean_patch_size,
  Patch_Size_Variance = patch_size_variance,
  Patch_Isometry = patch_isometry
)]
fwrite(trans_rates, file.path(work_dir, "trans_rates.csv"))
fwrite(expansion, file.path(work_dir, "expansion_table.csv"))
fwrite(patcher, file.path(work_dir, "patcher_table.csv"))
trans_rates
expansion
patcher

#' # Calibration

#| label: calibrate
invisible(timed(
  "dinamica-native",
  "calibrate",
  run_dinamica_serial("calibrate.ego")
))
invisible(file.copy(
  file.path(work_dir, c("weights.dcf", "ranges.dcf", "weights_report.csv", "weights_correlation.csv")),
  maps_dir,
  overwrite = TRUE
))
weights_report <- fread(file.path(work_dir, "weights_report.csv"))
setnames(weights_report, gsub("[* ]", "", names(weights_report)))
weights_report[, .(
  n_ranges = .N,
  n_significant = sum(Significant),
  max_abs_weight = max(abs(Weight_Coefficient))
), by = .(Transition_From, Transition_To, Variable)]

#' # Allocation, one realisation per run
#'
#' Dinamica draws a fresh random seed in every run unless `-predefined-seed` is given, which
#' would make all realisations identical.

#| label: simulate
#| output: false
for (i in seq_len(n_realisations)) {
  timed(
    "dinamica-native",
    "allocate",
    run_dinamica_serial("simulate.ego"),
    note = paste0("realisation=", i)
  )
  file.copy(
    file.path(work_dir, "posterior.tif"),
    file.path(maps_dir, sprintf("r%02d.tif", i)),
    overwrite = TRUE
  )
}
# the probability maps on their own: AllocateTransitions depletes them in place
invisible(run_dinamica_serial("probabilities.ego"))
file.copy(file.path(work_dir, "probabilities.tif"), maps_dir, overwrite = TRUE)

#| label: plot
#| fig-asp: 0.6
terra::plot(terra::rast(file.path(maps_dir, "probabilities.tif")))
terra::plot(terra::rast(file.path(maps_dir, "r01.tif")), col = lulc_colours)
