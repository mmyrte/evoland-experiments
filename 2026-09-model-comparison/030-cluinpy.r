#' ---
#' title: "PIE benchmark: CLUinPy (logistic suitability, CLUMondo allocation)"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' [CLUinPy](https://github.com/nomisthomsen/CLUinPy) is a Python re-implementation of
#' CLUMondo (van Asselen & Verburg 2013). The work happens in
#' [`030-cluinpy/run_pie.py`](030-cluinpy/run_pie.py); this step only calls it, so that the
#' pipeline runs from `execute-all.sh`.
#'
#' - **Suitability**: CLUinPy's own suitability module, logistic regression on the three
#'   factors (VIF filtering, stratified sample of 10 % per class, as in its tutorial), fitted
#'   on the *1991* map: like lulcc, the CLUE paradigm of where each class is.
#' - **Demand**: one service per class (its area, identity service and conversion matrices),
#'   interpolated linearly from the 1991 to the observed 1999 totals, allocated in 8 annual
#'   steps. Convergence tolerance 0.1 % of demand; the tutorial's 2 % leaves ~700 built cells
#'   unallocated on this case.
#' - **Parameters**: all conversions allowed; conversion resistance 0.8 (forest), 1 (built),
#'   0.6 (other) and a 3 × 3 neighbourhood weight of 0.3 for built land only, after the
#'   tutorial's values for comparable classes.
#' - CLUinPy draws its demand-adjustment speed from Python's `random`; each realisation seeds
#'   it.
#'
#' Needs `CLUINPY_REPO` (a clone of nomisthomsen/CLUinPy) and `CLUINPY_PYTHON` (a Python with
#' its requirements, GDAL bindings included).

#| label: run
source("2026-09-model-comparison/common.r")
cluinpy_repo <- Sys.getenv("CLUINPY_REPO")
cluinpy_python <- Sys.getenv("CLUINPY_PYTHON", "python3")
stopifnot("set CLUINPY_REPO to a clone of nomisthomsen/CLUinPy" = dir.exists(cluinpy_repo))
system2("git", c("-C", cluinpy_repo, "rev-parse", "HEAD"))
res <- processx::run(
  cluinpy_python,
  c(file.path(pie_dir, "030-cluinpy", "run_pie.py"), cluinpy_repo, n_realisations),
  env = c(
    "current",
    PYTHONPATH = paste(file.path(cluinpy_repo, "src"), file.path(cluinpy_repo, "src", "suitability"), sep = ":")
  ),
  echo = FALSE,
  error_on_status = TRUE
)
# the last year's convergence of the last realisation
tail(strsplit(res$stdout, "\n")[[1]], 3)

#| label: result
#| fig-asp: 0.4
terra::plot(terra::rast(file.path(outputs_dir, "maps", "cluinpy-suitability.tif")))
terra::plot(
  terra::rast(file.path(outputs_dir, "maps", "cluinpy", sprintf("r%02d.tif", n_realisations))),
  col = lulc_colours
)
