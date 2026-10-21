#' ---
#' title: "PIE benchmark: CLUinPy's CLUMondo allocation of every estimator's surface"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' [CLUinPy](https://github.com/nomisthomsen/CLUinPy) is a Python re-implementation of
#' CLUMondo (van Asselen & Verburg 2013). The work happens in
#' [`040-alloc-cluinpy/run_pie.py`](040-alloc-cluinpy/run_pie.py); this step only calls it, so that the
#' pipeline runs from `execute-all.sh`.
#'
#' - **Suitability, the crossing**: each estimator's class probability map from
#'   `020-potentials.r`, as CLUinPy's suitability stack, with only the viable conversions
#'   allowed (`010`).
#' - **Suitability, as shipped** (reference, runs 8601...): CLUinPy's own suitability module,
#'   logistic regression on the three factors (VIF filtering, stratified sample of 10 % per
#'   class, as in its tutorial), fitted on the *1991* map, with all conversions allowed: like
#'   lulcc, the CLUE paradigm of where each class is.
#' - **Demand**: one service per class (its area, identity service and conversion matrices),
#'   interpolated linearly from the 1991 to the observed 1999 totals, allocated in 8 annual
#'   steps. Convergence tolerance 0.1 % of demand as shipped (the tutorial's 2 % leaves ~700
#'   built cells unallocated on this case) and 0.5 % in the crossing: on the Weights of Evidence
#'   surface, binned into ranges so that many cells tie, a small change in elasticity flips a
#'   whole group of cells, and the loop oscillates around 0.25 % in 1996 without reaching 0.1 %.
#'   The same tolerance holds for every estimator; the quantity disagreement in `050` shows the
#'   cost (up to ~200 cells of a class).
#' - **Parameters**, the same for both: conversion resistance 0.8 (forest), 1 (built),
#'   0.6 (other) and a 3 × 3 neighbourhood weight of 0.3 for built land only, after the
#'   tutorial's values for comparable classes.
#' - CLUinPy draws its demand-adjustment speed from Python's `random`; each realisation seeds
#'   it. That speed is CLUinPy's only randomness: it changes where within the tolerance the
#'   demand loop stops. As shipped (tolerance 0.1 %), the 20 realisations differ by one cell at most; in the
#'   crossing (0.5 %) they differ by 10–220 cells.
#' - **Iterations.** CLUMondo raises a class's demand elasticity by the relative demand gap per
#'   iteration, about 1 % of the class total per year here, in shrinking steps. With the
#'   crossing's class probabilities (about 0.97 for the current class, a few per cent for any
#'   change), built land needs an elasticity of about 1.3 before it outbids forest and its
#'   conversion resistance: some 11 000 iterations in 1992, against CLUinPy's 3 000 in the
#'   tutorial. The limit is 30 000; later years start from the previous year's elasticities and
#'   converge within a few hundred. CLUinPy's own suitabilities are presence probabilities of
#'   similar size for every class, which is why the as-shipped run converges fast.
#'
#' Needs `CLUINPY_REPO` (a clone of nomisthomsen/CLUinPy) and `CLUINPY_PYTHON` (a Python with
#' its requirements, GDAL bindings included).

#| label: run
source("2026-09-model-comparison/common.r")
cluinpy_repo <- Sys.getenv("CLUINPY_REPO")
cluinpy_python <- Sys.getenv("CLUINPY_PYTHON", "python3")
stopifnot("set CLUINPY_REPO to a clone of nomisthomsen/CLUinPy" = dir.exists(cluinpy_repo))
system2("git", c("-C", cluinpy_repo, "rev-parse", "HEAD"))
reset_timings("cluinpy")
run_cluinpy <- function(n, ...) {
  processx::run(
    cluinpy_python,
    c(file.path(pie_dir, "040-alloc-cluinpy", "run_pie.py"), cluinpy_repo, n, ...),
    env = c(
      "current",
      PYTHONPATH = paste(
        file.path(cluinpy_repo, "src"),
        file.path(cluinpy_repo, "src", "suitability"),
        sep = ":"
      )
    ),
    echo = FALSE,
    error_on_status = TRUE
  )
}
members <- crossing_members()[allocator == "clumondo"]
for (est in estimators$estimator) {
  res <- run_cluinpy(
    members[estimator == est, .N],
    "crossing",
    file.path(outputs_dir, "suitability", paste0(est, ".tif")),
    external_maps_dir("clumondo", est),
    # realisation i is seeded seed_base + i
    members[estimator == est & member == 1L, seed] - 1L
  )
}
native <- native_pipelines[native == "cluinpy"]
res <- run_cluinpy(
  native$n_members,
  "native",
  "",
  file.path(outputs_dir, "maps", "cluinpy"),
  10000L + native$id_run + native$offset
)
# the last year's convergence of the last realisation
tail(strsplit(res$stdout, "\n")[[1]], 3)

#| label: result
#| fig-asp: 0.4
terra::plot(terra::rast(file.path(outputs_dir, "maps", "cluinpy-suitability.tif")))
for (est in estimators$estimator) {
  terra::plot(
    terra::rast(file.path(
      external_maps_dir("clumondo", est),
      sprintf("r%02d.tif", n_realisations)
    )),
    col = lulc_colours,
    main = est
  )
}
