# Shared by the numbered steps of the PIE benchmark; sourced, not rendered.

pie_dir <- "2026-09-model-comparison"
# PIE_SCALE = k > 1 tiles the PIE grid k x k (080-scaling); each scale has its own data, outputs
# and database, so the benchmark proper (k = 1) is never touched
# (an explicit PIE_SCALE=1 also gets its own directories)
pie_scale <- as.integer(Sys.getenv("PIE_SCALE", "1"))
scale_suffix <- if (nzchar(Sys.getenv("PIE_SCALE"))) paste0("-k", pie_scale) else ""
data_dir <- file.path(pie_dir, paste0("data", scale_suffix))
outputs_dir <- file.path(pie_dir, paste0("outputs", scale_suffix))
db_path <- file.path(pie_dir, paste0("pie", scale_suffix, ".evolanddb"))
dir.create(outputs_dir, showWarnings = FALSE, recursive = TRUE)

lulc_classes <- c(forest = 1L, built = 2L, other = 3L)
lulc_colours <- data.frame(value = 1:3, color = c("#91B690", "#5D5D8C", "#EB9486"))

# evoland periods: 1 = 1985, 2 = 1991, 3 = 1999 (held out)
id_period_by_year <- c("1985" = 1L, "1991" = 2L, "1999" = 3L)

# PIE_N_REALISATIONS lowers the count for quick tests
n_realisations <- as.integer(Sys.getenv("PIE_N_REALISATIONS", "20"))

# --- Runs: estimators crossed with allocators -------------------------------------------------
#
# Every estimator writes transition potentials into its own parent run below run 0, and every
# allocator reads them from there, so each estimator is crossed with each allocator. A
# realisation is run `estimator id_run + allocator offset + member`, a child of the estimator's
# run. The observed 1999 map is run 1, which no estimator or allocator reads.
id_run_observed <- 1L

estimators <- data.table::data.table(
  estimator = c("log_reg", "ranger", "woe", "naive_bayes"),
  id_run = c(1000L, 2000L, 3000L, 4000L),
  label = c("logistic regression", "random forest", "Weights of Evidence", "naive Bayes"),
  source = c(
    "evoland, mlr3 classif.log_reg",
    "evoland, mlr3 classif.ranger",
    "Dinamica EGO",
    "evoland, mlr3 classif.naive_bayes (e1071)"
  )
)
# PIE_ESTIMATORS restricts the estimators, e.g. to "log_reg" for the scaling runs
estimators <- estimators[
  estimator %in% strsplit(Sys.getenv("PIE_ESTIMATORS", "log_reg,ranger,woe,naive_bayes"), ",")[[1]]
]

allocators <- data.table::data.table(
  allocator = c("clumpy", "dinamica", "greedy", "clue-s", "ordered", "clumondo"),
  offset = c(100L, 200L, 300L, 400L, 500L, 600L),
  label = c("CLUMPY", "Dinamica", "greedy", "CLUE-S", "Ordered", "CLUMondo"),
  source = c("evoland", "Dinamica EGO via evoland", "evoland", "lulcc", "lulcc", "CLUinPy"),
  # one map each where the allocator is deterministic: greedy, and Ordered in the crossing (040)
  n_members = c(n_realisations, n_realisations, 1L, n_realisations, 1L, n_realisations)
)

# Each tool as shipped, with its own estimator, for reference only (not part of the crossing):
# lulcc and CLUinPy fit their own class-suitability models (logistic regressions on the three
# factors alone, the same family as evoland's log_reg), and the standalone Dinamica run
# allocates its Weights of Evidence probabilities with its own simulate.ego.
native_pipelines <- data.table::data.table(
  native = c("lulcc-clues", "lulcc-ordered", "cluinpy", "dinamica-native"),
  id_run = c(7000L, 7000L, 8000L, 3000L),
  offset = c(400L, 500L, 600L, 700L),
  n_members = n_realisations,
  estimator_label = c(
    "GLM (lulcc)",
    "GLM (lulcc)",
    "logistic regression (CLUinPy)",
    "Weights of Evidence"
  ),
  allocator_label = c("CLUE-S", "Ordered", "CLUMondo", "Dinamica (simulate.ego)")
)

# one row per realisation of every estimator x allocator pairing, with its run id and seed
crossing_members <- function() {
  crossed <- data.table::CJ(
    estimator = estimators$estimator,
    allocator = allocators$allocator,
    sorted = FALSE
  )
  crossed <- estimators[, .(estimator, id_run_parent = id_run, estimator_label = label)][
    crossed,
    on = "estimator"
  ]
  crossed <- allocators[, .(allocator, offset, n_members, allocator_label = label)][
    crossed,
    on = "allocator"
  ]
  members <- crossed[,
    .(member = seq_len(n_members)),
    by = .(estimator, allocator, id_run_parent, offset, estimator_label, allocator_label)
  ]
  members[, id_run := id_run_parent + offset + member]
  members[, seed := 10000L + id_run]
  members[]
}

# directory holding the maps of an allocation done outside the evoland database
external_maps_dir <- function(allocator, estimator) {
  file.path(outputs_dir, "maps", paste0(allocator, "-", estimator))
}

# --- Timings ----------------------------------------------------------------------------------
#
# Wall time of each step, one CSV per tool so that 050-compare can tabulate them; a step calls
# reset_timings() first, so a rerun replaces its earlier timings
timings_path <- function(tool) file.path(outputs_dir, paste0("timings-", tool, ".csv"))
reset_timings <- function(tool) unlink(timings_path(tool))
record_timing <- function(tool, stage, seconds, note = "") {
  path <- timings_path(tool)
  row <- data.frame(
    tool = tool,
    stage = stage,
    seconds = round(as.numeric(seconds), 2),
    note = note,
    host = Sys.info()[["nodename"]],
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )
  utils::write.table(
    row,
    path,
    sep = ",",
    append = file.exists(path),
    col.names = !file.exists(path),
    row.names = FALSE
  )
  invisible(row)
}

timed <- function(tool, stage, expr, note = "") {
  t0 <- Sys.time()
  out <- force(expr)
  record_timing(tool, stage, difftime(Sys.time(), t0, units = "secs"), note)
  out
}

read_lu <- function(year) terra::rast(file.path(data_dir, paste0("lu_", year, ".tif")))
