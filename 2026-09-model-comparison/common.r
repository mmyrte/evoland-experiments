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
# PIE_LEARNERS restricts the evoland estimators, e.g. to "log_reg" for the scaling runs
pie_learners <- strsplit(Sys.getenv("PIE_LEARNERS", "log_reg,ranger"), ",")[[1]]
dir.create(outputs_dir, showWarnings = FALSE, recursive = TRUE)

lulc_classes <- c(forest = 1L, built = 2L, other = 3L)
lulc_colours <- data.frame(value = 1:3, color = c("#91B690", "#5D5D8C", "#EB9486"))

# evoland periods: 1 = 1985, 2 = 1991, 3 = 1999 (held out)
id_period_by_year <- c("1985" = 1L, "1991" = 2L, "1999" = 3L)

# Runs in the evoland database. Estimators are parent runs; each allocation realisation is a
# child of its estimator. External tools write maps that 050-compare imports as runs.
id_run_observed <- 1L
runs_registry <- data.table::data.table(
  id_run = c(1000L, 2000L, 3000L, 4000L, 4100L, 5000L),
  tool = c("evoland", "evoland", "dinamica-native", "lulcc", "lulcc", "cluinpy"),
  estimator = c("log_reg", "ranger", "woe", "glm", "glm", "log_reg"),
  allocator = c(NA, NA, "dinamica", "clue-s", "ordered", "clumondo")
)
# PIE_N_REALISATIONS lowers the count for quick tests
n_realisations <- as.integer(Sys.getenv("PIE_N_REALISATIONS", "20"))

# Wall time of each step, one CSV per tool so that 050-compare can tabulate them; a step
# calls reset_timings() first, so a rerun replaces its earlier timings
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
