#' ---
#' title: "PIE benchmark: validation metrics compared"
#' date: last-modified
#' ---
#'
#' Scores every simulated 1999 map of the crossing and of the tools as shipped with the
#' validation metrics of `validation-metrics.r` (repository root; it documents each metric):
#' figure of merit, quantity and allocation disagreement, Kappa Simulation (with KTransition and
#' KTransloc), Fuzzy Kappa Simulation (exponential decay, halving distance 2 cells, radius 2 and
#' 4 cells), similarity of differences (all viable transitions, weighted by observed change),
#' TOC AUC and Brier score; and every ensemble, every estimator's potentials and the uniform
#' forecast as probabilistic forecasts. Reads only `results/` (written by `050-compare.r`) and
#' writes there:
#'
#' | file | content |
#' | --- | --- |
#' | `metrics-per-run.csv` | one row per map; `top5_potential` is the share of its change in the top 5 % of its estimator's potentials (among the cells that could make the same transition), `top5_expected` what an unbiased sampler of those potentials puts there |
#' | `metrics-forecast.csv` | ensembles (class frequencies, fair Brier), potentials and the uniform forecast: TOC AUC, Brier score and skill |
#' | `metrics-variance-shares.csv` | two-way ANOVA of each metric over the estimators × stochastic allocators, as in `050` for the figure of merit |
#' | `toc.csv` | TOC curves ("any change") of the forecasts |
#'
#' `PIE_WORKERS` (default: all cores) sets the number of forked workers.

#| label: setup
#| output: false
source("validation-metrics.r")
source("2026-09-model-comparison/common.r")
res_dir <- file.path(pie_dir, paste0("results", scale_suffix))
cells <- as.data.table(arrow::read_parquet(file.path(res_dir, "cells.parquet")))
runs <- fread(file.path(res_dir, "runs.csv"))
changes <- as.data.table(arrow::read_parquet(file.path(res_dir, "changes.parquet")))
potentials <- as.data.table(arrow::read_parquet(file.path(res_dir, "potentials.parquet")))
viable <- fread(file.path(res_dir, "viable.csv"))
n_workers <- as.integer(Sys.getenv("PIE_WORKERS", parallel::detectCores()))

n_classes <- length(lulc_classes)
grid <- vm_grid(cells$id_coord, cells$row, cells$col)
anterior <- cells$anterior
observed <- cells$observed
eligible <- anterior %in% viable$from
pos <- setNames(seq_len(nrow(cells)), cells$id_coord)
setkey(changes, id_run)
simulated_of <- function(run) {
  s <- anterior
  ch <- changes[.(run), nomatch = NULL]
  s[pos[as.character(ch$id_coord)]] <- ch$simulated
  s
}

#' # Single maps

#| label: per-run
t0 <- Sys.time()
chunks <- split(runs$id_run, cut(seq_along(runs$id_run), n_workers * 4L, labels = FALSE))
per_run <- rbindlist(parallel::mclapply(
  chunks,
  function(ids) {
    cbind(
      id_run = ids,
      score_maps(grid, anterior, observed, lapply(ids, simulated_of), viable, eligible, n_classes)
    )
  },
  mc.cores = n_workers
))
message("scored ", nrow(per_run), " maps in ", format(Sys.time() - t0, digits = 3))
per_run <- runs[per_run, on = "id_run"]

#' # Concentration on the potentials
#'
#' For every changed cell, the percentile of its estimator's potential among the cells that
#' could make the same transition. An unbiased sampler of the potentials puts the share
#' `top5_expected` of its change in the top 5 %: the potentials' own mass there. More means the
#' allocator concentrates change beyond what its input implies. The as-shipped lulcc and
#' CLUinPy runs have their own estimators, not in `potentials.parquet`, and get no value.

#| label: concentration
pct <- cells[, .(id_coord, anterior)][potentials, on = "id_coord"][anterior != class]
pct[, pct := frank(p, ties.method = "average") / .N, by = .(id_run_parent, anterior, class)]
expected_top5 <- pct[, .(top5_expected = sum(p[pct > 0.95]) / sum(p)), by = id_run_parent]
concentration <- changes[, .(id_run, id_coord, class = simulated)][
  runs[, .(id_run, id_run_parent)],
  on = "id_run"
][
  pct[, .(id_run_parent, id_coord, class, pct)],
  on = .(id_run_parent, id_coord, class),
  nomatch = NULL
][, .(top5_potential = mean(pct > 0.95)), by = id_run]
per_run <- expected_top5[concentration[per_run, on = "id_run"], on = "id_run_parent"]
observed_top5 <- cells[anterior != observed, .(id_coord, class = observed)][
  pct,
  on = .(id_coord, class),
  nomatch = NULL
][, .(top5_observed = mean(pct > 0.95)), by = id_run_parent]
per_run <- observed_top5[per_run, on = "id_run_parent"]
fwrite(per_run, file.path(res_dir, "metrics-per-run.csv"))

#| label: per-run-summary
summary_cols <- c(
  "figure_of_merit",
  "ksim",
  "ktransloc",
  "fks_r4",
  "similarity_of_differences",
  "allocation_disagreement",
  "auc_transition",
  "brier",
  "top5_potential",
  "top5_expected"
)
knitr::kable(
  per_run[, lapply(.SD, mean), by = ensemble, .SDcols = summary_cols],
  digits = 3
)

#' # Does the variance decomposition depend on the metric?
#'
#' The two-way ANOVA of `050-compare.r` (estimators × CLUMPY, Dinamica, CLUE-S, CLUMondo, with
#' 20 replicates each), repeated for each metric.

#| label: anova
anova_metrics <- c(
  "figure_of_merit",
  "ksim",
  "fks_r2",
  "fks_r4",
  "similarity_of_differences",
  "allocation_disagreement",
  "auc_transition",
  "brier"
)
crossed <- per_run[as_shipped == FALSE & !allocator %in% c("greedy", "ordered")]
variance_shares <- rbindlist(lapply(anova_metrics, function(m) {
  ss <- summary(stats::aov(
    stats::as.formula(paste(m, "~ estimator_label * allocator_label")),
    data = crossed
  ))[[1]][["Sum Sq"]]
  data.table(
    metric = m,
    component = c("estimator", "allocator", "interaction", "replicate (stochastic)"),
    share = ss / sum(ss)
  )
}))
knitr::kable(dcast(variance_shares, metric ~ component, value.var = "share"), digits = 3)
fwrite(variance_shares, file.path(res_dir, "metrics-variance-shares.csv"))

#' # Probabilistic forecasts

#| label: forecasts
with_persistence <- function(prob) {
  m <- matrix(0, nrow(cells), n_classes)
  i <- pos[as.character(prob$id_coord)]
  keep <- prob$class != anterior[i]
  m[cbind(i[keep], prob$class[keep])] <- prob$p[keep]
  m[cbind(seq_len(nrow(cells)), anterior)] <- pmax(0, 1 - rowSums(m))
  m
}
obs_rates <- data.table(from = anterior, to = observed)[
  viable,
  on = .(from, to),
  nomatch = NULL
][, .N, by = .(from, to)][
  data.table(from = anterior)[, .(n_from = .N), by = from],
  on = "from",
  nomatch = NULL
]
uniform <- cells[, .(id_coord, from = anterior)][
  obs_rates,
  on = "from",
  allow.cartesian = TRUE,
  nomatch = NULL
][, .(id_coord, class = to, p = N / n_from)]

ensemble_ids <- split(runs$id_run, runs$ensemble)
forecasts <- c(
  lapply(ensemble_ids, function(ids) {
    list(
      p = class_frequencies(lapply(ids, simulated_of), n_classes),
      n = if (length(ids) > 1L) length(ids) else NA_integer_,
      kind = "ensemble"
    )
  }),
  setNames(
    lapply(estimators$id_run, function(id) {
      list(
        p = with_persistence(potentials[id_run_parent == id, .(id_coord, class, p)]),
        n = NA_integer_,
        kind = "potentials"
      )
    }),
    paste(estimators$label, "potentials")
  ),
  list("uniform forecast" = list(p = with_persistence(uniform), n = NA_integer_, kind = "reference"))
)
forecast_scores <- rbindlist(lapply(names(forecasts), function(f) {
  cbind(
    forecast = f,
    kind = forecasts[[f]]$kind,
    n_members = forecasts[[f]]$n,
    score_forecast(anterior, observed, forecasts[[f]]$p, viable, eligible, forecasts[[f]]$n)
  )
}))
forecast_scores[, brier_skill := 1 - brier / forecast_scores[forecast == "uniform forecast", brier]]
forecast_scores <- unique(runs[, .(forecast = ensemble, estimator_label, allocator_label, as_shipped)])[
  forecast_scores,
  on = "forecast"
]
knitr::kable(forecast_scores[order(-brier_skill)], digits = 3)
fwrite(forecast_scores, file.path(res_dir, "metrics-forecast.csv"))

toc <- rbindlist(lapply(names(forecasts), function(f) {
  p <- forecasts[[f]]$p
  p_change <- 1 - p[cbind(seq_len(nrow(cells)), anterior)]
  cbind(forecast = f, toc_curve(p_change[eligible], (anterior != observed)[eligible]))
}))
fwrite(toc, file.path(res_dir, "toc.csv"))
