#' ---
#' title: "Fig. 2 case: validation metrics compared"
#' date: last-modified
#' ---
#'
#' Scores every map of `010-fig2-ensembles.r` (100 realisations each of CLUMPY, Dinamica,
#' CLUE-S and CLUMondo on the random-forest potentials, and the rank-and-fill map) with the
#' validation metrics of `validation-metrics.r`, which documents each metric. It reads only
#' `results/fig2/` and writes there:
#'
#' | file | content |
#' | --- | --- |
#' | `metrics-per-run.csv` | one row per map: figure of merit and components, quantity and allocation disagreement, KSim (KTransition, KTransloc), Fuzzy Kappa Simulation at radius 2 and 4 cells, similarity of differences, TOC AUC (any change, per transition), Brier score; and how concentrated its change is (`top5_truth`, `top5_potential`: share of the simulated change in the top 5 % of the true probabilities and of the potentials, among the cells that could make the same transition) |
#' | `metrics-forecast.csv` | each ensemble's class frequencies, the potentials, the true probabilities and the uniform forecast as probabilistic forecasts: TOC AUC, (fair) Brier score and skill |
#' | `toc.csv` | TOC curves ("any change") of those forecasts |
#'
#' `030-metric-relations-plot.r` draws them.

#| label: setup
#| output: false
source("validation-metrics.r")

res_dir <- "2026-09-paper-figures/results/fig2"
cells <- as.data.table(arrow::read_parquet(file.path(res_dir, "cells.parquet")))
runs <- fread(file.path(res_dir, "runs.csv"))
changes <- as.data.table(arrow::read_parquet(file.path(res_dir, "changes.parquet")))
potentials <- as.data.table(arrow::read_parquet(file.path(res_dir, "potentials.parquet")))
truth <- as.data.table(arrow::read_parquet(file.path(res_dir, "truth.parquet")))
viable <- fread(file.path(res_dir, "viable.csv"))

n_classes <- 4L
grid <- vm_grid(cells$id_coord, cells$row, cells$col)
anterior <- cells$anterior
observed <- cells$observed
eligible <- anterior %in% viable$from
pos <- setNames(seq_len(nrow(cells)), cells$id_coord)

simulated_of <- function(run) {
  s <- anterior
  ch <- changes[id_run == run]
  s[pos[as.character(ch$id_coord)]] <- ch$simulated
  s
}
sims <- lapply(runs$id_run, simulated_of)
names(sims) <- runs$id_run

#' # Single maps

#| label: per-run
t0 <- Sys.time()
per_run <- score_maps(grid, anterior, observed, sims, viable, eligible, n_classes)
message("scored ", length(sims), " maps in ", format(Sys.time() - t0, digits = 3))
per_run <- cbind(runs, per_run)

#' How concentrated is each map's change? For every changed cell, the percentile of its
#' probability among the cells that could make the same transition, by the true probabilities
#' and by the potentials the allocator was given. An unbiased sampler of the truth puts 36 % of
#' its change in the top 5 % of the true probabilities (`010-fig2-ensembles`).

#| label: concentration
percentile_of <- function(prob) {
  # prob: id_coord, class (posterior), p; ranked within the transition's candidate cells
  pr <- cells[, .(id_coord, anterior)][prob, on = "id_coord", nomatch = NULL][anterior != class]
  pr[, pct := frank(p, ties.method = "average") / .N, by = .(anterior, class)][, .(id_coord, class, pct)]
}
pct_truth <- percentile_of(truth[, .(id_coord, class, p = q)])
pct_pot <- percentile_of(potentials)
concentration <- changes[, .(id_run, id_coord, class = simulated)][
  pct_truth,
  on = .(id_coord, class),
  nomatch = NULL
][pct_pot, on = .(id_coord, class), pct_pot := i.pct][,
  .(top5_truth = mean(pct > 0.95), top5_potential = mean(pct_pot > 0.95, na.rm = TRUE)),
  by = id_run
]
per_run <- concentration[per_run, on = "id_run"]
# what an unbiased sampler of the potentials puts in their top 5 %: the potentials' mass there
pot_pct <- cells[, .(id_coord, anterior)][potentials, on = "id_coord", nomatch = NULL][
  pct_pot,
  on = .(id_coord, class)
]
per_run[, top5_expected := pot_pct[, sum(p[pct > 0.95]) / sum(p)]]
obs_top5 <- cells[anterior != observed, .(id_coord, class = observed)][
  pct_truth,
  on = .(id_coord, class),
  nomatch = NULL
][, mean(pct > 0.95)]
obs_top5

# the observed map against itself, as the reference row for "how concentrated is the truth"
per_run <- rbind(
  per_run,
  data.table(id_run = 0L, allocator = "observed", member = NA_integer_, top5_truth = obs_top5),
  fill = TRUE
)
fwrite(per_run, file.path(res_dir, "metrics-per-run.csv"))

#| label: per-run-summary
summary_cols <- c(
  "figure_of_merit",
  "ksim",
  "ktransloc",
  "fks_r2",
  "fks_r4",
  "similarity_of_differences",
  "allocation_disagreement",
  "auc_change",
  "auc_transition",
  "brier",
  "top5_truth"
)
knitr::kable(
  per_run[allocator != "observed", lapply(.SD, median), by = allocator, .SDcols = summary_cols],
  digits = 3
)

#' # Probabilistic forecasts

#| label: forecasts
with_persistence <- function(prob) {
  # prob: id_coord, class, p of leaving the anterior class; persistence takes the remainder
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

forecasts <- c(
  lapply(
    setNames(
      c("CLUMPY", "uPAM", "Dinamica", "CLUE-S", "CLUMondo"),
      paste(c("CLUMPY", "uPAM", "Dinamica", "CLUE-S", "CLUMondo"), "ensemble")
    ),
    function(a) {
      list(p = class_frequencies(sims[as.character(runs[allocator == a, id_run])], n_classes), n = sum(runs$allocator == a))
    }
  ),
  list(
    "adjusted potentials" = list(p = with_persistence(potentials), n = NA_integer_),
    "true probabilities" = list(p = with_persistence(truth[, .(id_coord, class, p = q)]), n = NA_integer_),
    "uniform forecast" = list(p = with_persistence(uniform), n = NA_integer_)
  )
)
forecast_scores <- rbindlist(
  lapply(names(forecasts), function(f) {
    cbind(
      forecast = f,
      score_forecast(anterior, observed, forecasts[[f]]$p, viable, eligible, forecasts[[f]]$n)
    )
  })
)
forecast_scores[, brier_skill := 1 - brier / forecast_scores[forecast == "uniform forecast", brier]]
knitr::kable(forecast_scores, digits = 3)
fwrite(forecast_scores, file.path(res_dir, "metrics-forecast.csv"))

toc <- rbindlist(lapply(names(forecasts), function(f) {
  p <- forecasts[[f]]$p
  p_change <- 1 - p[cbind(seq_len(nrow(cells)), anterior)]
  cbind(forecast = f, toc_curve(p_change[eligible], (anterior != observed)[eligible]))
}))
fwrite(toc, file.path(res_dir, "toc.csv"))
