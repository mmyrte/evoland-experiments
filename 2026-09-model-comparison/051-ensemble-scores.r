#' ---
#' title: "PIE benchmark: ensemble-level scores"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' The figure of merit scores one map at a time and rewards allocators that concentrate change
#' on the most probable cells (`050-compare.r`, finding 3 in the README). Here each ensemble is
#' scored as a probabilistic forecast instead. Per cell, the forecast probability of ending in
#' class k is the share of the ensemble's realisations (20, or 1 for the deterministic greedy
#' allocator) that put the cell in k. The **Brier score** is
#' multi-category (summed over the three classes) and averaged over all cells. With 20 members
#' the forecast probabilities are coarse, which penalises an ensemble for its finite size; the
#' **fair Brier score** (Ferro 2014) subtracts that expected penalty, p (1 - p) / (m - 1) per
#' class, and scores the ensemble as if it had infinitely many members. The **skill scores** are
#' relative to random allocation of the same quantities, where the probability of each
#' transition is its observed rate within the anterior class.
#'
#' A deterministic tool gives probabilities of 0 or 1 everywhere, so a single wrong cell costs
#' it the full penalty; that is the point of scoring ensembles this way.

#| label: setup
#| output: false
library(evoland)
library(data.table)
source("2026-09-model-comparison/common.r")
db <- evoland_db$new(path = db_path)
figures_dir <- file.path(pie_dir, "figures")
fom <- fread(file.path(figures_dir, "pie-fom-per-run.csv"))
members <- unique(fom[, .(id_run, ensemble)])

#| label: data
db$id_run <- NULL
anterior <- db$fetch("lulc_data_t", where = "id_run = 0 and id_period = 2")[,
  .(id_coord, anterior = id_lulc)
]
observed <- db$fetch("lulc_data_t", where = glue::glue("id_run = {id_run_observed} and id_period = 3"))[,
  .(id_coord, observed = id_lulc)
]
simulated <- db$fetch("lulc_data_t", where = glue::glue(
  "id_period = 3 and id_run in ({paste(members$id_run, collapse = ',')})"
))[members, on = "id_run"][, .(ensemble, id_coord, simulated = id_lulc)]
cells <- anterior[observed, on = "id_coord"]

#| label: brier
# forecast probabilities per ensemble, cell and class
ensemble_size <- members[, .(m = .N), by = ensemble]
forecast <- simulated[, .(n = .N), by = .(ensemble, id_coord, class = simulated)][
  ensemble_size,
  on = "ensemble"
][, .(ensemble, id_coord, class, p = n / m)]
grid <- CJ(ensemble = unique(members$ensemble), id_coord = cells$id_coord, class = 1:3)
forecast <- forecast[grid, on = .(ensemble, id_coord, class)][is.na(p), p := 0]
forecast <- forecast[cells, on = "id_coord"][, o := as.numeric(observed == class)]

# random allocation of the same quantities: transition rates within each anterior class
rates <- cells[, .(n = .N), by = .(anterior, observed)][, p := n / sum(n), by = anterior]
reference <- CJ(id_coord = cells$id_coord, class = 1:3)[cells, on = "id_coord"][
  rates[, .(anterior, class = observed, p)],
  on = .(anterior, class)
][is.na(p), p := 0][, o := as.numeric(observed == class)]
brier_reference <- reference[, sum((p - o)^2) / uniqueN(id_coord)]

# a single deterministic map (m = 1) has p in {0, 1}, so its finite-size correction is 0
brier <- forecast[ensemble_size, on = "ensemble"][,
  .(
    brier = sum((p - o)^2) / uniqueN(id_coord),
    brier_fair = sum((p - o)^2 - data.table::fifelse(m > 1, p * (1 - p) / (m - 1), 0)) /
      uniqueN(id_coord)
  ),
  by = ensemble
][, `:=`(
  brier_skill = 1 - brier / brier_reference,
  brier_fair_skill = 1 - brier_fair / brier_reference
)][order(-brier_fair_skill)]
brier_reference
brier

#| label: write
summary_dt <- fread(file.path(figures_dir, "pie-ensemble-summary.csv"))
out <- brier[summary_dt[, .(ensemble, figure_of_merit)], on = "ensemble"]
fwrite(out, file.path(figures_dir, "pie-ensemble-brier.csv"))
out
