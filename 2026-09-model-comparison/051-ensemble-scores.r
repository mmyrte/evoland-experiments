#' ---
#' title: "PIE benchmark: ensemble-level scores"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' The figure of merit scores one map at a time and rewards allocators that concentrate change
#' on the most probable cells (`050-compare.r`, finding 3 in the README). Here each ensemble is
#' scored as a probabilistic forecast instead, with evoland's `brier_score_v()`. Per cell, the
#' forecast probability of ending in class k is the share of the ensemble's realisations (20, or
#' 1 for the deterministic greedy and Ordered allocators) that put the cell in k. The **Brier
#' score** is multi-category (summed over the three classes) and averaged over all cells. With
#' 20 members the forecast probabilities are coarse, which penalises an ensemble for its finite
#' size; the **fair Brier score** (Ferro 2014) subtracts that expected penalty and scores the
#' ensemble as if it had infinitely many members. The **skill scores** are relative to the
#' uniform forecast, random allocation of the same quantities: each cell forecasts the observed
#' transition shares of its anterior class.
#'
#' Every ensemble is a set of runs in the evoland database, those of the tools outside it
#' (lulcc, CLUinPy, standalone Dinamica) imported by `050-compare.r`, so all are scored by the
#' same query. All cells are scored (`changeable_only = FALSE`), including the 8 cells of built →
#' forest, a transition that no tool models.
#'
#' A deterministic tool gives probabilities of 0 or 1 everywhere, so a single wrong cell costs
#' it the full penalty; that is the point of scoring ensembles this way.

#| label: setup
#| output: false
library(evoland.plus)
library(data.table)
source("2026-09-model-comparison/common.r")
db <- evoland_db$new(path = db_path)
fom <- fread(file.path(figures_dir, "pie-fom-per-run.csv"))
members <- unique(fom[, .(id_run, ensemble)])

#| label: brier
brier <-
  db$brier_score_v(
    id_period_anterior = 2L,
    id_period_post = 3L,
    id_run_reference = id_run_observed,
    ensembles = split(members$id_run, members$ensemble),
    changeable_only = FALSE
  )[,
    .(
      ensemble,
      n_members,
      n_cells,
      brier,
      brier_fair,
      brier_reference,
      brier_skill = skill,
      brier_fair_skill = skill_fair
    )
  ][
    order(-brier_fair_skill)
  ]
brier

#| label: write
summary_dt <- fread(file.path(figures_dir, "pie-ensemble-summary.csv"))
out <- brier[summary_dt[, .(ensemble, figure_of_merit)], on = "ensemble"]
fwrite(out, file.path(figures_dir, "pie-ensemble-brier.csv"))
out
