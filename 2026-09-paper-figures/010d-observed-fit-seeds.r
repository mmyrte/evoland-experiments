#' ---
#' title: "Diagnostic: does the observed change of Fig. 2 fit the true process too well?"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' `020-verify-placement.r` found that the observed change of Fig. 2 (period 2 -> 3) is closer
#' to the distribution its true probabilities imply than any of 2000 simulated draws of the
#' process (Kolmogorov-Smirnov D = 0.013, null median 0.031). Either the seed of Fig. 2
#' (`set.seed(1337)`) happens to give an unusually typical draw, or something makes the observed
#' change systematically closer to the truth than independent draws are (a bug in the process or
#' in how the truth is computed).
#'
#' This step separates the two. It generates the synthetic landscape of Fig. 2 for many seeds,
#' exactly as `010-fig2-ensembles.r` does (90 x 90 cells, `000-synthetic-process.r`), and for
#' each computes the same test as `020-verify-placement.r`: the KS distance between the changed
#' cells' percentiles of their true probability (within each transition) and the share of the
#' probability mass up to each percentile, with the p-value from simulated independent
#' multinomial draws per cell. If the process and the truth are right, the p-values are uniform
#' over seeds, and seed 1337 is one draw from that distribution.
#'
#' Needs data.table and terra only. Writes `results/verification/observed-fit-seeds.csv`.

#| label: setup
#| output: false
library(data.table)
library(terra)
options(synthetic_process.source_only = TRUE)
source("2026-09-paper-figures/000-synthetic-process.r")

out_dir <- "2026-09-paper-figures/results/verification"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

n_grid <- 90L
seeds <- c(1337L, seq_len(199L))
n_null <- 400L

template_rast <- terra::rast(
  crs = "EPSG:2056",
  extent = terra::ext(c(
    xmin = 2697000,
    xmax = 2697000 + n_grid * 100,
    ymin = 1252000,
    ymax = 1252000 + n_grid * 100
  )),
  resolution = 100
)

#' # One seed
#'
#' The landscape of Fig. 2 for a seed: drivers, initial map, two steps of the process. The
#' observed change is period 2 -> 3, its truth `true_probs()` on period 2. For seed 1337 the
#' map must equal the one stored in `results/fig2/cells.parquet`.

#| label: functions
fit_one <- function(seed) {
  set.seed(seed)
  drivers <- make_drivers(template_rast)
  initial <- make_landscape(template_rast, drivers)
  lulc_2 <- step_process(initial, drivers)
  lulc_3 <- step_process(lulc_2, drivers)
  anterior <- terra::values(lulc_2, mat = FALSE)
  posterior <- terra::values(lulc_3, mat = FALSE)

  truth <- true_probs(lulc_2, drivers)
  truth[, rank := frank(q, ties.method = "average") / .N, by = .(id_lulc_anterior, class)]
  mass <- truth[, .(w = sum(q)), keyby = rank]
  f0 <- cumsum(mass$w) / sum(mass$w)
  ks <- function(r) max(abs(findInterval(mass$rank, sort(r)) / length(r) - f0))

  changed <- data.table(cell = which(anterior != posterior), class = posterior[anterior != posterior])
  observed_rank <- truth[changed, on = .(cell, class), rank]
  d <- ks(observed_rank)

  # null: independent categorical draws per cell with the true probabilities
  by_cell <- truth[order(cell, class)][, slot := seq_len(.N), by = cell]
  wide <- dcast(by_cell, cell ~ slot, value.var = c("q", "rank"))
  q_cols <- grep("^q_", names(wide), value = TRUE)
  r_cols <- grep("^rank_", names(wide), value = TRUE)
  q_mat <- as.matrix(wide[, ..q_cols])
  q_mat[is.na(q_mat)] <- 0
  r_mat <- as.matrix(wide[, ..r_cols])
  cum <- t(apply(q_mat, 1L, cumsum))
  null_d <- vapply(seq_len(n_null), function(i) {
    u <- stats::runif(nrow(cum))
    pick <- rowSums(u >= cum) + 1L # first slot whose cumulative bound exceeds u
    hit <- pick <= ncol(cum)
    ks(r_mat[cbind(which(hit), pick[hit])])
  }, numeric(1))

  data.table(
    seed,
    changes = nrow(changed),
    expected_changes = sum(truth$q),
    d,
    null_median = stats::median(null_d),
    p = (1 + sum(null_d >= d)) / (1 + n_null),
    p_lower = (1 + sum(null_d <= d)) / (1 + n_null),
    lulc_3 = list(posterior)
  )
}

#' # All seeds

#| label: run
fits <- rbindlist(lapply(seeds, fit_one))

# seed 1337 must reproduce Fig. 2's observed map
cells <- as.data.table(arrow::read_parquet("2026-09-paper-figures/results/fig2/cells.parquet"))
cells[, cell := (row - 1L) * n_grid + col]
fig2_observed <- fits[seed == 1337L, lulc_3[[1]]][cells$cell]
reproduces_fig2 <- isTRUE(all.equal(as.numeric(fig2_observed), as.numeric(cells$observed)))
reproduces_fig2

fits[, lulc_3 := NULL]
fwrite(fits, file.path(out_dir, "observed-fit-seeds.csv"))

#' Over the seeds, the upper p-value (is the observed change too *far* from the truth?) and
#' the lower one (too *close*?) should both be uniform. Their 5 % tails should each hold about
#' 5 % of the seeds, and seed 1337's rank among them says how unusual it is.

#| label: summary
summary_tab <- fits[seed != 1337L, .(
  seeds = .N,
  median_d = stats::median(d),
  median_null_d = stats::median(null_median),
  share_p_below_5pct = mean(p < 0.05),
  share_p_lower_below_5pct = mean(p_lower < 0.05),
  ks_uniform_p = suppressWarnings(stats::ks.test(p, "punif")$p.value)
)]
knitr::kable(summary_tab, digits = 3)
seed_1337 <- fits[seed == 1337L][, rank_of_d_among_seeds := fits[seed != 1337L, mean(d <= fits[seed == 1337L, d])]]
knitr::kable(seed_1337, digits = 4)
