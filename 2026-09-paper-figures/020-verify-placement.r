#' ---
#' title: "Fig. 2 case: does each allocator place change where its potentials say?"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' Fig. 2 (panel (e) of `030-fig2-plot.r`) shows where each allocator puts change, by the
#' percentile of the changed cells' probability. This step turns that picture into tests. It
#' reads only `results/fig2/` (written by `010-fig2-ensembles.r`) and writes
#' `results/verification/`.
#'
#' **Two reference distributions.**
#'
#' - **The potentials the allocators get** (verification). Every allocator in Fig. 2 receives
#'   the same adjusted transition potentials p. An unbiased allocator changes cell j by
#'   transition k with probability p_jk, so the changed cells' percentiles of p follow a known
#'   distribution: the share of the potential mass up to that percentile,
#'   F0(x) = sum of p over (j, k) with percentile <= x, divided by the sum of all p. This is
#'   what uSAM (Mazy 2022, sec. 3.4.1) promises; uPAM keeps it for the pivots, and its
#'   patches spread change onto neighbours.
#' - **The true probabilities q** (evaluation, as drawn in panel (e)). The observed change is
#'   one draw of the synthetic process, so it follows the same construction with q. The
#'   allocators sample p, not q, so against q they also carry the estimation error.
#'
#' Percentiles are taken within each transition, among the cells that could make it, as in
#' panel (e); transitions are pooled.
#'
#' **The tests.** The Kolmogorov-Smirnov distance D = max_x |F(x) - F0(x)| between the
#' empirical distribution of a map's changed cells and F0. The usual KS p-values assume a
#' continuous F0 and independent draws with a fixed count; here F0 is a step function, and
#' under the null hypothesis each cell changes independently with its own probability, so the
#' number of changes varies. The null distribution of D is therefore simulated from exactly
#' that model: independent multinomial draws per cell (the stay included), `n_null` times for
#' a single map and `n_null_pooled` times for a pooled ensemble of `n_members` maps. Reported:
#'
#' - per map: D and its p-value; per allocator the median D and the share of maps rejected at
#'   5 % (an unbiased allocator: about 5 %);
#' - per ensemble: D of all its changed cells pooled, and its p-value (far more power: about
#'   30 000 changed cells per ensemble);
#' - two further checks of uSAM's promise that the ensemble reproduces the potentials, in
#'   quantity and cell by cell: the mean number of changes per transition against the sum of
#'   p (z-score, with the variance of a sum of independent Bernoulli draws), and the cell-wise
#'   class frequencies against p (Pearson's X^2 over cells and classes, as a z-score).

#| label: setup
#| output: false
library(data.table)

res_dir <- "2026-09-paper-figures/results/fig2"
out_dir <- "2026-09-paper-figures/results/verification"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cells <- as.data.table(arrow::read_parquet(file.path(res_dir, "cells.parquet")))
runs <- fread(file.path(res_dir, "runs.csv"))
changes <- as.data.table(arrow::read_parquet(file.path(res_dir, "changes.parquet")))
potentials <- as.data.table(arrow::read_parquet(file.path(res_dir, "potentials.parquet")))
truth <- as.data.table(arrow::read_parquet(file.path(res_dir, "truth.parquet")))

set.seed(20261118)
n_null <- 2000L
n_null_pooled <- 400L
n_members <- runs[allocator == "CLUMPY", .N]

#' # Reference distributions
#'
#' One row per cell and transition it can make, with p, q and the percentile of each within
#' the transition.

#| label: pairs
pairs <- potentials[cells[, .(id_coord, anterior)], on = "id_coord", nomatch = NULL][
  truth[, .(id_coord, class, q)],
  on = .(id_coord, class),
  nomatch = NULL
]
pairs[, `:=`(
  rank_p = frank(p, ties.method = "average") / .N,
  rank_q = frank(q, ties.method = "average") / .N
), by = .(anterior, class)]
pairs[, .(pairs = .N, sum_p = sum(p), sum_q = sum(q)), keyby = .(anterior, class)]

# the null CDF on the support of the percentiles: share of the probability mass up to and
# including each percentile
null_cdf <- function(rank, weight) {
  mass <- data.table(rank, weight)[, .(weight = sum(weight)), keyby = rank]
  list(support = mass$rank, cdf = cumsum(mass$weight) / sum(mass$weight))
}
ref <- list(
  potentials = null_cdf(pairs$rank_p, pairs$p),
  truth = null_cdf(pairs$rank_q, pairs$q)
)

# KS distance between a sample of percentiles and a null CDF given on its support. Both are
# step functions that jump only on the support (the sample's values lie on it by construction),
# so the supremum is attained at a support point.
ks_distance <- function(sample_rank, nc) {
  if (length(sample_rank) == 0L) {
    return(NA_real_)
  }
  emp <- findInterval(nc$support, sort(sample_rank)) / length(sample_rank)
  max(abs(emp - nc$cdf))
}

#' # Null distribution of D
#'
#' Independent multinomial draws per cell: a cell of class a changes by transition k with
#' probability p (or q) and stays with the rest. Simulated for one map and for a pooled
#' ensemble of `n_members` maps.

#| label: null-distribution
# every cell can make exactly two transitions; slot 1 and 2 are them, in class order
by_slot <- pairs[order(id_coord, class)][, slot := seq_len(.N), by = id_coord]
first <- by_slot[slot == 1L]
second <- by_slot[slot == 2L]
stopifnot(identical(first$id_coord, second$id_coord), by_slot[, max(slot)] == 2L)
prob1 <- list(potentials = first$p, truth = first$q)
prob2 <- list(potentials = second$p, truth = second$q)
rank1 <- list(potentials = first$rank_p, truth = first$rank_q)
rank2 <- list(potentials = second$rank_p, truth = second$rank_q)

draw_null_ranks <- function(reference, members = 1L) {
  n <- nrow(first)
  unlist(lapply(seq_len(members), function(m) {
    u <- stats::runif(n)
    takes_1 <- u < prob1[[reference]]
    takes_2 <- !takes_1 & u < prob1[[reference]] + prob2[[reference]]
    c(rank1[[reference]][takes_1], rank2[[reference]][takes_2])
  }))
}

null_d <- rbindlist(lapply(names(ref), function(reference) {
  rbind(
    data.table(
      reference,
      level = "map",
      d = vapply(seq_len(n_null), function(i) ks_distance(draw_null_ranks(reference), ref[[reference]]), numeric(1))
    ),
    data.table(
      reference,
      level = "ensemble",
      d = vapply(seq_len(n_null_pooled), function(i) {
        ks_distance(draw_null_ranks(reference, n_members), ref[[reference]])
      }, numeric(1))
    )
  )
}))
null_d[, .(median = median(d), q95 = quantile(d, 0.95)), keyby = .(reference, level)]
p_value <- function(d, reference_, level_) {
  nd <- null_d[reference == reference_ & level == level_, d]
  (1 + sum(nd >= d)) / (1 + length(nd))
}

#' # Every map and ensemble

#| label: maps
changed <- changes[cells[, .(id_coord, anterior)], on = "id_coord", nomatch = NULL][
  pairs[, .(id_coord, class, rank_p, rank_q)],
  on = .(id_coord, simulated = class),
  nomatch = NULL
][runs[, .(id_run, allocator)], on = "id_run", nomatch = NULL]
observed <- cells[anterior != observed, .(id_coord, simulated = observed)][
  pairs[, .(id_coord, class, rank_p, rank_q)],
  on = .(id_coord, simulated = class),
  nomatch = NULL
][, `:=`(id_run = 0L, allocator = "observed")]
changed <- rbind(changed, observed, use.names = TRUE, fill = TRUE)
stopifnot("a changed cell made a transition that has no potential" = {
  changed[, .N] == changes[cells[, .(id_coord, anterior)], on = "id_coord", nomatch = NULL][, .N] + nrow(observed)
})

per_map <- rbindlist(lapply(names(ref), function(reference) {
  col <- if (reference == "potentials") "rank_p" else "rank_q"
  changed[, .(changes = .N, d = ks_distance(get(col), ref[[reference]])), by = .(allocator, id_run)][,
    `:=`(reference = reference, p = vapply(d, p_value, numeric(1), reference_ = reference, level_ = "map"))
  ]
}))
fwrite(per_map, file.path(out_dir, "placement-ks-per-map.csv"))

ensembles <- rbindlist(lapply(names(ref), function(reference) {
  col <- if (reference == "potentials") "rank_p" else "rank_q"
  changed[, .(maps = uniqueN(id_run), changes = .N, d = ks_distance(get(col), ref[[reference]])), by = allocator][,
    `:=`(reference = reference)
  ]
}))
# a pooled ensemble's null is that of n_members maps; single maps (observed, rank-and-fill)
# take the single-map null
ensembles[, p := mapply(function(d, m, r) p_value(d, r, if (m > 1L) "ensemble" else "map"), d, maps, reference)]

placement <- per_map[,
  .(
    maps = .N,
    median_changes = as.numeric(median(changes)),
    median_d = median(d),
    share_rejected_5pct = mean(p < 0.05)
  ),
  by = .(reference, allocator)
][ensembles[, .(reference, allocator, ensemble_d = d, ensemble_p = p)], on = .(reference, allocator)]
setorder(placement, reference, median_d)
fwrite(placement, file.path(out_dir, "placement-ks.csv"))
knitr::kable(placement, digits = 3)

#' # uSAM: quantity and cell-wise frequencies
#'
#' Over the ensemble of each stochastic allocator, against the potentials: the mean number of
#' changes per transition and its z-score, and the cell-wise class frequencies (Pearson's X^2
#' over the categories with a positive potential, df = categories - cells, as a z-score:
#' (X^2 - df) / sqrt(2 df)). Under
#' uSAM both z-scores are standard normal; patch allocators, which correlate neighbouring
#' cells, and allocators that fix the quantity are not expected to pass the second.

#| label: quantity
members <- runs[allocator != "rank-and-fill"]
counts <- changes[members[, .(id_run, allocator)], on = "id_run", nomatch = NULL][
  cells[, .(id_coord, anterior)],
  on = "id_coord",
  nomatch = NULL
]
expected <- pairs[, .(expected = sum(p), var1 = sum(p * (1 - p))), by = .(anterior, class)]
quantity <- counts[, .N, by = .(allocator, id_run, anterior, class = simulated)][
  CJ(allocator = unique(members$allocator), anterior = expected$anterior, unique = TRUE)[
    expected, on = "anterior", allow.cartesian = TRUE
  ][, .(allocator, anterior, class)],
  on = .(allocator, anterior, class)
][!is.na(id_run), .(mean_changes = mean(N), members = .N), by = .(allocator, anterior, class)][
  expected, on = .(anterior, class)
][, z := (mean_changes - expected) / sqrt(var1 / members)]
setorder(quantity, allocator, anterior, class)
fwrite(quantity[, .(allocator, transition = paste(anterior, "->", class), members, mean_changes, expected, z)],
       file.path(out_dir, "placement-quantity.csv"))
knitr::kable(quantity[, .(allocator, transition = paste(anterior, "->", class), mean_changes, expected, z)], digits = 2)

#| label: dispersion
freq <- counts[, .(k = .N), by = .(allocator, id_coord, class = simulated)]
dispersion <- rbindlist(lapply(unique(members$allocator), function(a) {
  m <- members[allocator == a, .N]
  obs <- freq[allocator == a][pairs[, .(id_coord, class, p)], on = .(id_coord, class)][
    is.na(k), k := 0L
  ]
  # per cell, the two transitions and the stay: (O - E)^2 / E summed over the categories with
  # a positive expectation; a change where the potential is 0 is impossible under the null
  # and counted apart
  cats <- rbind(
    obs[, .(id_coord, o = k, e = m * p)],
    obs[, .(o = m - sum(k), e = m * (1 - sum(p))), by = id_coord]
  )
  impossible <- cats[e <= 0 & o > 0, sum(o)]
  cats <- cats[e > 0]
  x2 <- cats[, sum((o - e)^2 / e)]
  df <- cats[, .N - uniqueN(id_coord)]
  per_cell <- obs[, .(id_coord = unique(id_coord))]
  data.table(
    allocator = a, members = m, cells = nrow(per_cell), changes_where_p_is_0 = impossible,
    x2, df, z = (x2 - df) / sqrt(2 * df)
  )
}))
fwrite(dispersion, file.path(out_dir, "placement-dispersion.csv"))
knitr::kable(dispersion, digits = 2)
