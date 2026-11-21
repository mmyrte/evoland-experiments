#' ---
#' title: "Verification: when is uPAM unbiased with respect to the potentials?"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' `020-verify-placement.r` rejects uPAM on Fig. 2: its changed cells do not follow the
#' potentials, cell by cell. Is that a defect, or outside what uPAM promises?
#'
#' **What Mazy (2022) proves.** uPAM's pivot cells are drawn without bias (App. 3.B.2), and so
#' are the patch sizes and elongations, if patches do not merge with other patches of the same
#' step (App. 3.D.1.2). The other cells of a patch are not drawn from the potentials: the grower
#' adds, one at a time, the neighbour with the best score (potential over distance to the target
#' elongation), a deterministic choice among the cells next to the patch. That the allocated
#' cells as a whole still follow the transition probabilities rests on an assumption (App. 3.A.2):
#' the patch scale Lp = E(sigma)^(1/2) is small against the scale Lz over which the explanatory
#' variables, and hence the potentials, change, so that "the probability density distribution
#' of all pixels is the same in any patch, and this includes the pivot-cell". The unbiasedness
#' is then one in explanatory-variable space, at the scale Lz, not cell by cell.
#'
#' **The experiment.** One transition 1 -> 2 on a 100 x 100 grid of class 1, potentials with the
#' same marginal distribution (logit-normal, mean rate 0.03) and two spatial structures:
#'
#' - **white**: independent per cell (Lz = 1 cell, Fig. 2's situation in the extreme: its
#'   process changes cells independently, and its potentials vary from cell to cell);
#' - **smooth**: a Gaussian random field with a correlation length of about 15 cells
#'   (Lp << Lz for the patch sizes below).
#'
#' Allocated by evoland's C++ backend (`allocate_clumpy_cpp`), 100 realisations each:
#'
#' - **uSAM** (control);
#' - **uPAM, mono-pixel** (area 1): the iteration (quota, pool without replacement) without
#'   patches;
#' - **uPAM, patches** of mean area 4 and 9 cells (normal, sd 1 resp. 2), with and without
#'   aggregation avoidance.
#'
#' For each, the Kolmogorov-Smirnov distance D of the pooled changed cells' percentiles of the
#' potentials to the potential mass up to each percentile (as in `020-verify-placement.r`), with
#' its null distribution simulated from independent draws per cell. And, for the scale
#' argument, the same test on the potentials smoothed over a 7 x 7 window: "is change placed
#' where the potentials are high *in the neighbourhood*?"
#'
#' Needs an evoland.plus source checkout (`EVOLAND_PLUS_SRC`, default `../evoland.plus`).
#' Writes `results/verification/upam-bias.csv` and `upam-bias-deciles.csv`.

#| label: setup
#| output: false
library(data.table)
evoland_src <- normalizePath(Sys.getenv("EVOLAND_PLUS_SRC", "../evoland.plus"))
pkgload::load_all(evoland_src, quiet = TRUE, export_all = TRUE, helpers = FALSE)

out_dir <- "2026-09-paper-figures/results/verification"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20261119)
n_side <- 100L
n <- n_side^2
n_members <- 100L
n_null <- 200L

#' # Potentials

#| label: potentials
# a field with the given correlation length (cells), standardised
gaussian_field <- function(length_scale) {
  z <- matrix(stats::rnorm(n), n_side, n_side)
  if (length_scale > 0) {
    r <- terra::rast(z)
    w <- terra::focalMat(r, length_scale, type = "Gauss")
    z <- terra::as.matrix(terra::focal(r, w, fun = "sum", na.rm = TRUE), wide = TRUE)
  }
  as.vector(t((z - mean(z)) / sd(z))) # row-major, as allocate_clumpy_cpp expects
}
to_potential <- function(z) {
  p <- stats::plogis(-3.8 + 1.2 * z)
  p * 0.03 / mean(p) # mean rate 0.03
}
potentials <- list(white = to_potential(gaussian_field(0)), smooth = to_potential(gaussian_field(5)))
vapply(potentials, function(p) c(mean = mean(p), max = max(p)), numeric(2))

# the same potentials smoothed over a 7 x 7 window: the neighbourhood's potential
smooth_7 <- function(p) {
  r <- terra::rast(matrix(p, n_side, n_side, byrow = TRUE))
  as.vector(t(terra::as.matrix(terra::focal(r, w = 7, fun = mean, na.rm = TRUE), wide = TRUE)))
}

#' # Test

#| label: test
ref_cdf <- function(p, weight = p) {
  rank <- frank(p, ties.method = "average") / length(p)
  mass <- data.table(rank, weight)[, .(w = sum(weight)), keyby = rank]
  list(rank = rank, support = mass$rank, cdf = cumsum(mass$w) / sum(mass$w))
}
ks_distance <- function(sample_rank, ref) {
  max(abs(findInterval(ref$support, sort(sample_rank)) / length(sample_rank) - ref$cdf))
}
# null of a pooled ensemble: n_members maps of independent Bernoulli(p) draws
null_pooled <- function(p, ref) {
  vapply(seq_len(n_null), function(i) {
    ks_distance(unlist(lapply(seq_len(n_members), function(m) ref$rank[stats::runif(n) < p])), ref)
  }, numeric(1))
}

configs <- rbind(
  data.table(method = "uSAM", area = 1, sd = 0, avoid = FALSE),
  data.table(method = "uPAM", area = 1, sd = 0, avoid = FALSE),
  CJ(method = "uPAM", area = c(4, 9), avoid = c(FALSE, TRUE))[, sd := sqrt(area) / 2]
)

run <- function(field, cfg) {
  p <- potentials[[field]]
  cells <- seq_len(n)
  maps <- lapply(seq_len(n_members), function(m) {
    allocate_clumpy_cpp(
      landscape = rep(1L, n),
      nrow = n_side,
      ncol = n_side,
      trans_from = 1L,
      trans_to = 2L,
      prob_cell = list(cells),
      prob_value = list(p),
      area_mean = cfg$area,
      area_var = cfg$sd^2,
      elongation = 0.3,
      target_rate = mean(p),
      method = if (cfg$method == "uSAM") 0L else 1L,
      batch_size = 0L,
      rarefy = TRUE,
      shuffle = TRUE,
      avoid_aggregation = cfg$avoid,
      area_dist = 1L
    )
  })
  changed <- unlist(lapply(maps, function(x) which(x == 2L)))
  ref_cell <- ref_cdf(p)
  ref_local <- ref_cdf(smooth_7(p), weight = p)
  data.table(
    field,
    cfg,
    mean_changes = length(changed) / n_members,
    expected_changes = sum(p),
    d_cell = ks_distance(ref_cell$rank[changed], ref_cell),
    d_neighbourhood = ks_distance(ref_local$rank[changed], ref_local)
  )
}

results <- rbindlist(lapply(names(potentials), function(field) {
  rbindlist(lapply(seq_len(nrow(configs)), function(i) run(field, configs[i])))
}))

# null distributions of D for the pooled ensembles (depend on the field only)
nulls <- rbindlist(lapply(names(potentials), function(field) {
  p <- potentials[[field]]
  ref_cell <- ref_cdf(p)
  ref_local <- ref_cdf(smooth_7(p), weight = p)
  data.table(
    field,
    null_cell = null_pooled(p, ref_cell),
    null_neighbourhood = null_pooled(p, ref_local)
  )
}))
results[, `:=`(
  p_cell = mapply(function(f, d) (1 + sum(nulls[field == f, null_cell] >= d)) / (1 + n_null), field, d_cell),
  p_neighbourhood = mapply(
    function(f, d) (1 + sum(nulls[field == f, null_neighbourhood] >= d)) / (1 + n_null),
    field, d_neighbourhood
  )
)]
null_summary <- nulls[, .(null_q95_cell = quantile(null_cell, 0.95), null_q95_neighbourhood = quantile(null_neighbourhood, 0.95)), by = field]
results <- results[null_summary, on = "field"]
fwrite(results, file.path(out_dir, "upam-bias.csv"))
knitr::kable(results[, .(field, method, area, avoid, mean_changes, expected_changes, d_cell, p_cell, d_neighbourhood, p_neighbourhood)], digits = 3)

#' # The iteration alone: inclusion by decile of the potential
#'
#' Without patches (area 1), uSAM draws each cell once; uPAM re-draws from the same potentials
#' over the shrinking pool until the quota is met. The second is successive sampling, whose
#' inclusion probabilities are not proportional to the potentials: the most probable cells
#' saturate. Change frequency over potential by decile of the potential (white field), for uSAM
#' and for uPAM with the three batch settings (auto: ~1 % of the pool per pass; 1: one pivot per
#' pass; -1: all candidates of a pass). 300 realisations each.

#| label: deciles
p <- potentials$white
p_decile <- cut(p, stats::quantile(p, seq(0, 1, 0.1)), include.lowest = TRUE, labels = FALSE)
frequency <- function(method, batch_size) {
  counts <- integer(n)
  for (m in seq_len(300L)) {
    x <- allocate_clumpy_cpp(
      rep(1L, n), n_side, n_side, 1L, 2L, list(seq_len(n)), list(p), 1, 0, 0.3, mean(p),
      method, batch_size, TRUE, TRUE, FALSE, 1L
    )
    counts <- counts + (x == 2L)
  }
  counts / 300
}
freq <- list(
  uSAM = frequency(0L, 0L),
  `uPAM, batch auto` = frequency(1L, 0L),
  `uPAM, batch 1` = frequency(1L, 1L),
  `uPAM, one pass` = frequency(1L, -1L)
)
deciles <- data.table(decile = 1:10, mean_potential = as.numeric(tapply(p, p_decile, mean)))
for (a in names(freq)) {
  deciles[, (a) := as.numeric(tapply(freq[[a]], p_decile, mean)) / mean_potential]
}
fwrite(deciles, file.path(out_dir, "upam-bias-deciles.csv"))
knitr::kable(deciles, digits = 3)
