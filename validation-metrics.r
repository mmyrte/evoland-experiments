# Validation metrics for simulated land-use maps, shared by 2026-09-paper-figures and
# 2026-09-model-comparison; sourced, not rendered. Lives here until it moves into evoland-plus.
#
# Every function works on maps as integer vectors of class codes (1..K), aligned to one set of
# cells (the study area); the fuzzy metrics also need the grid those cells sit on, see
# vm_grid(). All maps of one comparison share the initial ("anterior") map.
#
# Single maps (one realisation, or a deterministic map), against the observed map:
#
# - figure of merit (Pontius et al. 2008), with its components;
# - quantity and allocation disagreement (Pontius & Millones 2011);
# - Kappa Simulation (van Vliet et al. 2011), split into KTransition (the quantity of each
#   transition) and KTransloc (their location), KSim = KTransition x KTransloc. It is Cohen's
#   kappa with the expected agreement of a random allocation *of change* from the initial map,
#   instead of a random allocation of the final map, so the persistence that dominates a
#   land-use map does not inflate it. It supersedes Cohen's kappa, and Pontius' Kappa_location
#   and Kappa_histo, for change; they are not computed.
# - Fuzzy Kappa Simulation (van Vliet, Hagen-Zanker et al. 2013): KSim with a location tolerance.
#   Each cell's agreement is the two-way fuzzy membership of Hagen (2003) between the maps of
#   transitions (anterior x final class): 1 if the transition is the same, else the
#   distance-decay weight of the nearest cell in the other map with the same transition. The
#   expected agreement follows Hagen-Zanker (2009): for cells that are a in one map and b in
#   the other, the two memberships are drawn independently from their distributions in the two
#   maps, which keeps the maps' own spatial autocorrelation; as in KSim, it is stratified by
#   the initial class. With a 1 x 1 window it equals KSim exactly (tested).
# - fuzzy similarity of differences (Hagen 2003, as in Dinamica EGO): the same membership on
#   the change of one transition at a time, without a chance correction; identical to evoland's
#   calc_transition_similarity() (tested), but computed with the shared kernel.
# - AUC of the Total Operating Characteristic (Pontius & Si 2014). TOC plots hits against hits +
#   false alarms as the threshold on an index moves; its area, normalised by the area of its
#   parallelogram, equals the AUC of the ROC. For a hard map the index is binary (one
#   threshold), for an ensemble it is the change frequency, for the potentials the potential.
#   Reported for "any change" (index: probability of leaving the initial class) and per
#   transition (index: probability of that transition, within its initial class), the latter
#   averaged with the observed change of each transition as weights.
# - multiclass Brier score (Brier 1950) of the map as a hard forecast.
#
# Ensembles: the per-cell class frequencies scored as a probabilistic forecast: fair Brier score
# (Ferro 2014) and TOC AUC; and the figure of merit of the mean counts.

suppressPackageStartupMessages(library(data.table))
local({
  cache <- file.path(".cache", "rcpp-validation-metrics")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  Rcpp::sourceCpp("validation-metrics.cpp", cacheDir = cache, env = globalenv())
})

# --- Grid -------------------------------------------------------------------------------------

#' The cells of a study area on their grid: `row`, `col` 1-based (row 1 at the top)
vm_grid <- function(id_coord, row, col) {
  stopifnot(length(id_coord) == length(row), length(row) == length(col))
  nrow <- max(row)
  ncol <- max(col)
  list(
    id_coord = id_coord,
    nrow = nrow,
    ncol = ncol,
    cell = (as.integer(row) - 1L) * ncol + as.integer(col)
  )
}

#' Grid of an evoland coords table (`id_coord`, `lon`, `lat` at cell centres, square cells)
vm_grid_from_coords <- function(coords, resolution) {
  col <- round((coords$lon - min(coords$lon)) / resolution) + 1L
  row <- round((max(coords$lat) - coords$lat) / resolution) + 1L
  vm_grid(coords$id_coord, row, col)
}

vm_expand <- function(grid, x) {
  full <- rep(NA_integer_, grid$nrow * grid$ncol)
  full[grid$cell] <- as.integer(x)
  full
}

#' Membership of every study cell in each of `n_categories` categories of `x`
vm_membership <- function(grid, x, n_categories, weights) {
  neighbourhood_membership(
    vm_expand(grid, x),
    grid$nrow,
    grid$ncol,
    n_categories,
    weights
  )[grid$cell, , drop = FALSE]
}

#' Distance-decay weights: `2^(-d / halving)` within Euclidean `radius` (cells), 0 beyond. Hagen
#' (2003) and the Map Comparison Kit default to exponential decay with halving distance 2 and
#' radius 4. Radius 0 is the crisp comparison.
fuzzy_weights <- function(radius = 4, halving = 2) {
  r <- floor(radius)
  off <- seq(-r, r)
  d <- sqrt(outer(off^2, off^2, "+"))
  w <- 2^(-d / halving)
  w[d > radius] <- 0
  w
}

#' Dinamica's / evoland's similarity-of-differences window: square, `exp(-d / divisor)`
dinamica_weights <- function(window_size = 11L, decay_divisor = 2) {
  off <- seq_len(window_size) - (window_size + 1) / 2
  exp(-sqrt(outer(off^2, off^2, "+")) / decay_divisor)
}

# --- Figure of merit and disagreement --------------------------------------------------------

figure_of_merit <- function(anterior, observed, simulated) {
  obs_change <- anterior != observed
  sim_change <- anterior != simulated
  hits <- sum(obs_change & sim_change & simulated == observed)
  wrong_hits <- sum(obs_change & sim_change & simulated != observed)
  misses <- sum(obs_change & !sim_change)
  false_alarms <- sum(!obs_change & sim_change)
  data.table(
    hits = hits,
    wrong_hits = wrong_hits,
    misses = misses,
    false_alarms = false_alarms,
    figure_of_merit = hits / (hits + wrong_hits + misses + false_alarms),
    producers_accuracy = hits / (hits + wrong_hits + misses),
    users_accuracy = hits / (hits + wrong_hits + false_alarms)
  )
}

#' Quantity and allocation disagreement of two maps (shares of the cells; Pontius & Millones
#' 2011); total disagreement is their sum
disagreement <- function(observed, simulated, classes = sort(unique(c(observed, simulated)))) {
  n <- length(observed)
  ct <- table(factor(observed, classes), factor(simulated, classes)) / n
  p_obs <- rowSums(ct)
  p_sim <- colSums(ct)
  d <- diag(ct)
  data.table(
    quantity_disagreement = sum(abs(p_obs - p_sim)) / 2,
    allocation_disagreement = sum(2 * pmin(p_obs - d, p_sim - d)) / 2
  )
}

# --- Kappa Simulation -------------------------------------------------------------------------

#' Transition shares p(final | anterior) of a map, long form
transition_shares <- function(anterior, final) {
  dt <- data.table(anterior = anterior, final = final)[, .N, by = .(anterior, final)]
  dt[, p := N / sum(N), by = anterior][]
}

kappa_simulation <- function(anterior, observed, simulated) {
  n <- length(anterior)
  w <- data.table(anterior = anterior)[, .(w = .N / n), by = anterior]
  po <- transition_shares(anterior, observed)[, .(anterior, final, p_obs = p)]
  ps <- transition_shares(anterior, simulated)[, .(anterior, final, p_sim = p)]
  both <- merge(po, ps, by = c("anterior", "final"), all = TRUE)
  both[is.na(p_obs), p_obs := 0][is.na(p_sim), p_sim := 0]
  both <- w[both, on = "anterior"]
  p_o <- mean(observed == simulated)
  p_e <- both[, sum(w * p_obs * p_sim)]
  p_max <- both[, sum(w * pmin(p_obs, p_sim))]
  data.table(
    ksim = (p_o - p_e) / (1 - p_e),
    ktransition = (p_max - p_e) / (1 - p_e),
    ktransloc = (p_o - p_e) / (p_max - p_e)
  )
}

# --- Fuzzy Kappa Simulation -------------------------------------------------------------------

#' E[min(X, Y)] for independent X, Y given as samples of their (discrete) values in [0, 1]
expected_min <- function(x, y) {
  if (length(x) == 0L || length(y) == 0L) {
    return(0)
  }
  v <- sort(unique(c(x[x > 0], y[y > 0])))
  if (length(v) == 0L) {
    return(0)
  }
  sx <- 1 - stats::ecdf(x)(v - 1e-12) # P(X >= v)
  sy <- 1 - stats::ecdf(y)(v - 1e-12)
  sum(diff(c(0, v)) * sx * sy)
}

#' Membership tables of the transition maps of `observed` and `simulated`; reused when one
#' observed map is compared against many simulated ones
fuzzy_memberships <- function(grid, anterior, final, n_classes, weights) {
  code <- (as.integer(anterior) - 1L) * n_classes + as.integer(final)
  list(code = code, m = vm_membership(grid, code, n_classes^2, weights))
}

fuzzy_kappa_simulation <- function(
  grid,
  anterior,
  observed,
  simulated,
  n_classes,
  weights = fuzzy_weights(),
  observed_memberships = NULL
) {
  mo <- observed_memberships %||% fuzzy_memberships(grid, anterior, observed, n_classes, weights)
  ms <- fuzzy_memberships(grid, anterior, simulated, n_classes, weights)
  idx <- seq_along(anterior)
  # two-way agreement of each cell: the observed transition's membership in the simulated map,
  # and the simulated transition's in the observed map
  s <- pmin(ms$m[cbind(idx, mo$code)], mo$m[cbind(idx, ms$code)])
  p <- mean(s)
  e <- 0
  n <- length(anterior)
  for (k in unique(anterior)) {
    in_k <- which(anterior == k)
    to <- mo$code[in_k]
    ts <- ms$code[in_k]
    pa <- table(to) / length(in_k)
    pb <- table(ts) / length(in_k)
    e_k <- 0
    for (a in names(pa)) {
      for (b in names(pb)) {
        ia <- as.integer(a)
        ib <- as.integer(b)
        e_ab <- if (ia == ib) {
          1
        } else {
          expected_min(ms$m[in_k[ts == ib], ia], mo$m[in_k[to == ia], ib])
        }
        e_k <- e_k + pa[[a]] * pb[[b]] * e_ab
      }
    }
    e <- e + length(in_k) / n * e_k
  }
  data.table(fuzzy_agreement = p, fuzzy_expected = e, fks = (p - e) / (1 - e))
}

# --- Fuzzy similarity of differences ----------------------------------------------------------

similarity_of_differences <- function(
  grid,
  anterior,
  observed,
  simulated,
  from_class,
  to_class,
  weights = dinamica_weights()
) {
  obs <- anterior == from_class & observed == to_class
  sim <- anterior == from_class & simulated == to_class
  n_obs <- sum(obs)
  n_sim <- sum(sim)
  if (n_obs == 0 && n_sim == 0) {
    return(data.table(similarity = NA_real_, n_observed = 0L, n_simulated = 0L))
  }
  m_obs <- vm_membership(grid, ifelse(obs, 1L, 2L), 2L, weights)[, 1]
  m_sim <- vm_membership(grid, ifelse(sim, 1L, 2L), 2L, weights)[, 1]
  o2s <- if (n_obs > 0) mean(m_sim[obs]) else NA_real_
  s2o <- if (n_sim > 0) mean(m_obs[sim]) else NA_real_
  data.table(similarity = min(o2s, s2o, na.rm = TRUE), n_observed = n_obs, n_simulated = n_sim)
}

# --- TOC / AUC --------------------------------------------------------------------------------

#' Area under the ROC curve (= normalised TOC area), ties counted half (Mann-Whitney)
auc <- function(index, target) {
  target <- as.logical(target)
  n1 <- sum(target)
  n0 <- length(target) - n1
  if (n1 == 0 || n0 == 0) {
    return(NA_real_)
  }
  r <- frank(index, ties.method = "average")
  (sum(r[target]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

#' TOC curve: hits against hits + false alarms over all thresholds on `index`, thinned to about
#' `n_points` points (always keeping the corners)
toc_curve <- function(index, target, n_points = 200L) {
  o <- order(-index)
  idx <- index[o]
  tg <- as.logical(target)[o]
  last <- c(idx[-1] != idx[-length(idx)], TRUE) # last cell of each tied block
  x <- c(0, seq_along(idx)[last])
  y <- c(0, cumsum(tg)[last])
  keep <- unique(c(1L, round(seq(1, length(x), length.out = n_points)), length(x)))
  data.table(
    hits_false_alarms = x[keep],
    hits = y[keep],
    threshold = c(Inf, idx[last])[keep]
  )
}

#' AUCs of one forecast: `p_change` (per cell) and `p_trans` (long: cell index `i`, `final`, `p`
#' for the viable transitions). "Any change" is scored over the `eligible` cells, those of a
#' class that can change; cells of a fixed class would add certain negatives.
auc_scores <- function(anterior, observed, p_change, p_trans, viable, eligible) {
  obs_n <- data.table(anterior = anterior, final = observed)[
    anterior != final,
    .N,
    by = .(anterior, final)
  ]
  per_trans <- viable[, {
    cells <- which(anterior == from)
    p <- numeric(length(anterior))
    pt <- p_trans[final == to]
    p[pt$i] <- pt$p
    .(auc = auc(p[cells], observed[cells] == to))
  }, by = .(from, to)][obs_n, on = .(from = anterior, to = final), n_observed := i.N][]
  per_trans[is.na(n_observed), n_observed := 0L]
  data.table(
    auc_change = auc(p_change[eligible], (anterior != observed)[eligible]),
    auc_transition = per_trans[!is.na(auc), sum(auc * n_observed) / sum(n_observed)]
  )
}

# --- Scoring a set of maps --------------------------------------------------------------------

#' Every single-map metric for each simulated map
#'
#' @param grid vm_grid() of the cells
#' @param anterior,observed Integer vectors of the cells' classes
#' @param simulated Integer matrix, one column per simulated map, or a named list of vectors
#' @param viable data.table of the modelled transitions, `from`, `to`
#' @param eligible Logical vector: the cells the Brier score is averaged over
#' @param n_classes Largest class code
#' @param fuzzy_radii Radii of the Fuzzy Kappa Simulation (halving distance 2)
score_maps <- function(
  grid,
  anterior,
  observed,
  simulated,
  viable,
  eligible,
  n_classes,
  fuzzy_radii = c(2, 4)
) {
  if (is.matrix(simulated)) {
    simulated <- lapply(seq_len(ncol(simulated)), function(j) simulated[, j])
  }
  fk_weights <- lapply(fuzzy_radii, fuzzy_weights)
  fk_obs <- lapply(fk_weights, function(w) fuzzy_memberships(grid, anterior, observed, n_classes, w))
  dw <- dinamica_weights()
  m_obs_change <- lapply(seq_len(nrow(viable)), function(t) {
    obs <- anterior == viable$from[t] & observed == viable$to[t]
    vm_membership(grid, ifelse(obs, 1L, 2L), 2L, dw)[, 1]
  })
  n_obs_trans <- vapply(
    seq_len(nrow(viable)),
    function(t) sum(anterior == viable$from[t] & observed == viable$to[t]),
    numeric(1)
  )
  rbindlist(lapply(simulated, function(sim) {
    fks <- vapply(seq_along(fuzzy_radii), function(j) {
      fuzzy_kappa_simulation(
        grid,
        anterior,
        observed,
        sim,
        n_classes,
        fk_weights[[j]],
        observed_memberships = fk_obs[[j]]
      )$fks
    }, numeric(1))
    sod <- vapply(seq_len(nrow(viable)), function(t) {
      obs <- anterior == viable$from[t] & observed == viable$to[t]
      s <- anterior == viable$from[t] & sim == viable$to[t]
      if (!any(obs) && !any(s)) {
        return(NA_real_)
      }
      m_sim <- vm_membership(grid, ifelse(s, 1L, 2L), 2L, dw)[, 1]
      min(
        if (any(obs)) mean(m_sim[obs]) else NA_real_,
        if (any(s)) mean(m_obs_change[[t]][s]) else NA_real_,
        na.rm = TRUE
      )
    }, numeric(1))
    i_change <- which(sim != anterior)
    auc_dt <- auc_scores(
      anterior,
      observed,
      p_change = as.numeric(sim != anterior),
      p_trans = data.table(i = i_change, final = sim[i_change], p = 1),
      viable = viable,
      eligible = eligible
    )
    cbind(
      figure_of_merit(anterior, observed, sim),
      disagreement(observed, sim),
      kappa_simulation(anterior, observed, sim),
      as.data.table(setNames(as.list(fks), paste0("fks_r", fuzzy_radii))),
      data.table(
        similarity_of_differences = sum(sod * n_obs_trans, na.rm = TRUE) /
          sum(n_obs_trans[!is.na(sod)])
      ),
      auc_dt,
      data.table(brier = 2 * mean(sim[eligible] != observed[eligible]))
    )
  }))
}

#' Multiclass Brier score of a probabilistic forecast `p` (cells x classes, rows summing to 1)
#' over `eligible` cells; `n_members` applies the fair correction for an ensemble frequency
brier_score <- function(p, observed, eligible, n_members = NA_integer_) {
  p <- p[eligible, , drop = FALSE]
  o <- matrix(0, nrow(p), ncol(p))
  o[cbind(seq_len(nrow(p)), observed[eligible])] <- 1
  bs <- rowSums((p - o)^2)
  if (!is.na(n_members)) {
    bs <- bs - rowSums(p * (1 - p)) / (n_members - 1)
  }
  mean(bs)
}

#' Probabilistic scores of an ensemble (its class frequencies) or of a probability surface
#'
#' @param p Numeric matrix cells x classes (rows sum to 1)
score_forecast <- function(anterior, observed, p, viable, eligible, n_members = NA_integer_) {
  idx <- seq_along(anterior)
  p_change <- 1 - p[cbind(idx, anterior)]
  p_trans <- rbindlist(lapply(seq_len(ncol(p)), function(k) {
    i <- which(p[, k] > 0 & anterior != k)
    data.table(i = i, final = rep(k, length(i)), p = p[i, k])
  }))
  cbind(
    auc_scores(anterior, observed, p_change, p_trans, viable, eligible),
    data.table(brier = brier_score(p, observed, eligible, n_members))
  )
}

#' Class frequencies of an ensemble (cells x classes)
class_frequencies <- function(simulated, n_classes) {
  if (is.list(simulated)) simulated <- do.call(cbind, simulated)
  f <- matrix(0, nrow(simulated), n_classes)
  for (k in seq_len(n_classes)) {
    f[, k] <- rowMeans(simulated == k)
  }
  f
}
