# Checks of validation-metrics.r; run from the repository root:
#   Rscript validation-metrics-tests.r
source("validation-metrics.r")
suppressPackageStartupMessages(library(terra))

check <- function(ok, what) {
  if (!isTRUE(all(ok))) stop("FAILED: ", what, call. = FALSE)
  message("ok: ", what)
}

set.seed(42)
nr <- 40L
nc <- 50L
grid <- vm_grid(seq_len(nr * nc), rep(seq_len(nr), each = nc), rep(seq_len(nc), nr))
anterior <- sample(1:3, nr * nc, replace = TRUE, prob = c(0.5, 0.3, 0.2))
change <- function(x, rate) {
  i <- which(runif(length(x)) < rate & x != 3L)
  x[i] <- ifelse(x[i] == 1L, 2L, 3L)
  x
}
observed <- change(anterior, 0.08)
simulated <- change(anterior, 0.08)

# --- Kappa Simulation: identities --------------------------------------------------------------
ks <- kappa_simulation(anterior, observed, simulated)
check(abs(ks$ksim - ks$ktransition * ks$ktransloc) < 1e-12, "KSim = KTransition x KTransloc")
check(abs(kappa_simulation(anterior, observed, observed)$ksim - 1) < 1e-12, "KSim of a perfect map is 1")
check(
  abs(kappa_simulation(anterior, observed, simulated)$ksim) < 0.1,
  "KSim of an independent allocation is near 0"
)
# hand-computed: 4 cells of class 1; observed 1 -> 2 on cells 1, 2; simulated on cells 2, 3.
# p_o = 2/4, p(2|1) = p(1|1) = 1/2 in both, p_e = 1/2, p_max = 1: KSim = 0, KTransition = 1
ks_hand <- kappa_simulation(c(1, 1, 1, 1), c(2, 2, 1, 1), c(1, 2, 2, 1))
check(
  abs(ks_hand$ksim) < 1e-12 && abs(ks_hand$ktransition - 1) < 1e-12,
  "KSim, hand-computed example"
)

# --- Fuzzy Kappa Simulation -------------------------------------------------------------------
fk0 <- fuzzy_kappa_simulation(grid, anterior, observed, simulated, 3L, fuzzy_weights(radius = 0))
check(abs(fk0$fks - ks$ksim) < 1e-12, "FKS with a 1 x 1 window equals KSim")
check(
  abs(fuzzy_kappa_simulation(grid, anterior, observed, observed, 3L)$fks - 1) < 1e-12,
  "FKS of a perfect map is 1"
)
fk4 <- fuzzy_kappa_simulation(grid, anterior, observed, simulated, 3L)
check(abs(fk4$fks) < 0.1, "FKS of an independent allocation is near 0")
# a simulation that places each observed change on a neighbour (where the class allows it)
# should gain from the tolerance
shifted <- anterior
obs_i <- which(observed != anterior)
for (i in obs_i) {
  j <- if (i %% nc != 0L) i + 1L else i - 1L
  if (anterior[j] == anterior[i] && shifted[j] == anterior[j]) shifted[j] <- observed[i] else shifted[i] <- observed[i]
}
ks_sh <- kappa_simulation(anterior, observed, shifted)$ksim
fk_sh <- fuzzy_kappa_simulation(grid, anterior, observed, shifted, 3L)$fks
check(fk_sh > ks_sh + 0.2, sprintf("near misses: FKS (%.2f) well above KSim (%.2f)", fk_sh, ks_sh))
# expected_min against simulation
x <- sample(c(0, 0.25, 0.5, 1), 1e5, TRUE)
y <- sample(c(0, 0.5, 0.7), 1e5, TRUE)
check(
  abs(expected_min(x, y) - mean(pmin(sample(x), sample(y)))) < 0.005,
  "expected_min of independent samples"
)

# --- Similarity of differences against evoland -------------------------------------------------
r <- function(v) terra::rast(matrix(v, nr, nc, byrow = TRUE))
for (tr in list(c(1L, 2L), c(2L, 3L))) {
  ev <- evoland::calc_transition_similarity(
    r(anterior),
    r(observed),
    r(simulated),
    from_class = tr[1],
    to_class = tr[2]
  )
  mine <- similarity_of_differences(grid, anterior, observed, simulated, tr[1], tr[2])
  check(
    abs(ev$similarity - mine$similarity) < 1e-9,
    sprintf("similarity of differences %d -> %d equals evoland's", tr[1], tr[2])
  )
}

# --- AUC / TOC ----------------------------------------------------------------------------------
idx <- round(runif(2000), 2)
tg <- runif(2000) < idx
roc <- toc_curve(idx, tg, n_points = 1e6)
n1 <- sum(tg)
n0 <- length(tg) - n1
fpr <- (roc$hits_false_alarms - roc$hits) / n0
tpr <- roc$hits / n1
check(
  abs(sum(diff(fpr) * (head(tpr, -1) + tail(tpr, -1)) / 2) - auc(idx, tg)) < 1e-12,
  "AUC equals the trapezoidal area of the ROC (and the normalised TOC)"
)
check(abs(auc(as.numeric(tg), tg) - 1) < 1e-12, "AUC of a perfect index is 1")

# --- Disagreement and figure of merit -----------------------------------------------------------
d <- disagreement(observed, simulated)
check(
  abs(d$quantity_disagreement + d$allocation_disagreement - mean(observed != simulated)) < 1e-12,
  "quantity + allocation disagreement = 1 - accuracy"
)
f <- figure_of_merit(anterior, observed, simulated)
check(f[, hits + wrong_hits + misses] == sum(observed != anterior), "FoM components cover observed change")

# --- Forecast scores -----------------------------------------------------------------------------
sims <- replicate(30, change(anterior, 0.08))
p <- class_frequencies(sims, 3L)
check(all(abs(rowSums(p) - 1) < 1e-12), "class frequencies sum to 1")
fs <- score_forecast(
  anterior,
  observed,
  p,
  data.table(from = c(1L, 2L), to = c(2L, 3L)),
  anterior != 3L,
  n_members = 30L
)
check(abs(fs$auc_change - 0.5) < 0.05, "AUC of an uninformative ensemble is near 0.5")
message("all checks passed")
