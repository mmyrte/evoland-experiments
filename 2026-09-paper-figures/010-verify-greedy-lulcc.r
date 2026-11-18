#' ---
#' title: "Verification: evoland's greedy allocator against lulcc's Ordered model"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' evoland's greedy allocator (`alloc_greedy_spec()`) is documented as the rank-and-fill of
#' lulcc's Ordered model (Fuchs et al. 2013; Moulds et al. 2015), with transitions in place of
#' classes. Does it give lulcc's maps where the two are the same procedure?
#'
#' **Where they are the same procedure.** lulcc's Ordered model (`lulcc:::.ordered`) walks the
#' classes in a given order. A class that must grow takes, among the cells not yet assigned,
#' those with the highest probability of the class until it reaches its demanded *total*; a
#' class that must shrink releases its least probable cells. evoland's greedy allocator fills
#' each *transition's* demanded count with the source cells of highest transition potential.
#' The two coincide when every growing class has a single source class and no source class
#' feeds two growing classes, so that a class total and a transition count are the same
#' number. They differ otherwise, by design: lulcc sees net change, evoland gross change. The
#' check covers both:
#'
#' | case | transitions | expected |
#' | --- | --- | --- |
#' | one transition | 1 → 2 | identical |
#' | two disjoint transitions | 1 → 2, 3 → 4 | identical |
#' | ties | as above, potentials rounded to 0.01 | identical (both take tied cells in cell order) |
#' | two sources | 1 → 2, 3 → 2 | different: lulcc fills class 2's total from either source |
#' | swap | 1 → 2, 2 → 1 | different: lulcc sees the net change only |
#'
#' Each case on 10 random 50 × 50 landscapes with four classes and random potentials. Both
#' allocators get the same potentials: evoland as transition potentials, lulcc as class
#' probabilities (from a cell of class i, the potential of i → k for each k ≠ i, and the rest
#' for staying), and the same quantity: evoland the transition counts, lulcc the class totals
#' they imply. lulcc's order puts the growing classes first, then the shrinking ones. evoland
#' runs both of its arbitrations (`"ordered"`, in lulcc's order of the growing classes, and
#' `"joint"`).
#'
#' **Two lulcc entry points.** (a) The core, `lulcc:::.ordered()`, called on the class
#' probabilities directly. (b) The full interface (`OrderedModel()`, `allocate()`), which
#' takes suitability from fitted models: as in `2026-09-model-comparison/040-alloc-lulcc.r`,
#' the explanatory variable of class k is the logit of its probability and its model a GLM with
#' coefficients fixed at (0, 1), so that lulcc's `predict()` returns the probability. (b) checks
#' that lulcc's wrapper (prediction, automatic conversion) does not change the result; it runs
#' on the cases expected to be identical.
#'
#' **Requirements.** An evoland.plus source checkout (`EVOLAND_PLUS_SRC`, default
#' `../evoland.plus`; loaded with `pkgload`, its commit is recorded) and lulcc 1.0.4.
#'
#' Writes `results/verification/greedy-lulcc.csv` and `greedy-lulcc-summary.csv`.

#| label: setup
#| output: false
library(data.table)
suppressPackageStartupMessages(library(lulcc))

evoland_src <- normalizePath(Sys.getenv("EVOLAND_PLUS_SRC", "../evoland.plus"))
pkgload::load_all(evoland_src, quiet = TRUE, export_all = TRUE, helpers = FALSE)
evoland_commit <- system2("git", c("-C", evoland_src, "rev-parse", "HEAD"), stdout = TRUE)

res_dir <- "2026-09-paper-figures/results/verification"
dir.create(res_dir, recursive = TRUE, showWarnings = FALSE)

n_side <- 50L
n_landscapes <- 10L
categories <- 1:4

cases <- list(
  "one transition" = list(from = 1L, to = 2L, digits = NA, expected = "identical"),
  "two disjoint transitions" = list(from = c(1L, 3L), to = c(2L, 4L), digits = NA, expected = "identical"),
  "ties" = list(from = c(1L, 3L), to = c(2L, 4L), digits = 2L, expected = "identical"),
  "two sources" = list(from = c(1L, 3L), to = c(2L, 2L), digits = NA, expected = "different"),
  "swap" = list(from = c(1L, 2L), to = c(2L, 1L), digits = NA, expected = "different")
)

#' # One case on one landscape

#| label: functions
make_case <- function(case, seed) {
  set.seed(seed)
  n <- n_side^2
  lulc <- sample(categories, n, replace = TRUE, prob = c(0.4, 0.2, 0.3, 0.1))
  viable <- data.table(
    id_trans = seq_along(case$from),
    id_lulc_anterior = case$from,
    id_lulc_posterior = case$to
  )
  # potentials on the source cells; at least 1e-4 so that lulcc's logit route keeps them apart
  # from the zero probability of the other cells
  potentials <- viable[,
    {
      cells <- which(lulc == id_lulc_anterior)
      p <- stats::rbeta(length(cells), 0.7, 6) * 0.5 / length(case$from) + 1e-4
      if (!is.na(case$digits)) p <- pmax(round(p, case$digits), 10^-case$digits)
      .(id_coord = cells, value = p)
    },
    by = id_trans
  ]
  # demand: a share of each source class, well below its cells with a potential
  quota <- viable[,
    .(id_trans, quota = as.integer(round(sum(lulc == id_lulc_anterior) * runif(1, 0.03, 0.12)))),
    by = seq_len(nrow(viable))
  ][, .(id_trans, quota)]
  list(lulc = lulc, viable = viable, potentials = potentials, quota = quota)
}

evoland_greedy <- function(x, arbitration, order = NULL) {
  ctx <- list(
    anterior = data.table(id_coord = seq_along(x$lulc), id_lulc = x$lulc),
    viable_trans = x$viable,
    quota = x$quota,
    potentials = x$potentials
  )
  posterior <- alloc_greedy_spec(arbitration, order)$one_period(ctx)
  setorder(posterior, id_coord)[["id_lulc"]]
}

# class probabilities: from a cell of class i, the potential of i -> k, and the rest for i
class_probabilities <- function(x) {
  n <- length(x$lulc)
  tprob <- matrix(0, n, length(categories), dimnames = list(NULL, categories))
  tprob[cbind(seq_len(n), x$lulc)] <- 1
  pot <- x$potentials[x$viable, on = "id_trans"]
  tprob[cbind(pot$id_coord, pot$id_lulc_posterior)] <- pot$value
  tprob[cbind(pot$id_coord, pot$id_lulc_anterior)] <-
    tprob[cbind(pot$id_coord, pot$id_lulc_anterior)] - pot$value
  tprob
}

# the class totals the transition counts imply, and lulcc's order: growing classes first
class_demand <- function(x) {
  before <- tabulate(x$lulc, length(categories))
  flows <- x$quota[x$viable, on = "id_trans"]
  after <- before
  for (i in seq_len(nrow(flows))) {
    after[flows$id_lulc_anterior[i]] <- after[flows$id_lulc_anterior[i]] - flows$quota[i]
    after[flows$id_lulc_posterior[i]] <- after[flows$id_lulc_posterior[i]] + flows$quota[i]
  }
  growing <- categories[after > before]
  list(
    before = before,
    after = after,
    order = c(growing, setdiff(categories, growing))
  )
}

lulcc_core <- function(x) {
  d <- class_demand(x)
  lulcc:::.ordered(
    tprob = class_probabilities(x),
    map0.vals = x$lulc,
    demand = d$after,
    categories = categories,
    order = d$order,
    stochastic = FALSE
  )
}

lulcc_interface <- function(x) {
  d <- class_demand(x)
  r0 <- raster::raster(nrows = n_side, ncols = n_side, xmn = 0, xmx = n_side, ymn = 0, ymx = n_side)
  lu <- raster::stack(
    raster::setValues(r0, x$lulc),
    # the observed second map is not used for allocation; lulcc wants two
    raster::setValues(r0, x$lulc)
  )
  names(lu) <- c("lu_0", "lu_1")
  obs <- lulcc::ObsLulcRasterStack(
    x = lu,
    pattern = "lu",
    categories = categories,
    labels = paste0("c", categories),
    t = c(0, 1)
  )
  prob <- class_probabilities(x)
  suit <- raster::stack(lapply(categories, function(k) {
    raster::setValues(r0, stats::qlogis(pmin(pmax(prob[, k], 1e-6), 1 - 1e-6)))
  }))
  names(suit) <- paste0("suit_", categories)
  ef <- lulcc::ExpVarRasterList(x = suit, pattern = "suit")
  train <- lulcc::getPredictiveModelInputData(obs = obs, ef = ef, cells = seq(1, n_side^2, by = 7), t = 0)
  forms <- lapply(categories, function(k) stats::as.formula(paste0("c", k, " ~ suit_", k)))
  models <- suppressWarnings(lulcc::glmModels(formula = forms, family = binomial, data = train, obs = obs))
  for (j in seq_along(models@models)) {
    beta <- models@models[[j]]$coefficients
    models@models[[j]]$coefficients <- setNames(c(0, 1), names(beta))
  }
  model <- lulcc::OrderedModel(
    obs = obs,
    ef = ef,
    models = models,
    time = c(0, 1),
    demand = rbind(d$before, d$after),
    order = d$order
  )
  model <- lulcc::allocate(model, stochastic = FALSE)
  raster::values(model@output[[raster::nlayers(model@output)]])
}

compare_maps <- function(case_name, seed, method, lulcc_map, evoland_map, anterior) {
  changed <- lulcc_map != anterior | evoland_map != anterior
  data.table(
    case = case_name,
    landscape = seed,
    comparison = method,
    cells = length(anterior),
    changed_lulcc = sum(lulcc_map != anterior),
    changed_evoland = sum(evoland_map != anterior),
    cells_differing = sum(lulcc_map != evoland_map),
    share_of_changed_differing = if (any(changed)) mean(lulcc_map[changed] != evoland_map[changed]) else 0,
    class_totals_equal = all(tabulate(lulcc_map, 4L) == tabulate(evoland_map, 4L))
  )
}

#' # All cases

#| label: run
results <- rbindlist(lapply(names(cases), function(case_name) {
  case <- cases[[case_name]]
  rbindlist(lapply(seq_len(n_landscapes), function(seed) {
    x <- make_case(case, seed)
    d <- class_demand(x)
    # evoland's "ordered" priority: transitions into the growing classes in lulcc's order
    trans_order <- x$viable[order(match(id_lulc_posterior, d$order), id_trans), id_trans]
    evo_ordered <- evoland_greedy(x, "ordered", trans_order)
    evo_joint <- evoland_greedy(x, "joint")
    core <- lulcc_core(x)
    out <- rbind(
      compare_maps(case_name, seed, "lulcc core vs evoland ordered", core, evo_ordered, x$lulc),
      compare_maps(case_name, seed, "lulcc core vs evoland joint", core, evo_joint, x$lulc)
    )
    if (case$expected == "identical") {
      full <- lulcc_interface(x)
      out <- rbind(
        out,
        compare_maps(case_name, seed, "lulcc interface vs evoland ordered", full, evo_ordered, x$lulc),
        compare_maps(case_name, seed, "lulcc interface vs lulcc core", full, core, x$lulc)
      )
    }
    expected_result <- case$expected
    out[, expected := expected_result]
  }))
}))
fwrite(results, file.path(res_dir, "greedy-lulcc.csv"))

summary <- results[,
  .(
    landscapes = .N,
    identical = sum(cells_differing == 0L),
    cells_differing = sum(cells_differing),
    changed_cells = sum(changed_lulcc),
    share_of_changed_differing = round(mean(share_of_changed_differing), 3),
    class_totals_equal = all(class_totals_equal)
  ),
  keyby = .(case, expected, comparison)
][, evoland_commit := substr(evoland_commit, 1, 7)]
fwrite(summary, file.path(res_dir, "greedy-lulcc-summary.csv"))
knitr::kable(summary)

stopifnot(
  "greedy: differs from lulcc where they are the same procedure" =
    results[expected == "identical", all(cells_differing == 0L)]
)
