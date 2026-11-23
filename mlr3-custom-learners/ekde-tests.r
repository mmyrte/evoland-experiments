# Checks of ekde.r; run from the repository root:
#   Rscript mlr3-custom-learners/ekde-tests.r
#
# Parts that need more than R are skipped with a message when it is missing:
# - (c) and the Python side of (g) need the reference: a Python with clumpy, ekde and hyperclip,
#   found in this order: $EKDE_REFERENCE_PYTHON; the repository's uv environment
#   (python-env.r, `uv sync`); `uv run` in the uv project $EKDE_CLUMPY_DIR (default ../clumpy,
#   a checkout of mmyrte/clumpy, where `uv sync` builds it);
# - (f) needs the evoland.plus source in $EKDE_EVOLAND_DIR (default ../evoland.plus), loaded with
#   pkgload::load_all();
# - (g) runs fits on 1e6 rows; EKDE_TIMING=0 skips it.
source("mlr3-custom-learners/ekde.r")
suppressPackageStartupMessages(library(data.table))
lgr::get_logger("mlr3")$set_threshold("warn")

check <- function(ok, what) {
  if (!isTRUE(all(ok))) stop("FAILED: ", what, call. = FALSE)
  message("ok: ", what)
}
note <- function(...) message("   ", sprintf(...))
cpp <- ekde_impl$cpp()
scratch <- tempfile("ekde-tests-")
dir.create(scratch)

# --- brute-force estimator ----------------------------------------------------------------------
# The same estimator written out directly, without the grid search: every training row in a cell
# within (q - 1) / 2 cells of the query's cell in every coordinate counts with weight 1 / (clipped
# volume of its cell's hypercube). The whitening, bandwidth and bounds are those of the model,
# which are checked on their own below.

# share of the square of side `side` centred at `ctr` on the positive side of the half-planes
# a . x + r >= 0 (columns of A), by Sutherland-Hodgman clipping of the polygon
clip_square <- function(ctr, side, A, R) {
  P <- cbind(ctr[1] + side / 2 * c(-1, 1, 1, -1), ctr[2] + side / 2 * c(-1, -1, 1, 1))
  for (k in seq_along(R)) {
    g <- drop(P %*% A[, k]) + R[k]
    out <- NULL
    np <- nrow(P)
    if (np == 0L) break
    for (i in seq_len(np)) {
      j <- if (i == np) 1L else i + 1L
      if (g[i] >= 0) out <- rbind(out, P[i, ])
      if ((g[i] >= 0) != (g[j] >= 0)) {
        t <- g[i] / (g[i] - g[j])
        out <- rbind(out, P[i, ] + t * (P[j, ] - P[i, ]))
      }
    }
    P <- if (is.null(out)) matrix(numeric(), 0, 2) else out
  }
  if (nrow(P) < 3L) return(0)
  x <- P[, 1]
  y <- P[, 2]
  abs(sum(x * c(y[-1], y[1]) - c(x[-1], x[1]) * y)) / 2 / side^2
}
clip_interval <- function(ctr, side, A, R) {
  lo <- ctr - side / 2
  hi <- ctr + side / 2
  for (k in seq_along(R)) {
    # a x + r >= 0
    b <- -R[k] / A[1, k]
    if (A[1, k] > 0) lo <- max(lo, b) else hi <- min(hi, b)
  }
  max(0, hi - lo) / side
}
# the reference's rule for which half-spaces a hypercube is tested against (hyperfunc.pyx
# inf_distances_to_hyperplanes())
reaches <- function(ctr, side, a, r) {
  abs(sum(ctr * a) + r) / sum(a^2) * max(abs(a)) <= side / 2
}

brute_density <- function(model, X, Y, side = model$h) {
  Xw <- sweep(as.matrix(X), 2L, model$mean) %*% model$transform
  Yw <- sweep(as.matrix(Y), 2L, model$mean) %*% model$transform
  cell <- function(W) trunc(sweep(W, 2L, model$x_min) / model$dx)
  Zx <- cell(Xw)
  Zy <- cell(Yw)
  w <- rep(1, nrow(Zx))
  if (length(model$R) > 0L) {
    ctr <- sweep(Zx * model$dx, 2L, model$x_min, "+") + 0.5 * model$dx
    w <- 1 /
      apply(ctr, 1L, function(c0) {
        use <- vapply(
          seq_along(model$R),
          function(k) reaches(c0, side, model$A[, k], model$R[k]),
          logical(1)
        )
        if (!any(use)) return(1)
        clip <- if (model$d == 1L) clip_interval else clip_square
        clip(c0, side, model$A[, use, drop = FALSE], model$R[use])
      })
  }
  margin <- (model$q - 1L) / 2L
  f <- apply(Zy, 1L, function(z) {
    near <- apply(abs(sweep(Zx, 2L, z)), 1L, max) <= margin
    sum(w[near])
  })
  outside <- rep(FALSE, nrow(Yw))
  for (k in seq_along(model$R)) {
    outside <- outside | drop(Yw %*% model$A[, k]) + model$R[k] + 1e-7 < 0
  }
  f[outside] <- 0
  f / (nrow(Zx) * (2 * model$h)^model$d) / model$scale
}

# Sum of the estimate over every grid cell times the cell's volume in the original space: the
# integral of the binned estimator. Cell k is evaluated at a point that truncates to k.
grid_integral <- function(model) {
  margin <- (model$q - 1L) / 2L
  rng <- lapply(seq_len(model$d), function(j) {
    (min(model$U[, j]) - margin):(max(model$U[, j]) + margin)
  })
  K <- as.matrix(expand.grid(rng))
  W <- sweep((K + ifelse(K < 0, -0.5, 0.5)) * model$dx, 2L, model$x_min, "+")
  tm_inv <- solve(model$transform)
  X <- sweep(W %*% tm_inv, 2L, model$mean, "+")
  sum(ekde_predict(model, X)) * model$dx^model$d * model$scale
}

# --- (a) the estimator: whitening, bandwidth, grid search, integral -----------------------------
set.seed(1)
x1 <- matrix(rgamma(400, 2), ncol = 1)
y1 <- matrix(seq(-1, 9, length.out = 300), ncol = 1)
x2 <- cbind(rgamma(500, 2), 0)
x2[, 2] <- 0.6 * x2[, 1] + rexp(500)
y2 <- cbind(runif(400, -1, 8), runif(400, -1, 10))

m2 <- ekde_fit(x2)
xw <- sweep(x2, 2L, m2$mean) %*% m2$transform
check(max(abs(crossprod(xw) / (nrow(x2) - 1) - diag(2))) < 1e-12, "whitened data have unit covariance")
check(
  max(abs(m2$transform %*% t(m2$transform) - solve(stats::cov(x2)))) < 1e-10,
  "the whitening W satisfies W W' = C^-1 (Mazy 2022, Eq. 2.10)"
)
terrell <- function(n, d) {
  ((d + 8)^((d + 6) / 2) * pi^(d / 2) * 2^-d / (16 * n * (d + 2) * gamma(d / 2 + 4)))^(1 / (d + 4))
}
check(abs(m2$h - terrell(500, 2)) < 1e-15, "Terrell's bandwidth (Mazy 2022, Eq. 2.24)")
check(
  abs(ekde_fit(x2, bandwidth = "scott")$h - 500^(-1 / 6) * 1.288) < 1e-15 &&
    ekde_fit(x2, h = 0.3)$h == 0.3,
  "Scott's rule and a fixed bandwidth"
)

for (case in list(
  list(x = x1, y = y1, bounds = "none", what = "1-D"),
  list(x = x1, y = y1, bounds = "left", what = "1-D, left bound"),
  list(x = x1, y = y1, bounds = "both", what = "1-D, both bounds"),
  list(x = x2, y = y2, bounds = c("none", "none"), what = "2-D"),
  list(x = x2, y = y2, bounds = c("left", "left"), what = "2-D, left bounds"),
  list(x = x2, y = y2, bounds = c("both", "left"), what = "2-D, both + left bounds")
)) {
  for (correction in c("reference", "kernel")) {
    if (all(case$bounds == "none") && correction == "kernel") next
    m <- ekde_fit(case$x, bounds = case$bounds, boundary_correction = correction)
    yy <- rbind(case$y, case$x[1:50, , drop = FALSE])
    f <- ekde_predict(m, yy)
    fb <- brute_density(m, case$x, yy, side = if (correction == "kernel") 2 * m$h else m$h)
    err <- max(abs(f - fb) / pmax(fb, 1e-300))
    check(
      err < 1e-12,
      sprintf("%s (%s correction): grid search = brute force (max rel. diff. %.1e)", case$what, correction, err)
    )
  }
}
for (case in list(list(x = x1, what = "1-D"), list(x = x2, what = "2-D"))) {
  integral <- grid_integral(ekde_fit(case$x))
  check(abs(integral - 1) < 1e-12, sprintf("%s: the estimate integrates to 1 (%.15f)", case$what, integral))
}

# the clipped volume (Cho & Kim 2020) against exact areas and Monte Carlo
set.seed(2)
err <- 0
for (i in 1:200) {
  A <- matrix(rnorm(2 * 3), 2)
  R <- rnorm(3, 0.5)
  err <- max(err, abs(cpp$.ekde_unit_clipped_volume(A, R, 1e-14) - clip_square(c(0.5, 0.5), 1, A, R)))
}
check(err < 1e-12, sprintf("clipped area of the unit square, 200 random cases (max diff. %.1e)", err))
for (d in c(3L, 5L)) {
  worst <- 0
  for (i in 1:5) {
    A <- matrix(rnorm(d * 2), d)
    R <- -colSums(A) * runif(2, 0.2, 0.6) # the hyperplanes cut the cube
    u <- matrix(runif(2e5 * d), ncol = d)
    inside <- rowSums((u %*% A + rep(R, each = nrow(u))) >= 0) == 2
    mc <- mean(inside)
    v <- cpp$.ekde_unit_clipped_volume(A, R, 1e-14)
    worst <- max(worst, abs(v - mc) / sqrt(mc * (1 - mc) / nrow(u)))
  }
  check(worst < 4.5, sprintf("clipped volume of the %d-cube within Monte Carlo error (max %.1f sd)", d, worst))
}

# --- (b) the boundary correction at a bound: uniform data on [0, 1] -------------------------------
set.seed(3)
xu <- matrix(runif(20000), ncol = 1)
grid01 <- matrix(seq(0, 1, length.out = 20001), ncol = 1)
bias <- list()
for (correction in c("none", "reference", "kernel")) {
  m <- ekde_fit(xu, bounds = "both", lower = 0, upper = 1, boundary_correction = correction)
  f <- ekde_predict(m, grid01)
  h_orig <- m$h * m$scale # bandwidth in the units of x
  at_bound <- grid01[, 1] <= h_orig / 4 | grid01[, 1] >= 1 - h_orig / 4
  interior <- grid01[, 1] >= 2 * h_orig & grid01[, 1] <= 1 - 2 * h_orig
  bias[[correction]] <- c(
    bound = mean(f[at_bound]) - 1,
    interior = mean(f[interior]) - 1,
    mass = mean(f) - 1
  )
  note(
    "uniform on [0, 1], correction %-9s: mean bias within h/4 of a bound %+.3f, interior %+.3f, mass on [0, 1] %+.4f (h = %.3f)",
    correction,
    bias[[correction]]["bound"],
    bias[[correction]]["interior"],
    bias[[correction]]["mass"],
    h_orig
  )
}
check(
  abs(bias$reference["bound"]) < 0.8 * abs(bias$none["bound"]) &&
    abs(bias$kernel["bound"]) < 0.6 * abs(bias$none["bound"]),
  paste(
    "the boundary correction reduces the bias at the bound, the reference's by about a quarter,",
    "the kernel-support one by about half; neither removes it (it renormalizes at the source)"
  )
)
check(
  abs(bias$kernel["mass"]) < 0.002 && abs(bias$none["mass"]) > 0.02,
  "with the kernel-support correction the estimate keeps its mass within the bounds"
)
check(all(abs(sapply(bias, `[`, "interior")) < 0.02), "no bias in the interior")

# --- (c) against the Python reference ---------------------------------------------------------
find_reference <- function() {
  probe <- function(cmd, args) {
    out <- suppressWarnings(tryCatch(
      system2(cmd, c(args, "-c", shQuote("import ekde, hyperclip, clumpy")), stdout = TRUE, stderr = TRUE),
      error = function(e) structure("", status = 1L)
    ))
    is.null(attr(out, "status")) || attr(out, "status") == 0L
  }
  cand <- Sys.getenv("EKDE_REFERENCE_PYTHON")
  if (nzchar(cand) && probe(cand, character())) return(list(cmd = cand, args = character()))
  source("python-env.r", local = TRUE)
  cand <- project_python()
  if (file.exists(cand) && probe(cand, character())) return(list(cmd = cand, args = character()))
  clumpy <- Sys.getenv("EKDE_CLUMPY_DIR", "../clumpy")
  if (nzchar(Sys.which("uv")) && dir.exists(clumpy)) {
    args <- c("run", "--quiet", "--project", normalizePath(clumpy), "python")
    if (probe("uv", args)) return(list(cmd = "uv", args = args))
  }
  NULL
}
reference <- find_reference()

# writes data with all 17 digits, so that both sides read the same doubles
write_exact <- function(x, path) {
  x <- as.data.table(x)
  for (j in names(x)) if (is.double(x[[j]])) set(x, j = j, value = sprintf("%.17g", x[[j]]))
  fwrite(x, path)
}
run_reference <- function(cfg, files) {
  wd <- tempfile("ref-", tmpdir = scratch)
  dir.create(wd)
  for (f in names(files)) write_exact(files[[f]], file.path(wd, f))
  jsonlite::write_json(cfg, file.path(wd, "config.json"), auto_unbox = TRUE, digits = NA)
  out <- system2(
    reference$cmd,
    c(reference$args, "mlr3-custom-learners/ekde-reference/reference.py", wd),
    stdout = TRUE,
    stderr = TRUE
  )
  if (!is.null(attr(out, "status"))) stop("reference failed:\n", paste(out, collapse = "\n"))
  timing <- as.numeric(strsplit(sub("^TIMING,", "", grep("^TIMING,", out, value = TRUE)), ",")[[1]])
  read <- function(f) as.matrix(fread(file.path(wd, f), header = TRUE))
  list(
    dir = wd,
    timing = c(fit = timing[1], predict = timing[2]),
    info = jsonlite::read_json(file.path(wd, "info.json")),
    read = read
  )
}
# rows in lexicographic order: ekde_fit() sorts the rows before whitening, the reference whitens
# them in the order given; the same order makes both round alike
lex <- function(x) as.matrix(x)[do.call(order, unname(as.data.frame(x))), , drop = FALSE]
rel_diff <- function(a, b) max(abs(a - b) / pmax(abs(b), 1e-300))

if (is.null(reference)) {
  message("SKIPPED (c): no Python with clumpy, ekde and hyperclip; see the head of this file")
} else {
  message("(c) reference: ", paste(c(reference$cmd, reference$args), collapse = " "))

  # c1: the clipped volumes, on identical inputs
  set.seed(4)
  for (d in c(2L, 4L)) {
    A <- matrix(rnorm(d * d), d)
    R <- rnorm(d)
    C <- matrix(rnorm(300 * d, sd = 0.5), ncol = d)
    side <- 0.8
    ref <- run_reference(
      list(mode = "volumes", h = side, zero = 1e-14),
      list("A.csv" = A, "R.csv" = data.table(r = R), "C.csv" = C)
    )
    v <- cpp$.ekde_clipped_volumes(A, R, C, side, 1e-14)
    vr <- ref$read("volumes.csv")[, 1]
    check(
      max(abs(v - vr)) < 1e-12,
      sprintf("c1, %d-D: clipped volumes = hyperclip's (max diff. %.1e, %d of 300 clipped)", d, max(abs(v - vr)), sum(vr < 1))
    )
  }

  # c2: densities, signs of the whitening made canonical in the reference as well
  for (case in list(
    list(x = x1, y = y1, bounds = list("none"), what = "1-D"),
    list(x = x1, y = y1, bounds = list("left"), what = "1-D, left bound"),
    list(x = x2, y = y2, bounds = c("left", "both"), what = "2-D, left + both bounds"),
    list(x = cbind(x2, x2[, 1] * rexp(500)), y = cbind(y2, runif(400, 0, 20)),
         bounds = rep("left", 3), what = "3-D, left bounds")
  )) {
    xs <- lex(case$x)
    colnames(xs) <- paste0("x", seq_len(ncol(xs)))
    yy <- case$y
    colnames(yy) <- colnames(xs)
    ref <- run_reference(
      list(mode = "density", bounds = case$bounds, canonical_signs = TRUE),
      list("train.csv" = xs, "pred.csv" = yy)
    )
    m <- ekde_fit(xs, bounds = unlist(case$bounds))
    f <- ekde_predict(m, yy)
    fr <- ref$read("density.csv")[, 1]
    check(
      abs(m$h - ref$info$h[[1]]) < 1e-15 && rel_diff(f, fr) < 1e-10,
      sprintf("c2, %s: density = reference's (max rel. diff. %.1e)", case$what, rel_diff(f, fr))
    )
    if (case$what == "2-D, left + both bounds") {
      ref_raw <- run_reference(
        list(mode = "density", bounds = case$bounds, canonical_signs = FALSE),
        list("train.csv" = xs, "pred.csv" = yy)
      )
      fr_raw <- ref_raw$read("density.csv")[, 1]
      note(
        "c2, %s, the reference with its own singular-vector signs: max rel. diff. %.2f (%s)",
        case$what,
        rel_diff(f, fr_raw),
        if (rel_diff(f, fr_raw) < 1e-10) "the signs agree here" else "a reflected grid, see README"
      )
    }
  }

  # c3: Bayes-eKDE with clumpy's adjustment, as 015-clumpy-kde runs it: three classes, three
  # features, left bounds; and two classes, five features with integer counts (evoland-like)
  make_case <- function(n, d, k, seed) {
    set.seed(seed)
    x <- matrix(rgamma(n * d, 2), ncol = d)
    if (d >= 5) x[, 4:5] <- rpois(n * 2, 3) # integer counts, with ties
    lin <- x[, 1] - 0.5 * x[, 2] + if (d >= 3) 0.3 * x[, 3] else 0
    y <- if (k == 2L) {
      as.integer(runif(n) < plogis(-3 + lin))
    } else {
      z <- lin + rnorm(n)
      cut(z, stats::quantile(z, c(0, 0.6, 0.9, 1)), labels = FALSE, include.lowest = TRUE)
    }
    colnames(x) <- paste0("x", seq_len(d))
    o <- do.call(order, unname(as.data.frame(x)))
    list(x = x[o, , drop = FALSE], y = as.integer(y[o]))
  }
  for (case in list(
    list(train = make_case(4000, 3, 3L, 5), pred = make_case(3000, 3, 3L, 6), what = "3 classes, 3-D", rem = "1"),
    list(train = make_case(6000, 5, 2L, 7), pred = make_case(5000, 5, 2L, 8), what = "2 classes, 5-D", rem = "0")
  )) {
    d <- ncol(case$train$x)
    cfg <- list(mode = "bayes", bounds = rep("left", d), canonical_signs = TRUE, seed = 1)
    ref <- run_reference(
      cfg,
      list("train.csv" = cbind(as.data.table(case$train$x), y = case$train$y), "pred.csv" = case$pred$x)
    )
    task <- mlr3::as_task_classif(
      cbind(as.data.table(case$train$x), y = factor(case$train$y)),
      target = "y"
    )
    learner <- mlr3::lrn(
      "classif.ekde",
      bounds = "left",
      adjustment = "clumpy",
      remainder_class = case$rem,
      predict_type = "prob"
    )
    learner$train(task)
    newdata <- as.data.table(case$pred$x)
    dv <- learner$densities(newdata)
    dvr <- ref$read("dens_v.csv")[, colnames(dv)]
    dm <- ekde_predict(ekde_fit(case$pred$x, bounds = "left"), case$pred$x)
    dmr <- ref$read("dens.csv")[, 1]
    P <- learner$predict_newdata(newdata)$prob
    Pr <- ref$read("prob.csv")[, colnames(P)]
    others <- setdiff(colnames(P), case$rem)
    pd <- max(abs(P[, others] - Pr[, others]))
    check(
      rel_diff(dv, dvr) < 1e-8 && rel_diff(dm, dmr) < 1e-8 && pd < 1e-8,
      sprintf(
        "c3, %s: P(z|v) (max rel. diff. %.1e), P(z) (%.1e) and P(v|z) (max abs. diff. %.1e) = reference's",
        case$what, rel_diff(dv, dvr), rel_diff(dm, dmr), pd
      )
    )
    if (case$what == "2 classes, 5-D") {
      # the replay issue of the model comparison: the same training and prediction rows in
      # another order
      ref_raw <- run_reference(
        modifyList(cfg, list(canonical_signs = FALSE)),
        list("train.csv" = cbind(as.data.table(case$train$x), y = case$train$y), "pred.csv" = case$pred$x)
      )
      p_raw <- ref_raw$read("prob.csv")[, "1"]
      worst <- 0
      for (s in 1:3) {
        set.seed(100 + s)
        perm <- sample(nrow(case$pred$x))
        perm_train <- sample(nrow(case$train$x))
        ref_perm <- run_reference(
          modifyList(cfg, list(canonical_signs = FALSE)),
          list(
            "train.csv" = cbind(as.data.table(case$train$x), y = case$train$y)[perm_train],
            "pred.csv" = case$pred$x[perm, ]
          )
        )
        worst <- max(worst, abs(p_raw[perm] - ref_perm$read("prob.csv")[, "1"]))
      }
      note(
        "c3, the reference on the same rows in three other orders: P(1|z) differs by up to %.3f",
        worst
      )
      note(
        "c3, the reference with its own singular-vector signs against the port: P(1|z) differs by up to %.3f",
        max(abs(P[, "1"] - p_raw))
      )
      P_perm <- learner$predict_newdata(newdata[perm])$prob
      check(
        max(abs(P_perm - P[perm, ])) < 1e-12,
        sprintf(
          "c3: the port, adjustment = \"clumpy\", on the prediction rows in another order: max diff. %.1e (rounding of the column means)",
          max(abs(P_perm - P[perm, ]))
        )
      )
      learner_ipf <- mlr3::lrn("classif.ekde", bounds = "left", predict_type = "prob")
      learner_ipf$train(task)
      learner_perm <- learner_ipf$clone(deep = TRUE)
      learner_perm$train(mlr3::as_task_classif(
        cbind(as.data.table(case$train$x), y = factor(case$train$y))[sample(.N)],
        target = "y"
      ))
      check(
        identical(learner_perm$model, learner_ipf$model) &&
          identical(learner_ipf$predict_newdata(newdata[perm])$prob, learner_ipf$predict_newdata(newdata)$prob[perm, ]),
        "c3: the default learner gives an identical model and identical probabilities for the rows in any order"
      )
    }
  }
}

# --- (d) the default learner: probabilities consistent with the priors ---------------------------
mk <- function(n, seed, k) {
  set.seed(seed)
  x <- matrix(rgamma(n * 3, 2), ncol = 3, dimnames = list(NULL, c("a", "b", "c")))
  lin <- x[, 1] - 0.5 * x[, 2] + 0.3 * x[, 3]
  y <- if (k == 2L) {
    factor(runif(n) < plogis(-3 + lin), c("FALSE", "TRUE"))
  } else {
    factor(cut(lin + rnorm(n), c(-Inf, 1, 3, Inf), labels = c("p", "q", "r")))
  }
  cbind(as.data.table(x), y = y)
}
for (k in c(2L, 3L)) {
  dat <- mk(3000, 11 + k, k)
  task <- mlr3::as_task_classif(dat, target = "y", positive = if (k == 2L) "TRUE")
  learner <- mlr3::lrn("classif.ekde", predict_type = "prob", bounds = "left")
  learner$train(task)
  prob <- learner$predict(task)$prob
  prior <- prop.table(table(dat$y))
  check(
    all(prob >= 0 & prob <= 1) && max(abs(rowSums(prob) - 1)) < 1e-12,
    sprintf("(d) %d classes: probabilities in [0, 1] summing to 1", k)
  )
  dev <- max(abs(colMeans(prob) - as.numeric(prior[colnames(prob)])))
  check(dev < 1e-9, sprintf("(d) %d classes: their mean over the training rows is the prior (max diff. %.1e)", k, dev))
  # without the adjustment, plain Bayes' rule with the mixture as rho(z)
  learner_none <- mlr3::lrn("classif.ekde", predict_type = "prob", bounds = "left", adjustment = "none")
  learner_none$train(task)
  dens <- learner_none$densities(dat)
  num <- dens * rep(as.numeric(prior[colnames(dens)]), each = nrow(dens))
  check(
    max(abs(learner_none$predict(task)$prob - num / rowSums(num))) < 1e-12,
    sprintf("(d) %d classes: adjustment = \"none\" is P(v) rho(z|v) / sum_w P(w) rho(z|w)", k)
  )
  note(
    "(d) %d classes: without the adjustment the mean probabilities deviate from the priors by up to %.4f; ipf factors %s",
    k,
    max(abs(colMeans(learner_none$predict(task)$prob) - as.numeric(prior[colnames(dens)]))),
    paste(sprintf("%.3f", learner$model$factors), collapse = ", ")
  )
}
dat2 <- mk(3000, 13, 2L)
task2 <- mlr3::as_task_classif(dat2, target = "y", positive = "TRUE")
learner <- mlr3::lrn("classif.ekde", bounds = "left")
learner$train(task2)
check(
  is.factor(learner$predict(task2)$response) && learner$predict(task2)$score(mlr3::msr("classif.acc")) > 0.8,
  "(d) predict_type = \"response\""
)
learner_thr <- mlr3::lrn("classif.ekde", bounds = "left", predict_type = "prob", num_threads = 3L)
learner_thr$train(task2)
learner_one <- mlr3::lrn("classif.ekde", bounds = "left", predict_type = "prob")
learner_one$train(task2)
check(
  identical(learner_thr$model, learner_one$model) &&
    identical(learner_thr$predict(task2)$prob, learner_one$predict(task2)$prob),
  "(d) num_threads = 3 gives the identical model and probabilities"
)
learner_cl <- mlr3::lrn("classif.ekde", bounds = "left", predict_type = "prob", adjustment = "clumpy")
learner_cl$train(task2)
prob_cl <- suppressWarnings(learner_cl$predict(task2)$prob)
check(
  all(prob_cl >= 0 & prob_cl <= 1) && max(abs(rowSums(prob_cl) - 1)) < 1e-12,
  "(d) adjustment = \"clumpy\", two classes: the negative class takes the remainder"
)
far <- data.table(a = 100, b = 100, c = 100)
learner$predict_type <- "prob"
check(
  isTRUE(all.equal(as.numeric(learner$predict_newdata(far)$prob), as.numeric(learner$model$prior))),
  "(d) a row where every class density is 0 gets the priors (zero_density = \"prior\")"
)
check(
  inherits(try(mlr3::lrn("classif.ekde")$train(mlr3::tsk("german_credit")), silent = TRUE), "try-error"),
  "(d) factor features are rejected"
)
err <- try(
  mlr3::lrn("classif.ekde")$train(mlr3::as_task_classif(
    data.table(a = rnorm(100), b = 1, y = factor(rep(c("u", "v"), 50))),
    target = "y"
  )),
  silent = TRUE
)
check(grepl("constant column.*b", err), "(d) a constant feature is an error naming it")

# --- (e) serialization, a fresh session, a mirai daemon -----------------------------------------
learner <- mlr3::lrn("classif.ekde", predict_type = "prob", bounds = "left")
learner$train(task2)
p0 <- learner$predict(task2)$prob
blob <- qs2::qs_serialize(learner)
learner2 <- qs2::qs_deserialize(blob)
check(identical(learner2$predict(task2)$prob, p0), "(e) qs2 serialize / deserialize predicts identically")
has_extptr <- function(x) {
  if (typeof(x) == "externalptr") return(TRUE)
  if (is.list(x)) return(any(vapply(x, has_extptr, logical(1))))
  FALSE
}
check(!has_extptr(learner$model), "(e) the model holds no external pointer")
path <- file.path(scratch, "learner.qs2")
qs2::qs_save(learner, path)
fwrite(dat2, file.path(scratch, "dat2.csv"))
saveRDS(p0, file.path(scratch, "p0.rds"))
# a fresh R session that does not source ekde.r: the learner loads the compiled code itself
out <- system2(
  file.path(R.home("bin"), "Rscript"),
  c(
    "--vanilla",
    "-e",
    shQuote(sprintf(
      paste(
        "l <- qs2::qs_read('%s'); d <- data.table::fread('%s', stringsAsFactors = TRUE);",
        "p <- l$predict_newdata(d)$prob; cat(identical(p, readRDS('%s')))"
      ),
      path,
      file.path(scratch, "dat2.csv"),
      file.path(scratch, "p0.rds")
    ))
  ),
  stdout = TRUE,
  stderr = TRUE
)
check(identical(tail(out, 1), "TRUE"), "(e) predicts identically in a fresh session without sourcing ekde.r")
if (requireNamespace("mirai", quietly = TRUE)) {
  mirai::daemons(2)
  res <- mirai::mirai(
    {
      l <- learner$clone(deep = TRUE)
      l$train(task)
      list(prob = l$predict(task)$prob, model = l$model)
    },
    learner = mlr3::lrn("classif.ekde", predict_type = "prob", bounds = "left"),
    task = task2
  )[]
  pred_daemon <- mirai::mirai(qs2::qs_deserialize(blob)$predict(task)$prob, blob = blob, task = task2)[]
  mirai::daemons(0)
  check(
    identical(res$prob, p0) && identical(res$model, learner$model) && identical(pred_daemon, p0),
    "(e) trains and predicts identically on mirai daemons, which did not source ekde.r"
  )
}

# --- (f) as an evoland.plus transition model ----------------------------------------------------
evoland_dir <- Sys.getenv("EKDE_EVOLAND_DIR", "../evoland.plus")
if (!file.exists(file.path(evoland_dir, "DESCRIPTION")) || !requireNamespace("pkgload", quietly = TRUE)) {
  message("SKIPPED (f): no evoland.plus source in ", evoland_dir, " (set EKDE_EVOLAND_DIR)")
} else {
  suppressMessages(pkgload::load_all(evoland_dir, quiet = TRUE, helpers = FALSE))
  source(file.path(evoland_dir, "inst", "tinytest", "helper_testdb.R"))
  db <- suppressMessages(make_test_db(
    include_neighbors = FALSE,
    include_trans_preds = TRUE,
    include_alloc_params = FALSE
  ))
  # the fixture's predictors: elevation (float), population (int), is_protected (bool),
  # soil_type (factor); the learner takes the numeric ones
  numeric_preds <- db$pred_meta_t[data_type %in% c("float", "int"), id_pred]
  tp <- db$trans_preds_t
  tp[, is_selected := id_pred %in% numeric_preds]
  suppressMessages(db$trans_preds_t <- tp)
  learner <- mlr3::lrn("classif.ekde", predict_type = "prob", bounds = "left")
  models <- suppressMessages(db$fit_full_models(learner = learner))
  check(
    nrow(models) == nrow(db$trans_meta_t[is_viable == TRUE]) &&
      all(!vapply(models$learner_full, is.null, logical(1))),
    "(f) fit_full_models() fits classif.ekde for every viable transition"
  )
  suppressMessages(db$trans_models_t <- models)
  suppressMessages(db$predict_trans_pot(
    id_period_post = 4L,
    select_score = "no.crossval",
    select_maximize = TRUE
  ))
  pot <- db$trans_pot_t
  check(
    nrow(pot) > 0L && all(pot$value >= 0 & pot$value <= 1) &&
      setequal(unique(pot$id_trans), models$id_trans),
    sprintf("(f) predict_trans_pot() writes potentials in [0, 1] for every transition (%d rows)", nrow(pot))
  )
  # each model's mean potential over its training rows is the observed transition rate
  item <- models[1L]
  fitted <- qs2::qs_deserialize(item$learner_full[[1]])
  train <- db$pred_wide_train_v(id_trans = item$id_trans, id_pred = sort(numeric_preds))
  p <- fitted$predict_newdata(train)$prob[, "TRUE"]
  check(
    abs(mean(p) - mean(train$did_transition)) < 1e-9,
    sprintf("(f) its mean potential over the training rows is the transition rate (%.4f)", mean(train$did_transition))
  )
}

# --- (g) timing against the reference -----------------------------------------------------------
if (identical(Sys.getenv("EKDE_TIMING"), "0")) {
  message("SKIPPED (g): EKDE_TIMING=0")
} else {
  sizes <- expand.grid(n = c(1e5, 1e6), d = c(3L, 5L))
  timings <- NULL
  for (i in seq_len(nrow(sizes))) {
    n <- sizes$n[i]
    d <- sizes$d[i]
    set.seed(20 + i)
    x <- matrix(rgamma(n * d, 2), ncol = d, dimnames = list(NULL, paste0("x", seq_len(d))))
    y <- as.integer(runif(n) < plogis(-4 + x[, 1] - 0.5 * x[, 2]))
    dat <- cbind(as.data.table(x), y = factor(y))
    task <- mlr3::as_task_classif(dat, target = "y", positive = "1")
    for (setting in list(list("ipf", 1L), list("clumpy", 1L), list("ipf", 4L))) {
      learner <- mlr3::lrn(
        "classif.ekde",
        predict_type = "prob",
        bounds = "left",
        adjustment = setting[[1]],
        num_threads = setting[[2]]
      )
      t_fit <- system.time(learner$train(task))[["elapsed"]]
      t_pred <- system.time(suppressWarnings(learner$predict(task)))[["elapsed"]]
      timings <- rbind(
        timings,
        data.table(n = n, d = d, what = sprintf("port, %s, %d thread(s)", setting[[1]], setting[[2]]), fit = t_fit, predict = t_pred)
      )
    }
    if (!is.null(reference)) {
      ref <- run_reference(
        list(mode = "bayes", bounds = rep("left", d), seed = 1),
        list("train.csv" = dat[, y := as.integer(as.character(y))], "pred.csv" = x)
      )
      timings <- rbind(timings, data.table(n = n, d = d, what = "reference", fit = ref$timing[["fit"]], predict = ref$timing[["predict"]]))
    }
    print(utils::tail(timings, 4L))
  }
  message("(g) wall time in seconds; fit: the class densities (and for ipf the prior factors), ",
          "predict: the densities at all rows (and for clumpy and the reference, P(z) fitted on them)")
  print(timings)
}

unlink(scratch, recursive = TRUE)
