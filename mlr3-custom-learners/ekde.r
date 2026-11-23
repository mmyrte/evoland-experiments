# Bayes-eKDE transition probabilities (Mazy & Longaretti) as an mlr3 learner.
# Source this file; it compiles ekde.cpp (once per machine, see below), defines
# `LearnerClassifEkde` and registers it as "classif.ekde" in mlr3::mlr_learners, and defines
# ekde_fit() / ekde_predict(), the density estimator on its own. See README.md in this directory
# for the method and the parameters.
#
# SPDX-License-Identifier: GPL-3.0-or-later
# A derived work of the reference implementation (mmyrte/clumpy at e9f73b9): the density
# estimator ports ekde (BSD-3-Clause, Copyright (c) François-Rémi Mazy; its notice is kept in
# ekde.cpp) and hyperclip (GPL-3.0), the Bayes adjustment ports clumpy's
# transition_probability_estimation/_bayes.py (GPL-3.0). Hence GPL-3.0 (or later) as a whole.
#
# Compilation. ekde.cpp is compiled with Rcpp::sourceCpp() into `.cache/rcpp-ekde/` under the
# directory this file is sourced from (the repository root; git-ignored), keyed by R version and
# platform. sourceCpp() reuses a build in that directory when ekde.cpp has not changed, so a
# second session, and every mirai daemon, loads the shared library instead of recompiling. A lock
# directory serializes the first build when several processes source the file at once.
#
# Serialization. The fitted model ($model) holds only plain vectors, matrices and lists: no
# external pointers, so it survives qs2/saveRDS. The learner's methods do not refer to the global
# environment: they live in an environment of their own that travels with the learner, and they
# find the compiled functions through getOption("ekde.cpp"), compiling ekde.cpp (from the
# absolute path recorded when this file was sourced) on first use in a session that has not
# sourced the file, e.g. a mirai daemon or a fresh session that deserialized a learner.

ekde_impl <- new.env(parent = baseenv())
evalq(
  {
    cpp_file <- normalizePath("mlr3-custom-learners/ekde.cpp", mustWork = TRUE)
    cache_dir <- file.path(
      normalizePath(".", mustWork = TRUE),
      ".cache",
      "rcpp-ekde",
      paste0("R-", getRversion(), "-", R.version$platform)
    )

    # --- compiled code ------------------------------------------------------------------------------

    # The environment of the compiled functions; compiles (or loads the cached build) once per
    # session.
    cpp <- function() {
      env <- getOption("ekde.cpp")
      if (is.environment(env) && exists(".ekde_box_sums", envir = env, inherits = FALSE)) {
        return(env)
      }
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      lock <- file.path(cache_dir, "lock")
      waited <- 0
      while (!dir.create(lock, showWarnings = FALSE) && waited < 600) {
        Sys.sleep(0.5)
        waited <- waited + 0.5
      }
      on.exit(unlink(lock, recursive = TRUE), add = TRUE)
      env <- new.env(parent = globalenv())
      Rcpp::sourceCpp(cpp_file, cacheDir = cache_dir, env = env)
      options(ekde.cpp = env)
      env
    }

    # --- density estimator (ekde/ekde/base.py: KDE) ---------------------------------------------

    # Terrell's maximal smoothing bandwidth in whitened space (Mazy 2022, Eq. 2.24), with the
    # roughness of the box kernel, R(K) = 2^-d: base.py terrel_bandwidth()
    bw_terrel <- function(n, d) {
      num <- (d + 8)^((d + 6) / 2) * pi^(d / 2) * (1 / 2)^d
      den <- 16 * n * gamma((d + 8) / 2) * (d + 2)
      (num / den)^(1 / (d + 4))
    }
    # Scott's (= Silverman's) rule n^(-1/(d+4)), widened by 1.288 for the box kernel as the
    # reference does: base.py scott_bandwidth()
    bw_scott <- function(n, d) n^(-1 / (d + 4)) * 1.288

    # Fits the eKDE of the rows of X (numeric matrix).
    # bounds: per column "none", "left", "right" or "both"; lower / upper: per column, the bound
    # positions (NA: the minimum / maximum of X, as the reference).
    # boundary_correction: "reference" divides the weight of each occupied cell by the share of
    # the hypercube of side h centred on the cell that lies within the bounds, as the reference
    # calls hyperclip; "kernel" uses the support of the kernel (side 2h) instead, as Eq. 2.25 of
    # Mazy (2022) states; "none" skips the correction.
    ekde_fit <- function(
      X,
      bandwidth = "terrel",
      h = NULL,
      q = 51L,
      whiten = TRUE,
      bounds = "none",
      lower = NA_real_,
      upper = NA_real_,
      boundary_correction = "reference",
      zero = 1e-14
    ) {
      X <- as.matrix(X)
      storage.mode(X) <- "double"
      n <- nrow(X)
      d <- ncol(X)
      if (q %% 2L != 1L) stop("q must be odd")
      if (anyNA(X) || any(!is.finite(X))) stop("missing or infinite values")
      bounds <- rep_len(bounds, d)
      lower <- rep_len(as.numeric(lower), d)
      upper <- rep_len(as.numeric(upper), d)
      real_min <- apply(X, 2L, min)
      real_max <- apply(X, 2L, max)
      lower[is.na(lower)] <- real_min[is.na(lower)]
      upper[is.na(upper)] <- real_max[is.na(upper)]

      # the rows in lexicographic order, so that the fit does not depend on the order of the rows,
      # not even by rounding (the reference whitens the rows in the order given)
      X <- X[do.call(order, c(unname(as.data.frame(X)), method = "radix")), , drop = FALSE]

      # whitening transformation (whitening_transformer.py). The right singular vectors take the
      # sign that makes their largest component positive; the reference keeps the signs LAPACK
      # returns, which depend on the order of the rows, see README.
      if (whiten) {
        const <- colnames(X)[real_min == real_max] %||% which(real_min == real_max)
        if (length(const) > 0L) {
          stop("constant column(s) cannot be whitened: ", paste(const, collapse = ", "))
        }
        if (n <= d) stop(sprintf("whitening needs more than %d rows, got %d", d, n))
        mu <- colMeans(X)
        Xc <- sweep(X, 2L, mu)
        sv <- svd(Xc, nu = 0L)
        if (min(sv$d) <= max(sv$d) * 1e-12) {
          stop("the columns are linearly dependent within these rows; cannot whiten")
        }
        v <- sv$v
        big <- cbind(apply(abs(v), 2L, which.max), seq_len(d))
        v <- v * rep(ifelse(v[big] < 0, -1, 1), each = d)
        tm <- v %*% diag(1 / sv$d, d) * sqrt(n - 1)
        tm_inv <- diag(sv$d, d) %*% t(v) / sqrt(n - 1)
        scale <- abs(det(tm_inv))
        Xw <- Xc %*% tm
      } else {
        mu <- rep(0, d)
        tm <- tm_inv <- diag(d)
        scale <- 1
        Xw <- X
      }
      x_min <- apply(Xw, 2L, min)

      # bounds as half-spaces a . x + r >= 0 in whitened space (base.py _set_boundaries(); the
      # reference finds the hyperplane through d points and orients it with a data point, which
      # gives this hyperplane, scaled to |r| = 1, see README)
      hyp <- list()
      for (k in seq_len(d)) {
        sides <- switch(
          bounds[k],
          none = character(),
          left = "left",
          right = "right",
          both = c("left", "right"),
          stop("bounds must be none, left, right or both, not ", bounds[k])
        )
        for (side in sides) {
          value <- if (side == "left") lower[k] else upper[k]
          a <- tm_inv[, k]
          r <- mu[k] - value
          s <- if (side == "left") 1 else -1
          norm <- if (abs(r) > 0) abs(r) else sqrt(sum(a^2))
          hyp[[length(hyp) + 1L]] <- c(s * a / norm, s * r / norm)
        }
      }
      A <- matrix(vapply(hyp, function(x) x[seq_len(d)], numeric(d)), nrow = d)
      R <- vapply(hyp, function(x) x[d + 1L], numeric(1))

      # bandwidth (base.py _compute_bandwidth())
      if (is.null(h)) {
        h <- switch(
          bandwidth,
          terrel = bw_terrel(n, d),
          scott = ,
          silverman = bw_scott(n, d),
          stop("unknown bandwidth rule ", bandwidth)
        )
      }
      h <- as.numeric(h)
      dx <- h * 2 * 1 / q

      cpp_env <- cpp()
      binned <- cpp_env$.ekde_bin(Xw, x_min, dx)
      nu <- binned$nu
      if (length(R) > 0L && boundary_correction != "none") {
        centers <- t(t(binned$U * dx) + x_min) + 0.5 * dx
        side <- switch(boundary_correction, reference = h, kernel = 2 * h)
        vols <- cpp_env$.ekde_clipped_volumes(A, R, centers, side, zero)
        nu <- nu / vols
      }
      list(
        d = d,
        n = n,
        mean = mu,
        transform = tm,
        scale = scale,
        x_min = x_min,
        h = h,
        q = as.integer(q),
        dx = dx,
        U = binned$U,
        nu = nu,
        A = A,
        R = R,
        normalization = n * (2 * h)^d
      )
    }

    # Density of a fitted eKDE at the rows of X (base.py KDE.predict()): the box kernel on the grid,
    # zero outside the bounds.
    ekde_predict <- function(model, X, num_threads = 1L) {
      X <- as.matrix(X)
      storage.mode(X) <- "double"
      if (ncol(X) != model$d) stop("expected ", model$d, " columns")
      Xw <- sweep(X, 2L, model$mean) %*% model$transform
      out <- rep(FALSE, nrow(X))
      for (k in seq_along(model$R)) {
        # hyperplane.py side(), with its tolerance of 1e-7
        out <- out | !(drop(Xw %*% model$A[, k]) + model$R[k] + 1e-7 >= 0)
      }
      g <- cpp()$.ekde_box_sums(
        model$U,
        model$nu,
        Xw,
        model$x_min,
        model$dx,
        model$q,
        as.integer(num_threads)
      )
      g[out] <- 0
      g / model$normalization / model$scale
    }

    # --- Bayes rule (clumpy transition_probability_estimation/_bayes.py) ----------------------------

    # clumpy's Bayes.bayes_adjustment() without log computations: P(v|z) = rho(z|v) / rho(z),
    # rescaled column by column to mean P(v), then rows that sum to more than 1 rescaled to 1 and
    # the columns again, until no row exceeds 1 or n_max rounds have passed.
    bayes_adjustment_clumpy <- function(dens_v, dens, prior, n_max) {
      P <- matrix(0, nrow(dens_v), ncol(dens_v), dimnames = dimnames(dens_v))
      ok <- dens > 0
      P[ok, ] <- dens_v[ok, , drop = FALSE] / dens[ok]
      rescale <- function(P) {
        m <- colMeans(P)
        cols <- m != 0
        P[, cols] <- P[, cols, drop = FALSE] * rep(prior[cols] / m[cols], each = nrow(P))
        P
      }
      P <- rescale(P)
      s <- rowSums(P)
      rounds <- 0L
      while (any(s > 1) && rounds < n_max) {
        big <- s > 1
        P[big, ] <- P[big, , drop = FALSE] / s[big]
        P <- rescale(P)
        s <- rowSums(P)
        rounds <- rounds + 1L
      }
      if (rounds == n_max && any(s > 1)) {
        warning(
          "classif.ekde: the clumpy adjustment stopped after n_corrections_max = ",
          n_max,
          " rounds with ",
          sum(s > 1),
          " rows summing to more than 1 (at most ",
          format(max(s), digits = 15),
          ")",
          call. = FALSE
        )
      }
      big <- s > 1
      P[big, ] <- P[big, , drop = FALSE] / s[big]
      P[!is.finite(P)] <- 0
      P
    }

    # Prior-shift factors c_v such that P(v|z) = c_v P(v) rho(z|v) / sum_w c_w P(w) rho(z|w) has
    # mean P(v) over the rows of dens_v: clumpy's two rescaling steps (columns to their prior,
    # rows to 1) applied to every row and iterated to convergence (iterative proportional fitting).
    # Rows where every density vanishes are left out.
    ipf_factors <- function(dens_v, prior, n_max, tol) {
      active <- prior > 0
      W <- dens_v[, active, drop = FALSE] * rep(prior[active], each = nrow(dens_v))
      W <- W[rowSums(W) > 0, , drop = FALSE]
      target <- prior[active]
      cf <- rep(1, sum(active))
      converged <- FALSE
      if (nrow(W) > 0L) {
        for (it in seq_len(n_max)) {
          P <- W * rep(cf, each = nrow(W))
          P <- P / rowSums(P)
          m <- colMeans(P)
          if (max(abs(m / target - 1)) < tol) {
            converged <- TRUE
            break
          }
          cf <- cf * target / m
          cf <- cf / max(cf)
        }
        if (!converged) {
          warning(
            "classif.ekde: the prior adjustment did not converge in n_corrections_max = ",
            n_max,
            " iterations",
            call. = FALSE
          )
        }
      }
      out <- rep(1, length(prior))
      out[active] <- cf
      stats::setNames(out, names(prior))
    }

    ipf_predict <- function(dens_v, prior, cf, zero_density) {
      W <- dens_v * rep(prior * cf, each = nrow(dens_v))
      s <- rowSums(W)
      P <- W / s
      empty <- !(s > 0)
      if (any(empty)) {
        fill <- if (zero_density == "prior") prior else rep(1 / length(prior), length(prior))
        P[empty, ] <- rep(fill, each = sum(empty))
      }
      P
    }

    # rows of X, or a subsample of n_max of them; the subsample does not depend on the order of
    # the rows (they are sorted first), only on the R random number stream
    subsample <- function(X, n_max) {
      if (nrow(X) <= n_max) {
        return(X)
      }
      X <- X[do.call(order, unname(as.data.frame(X))), , drop = FALSE]
      X[sort(sample.int(nrow(X), n_max)), , drop = FALSE]
    }
  },
  envir = ekde_impl
)

# --- the learner --------------------------------------------------------------------------------

# parent_env: the methods find the functions above in ekde_impl, which is serialized with the
# learner, instead of in the global environment
LearnerClassifEkde <- R6::R6Class(
  "LearnerClassifEkde",
  inherit = mlr3::LearnerClassif,
  parent_env = ekde_impl,

  public = list(
    initialize = function() {
      ps <- paradox::ps(
        # density estimator
        kernel = paradox::p_fct("box", default = "box", tags = "train"),
        bandwidth = paradox::p_fct(
          c("terrel", "scott", "silverman"),
          default = "terrel",
          tags = "train"
        ),
        h = paradox::p_dbl(
          0,
          default = NULL,
          special_vals = list(NULL),
          tags = "train"
        ),
        q = paradox::p_int(1L, default = 51L, tags = "train"),
        whiten = paradox::p_lgl(default = TRUE, tags = "train"),
        bounds = paradox::p_uty(
          default = "none",
          tags = c("train", "predict"),
          custom_check = function(x) {
            if (!is.character(x) || !all(x %in% c("none", "left", "right", "both"))) {
              return("bounds must be a character vector of none, left, right, both")
            }
            TRUE
          }
        ),
        lower = paradox::p_uty(default = NULL, tags = c("train", "predict")),
        upper = paradox::p_uty(default = NULL, tags = c("train", "predict")),
        boundary_correction = paradox::p_fct(
          c("reference", "kernel", "none"),
          default = "reference",
          tags = c("train", "predict")
        ),
        zero = paradox::p_dbl(0, default = 1e-14, tags = c("train", "predict")),
        n_fit_max = paradox::p_int(1L, default = 100000L, tags = c("train", "predict")),
        # Bayes rule
        adjustment = paradox::p_fct(
          c("ipf", "clumpy", "none"),
          default = "ipf",
          tags = c("train", "predict")
        ),
        n_corrections_max = paradox::p_int(1L, default = 1000L, tags = c("train", "predict")),
        tol = paradox::p_dbl(0, default = 1e-10, tags = "train"),
        zero_density = paradox::p_fct(
          c("prior", "uniform"),
          default = "prior",
          tags = "predict"
        ),
        remainder_class = paradox::p_uty(default = NULL, tags = "predict"),
        # threads of the density evaluation; the result does not depend on them
        num_threads = paradox::p_int(1L, default = 1L, tags = c("train", "predict"))
      )
      super$initialize(
        id = "classif.ekde",
        param_set = ps,
        predict_types = c("response", "prob"),
        feature_types = c("integer", "numeric"),
        properties = c("twoclass", "multiclass"),
        packages = "Rcpp",
        label = "Bayes-eKDE (Mazy & Longaretti)",
        man = NA_character_
      )
    },

    # Density of the features at the rows of `newdata` under the class-conditional eKDE of
    # each class (columns), as fitted.
    densities = function(newdata) {
      stopifnot("No model stored; call $train() first" = !is.null(self$model))
      X <- as.matrix(as.data.frame(newdata)[, self$model$features, drop = FALSE])
      private$.dens(X, self$model, private$.pv()$num_threads)
    }
  ),

  private = list(
    .pv = function() {
      utils::modifyList(self$param_set$default, self$param_set$values)
    },

    # per-feature bound settings, from a value of length 1 or one per feature (named or not)
    .per_feature = function(x, features, fill) {
      if (is.null(x)) {
        return(stats::setNames(rep(fill, length(features)), features))
      }
      if (!is.null(names(x))) {
        unknown <- setdiff(names(x), features)
        if (length(unknown) > 0L) {
          stop("classif.ekde: not a feature: ", paste(unknown, collapse = ", "))
        }
        out <- stats::setNames(rep(fill, length(features)), features)
        out[names(x)] <- x
        return(out)
      }
      if (length(x) == 1L) {
        return(stats::setNames(rep(x, length(features)), features))
      }
      if (length(x) != length(features)) {
        stop("classif.ekde: give one value, or one per feature, or name the features")
      }
      stats::setNames(x, features)
    },

    .fit_one = function(X, pv) {
      features <- colnames(X)
      ekde_fit(
        subsample(X, pv$n_fit_max),
        bandwidth = pv$bandwidth,
        h = pv$h,
        q = pv$q,
        whiten = pv$whiten,
        bounds = private$.per_feature(pv$bounds, features, "none"),
        lower = private$.per_feature(pv$lower, features, NA_real_),
        upper = private$.per_feature(pv$upper, features, NA_real_),
        boundary_correction = pv$boundary_correction,
        zero = pv$zero
      )
    },

    .dens = function(X, m, num_threads) {
      out <- vapply(
        m$classes,
        function(cl) {
          if (is.null(m$kde[[cl]])) {
            rep(0, nrow(X))
          } else {
            ekde_predict(m$kde[[cl]], X, num_threads)
          }
        },
        numeric(nrow(X))
      )
      matrix(out, nrow(X), length(m$classes), dimnames = list(NULL, m$classes))
    },

    .train = function(task) {
      pv <- private$.pv()
      if (pv$kernel != "box") stop("classif.ekde: only the box kernel is implemented")
      features <- task$feature_names
      X <- as.matrix(task$data(cols = features))
      storage.mode(X) <- "double"
      y <- task$truth()
      # rows in a fixed order (features, then class), so that the model does not depend on the
      # order of the task's rows, not even by rounding
      o <- do.call(order, c(unname(as.data.frame(X)), list(as.integer(y)), method = "radix"))
      X <- X[o, , drop = FALSE]
      y <- y[o]
      classes <- task$class_names
      prior <- as.numeric(table(factor(y, levels = classes))) / length(y)
      names(prior) <- classes
      kde <- stats::setNames(vector("list", length(classes)), classes)
      for (cl in classes) {
        Xc <- X[y == cl, , drop = FALSE]
        if (nrow(Xc) == 0L) next # reference: NullEstimator, density 0
        kde[[cl]] <- tryCatch(
          private$.fit_one(Xc, pv),
          error = function(e) {
            stop(
              sprintf(
                "classif.ekde: class '%s' (%d rows): %s",
                cl,
                nrow(Xc),
                conditionMessage(e)
              ),
              call. = FALSE
            )
          }
        )
      }
      model <- list(
        features = features,
        classes = classes,
        prior = prior,
        kde = kde,
        adjustment = pv$adjustment,
        factors = stats::setNames(rep(1, length(classes)), classes)
      )
      if (pv$adjustment == "ipf") {
        model$factors <- ipf_factors(private$.dens(X, model, pv$num_threads), prior, pv$n_corrections_max, pv$tol)
      }
      model
    },

    .predict = function(task) {
      pv <- private$.pv()
      m <- self$model
      X <- as.matrix(task$data(cols = m$features))
      storage.mode(X) <- "double"
      dens_v <- private$.dens(X, m, pv$num_threads)
      if (m$adjustment == "clumpy") {
        # reference: rho(z | u) is fitted on the rows to predict, and the adjustment is
        # computed over them
        marginal <- private$.fit_one(X, pv)
        prob <- bayes_adjustment_clumpy(
          dens_v,
          ekde_predict(marginal, X, pv$num_threads),
          m$prior,
          pv$n_corrections_max
        )
        rem <- pv$remainder_class
        if (is.null(rem)) {
          if (length(m$classes) != 2L) {
            stop("classif.ekde: with adjustment = \"clumpy\" and more than two classes, ",
                 "set remainder_class")
          }
          rem <- task$negative
        }
        if (!rem %in% m$classes) stop("classif.ekde: remainder_class is not a class: ", rem)
        others <- setdiff(m$classes, rem)
        prob[, rem] <- pmax(0, 1 - rowSums(prob[, others, drop = FALSE]))
      } else {
        prob <- ipf_predict(dens_v, m$prior, m$factors, pv$zero_density)
      }
      if (self$predict_type == "prob") {
        list(prob = prob)
      } else {
        list(response = factor(m$classes[max.col(prob, ties.method = "first")], m$classes))
      }
    }
  )
)

# the density estimator on its own, for tests and diagnostics; compile now rather than on first use
ekde_fit <- ekde_impl$ekde_fit
ekde_predict <- ekde_impl$ekde_predict
options(ekde.cpp = NULL) # a changed ekde.cpp is rebuilt when the file is sourced again
invisible(ekde_impl$cpp())

mlr3::mlr_learners$add("classif.ekde", LearnerClassifEkde)
