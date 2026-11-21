# Checks of grrf.r; run from the repository root:
#   Rscript mlr3-custom-learners/grrf-tests.r
source("mlr3-custom-learners/grrf.r")
lgr::get_logger("mlr3")$set_threshold("warn")

check <- function(ok, what) {
  if (!isTRUE(all(ok))) stop("FAILED: ", what, call. = FALSE)
  message("ok: ", what)
}

# two informative features, one redundant copy of the first, three nuisance features
set.seed(42)
n <- 600L
dat <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  noise1 = rnorm(n),
  noise2 = runif(n),
  noise3 = factor(sample(letters[1:4], n, replace = TRUE))
)
dat$x1_copy <- dat$x1 + rnorm(n, sd = 0.3)
dat$y <- factor(dat$x1 + dat$x2 + rnorm(n, sd = 0.5) > 0, levels = c("FALSE", "TRUE"))
task <- mlr3::as_task_classif(dat, target = "y", positive = "TRUE")
x <- task$data(cols = task$feature_names)
y <- task$truth()

# --- registration and defaults ----------------------------------------------------------------
learner <- mlr3::lrn("classif.grrf")
check(inherits(learner, "LearnerClassifGrrf"), "lrn(\"classif.grrf\") builds the learner")
check(
  identical(
    learner$param_set$default[c("gamma", "lambda", "num.trees", "max.depth", "num.threads")],
    list(gamma = 0.5, lambda = 1, num.trees = 500L, max.depth = 0L, num.threads = 1L)
  ),
  "declared defaults"
)

# --- the coefficient is (1 - gamma) * lambda + gamma * imp / max(imp) ---------------------------
learner <- mlr3::lrn("classif.grrf", gamma = 0.7, lambda = 0.6, num.trees = 50L)
set.seed(1)
learner$train(task)
set.seed(1)
imp_first <- ranger::ranger(
  x = x,
  y = y,
  num.trees = 50L,
  importance = "impurity",
  num.threads = 1L
)$variable.importance
check(
  isTRUE(all.equal(learner$model$importance_initial, imp_first)),
  "first-stage importance is that of an ordinary forest under the same seed"
)
check(
  isTRUE(all.equal(
    learner$model$coef_reg,
    (1 - 0.7) * 0.6 + 0.7 * imp_first / max(imp_first)
  )),
  "coefficients follow Deng & Runger (2013)"
)
check(
  isTRUE(all.equal(
    learner$model$forest$call$regularization.factor,
    unname(learner$model$coef_reg)
  )),
  "the coefficients reach ranger as regularization.factor"
)

# --- parameters reach ranger ------------------------------------------------------------------
learner <- mlr3::lrn(
  "classif.grrf",
  num.trees = 7L,
  mtry = 3L,
  min.node.size = 5L,
  max.depth = 2L,
  replace = FALSE,
  splitrule = "extratrees",
  balance_classes = TRUE
)
learner$train(task)
forest <- learner$model$forest
check(
  forest$num.trees == 7L && forest$mtry == 3L && forest$min.node.size == 5L &&
    forest$max.depth == 2L && !forest$replace && forest$splitrule == "extratrees",
  "num.trees, mtry, min.node.size, max.depth, replace, splitrule reach ranger"
)
check(
  all(vapply(seq_len(7L), \(i) nrow(ranger::treeInfo(forest, i)), integer(1)) <= 7L),
  "max.depth = 2 grows trees of at most 7 nodes"
)
n_class <- table(y)
check(
  isTRUE(all.equal(
    forest$call$case.weights,
    as.numeric(n / (2 * n_class[as.character(y)]))
  )),
  "balance_classes = TRUE passes n / (n_classes * n_class) as case weights"
)
# replacing the values drops the inits, the declared defaults still apply
learner$param_set$values <- list(gamma = 0.5, num.trees = 5L)
learner$train(task)
forest <- learner$model$forest
check(
  forest$max.depth == 0L && forest$call$num.threads == 1L &&
    is.null(forest$call$case.weights) &&
    # lambda = 1: the most important feature of the first forest goes unpenalized
    max(learner$model$coef_reg) == 1,
  "unset parameters run at their declared defaults"
)
check(
  any(vapply(seq_len(5L), \(i) nrow(ranger::treeInfo(forest, i)), integer(1)) > 7L),
  "the default max.depth grows trees deeper than 2"
)

# --- gamma = 0, lambda = 1 is an ordinary random forest ----------------------------------------
learner <- mlr3::lrn("classif.grrf", gamma = 0, lambda = 1, num.trees = 50L)
set.seed(3)
learner$train(task)
set.seed(3)
plain <- ranger::ranger(x = x, y = y, num.trees = 50L, importance = "impurity", num.threads = 1L)
check(
  is.null(learner$model$importance_initial) && all(learner$model$coef_reg == 1),
  "gamma = 0 fits no first forest, all coefficients are lambda"
)
check(
  identical(learner$model$forest$variable.importance, plain$variable.importance) &&
    identical(learner$model$forest$predictions, plain$predictions),
  "gamma = 0, lambda = 1 reproduces ranger under the same seed"
)
# gamma = 0, lambda < 1 is an unguided regularized forest: a uniform penalty
learner <- mlr3::lrn("classif.grrf", gamma = 0, lambda = 0.5, num.trees = 10L)
learner$train(task)
check(all(learner$model$coef_reg == 0.5), "gamma = 0, lambda = 0.5 penalizes all features alike")

# --- nuisance features score below informative ones -------------------------------------------
learner <- mlr3::lrn("classif.grrf", gamma = 0.9, num.trees = 200L)
set.seed(4)
learner$train(task)
imp <- learner$importance()
check(
  max(imp[c("noise1", "noise2", "noise3")]) < min(imp[c("x1", "x2")]),
  sprintf(
    "nuisance features (max %.1f) score below informative ones (min %.1f)",
    max(imp[c("noise1", "noise2", "noise3")]),
    min(imp[c("x1", "x2")])
  )
)
noise_share <- \(v) sum(v[c("noise1", "noise2", "noise3")]) / sum(v)
check(
  noise_share(imp) < noise_share(learner$model$importance_initial),
  sprintf(
    "the guided forest gives the nuisance features a smaller share (%.3f) than the first (%.3f)",
    noise_share(imp),
    noise_share(learner$model$importance_initial)
  )
)

# --- as an mlr3 filter ------------------------------------------------------------------------
filter <- mlr3filters::flt("importance", learner = mlr3::lrn("classif.grrf", num.trees = 50L))
set.seed(5)
filter$calculate(task)
check(
  setequal(names(filter$scores), task$feature_names) && all(filter$scores >= 0),
  "flt(\"importance\") scores every feature"
)

# --- probability prediction -------------------------------------------------------------------
learner <- mlr3::lrn("classif.grrf", num.trees = 50L, predict_type = "prob")
split <- mlr3::partition(task)
learner$train(task, split$train)
pred <- learner$predict(task, split$test)
check(
  identical(colnames(pred$prob), task$class_names) &&
    all(abs(rowSums(pred$prob) - 1) < 1e-12) &&
    length(pred$response) == length(split$test),
  "predict_type = \"prob\" returns class probabilities"
)
check(pred$score(mlr3::msr("classif.auc")) > 0.8, "held-out AUC above 0.8")

# --- on mirai daemons, as get_pred_filter_score() runs it ------------------------------------
if (requireNamespace("mirai", quietly = TRUE)) {
  mirai::daemons(1)
  scores <- mirai::mirai(
    {
      filter$calculate(task)
      filter$scores
    },
    filter = mlr3filters::flt("importance", learner = mlr3::lrn("classif.grrf", num.trees = 20L)),
    task = task
  )[]
  mirai::daemons(0)
  check(is.numeric(scores) && length(scores) == 6L, "the learner trains on a mirai daemon")
}
