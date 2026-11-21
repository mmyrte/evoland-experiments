# Guided regularized random forest (GRRF; Deng & Runger 2013) as an mlr3 learner.
# Source this file; it defines `LearnerClassifGrrf` and registers it as "classif.grrf" in
# mlr3::mlr_learners. See README.md in this directory for the method and its parameters.
#
# The methods refer to nothing in the global environment, only to package namespaces: a learner
# sent to mirai daemons (as evoland.plus's get_pred_filter_score() does) arrives without the
# objects of the session that sourced this file.

LearnerClassifGrrf <- R6::R6Class(
  "LearnerClassifGrrf",
  inherit = mlr3::LearnerClassif,

  public = list(
    initialize = function() {
      ps <- paradox::ps(
        # guided regularization, Deng & Runger (2013)
        gamma = paradox::p_dbl(0, 1, default = 0.5, tags = "train"),
        lambda = paradox::p_dbl(0, 1, default = 1, tags = "train"),
        regularization.usedepth = paradox::p_lgl(default = FALSE, tags = "train"),
        # class-balanced case weights, n / (n_classes * n_class)
        balance_classes = paradox::p_lgl(default = FALSE, tags = "train"),
        # passed on to both ranger forests; defaults are ranger's, except num.threads
        num.trees = paradox::p_int(1L, default = 500L, tags = "train"),
        mtry = paradox::p_int(1L, tags = "train"),
        min.node.size = paradox::p_int(1L, tags = "train"),
        max.depth = paradox::p_int(0L, default = 0L, tags = "train"),
        sample.fraction = paradox::p_dbl(0, 1, tags = "train"),
        replace = paradox::p_lgl(default = TRUE, tags = "train"),
        splitrule = paradox::p_fct(
          c("gini", "extratrees", "hellinger"),
          default = "gini",
          tags = "train"
        ),
        num.random.splits = paradox::p_int(1L, default = 1L, tags = "train"),
        respect.unordered.factors = paradox::p_fct(
          c("ignore", "order", "partition"),
          tags = "train"
        ),
        num.threads = paradox::p_int(1L, default = 1L, tags = c("train", "predict")),
        seed = paradox::p_int(default = NULL, special_vals = list(NULL), tags = "train")
      )
      super$initialize(
        id = "classif.grrf",
        param_set = ps,
        predict_types = c("response", "prob"),
        feature_types = c("logical", "integer", "numeric", "factor", "ordered"),
        properties = c("twoclass", "multiclass", "importance"),
        packages = "ranger",
        label = "Guided Regularized Random Forest",
        man = NA_character_
      )
    },

    # Impurity importance of the guided regularized (second) forest, sorted decreasingly.
    # Features the forest never split on score 0.
    importance = function() {
      stopifnot("No model stored; call $train() first" = !is.null(self$model))
      sort(self$model$forest$variable.importance, decreasing = TRUE)
    }
  ),

  private = list(
    .train = function(task) {
      # declared defaults fill unset parameters, so that what the parameter set documents is what
      # runs even after `$param_set$values <- list(...)` has dropped them
      pv <- utils::modifyList(
        self$param_set$default,
        self$param_set$get_values(tags = "train")
      )
      stopifnot("lambda must be greater than 0" = pv[["lambda"]] > 0)

      data <- task$data()
      y <- data[[task$target_names]]
      ranger_args <- c(
        list(
          x = data[, task$feature_names, with = FALSE],
          y = y,
          probability = self$predict_type == "prob",
          importance = "impurity"
        ),
        pv[setdiff(names(pv), c("gamma", "lambda", "balance_classes"))]
      )
      if (isTRUE(pv[["balance_classes"]])) {
        n_class <- table(y)
        ranger_args[["case.weights"]] <-
          as.numeric(length(y) / (length(n_class) * n_class[as.character(y)]))
      }

      # with gamma = 0 the coefficients do not depend on importance, so there is no first forest
      # and gamma = 0, lambda = 1 is a single, ordinary random forest
      importance_initial <- NULL
      imp_norm <- 0
      if (pv[["gamma"]] > 0) {
        importance_initial <-
          do.call(ranger::ranger, ranger_args)$variable.importance[task$feature_names]
        # a first forest without a single split carries no guidance: treat all features alike
        imp_norm <- if (max(importance_initial) > 0) {
          importance_initial / max(importance_initial)
        } else {
          rep(1, length(importance_initial))
        }
      }
      coef_reg <-
        ((1 - pv[["gamma"]]) * pv[["lambda"]] + pv[["gamma"]] * imp_norm) |>
        rep_len(length(task$feature_names)) |>
        stats::setNames(task$feature_names)

      ranger_args[["regularization.factor"]] <- unname(coef_reg)
      list(
        forest = do.call(ranger::ranger, ranger_args),
        importance_initial = importance_initial,
        coef_reg = coef_reg
      )
    },

    .predict = function(task) {
      num_threads <- self$param_set$values[["num.threads"]] %||%
        self$param_set$default[["num.threads"]]
      pred <- stats::predict(
        self$model$forest,
        data = task$data(cols = task$feature_names),
        num.threads = num_threads
      )$predictions
      if (self$predict_type == "prob") {
        list(prob = pred)
      } else {
        list(response = pred)
      }
    }
  )
)

mlr3::mlr_learners$add("classif.grrf", LearnerClassifGrrf)
