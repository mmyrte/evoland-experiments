#' ---
#' title: "PIE benchmark: clumpy's Bayes-eKDE transition probabilities"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' The fifth estimator of the crossing: the transition probability estimator of clumpy
#' (Mazy & Longaretti; the fork mmyrte/clumpy, pinned in the repository's `pyproject.toml`).
#' For each anterior class u it estimates the density of the predictors among the cells that
#' went to each final class v, P(Z | u, v), by kernel density estimation (`ekde`), and turns
#' these into P(v | u, Z) by Bayes' rule with the calibration frequencies P(v | u). Unlike the
#' learners of `010`, it fits no discriminative model: the transition probability is a ratio of
#' densities.
#'
#' - **Predictors**: the same nine as the evoland learners (the three factors and the six
#'   neighbourhood counts), read from evoland's database, so that the estimator alone differs.
#'   Every predictor is non-negative and bounded on the left, as clumpy bounds its distances.
#' - **Calibration** on 1985 → 1991 (the cells of each class in 1985, with their class in 1991),
#'   **prediction** on the 1991 predictors of the cells of each class in 1991, as in `010`.
#' - clumpy fits each density on at most 10⁵ cells (none of PIE's classes reaches that) and
#'   draws that subsample with NumPy's global random state, seeded here.
#'
#' The potentials go into `trans_pot_t` under run 5000, like Weights of Evidence under 3000
#' (`020`), so that every allocator reads them. Needs the repository's uv environment
#' (`uv sync`), or `CLUMPY_PYTHON` pointing at a Python with clumpy installed.

#| label: setup
#| output: false
library(evoland)
library(data.table)
source("2026-09-model-comparison/common.r")
stopifnot("kde is not among PIE_ESTIMATORS" = "kde" %in% estimators$estimator)
db <- evoland_db$new(path = db_path)
reset_timings("clumpy-kde")
work_dir <- file.path(outputs_dir, "clumpy-kde-work")
unlink(work_dir, recursive = TRUE)
dir.create(work_dir, recursive = TRUE)
python <- Sys.getenv("CLUMPY_PYTHON", file.path(".venv", "bin", "python"))

db$id_run <- 0L
viable <- db$trans_meta_t[is_viable == TRUE]
trans_preds <- db$trans_preds_t[id_run == 0L]

#' # Calibration and prediction data
#'
#' Per anterior class, the training table has one row per cell of that class in 1985, its
#' predictors and its class in 1991; the prediction table the 1991 predictors of the cells of
#' that class in 1991.

#| label: data
for (u in sort(unique(viable$id_lulc_anterior))) {
  from_u <- viable[id_lulc_anterior == u]
  train <- NULL
  for (i in seq_len(nrow(from_u))) {
    x <- db$trans_pred_data_v(
      id_trans = from_u$id_trans[i],
      id_pred = sort(trans_preds[id_trans == from_u$id_trans[i], id_pred])
    )
    if (is.null(train)) {
      train <- x[, !c("did_transition", "id_period_anterior")]
      train[, final := u]
    }
    train[
      x[did_transition == TRUE],
      on = "id_coord",
      final := from_u$id_lulc_posterior[i]
    ]
  }
  fwrite(train, file.path(work_dir, sprintf("train_%d.csv", u)))
  fwrite(
    db$pred_data_wide_v(id_trans = from_u$id_trans[1], id_period_anterior = 2L)[,
      !c("id_lulc")
    ],
    file.path(work_dir, sprintf("pred_%d.csv", u))
  )
  print(train[, .(anterior = u, n = .N), by = final])
}

#' # Fit and predict

#| label: fit
id_run_kde <- estimators[estimator == "kde", id_run]
res <- processx::run(
  python,
  c(file.path(pie_dir, "015-clumpy-kde", "fit_predict.py"), work_dir, id_run_kde),
  echo = FALSE,
  error_on_status = TRUE
)
stdout_lines <- strsplit(res$stdout, "\n")[[1]]
class_times <- fread(
  text = sub("^TIMING,", "", grep("^TIMING,", stdout_lines, value = TRUE)),
  header = FALSE,
  col.names = c("id_lulc_anterior", "seconds", "n_train", "n_pred")
)
stopifnot(setequal(class_times$id_lulc_anterior, viable$id_lulc_anterior))
class_times
for (i in seq_len(nrow(class_times))) {
  record_timing(
    "clumpy-kde",
    "fit_predict",
    class_times$seconds[i],
    paste0("id_lulc_anterior=", class_times$id_lulc_anterior[i])
  )
}

#' # Into `trans_pot_t`

#| label: commit
kde_pot <-
  rbindlist(lapply(class_times$id_lulc_anterior, function(u) {
    fread(file.path(work_dir, sprintf("potentials_%d.csv", u)))[, id_lulc_anterior := u]
  }))[
    viable[, .(id_trans, id_lulc_anterior, id_lulc_posterior)],
    on = .(id_lulc_anterior, id_lulc_posterior),
    nomatch = NULL
  ]
db$id_run <- id_run_kde
db$commit(
  as_trans_pot_t(kde_pot[, .(
    id_run = id_run_kde,
    id_trans,
    id_period_post = 3L,
    id_coord,
    value
  )]),
  "trans_pot_t",
  method = "upsert"
)
kde_pot[,
  .(n = .N, min = min(value), mean = mean(value), max = max(value), expected = sum(value)),
  by = id_trans
]
