devtools::load_all("~/github-repos/evoland-plus/")

#' How it works in the olden evoland:
#' 1. dataset prep makes
#'  - trans_result: 0 where no transition occurs, 1 where one occurs - only looking at
#'    those cells that had a given class to begin with
#'  - cov_data is a dataframe over whose columns we later iterate
#'  - non_cov_data is never used again
#'  - collin_weights: a vector of weights for the GLM fits later on; presuming there are
#'    always more transitions than there are non-transitions, the weight is 1 for
#'    non-transitions, whereas it is trans_weight = round(number_of_nontrans / number_of_trans)
#'  - embed_weights is a label of weights for each class, i.e. c("0" = 1, "1" =
#'    trans_weight) which is used for a feature selection using GRRF (Embedded feature
#'    selection with Guided Regularized Random Forests)
#'  - imbalance ratio: non-rounded trans_weight; currently only used to determine
#'    whether to downsample for final model fit. hard coded to use downsampling if
#'    imbalance between 0.05 and 60 (which isn't reciprocal, so i don't get it?)
#' 2. once these data are in place, pass them (ultimately) to a hot loop that fits
#'    binomial GLMs of trans_result ~ poly(x, degree = 2) where x = a single
#'    neighbourhood covariate; get the lower p value for each of the degrees.
#' 3. the p-values for all covariates are sorted ascending and returned as a dataframe
#'    (covariate name / p value)
#'      ATTN: it may happen that none of the GLM fits converge and that all p-vals are
#'      0, which will bias the ordering for the while loop in 6.
#' 4. the order of the dataframe is used to reorder the underlying predictors with
#'    better p-vals earlier
#' 5. get the absolutes of complete pairwise correlations between all (neighbor)
#'    predictors
#' 6. iteratively subset the first col of the correlation matrix, subset to all
#'    variables smaller than or equal to a correlation cutoff, add the active column
#'    name (i.e. predictor against which we're checking correlations) to the subset to
#'    be returned. subset the correlation matrix to those predictors that are below the
#'    correlation threshold. keep iterating until the correlation matrix is exhausted.
#' 7. return the variables selected by the correlation selection

db <- evoland_db$new(path = "smaller.evolanddb")

transitions <- db$transitions_v
db$trans_meta_t <- create_trans_meta_t(transitions)

# covariance filter
trans_meta <- db$trans_meta_t

# rough outline: for each id_trans, fetch all

for (r in split(trans_meta[is_viable == TRUE], by = "id_trans")) {
  r <- r[[1]]
  transitions_preds_dt <- transitions[r, on = c("id_lulc_anterior", "id_lulc_posterior")]
  preds <- db$fetch("pred_data_t_float", where = NULL)
}

db$attach_table("pred_data_t_float")
db$attach_table("pred_data_t_int")
db$attach_table("pred_data_t_bool")

db$get_query(
  r"{
  

  }"
)
