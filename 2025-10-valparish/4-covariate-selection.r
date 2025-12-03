devtools::load_all("~/github-repos/evoland-plus/")


db <- evoland_db$new(path = "smaller.evolanddb")


db$trans_meta_t <- create_trans_meta_t(db$transitions_v, min_cardinality_abs = 4000)

devtools::load_all("~/github-repos/evoland-plus/")
db <- evoland_db$new(path = "fullch.evolanddb")
db$set_full_trans_preds(overwrite = TRUE)
trans_preds_covfiltered <- db$get_pruned_trans_preds_t(
  filter_fun = covariance_filter,
  corcut = 0.7,
  na_value = 0
)

db$commit(trans_preds_covfiltered, "trans_preds_t", method = "overwrite")

trans_preds_grrffiltered <- db$get_pruned_trans_preds_t(
  filter_fun = grrf_filter,
  num.trees = 100,
  max.depth = 20,
  gamma = 0.8
)

stopifnot(
  # check that we've actually covered all viable predictions before committing
  setequal(
    trans_preds_grrffiltered$id_trans,
    db$trans_meta_t[is_viable == TRUE]$id_trans
  )
)

db$commit(trans_preds_grrffiltered, "trans_preds_t", method = "overwrite")
