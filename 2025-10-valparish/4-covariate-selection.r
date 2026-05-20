library(evoland)
db <- evoland_db$new(path = "small.evolanddb")

db$trans_meta_t <- create_trans_meta_t(
  db$trans_v
  # min_cardinality_abs = 10000, # TODO set this to some value we deem appropriate
  # exclude_anterior = 9
)

db$set_full_trans_preds(overwrite = TRUE)
trans_preds_covfiltered <- db$get_pruned_trans_preds_t(
  filter_fun = covariance_filter,
  corcut = 0.7,
  na_value = 0,
  cores = 6 # going higher thrashes my memory (on M3 Pro!)
)

db$commit(trans_preds_covfiltered, "trans_preds_t", method = "overwrite")

trans_preds_grrffiltered <- db$get_pruned_trans_preds_t(
  filter_fun = grrf_filter,
  num.trees = 100,
  max.depth = 20,
  gamma = 0.8,
  cores = 6 # going higher thrashes my memory (on M3 Pro!)
)

stopifnot(
  # check that we've actually covered all viable predictions before committing
  setequal(
    trans_preds_grrffiltered$id_trans,
    db$trans_meta_t[is_viable == TRUE]$id_trans
  )
)

db$commit(trans_preds_grrffiltered, "trans_preds_t", method = "overwrite")
