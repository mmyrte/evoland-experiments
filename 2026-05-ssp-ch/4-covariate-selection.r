library(evoland)
library(ggplot2)
db <- evoland_db$new(path = "ssp-ch.evolanddb")

# investigate available transition observations for modeling ####
lulc_meta <- db$lulc_meta_t

trans_investigate <-
  create_trans_meta_t(
    db$trans_v,
    # min_frequency_rel = 0.005, # this is what ben used: the change rate relative
    # to the other change rates
    # min_cardinality_abs = 5000, # what might be appropriate value?
    exclude_anterior = lulc_meta[name == "static", id_lulc]
  )[
    lulc_meta[, .(id_lulc, name, pretty_name)],
    on = .(id_lulc_anterior = id_lulc)
  ]

n_obs_ransitions <-
  ggplot(
    trans_investigate,
    aes(
      color = paste0(id_lulc_anterior, ": ", pretty_name),
      x = pretty_name,
      y = cardinality,
      label = paste0(id_lulc_anterior, "=>", id_lulc_posterior)
    )
  ) +
  geom_text(
    size = 3,
    check_overlap = TRUE
  ) +
  scale_y_log10() +
  labs(
    title = "Numbers of observed transitions",
    y = "Cardinality (# of observed transitions)",
    x = NULL,
    color = "Anterior Class w/ID"
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

evoland:::ensure_dir("./2026-05-ssp-ch/graphs/")
ggsave(
  "2026-05-ssp-ch/graphs/4-no-obs-trans.svg",
  n_obs_ransitions,
  width = 9,
  height = 6
)

# commit with is_viable flag set according to crit derived from graph ####
db$trans_meta_t <-
  create_trans_meta_t(
    db$trans_v,
    # min_frequency_rel = 0.005, # this is what ben used: the change rate relative
    # to the other change rates
    min_cardinality_abs = 1000,
    exclude_anterior = lulc_meta[name == "static", id_lulc]
  )

# score usefulness of covariates ####
db$set_full_trans_preds(overwrite = TRUE)
# db$trans_preds_t
db$runs_t

grrf_learner <- LearnerClassifGrrf$new()
grrf_learner$param_set$values <- list(gamma = 0.9, num.trees = 50L)

clust <- mirai::make_cluster(n = 2) # going higher thrashes my memory (on M3 Pro!)

trans_preds_covfiltered <- db$get_pred_filter_score(
  filter = mlr3filters::FilterImportance$new(learner = grrf_learner),
  cluster = clust
)

db$trans_preds_t <- trans_preds_covfiltered

trans_preds_grrffiltered <- db$get_pred_filter_score(
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
