library(evoland)
db <- evoland_db$new(path = "small.evolanddb")

# Example usage: Fit GLM models
# The fit_glm and gof_glm functions are exported from the evoland package
glm_models <- db$fit_partial_models(
  fit_fun = fit_glm,
  gof_fun = gof_glm,
  sample_frac = 0.3, # TODO what is a sensible split?
  seed = 42,
  na_value = 0,
  cores = 6
)

db$trans_models_t <- glm_models

# Example usage: Fit Random Forest models
# The fit_ranger and gof_ranger functions are exported from the evoland package
# rf_models <- db$fit_partial_models(
#   fit_fun = fit_ranger,
#   gof_fun = gof_ranger,
#   sample_frac = 30,
#   seed = 42,
#   num.trees = 100
# )

# db$trans_models_t <- rf_models

# View results
print(glm_models)
print(rf_models)

# Fit full models using best partial models (based on AUC)
glm_full <- db$fit_full_models(
  partial_models = glm_models,
  select_score = "classif.auc",
  select_maximize = TRUE
)

# rf_full <- db$fit_full_models(
#   partial_models = rf_models,
#   select_score = "classif.auc",
#   select_maximize = TRUE
# )

db$trans_models_t <- glm_full
# db$trans_models_t <- rf_full
