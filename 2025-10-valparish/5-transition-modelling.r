devtools::load_all("~/github-repos/evoland-plus/")
db <- evoland_db$new(path = "fullch.evolanddb")

# Example usage: Fit GLM models
# The fit_glm and gof_glm functions are exported from the evoland package
glm_models <- db$fit_partial_models(
  fit_fun = fit_glm,
  gof_fun = gof_glm,
  sample_pct = 30,
  seed = 42,
  na_value = 0
)

db$trans_models_t <- glm_models

# Example usage: Fit Random Forest models
# The fit_ranger and gof_ranger functions are exported from the evoland package
rf_models <- db$fit_partial_models(
  fit_fun = fit_ranger,
  gof_fun = gof_ranger,
  sample_pct = 30,
  seed = 42,
  num.trees = 100
)

db$trans_models_t <- rf_models

# View results
print(glm_models)
print(rf_models)

# Fit full models using best partial models (based on AUC)
glm_full <- db$fit_full_models(
  partial_models = glm_models,
  gof_criterion = "auc",
  maximize = TRUE
)
rf_full <- db$fit_full_models(
  partial_models = rf_models,
  gof_criterion = "auc",
  maximize = TRUE
)

db$trans_models_t <- glm_full
db$trans_models_t <- rf_full
