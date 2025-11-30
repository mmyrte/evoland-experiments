devtools::load_all("~/github-repos/evoland-plus/")

db <- evoland_db$new(path = "smaller.evolanddb")

db$set_neighbors(
  max_distance = 1000,
  distance_breaks = c(0, 100, 500, 1000),
  resolution = 100
)

db$generate_neighbor_predictors()
