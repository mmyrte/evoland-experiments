devtools::load_all("~/github-repos/evoland-plus/")

db <- evoland_db$new(path = "smaller.evolanddb")

# attn this may take a couple minutes
db$set_neighbors(
  max_distance = 1000,
  distance_breaks = c(0, 100, 500, 1000),
  resolution = 100
)

# this however is pretty fast
db$generate_neighbor_predictors()
