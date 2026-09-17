library(evoland)

db <- evoland_db$new(path = "small.evolanddb")

# attn this may take a couple minutes
db$set_neighbors(
  max_distance = 1000,
  distance_breaks = c(0, 100, 500, 1000)
)

# this however is pretty fast
db$generate_neighbor_predictors()
