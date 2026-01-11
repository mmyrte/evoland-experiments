pkg_path <- paste0("../evoland-plus-", Sys.info()[["sysname"]] |> tolower())
devtools::load_all(pkg_path)

db <- evoland_db$new(path = "fullch.evolanddb")

# attn this may take a couple minutes
db$set_neighbors(
  max_distance = 1000,
  distance_breaks = c(0, 100, 500, 1000)
)

# this however is pretty fast
db$generate_neighbor_predictors()
