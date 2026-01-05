devtools::load_all("../evoland-plus-linux/")
devtools::load_all("../evoland-plus-macos/")
db <- evoland_db$new(path = "fullch.evolanddb")

db$alloc_params_t <- db$create_alloc_params_t()
