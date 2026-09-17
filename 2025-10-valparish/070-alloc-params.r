library(evoland)
db <- evoland_db$new(path = "small.evolanddb")

db$alloc_params_t <- db$create_alloc_params_t()
# db$execute("SET temp_directory = '/tmp/evoland';")
db$execute("SET preserve_insertion_order = false;")
db$execute("SET threads = 4;")
if (Sys.which("DinamicaConsole") != "") {
  alloc_params_t <- db$eval_alloc_params_t()
}
