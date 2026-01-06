pkg_path <- paste0("../evoland-plus-", Sys.info()[["sysname"]] |> tolower())
devtools::load_all(pkg_path)
db <- evoland_db$new(path = "fullch.evolanddb")

db$alloc_params_t <- db$create_alloc_params_t()

if (Sys.which("DinamicaConsole") != "") {
  alloc_params_t <- db$eval_alloc_params_t()
}
