# this is necessary because of some strange startup condition on
# openSuse with R-as-modules
lapply(
  getOption("defaultPackages"),
  library
)

if (!file.exists("/.dockerenv")) {
  # Don't activate renv if running in a docker container -
  # The assumption is that we use the system library there
  # source("renv/activate.R")
  source("rv/scripts/rvr.R")
  source("rv/scripts/activate.R")
}
options(
  "menu.graphics" = FALSE,
  # see https://rlang.r-lib.org/reference/rlang_backtrace_on_error.html
  rlang_backtrace_on_error = "full",
  error = rlang::entrace
)
