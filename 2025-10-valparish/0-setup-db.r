#' Setup valpar-ish database. Aimed at Arealstatistik + Swiss Predictors
devtools::load_all("~/github-repos/evoland-plus")

db <- evoland_db$new(
  path = "smaller.evolanddb",
  report_name = "valparish",
  report_name_pretty = "ValPar.CH inspired model setup"
)

db$set_coords(
  type = "square",
  epsg = 2056,
  extent = terra::ext(c(
    xmin = 2697000,
    xmax = 2698000,
    ymin = 1252000,
    ymax = 1253000
  )),
  resolution = 100
)

db$set_periods(
  period_length_str = "P10Y",
  start_observed = "1975-01-01",
  end_observed = "2020-01-01",
  end_extrapolated = "2060-01-01"
)
