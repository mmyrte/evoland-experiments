#' Setup SSP-CH experiment.
#' Aimed at Arealstatistik + Swiss Predictors
library(evoland)

db <- evoland_db$new(
  path = "ssp-ch.evolanddb",
  report_name = "ssp-ch",
  report_name_pretty = "Replication of SSP-CH in new evoland"
)

db$coords_t <- create_coords_t_square(
  epsg = 2056,
  extent = terra::ext(c(
    # somewhere in the middle of the country
    # xmin = 2697000,
    # xmax = 2698000,
    # ymin = 1252000,
    # ymax = 1253000

    # full extent
    xmin = 2480000,
    xmax = 2840000,
    ymin = 1070000,
    ymax = 1300000
  )),
  resolution = 100
)

db$periods_t <- create_periods_t(
  period_length_str = "P10Y",
  start_observed = "1985-01-01",
  end_observed = "2020-01-01",
  end_extrapolated = "2060-01-01"
)
