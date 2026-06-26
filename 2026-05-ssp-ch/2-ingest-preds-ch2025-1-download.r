#' Purpose: discover and download CH2025 climate indicator netCDFs
#' date: 2026-06-25
#' auth: jhartman@ethz.ch
#'
#' Source: MeteoSwiss CH2025 web atlas
#' https://www.meteoschweiz.admin.ch/service-und-publikationen/applikationen/ext/climate-ch2025-maps.html
#'
#' Stage 1 (probe) + Stage 2 (download). The ETL that reads the cached netCDFs into
#' the DB lives in 2-ingest-preds-ch2025-2-etl.r, which recovers all dimensions from
#' the cached file names, so nothing is passed between the two scripts in memory.
#'
#' Two product families share the same set of climate indicators:
#'   -gwl  projections, indexed by global warming level × uncertainty quantile
#'         (annual aggregate only).
#'   -obs  observed climatologies, indexed by reference period × time of year
#'         (four seasons + annual; no uncertainty quantile).
#' Not every indicator × dimension combination is published, so candidate URLs are
#' first probed with a HEAD request and only the live ones are downloaded.
#'
#' TODO: SSP5-8.5 late-century (~2071–2100) implies ~5–6 °C global warming, which
#' exceeds the highest GWL probed here (GWL3.0). Those time steps cannot be mapped to
#' an existing CH2025 aggregate and are out of scope for now.

library(data.table)
library(evoland)

# ---- Parameters -------------------------------------------------------------
base_url <- "https://service.meteoswiss.ch/pbbackend/api/v1/products"

gwl_levels <- c("GWL1.5", "GWL2.0", "GWL3.0")
uncertainty_levels <- c("q5", "q50", "q95")
reference_periods <- c("1961-1990", "1991-2020")
times_of_year <- c("DJF", "MAM", "JJA", "SON", "yearly")

# Climate categories and their scenarioIndicators, observed from the CH2025 atlas
# URL patterns. The same indicator sets back both the -gwl and -obs product families.
category_indicators <-
  tibble::tribble(
    ~category, ~indicator,
    "cold", c("ID", "FD", "HDD", "HED"),
    "heat", c("HD", "HW2", "HW3", "HW4", "CoDD", "COD", "VHD", "SD", "TN"),
    "precip", c("pr", "PR20", "PR40", "PR60"),
    "snow", c("SNFD"),
    "temperature", c("tas", "tasmax", "tasmin"),
    "drought", c("CDD")
  ) |>
  tidyr::unnest_longer(indicator)

# ---- Build candidate URL matrix ---------------------------------------------

# Projections: indicators × (GWL × uncertainty), annual aggregate only.
gwl_candidates <-
  category_indicators |>
  dplyr::cross_join(tidyr::expand_grid(
    gwl = gwl_levels,
    uncertainty = uncertainty_levels
  )) |>
  dplyr::mutate(
    product = paste0("climate-ch2025-maps-", category, "-gwl"),
    time_of_year = "yearly"
  )

# Observations: indicators × (reference period × time of year), no uncertainty.
obs_candidates <-
  category_indicators |>
  dplyr::cross_join(tidyr::expand_grid(
    reference_period = reference_periods,
    time_of_year = times_of_year
  )) |>
  dplyr::mutate(
    product = paste0("climate-ch2025-maps-", category, "-obs")
  )

# bind_rows fills the dimensions absent from each family with NA
# (reference_period for -gwl; gwl / uncertainty for -obs).
candidates <- dplyr::bind_rows(gwl_candidates, obs_candidates)

# Politeness: cap the rate at which we hit the MeteoSwiss host. capacity tokens
# refill over fill_time_s, sustaining ~2 req/s with a small burst. req_throttle
# buckets by host (default realm), so the probe and download stages share one
# budget, and req_perform_parallel honours it across concurrent requests.
throttle <- function(req) {
  httr2::req_throttle(req, capacity = 10, fill_time_s = 5)
}

# ---- Stage 1: probe ---------------------------------------------------------
# Build one HEAD request per row with purrr::pmap, then probe in parallel.
# req_error(is_error = FALSE) prevents httr2 from throwing on 4xx/5xx responses,
# so req_perform_parallel can return the response object for every row.
# httr2::req_url_query() handles percent-encoding, including the slash in
# "application/netcdf" → "application%2Fnetcdf", matching the observed API pattern.
# NA-valued query parameters are dropped so the same builder serves both families.

make_request <- function(product, indicator, gwl, uncertainty, reference_period, time_of_year) {
  drop_na <- function(x) if (is.na(x)) NULL else x
  httr2::request(paste0(base_url, "/", product, "/realizations")) |>
    httr2::req_url_query(
      language = "de",
      globalWarmingLevel = drop_na(gwl),
      referencePeriod = drop_na(reference_period),
      timeOfYear = time_of_year,
      uncertainty = drop_na(uncertainty),
      productType = "data",
      productName = product,
      scenarioIndicator = indicator,
      mediaType = "application/netcdf"
    )
}

probe_reqs <- purrr::pmap(
  candidates,
  function(product, indicator, gwl, uncertainty, reference_period, time_of_year, ...) {
    make_request(product, indicator, gwl, uncertainty, reference_period, time_of_year) |>
      httr2::req_method("HEAD") |>
      httr2::req_error(is_error = \(r) FALSE) |>
      throttle()
  }
)

# Store the fully-built URLs extracted from the request objects
candidates$url <- purrr::map_chr(probe_reqs, \(r) r$url)

message("Probing ", length(probe_reqs), " URLs...")
probe_resps <- httr2::req_perform_parallel(probe_reqs, on_error = "return")

is_live <- function(r) {
  !inherits(r, "error") && httr2::resp_status(r) < 400L
}
candidates$live <- vapply(probe_resps, is_live, logical(1))

message(sum(candidates$live), " / ", nrow(candidates), " URLs live")
print(dplyr::count(candidates, product, live), n = Inf)

# ---- Stage 2: download ------------------------------------------------------

cache_dir <- file.path(getOption("evoland.cachedir"), "ch2025")
evoland:::ensure_dir(cache_dir)

nc_filename <- function(product, indicator, gwl, uncertainty, reference_period, time_of_year) {
  # keep the -gwl / -obs suffix from the product, then append the dimensions that
  # actually apply to this family (NAs from the other family are dropped). The ETL
  # script parses these tokens back out, so the encoding must stay reversible.
  stem <- sub("^climate-ch2025-maps-", "", product)
  tokens <- c(indicator, gwl, uncertainty, reference_period, time_of_year)
  paste0(stem, "_", paste(tokens[!is.na(tokens)], collapse = "_"), ".nc")
}

download_nc <- function(url, product, indicator, gwl, uncertainty, reference_period, time_of_year) {
  fname <- nc_filename(product, indicator, gwl, uncertainty, reference_period, time_of_year)
  dest <- file.path(cache_dir, fname)
  if (file.exists(dest)) {
    message("Cached:      ", fname)
    return(dest)
  }
  message("Downloading: ", fname)
  httr2::request(url) |> throttle() |> httr2::req_perform(path = dest)
  dest
}

live_candidates <- dplyr::filter(candidates, live)

live_candidates$local_path <- purrr::pmap_chr(
  live_candidates,
  function(url, product, indicator, gwl, uncertainty, reference_period, time_of_year, ...) {
    download_nc(url, product, indicator, gwl, uncertainty, reference_period, time_of_year)
  }
)

message("Done. ", nrow(live_candidates), " files written to ", cache_dir)
