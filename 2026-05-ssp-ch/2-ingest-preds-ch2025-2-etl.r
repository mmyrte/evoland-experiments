#' Purpose: ETL cached CH2025 observation netCDFs into the predictor tables
#' date: 2026-06-25
#'
#' Reads the -obs netCDFs cached by 2-ingest-preds-ch2025-1-download.r and ingests
#' each as a predictor at id_period = 0 (baseline), under the active id_run (= 0).
#'
#' All dimensions are recovered from the cached file names (see nc_filename() in the
#' download script), so nothing is passed between the two scripts in memory. Each
#' file is read, transformed, and written one at a time to keep memory flat: a single
#' long table over ~4M coords × 67 files would not fit comfortably.
#'
#' Data model (the dimensions of pred_data_t):
#'   - name  = {indicator}_{time_of_year}, e.g. "tas_yearly", "pr_DJF". A predictor is
#'             one climate variable and must stay consistent across historic (observed)
#'             and future (extrapolated) periods, so the reference period is NOT part of
#'             the name.
#'   - id_period encodes the temporal progression: the observed reference period here,
#'             and (deferred) the progression through global warming levels for -gwl.
#'   - id_run  encodes quantile × SSP scenario (relevant for the deferred -gwl runs).
#'
#' Stage 4 uses db$add_predictor (safe upsert of both pred_meta_t and pred_data_t),
#' which is the right tool for this initial commit. When appending the -gwl scenario
#' projections later, write directly to pred_data_t keyed by the id_pred that already
#' exists in pred_meta_t (one id_pred per {indicator}_{time_of_year} predictor), at the
#' mapped id_period and the scenario/quantile id_run.
#'
#' Scope for now: -obs files only, at id_period = 0 (baseline) under the active id_run
#' (= 0). Because the name drops the reference period, only ONE reference period can
#' occupy id_period 0; we use the current WMO normal 1991-2020, which covers all 21
#' indicators (1961-1990 covers only 14, a subset). 1961-1990 stays cached for a later
#' historic id_period if wanted. The -gwl projections remain deferred.

library(data.table)
library(evoland)

base_url <- "https://service.meteoswiss.ch/pbbackend/api/v1/products"
cache_dir <- file.path(getOption("evoland.cachedir"), "ch2025")

# Reference period to populate the id_period = 0 baseline (see header).
obs_reference_period <- "1991-2020"

# ---- Per-indicator metadata -------------------------------------------------
# Pattern mirrors 2-ingest-preds-swisstlm3d.r: one spec per indicator, tied to the
# files via the indicator token parsed from each file name. unit / pretty_name track
# the netCDF long_name & units; description adds the CH2025 definition +
# provenance.

# comment out lines to skip them at ingest, even if netcdf is already on disk
# TODO this is missing bioclimatic indicators,
# https://www.chelsa-climate.org/datasets/chelsa_bioclim has a good list
prov <- "MeteoSwiss CH2025 observed climatology (1 km, EPSG:2056)."
indicator_specs <- list(
  ID = list(
    unit = "days",
    pretty_name = "Ice days",
    description = paste("Annual number of ice days (daily maximum temperature below 0 C).", prov)
  ),
  FD = list(
    unit = "days",
    pretty_name = "Frost days",
    description = paste("Annual number of frost days (daily minimum temperature below 0 C).", prov)
  ),
  # HDD = list(
  #   unit = "Kd",
  #   pretty_name = "Heating degree days",
  #   description = paste(
  #     "Heating degree days: annual sum of the daily shortfall of mean temperature below the heating target, i.e. difference on days below 12°C mean and 20°C heating target.",
  #     prov
  #   )
  # ),
  # HED = list(
  #   unit = "days",
  #   pretty_name = "Heating days",
  #   description = paste("Annual number of heating days, i.e. mean temp. ≤ 12°C", prov)
  # ),
  HD = list(
    unit = "days",
    pretty_name = "Heat days",
    description = paste("Annual number of heat days, i.e. max. temp. >= 30°C", prov)
  ),
  HW2 = list(
    unit = "warning days",
    pretty_name = "Heat warning days (Level 2)",
    description = paste(
      "Annual number of heat-warning days at MeteoSwiss warning level 2, mean temp >= 25",
      prov
    )
  ),
  HW3 = list(
    unit = "warning days",
    pretty_name = "Heat warning days (Level 3)",
    description = paste(
      "Annual number of heat-warning days at MeteoSwiss warning level 3, mean temp >= 25° for at least 3 days",
      prov
    )
  ),
  HW4 = list(
    unit = "warning days",
    pretty_name = "Heat warning days (Level 4)",
    description = paste(
      "Annual number of heat-warning days at MeteoSwiss warning level 4, mean temp >= 27°C for at least 3 days",
      prov
    )
  ),
  # CoDD = list(
  #   unit = "Kd",
  #   pretty_name = "Cooling degree days",
  #   description = paste(
  #     "Cooling degree days: annual sum of the daily excess of mean temperature above the cooling target (18.3°C).",
  #     prov
  #   )
  # ),
  # COD = list(
  #   unit = "days",
  #   pretty_name = "Cooling days",
  #   description = paste("Annual number of cooling days (daily mean >= 18.3).", prov)
  # ),
  VHD = list(
    unit = "days",
    pretty_name = "Very hot days",
    description = paste(
      "Annual number of very hot days (very high daily maximum temperature, >= 35).",
      prov
    )
  ),
  SD = list(
    unit = "days",
    pretty_name = "Summer days",
    description = paste(
      "Annual number of summer days (daily maximum temperature at or above the summer-day threshold >= 25).",
      prov
    )
  ),
  TN = list(
    unit = "days",
    pretty_name = "Tropical nights",
    description = paste(
      "Annual number of tropical nights (high daily minimum temperature >=20mm).",
      prov
    )
  ),
  pr = list(
    unit = "mm day-1",
    pretty_name = "Precipitation",
    description = paste("Mean daily precipitation amount.", prov)
  ),
  PR20 = list(
    unit = "days",
    pretty_name = "Moderate precipitation days",
    description = paste("Annual number of days with moderate precipitation. >= 20mm", prov)
  ),
  PR40 = list(
    unit = "days",
    pretty_name = "Heavy precipitation days",
    description = paste("Annual number of days with heavy precipitation. >= 40mm", prov)
  ),
  PR60 = list(
    unit = "days",
    pretty_name = "Very heavy precipitation days",
    description = paste("Annual number of days with very heavy precipitation. >= 60mm", prov)
  ),
  SNFD = list(
    unit = "days",
    pretty_name = "Snowfall days",
    description = paste("Annual number of days with snowfall, <2deg mean & >1mm/d precip", prov)
  ),
  tas = list(
    unit = "degrees_C",
    pretty_name = "Near-surface air temperature",
    description = paste("Mean near-surface air temperature.", prov)
  ),
  tasmax = list(
    unit = "degrees_C",
    pretty_name = "Daily maximum near-surface air temperature",
    description = paste("Daily maximum near-surface air temperature.", prov)
  ),
  tasmin = list(
    unit = "degrees_C",
    pretty_name = "Daily minimum near-surface air temperature",
    description = paste("Daily minimum near-surface air temperature.", prov)
  ),
  CDD = list(
    unit = "days",
    pretty_name = "Consecutive dry days",
    description = paste("Maximum run of consecutive dry days, <1mm/d", prov)
  )
)

# Reconstruct the source API URL from the parsed dimensions, mirroring the query in
# the download script. No published md5sum (the endpoint is generated on demand).
obs_url <- function(product, indicator, reference_period, time_of_year) {
  req <- httr2::request(paste0(base_url, "/", product, "/realizations")) |>
    httr2::req_url_query(
      language = "de",
      referencePeriod = reference_period,
      timeOfYear = time_of_year,
      productType = "data",
      productName = product,
      scenarioIndicator = indicator,
      mediaType = "application/netcdf"
    )
  req$url
}

# ---- Recover the file inventory from cache ----------------------------------
# obs file names are {category}-obs_{indicator}_{reference_period}_{time_of_year}.nc;
# indicator codes carry no underscore and reference_period uses a hyphen, so the stem
# splits cleanly into exactly four underscore-separated tokens.
obs_files <- list.files(cache_dir, pattern = "-obs_.*\\.nc$")

inventory <- data.table(file = obs_files)
inventory[, stem := sub("\\.nc$", "", file)]
inventory[,
  c(
    "product_stem",
    "indicator",
    "reference_period",
    "time_of_year"
  ) := data.table::tstrsplit(
    stem,
    "_",
    fixed = TRUE
  )
]
inventory[, product := paste0("climate-ch2025-maps-", product_stem)]
inventory[, pred_name := paste(indicator, time_of_year, sep = "_")]

if (anyNA(inventory[, .(indicator, reference_period, time_of_year)])) {
  stop(
    "Unparseable obs file name(s):\n  ",
    paste(
      inventory[
        is.na(indicator) | is.na(reference_period) | is.na(time_of_year),
        file
      ],
      collapse = "\n  "
    )
  )
}

# id_period 0 baseline takes a single reference period
inventory <- inventory[reference_period == obs_reference_period]

message(
  "Found ",
  nrow(inventory),
  " obs files (",
  obs_reference_period,
  ") across ",
  data.table::uniqueN(inventory, by = "indicator"),
  " indicators on disk."
)

# ---- Stage 3+4: read -> transform -> write, one file at a time --------------
db <- evoland_db$new(path = "ssp-ch.evolanddb")
coords_minimal <- db$coords_minimal
extent_wide <- db$extent |> terra::extend(1000)

orig_format <- paste0(
  "MeteoSwiss CH2025 netCDF (EPSG:2056, 1km); baseline observed ",
  obs_reference_period,
  " period, point-extracted at 100m model coords (nearest cell, no bilinear interp.)"
)

for (i in seq_len(nrow(inventory))) {
  row <- inventory[i]
  spec <- indicator_specs[[row[["indicator"]]]]
  if (is.null(spec)) {
    warning("No metadata spec for indicator '", row[["indicator"]], "'; skipping ", row[["file"]])
    next
  }

  message(sprintf("[%d/%d] %s", i, nrow(inventory), row[["pred_name"]]))

  r <- terra::rast(file.path(cache_dir, row[["file"]])) |> terra::crop(extent_wide)
  terra::set.names(r, row[["pred_name"]])
  pred_data <- extract_using_coords_t(r, coords_minimal)

  db$add_predictor(
    pred_data_raw = pred_data[, .(id_coord, id_period = 0L, value)],
    name = row[["pred_name"]],
    fill_value = NA,
    unit = spec[["unit"]],
    pretty_name = sprintf("%s (%s)", spec[["pretty_name"]], row[["time_of_year"]]),
    orig_format = orig_format,
    description = spec[["description"]],
    sources = data.table(
      url = obs_url(
        row[["product"]],
        row[["indicator"]],
        row[["reference_period"]],
        row[["time_of_year"]]
      ),
      md5sum = NA_character_
    )
  )
}

message("Done. Ingested ", nrow(inventory), " obs predictors at id_period = 0.")
