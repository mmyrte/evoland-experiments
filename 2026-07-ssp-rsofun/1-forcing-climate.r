#' Purpose: derive the rsofun P-model daily forcing from CH2025 daily-gridded climate
#' date: 2026-07-03
#'
#' CH2025 daily-gridded delivers only pr, tas, tasmax, tasmin (1 km, EPSG:2056,
#' 365_day calendar). The rsofun P-model needs more, so this script derives the rest
#' from first principles (FAO-56 / SPLASH) and assembles the forcing contract verified
#' against the fork at mmyrte/rsofun R/run_pmodel_f_bysite.R:205-247:
#'
#'   mandatory : temp, rain, snow, vpd, co2, fapar, patm, tmin, tmax
#'   radiation : supply `ccov` (cloud cover %); the interface derives fsun=(100-ccov)/100
#'               and, with ppfd=NA, SPLASH computes PPFD internally from its own SOLAR
#'               (incl. topographic corrections) -- matching "radiation inside rsofun".
#'               We set ccov = 100*(1 - fsun_Hargreaves); ppfd is left NA (optional).
#'   NOT needed: netrad         (SPLASH computes net radiation internally)
#'
#' `fapar` is left as NA here; it is joined from 3-landcover-fapar.r (WASIM LAI->fAPAR).
#'
#' Design: the PURE derivation functions below are the reusable, unit-testable core and
#' carry no I/O. The full 4.1M-coord x 10950-day x (modelchain x GWL) product cannot be
#' materialised, so the CH2025 reader is chunk-oriented (takes a coord subset) and the
#' streaming application over the grid lives in 4-run-rsofun.r, which sources this file.
#'
#' CH2025 layout (from ncdump):
#'   root/{pr,tas,tasmax,tasmin}/
#'     ogd-climate-scenarios-ch2025-grid_ch_{var}_{modelchain}_{gwl}.nc
#'   var(time, N, E) float; time = 10950 d (30 yr x 365, calendar "365_day");
#'   grid 370x240 @ 1 km, CRS EPSG:2056; _FillValue -999.99.
#'   The ensemble/scenario axis is the FILENAME (modelchain x GWL), not a netCDF dim.

library(data.table)

# ---- physical constants -----------------------------------------------------
GSC_MJ <- 0.0820 # solar constant, MJ m-2 min-1 (FAO-56)
PAR_FRAC <- 0.5 # shortwave -> PAR energy fraction
UMOL_PER_J_PAR <- 4.57 # PAR energy -> photon flux, umol J-1 (Meek et al. 1984)
# barometric formula constants (identical to rsofun calc_patm / SPLASH global.cpp)
P0 <- 101325 # standard sea-level pressure, Pa
T0 <- 288.15 # standard sea-level temperature, K
L_LAPSE <- 0.0065 # environmental lapse rate, K m-1
G_ACC <- 9.80665 # gravitational acceleration, m s-2
M_AIR <- 0.028963 # molar mass of dry air, kg mol-1
R_GAS <- 8.31447 # universal gas constant, J mol-1 K-1

# =============================================================================
# Pure derivation functions (no I/O; vectorised over days)
# =============================================================================

#' Atmospheric pressure from elevation (barometric formula; FAO-56 / SPLASH).
#' @param elv elevation, m a.s.l.
#' @return pressure, Pa
calc_patm <- function(elv) {
  P0 * (1 - L_LAPSE * elv / T0)^(G_ACC * M_AIR / (R_GAS * L_LAPSE))
}

#' Saturation vapour pressure (Tetens/FAO-56).
#' @param tc air temperature, deg C
#' @return e_sat, Pa
esat <- function(tc) {
  611.0 * exp(17.27 * tc / (tc + 237.3))
}

#' Vapour pressure deficit from daily temperature range (FAO-56), dewpoint ~= tmin.
#' Mean saturation vp uses esat(tmax) & esat(tmin); actual vp uses esat(tmin).
#' @return VPD, Pa (>= 0). NOTE: dewpoint~=tmin biases dry (Alpine) air; validate.
calc_vpd <- function(tmin, tmax) {
  es <- (esat(tmax) + esat(tmin)) / 2
  ea <- esat(tmin)
  pmax(es - ea, 0)
}

#' Partition precipitation into rain and snow by a linear temperature ramp.
#' Fraction rain rises linearly from 0 at `t_all_snow` to 1 at `t_all_rain`.
#' (SPLASH/WASIM use a Kienzle sigmoid; the linear ramp is the simple default.)
#' @return list(rain, snow), mm
split_precip <- function(pr, temp, t_all_snow = 0, t_all_rain = 2) {
  f_rain <- pmin(pmax((temp - t_all_snow) / (t_all_rain - t_all_snow), 0), 1)
  list(rain = pr * f_rain, snow = pr * (1 - f_rain))
}

#' Extraterrestrial radiation on a horizontal surface (FAO-56 eq. 21).
#' @param doy day of year (1..365)
#' @param lat_deg latitude, degrees
#' @return Ra, MJ m-2 d-1
extraterrestrial_radiation <- function(doy, lat_deg) {
  phi <- lat_deg * pi / 180
  dr <- 1 + 0.033 * cos(2 * pi / 365 * doy) # inverse relative earth-sun distance
  decl <- 0.409 * sin(2 * pi / 365 * doy - 1.39) # solar declination, rad
  # sunset hour angle, guarding polar day/night
  x <- -tan(phi) * tan(decl)
  ws <- acos(pmin(pmax(x, -1), 1))
  (24 * 60 / pi) * GSC_MJ * dr * (ws * sin(phi) * sin(decl) + cos(phi) * cos(decl) * sin(ws))
}

#' Hargreaves shortwave radiation, plus the derived PPFD and sunshine fraction.
#' Rs = k * sqrt(tmax - tmin) * Ra, capped at clear-sky Rso = (0.75 + 2e-5*elv)*Ra.
#' fsun (n/N) inverted from Angstrom-Prescott Rs/Ra = a_s + b_s*(n/N).
#' @return list(rs [MJ m-2 d-1], ppfd [mol m-2 d-1], fsun [0..1])
hargreaves <- function(tmin, tmax, doy, lat_deg, elv, k = 0.17, a_s = 0.25, b_s = 0.50) {
  ra <- extraterrestrial_radiation(doy, lat_deg)
  rso <- (0.75 + 2e-5 * elv) * ra
  rs <- pmin(k * sqrt(pmax(tmax - tmin, 0)) * ra, rso)
  # PPFD: MJ -> J (1e6), * PAR fraction * umol/J, umol -> mol (1e-6)
  ppfd <- rs * 1e6 * PAR_FRAC * UMOL_PER_J_PAR * 1e-6
  tau <- ifelse(ra > 0, rs / ra, 0)
  fsun <- pmin(pmax((tau - a_s) / b_s, 0), 1)
  list(rs = rs, ppfd = ppfd, fsun = fsun)
}

#' No-leap (365_day) date sequence, to give the GWL slice nominal calendar dates
#' that rsofun's year/spin-up logic can consume. Drops Feb-29 so 365 days/year align
#' with the CH2025 "365_day" calendar and a plain doy = ((i-1) %% 365) + 1.
#' @param nt number of days
#' @param base_year first nominal year
ch2025_dates <- function(nt, base_year = 2001) {
  n_years <- ceiling(nt / 365)
  d <- seq(
    as.Date(paste0(base_year, "-01-01")),
    as.Date(paste0(base_year + n_years, "-12-31")),
    by = "day"
  )
  d <- d[!(format(d, "%m-%d") == "02-29")]
  d[seq_len(nt)]
}

#' Assemble the rsofun P-model forcing for ONE pixel's daily series.
#' @param pr,tas,tasmin,tasmax daily vectors (mm, degC, degC, degC), equal length
#' @param lat_deg,elv pixel latitude (deg) and elevation (m)
#' @param co2 atmospheric CO2, ppm (scalar per GWL run, or a vector of length(pr))
#' @param base_year nominal first year for the no-leap date axis
#' @return data.table with the rsofun forcing columns; `fapar` = NA (joined later)
derive_pmodel_forcing <- function(pr, tas, tasmin, tasmax, lat_deg, elv, co2, base_year = 2001) {
  nt <- length(tas)
  stopifnot(length(pr) == nt, length(tasmin) == nt, length(tasmax) == nt)
  dates <- ch2025_dates(nt, base_year)
  doy <- as.integer(((seq_len(nt) - 1L) %% 365L) + 1L)
  rs <- hargreaves(tasmin, tasmax, doy, lat_deg, elv)
  ps <- split_precip(pr, tas)
  data.table(
    date = dates,
    temp = tas,
    tmin = tasmin,
    tmax = tasmax,
    rain = ps$rain,
    snow = ps$snow,
    vpd = calc_vpd(tasmin, tasmax), # Pa
    ccov = 100 * (1 - rs$fsun), # -> interface derives fsun; SPLASH computes PPFD
    ppfd = NA_real_, # let SPLASH compute; set rs$ppfd to force Hargreaves PPFD instead
    netrad = NA_real_, # ignored by rsofun; SPLASH computes it
    co2 = co2, # ppm
    fapar = NA_real_, # joined from 3-landcover-fapar.r
    patm = calc_patm(elv) # Pa
  )
}

# =============================================================================
# CH2025 reader (chunk-oriented; applied per spatial chunk in 4-run-rsofun.r)
# =============================================================================

#' Inventory the CH2025 daily-gridded tree into (var, modelchain, gwl, path).
#' Filenames: ogd-climate-scenarios-ch2025-grid_ch_{var}_{modelchain}_{gwl}.nc
ch2025_inventory <- function(root) {
  files <- list.files(root, pattern = "\\.nc$", recursive = TRUE, full.names = TRUE)
  inv <- data.table(
    path = files,
    tail = sub("\\.nc$", "", sub("^ogd-climate-scenarios-ch2025-grid_ch_", "", basename(files)))
  )
  # tail = {var}_{modelchain}_{gwl}; modelchain may contain "_", so take var as the
  # first token and gwl as the last (^gwl...), modelchain as everything between.
  inv[, var := sub("_.*$", "", tail)]
  inv[, gwl := sub("^.*_", "", tail)]
  inv[, modelchain := sub(paste0("^", var, "_(.*)_", gwl, "$"), "\\1", tail), by = tail]
  bad <- inv[!grepl("^gwl", gwl) | modelchain == "" | modelchain == tail]
  if (nrow(bad)) {
    stop("Unparseable CH2025 file name(s):\n  ", paste(bad$path, collapse = "\n  "))
  }
  inv[, .(var, modelchain, gwl, path)]
}

#' Open the four forcing variables for one (modelchain, gwl) as terra SpatRasters.
#' @return named list(pr, tas, tasmax, tasmin) of SpatRaster (time as layers)
ch2025_open <- function(inventory, modelchain, gwl, vars = c("pr", "tas", "tasmax", "tasmin")) {
  mc <- modelchain
  gw <- gwl
  sub <- inventory[modelchain == mc & gwl == gw]
  out <- lapply(vars, function(v) {
    p <- sub[var == v, path]
    if (length(p) != 1) {
      stop("expected exactly one file for var=", v, "; got ", length(p))
    }
    r <- terra::rast(p)
    terra::crs(r) <- "EPSG:2056" # LV95 per swiss_lv95_coordinates grid_mapping
    r
  })
  setNames(out, vars)
}

#' Extract the daily series for a subset of model coords (one spatial chunk).
#' Nearest-cell, matching the baseline ETL (no bilinear interpolation).
#' @param rasters output of ch2025_open()
#' @param coords data.table/data.frame with columns id_coord, lon, lat (EPSG:2056 E/N)
#' @return named list of matrices [n_coord x n_time], one per variable; -999.99 -> NA
ch2025_extract_chunk <- function(rasters, coords) {
  xy <- as.matrix(coords[, c("lon", "lat")])
  lapply(rasters, function(r) {
    m <- terra::extract(r, xy, method = "simple")
    m <- as.matrix(m)
    m[m <= -999.98] <- NA_real_ # _FillValue / missing_value -999.99
    m
  })
}

# =============================================================================
# Sanity checks (run when this file is sourced on the cluster; no data needed)
# =============================================================================
local({
  # sea-level pressure ~ 101325 Pa; ~ 89870 Pa near 1100 m (SPLASH reference)
  stopifnot(abs(calc_patm(0) - 101325) < 1e-6)
  stopifnot(calc_patm(2000) < calc_patm(0))
  # VPD is zero when tmin == tmax (saturated) and positive for a spread
  stopifnot(calc_vpd(10, 10) == 0, calc_vpd(5, 25) > 0)
  # rain/snow split conserves mass and behaves at the ends
  sp <- split_precip(c(10, 10, 10), c(-5, 1, 8))
  stopifnot(
    all(abs(sp$rain + sp$snow - 10) < 1e-9),
    sp$snow[1] == 10,
    sp$rain[3] == 10
  )
  # Ra at the equator near equinox ~ 36-38 MJ/m2/d (FAO-56)
  ra_eq <- extraterrestrial_radiation(80, 0)
  stopifnot(ra_eq > 35, ra_eq < 39)
  # Hargreaves: fsun in [0,1], ppfd >= 0, Rs capped below clear-sky
  hg <- hargreaves(tmin = 5, tmax = 18, doy = 180, lat_deg = 47, elv = 500)
  stopifnot(hg$fsun >= 0, hg$fsun <= 1, hg$ppfd >= 0)
  # no-leap date axis: exactly 365 distinct days per nominal year, no Feb-29
  d <- ch2025_dates(730)
  stopifnot(length(d) == 730, !any(format(d, "%m-%d") == "02-29"))
  message("1-forcing-climate.r: sanity checks passed.")
})
