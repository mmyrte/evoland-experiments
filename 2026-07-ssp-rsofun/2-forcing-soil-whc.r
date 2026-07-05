#' Purpose: derive soil water-holding capacity (whc) from the Swiss Soil Property Map
#' date: 2026-07-03
#' auth: jan.hartman@ethz.ch
#'
#' rsofun's P-model takes a single site parameter `whc` (mm) = plant-available water in
#' the root zone; its SPLASH bucket sets the water-stress threshold at theta* = 0.6*whc.
#' whc therefore depends on BOTH soil (this script) AND rooting depth (land cover, from
#' 3-landcover-fapar.r). To keep the two concerns separable we compute here a purely
#' soil-derived, land-cover-independent profile: plant-available water (mm) per SSPM
#' depth layer. The final scalar whc = integral of that profile to the class rooting
#' depth is formed at run assembly (see `whc_from_profile()` / 4-run-rsofun.r).
#'
#' Source: Swiss Soil Property Map (SSPM; Gupta, Hasler & Alewell 2024,
#' doi:10.1016/j.geodrs.2023.e00747). QRF maps at 30 m, EPSG:2056, at depths
#' 0/30/60/100 cm, delivering sand & clay (%), organic carbon OC (%), + N, P (unused).
#' NOT provided and therefore estimated/assumed: bulk density (PTF estimate below) and
#' coarse-fragment fraction (assumed 0 unless supplied) -- see README open questions.
#'
#' The pedotransfer functions port rsplash::soil_hydro (Saxton & Rawls 2006; Balland et
#' al. 2008; Sandoval et al. in prep.), the same PTF used by SPLASH, so the interim run
#' and any later rSPLASH work share one soil parameterisation.

library(data.table)

# SSPM depth layers (cm): properties given at horizon tops 0/30/60/100; we treat the
# reported values as the layer means over these three intervals.
SSPM_LAYERS <- data.table(
  top_cm = c(0, 30, 60),
  bot_cm = c(30, 60, 100),
  thickness_m = c(0.30, 0.30, 0.40)
)
OM_PER_OC <- 1.724 # organic matter = organic carbon * van Bemmelen factor

# =============================================================================
# Pedotransfer core (pure; vectorised). Inputs sand/clay/OM in PERCENT (w/w).
# =============================================================================

#' Estimate bulk density from texture + OM (Balland et al. 2008), used when the SSPM
#' does not supply it. @return g cm-3
estimate_bd <- function(sand, clay, OM, depth_cm = 30) {
  fclay <- clay / 100
  fOM <- OM / 100
  dp <- 1 / ((fOM / 1.3) + ((1 - fOM) / 2.65)) # particle density
  bd <- (1.5 + (dp - 1.5 - 1.10 * (1 - fclay)) * (1 - exp(-0.022 * depth_cm))) /
    (1 + 6.27 * fOM)
  pmax(bd, 0.81) # low-bd floor (rsplash)
}

#' Volumetric field capacity (33 kPa) and wilting point (1500 kPa) via the SPLASH PTF.
#' @param bd bulk density g cm-3; if NA, estimated from texture + OM
#' @return list(fc, wp, sat) volumetric fractions
soil_hydro_fc_wp <- function(sand, clay, OM, bd = NA_real_) {
  fsand <- sand / 100
  fclay <- clay / 100
  fOM <- OM / 100
  dp <- 1 / ((fOM / 1.3) + ((1 - fOM) / 2.65))
  bd <- ifelse(is.na(bd), estimate_bd(sand, clay, OM), bd)
  bd <- pmax(bd, 0.81)
  sat <- 1 - bd / dp
  fc <- (sat / bd) *
    (0.4760944 + (0.9402962 - 0.4760944) * fclay^0.5) *
    exp(-1 * (0.05472678 * fsand - 0.01 * fOM) / (sat / bd))
  # wilting point: Sandoval (percent inputs); fall back to Balland if invalid
  wp <- -2.464e-05 * sand + 3.650e-03 * clay + 8.680e-03 * OM + 9.393e-03 * bd
  wp_ball <- fc * (0.2018522 + (0.7809203 - 0.2018522) * fclay^0.5)
  bad <- is.na(wp) | wp <= 0 | wp >= fc
  wp[bad] <- wp_ball[bad]
  list(fc = fc, wp = wp, sat = sat)
}

#' Plant-available water (mm) in a layer of given thickness.
#' AWC_vol = (FC - WP)*(1 - coarse_frac); whc_mm = AWC_vol * thickness * 1000.
awc_layer_mm <- function(sand, clay, OM, thickness_m, bd = NA_real_, coarse_frac = 0) {
  h <- soil_hydro_fc_wp(sand, clay, OM, bd)
  pmax(h$fc - h$wp, 0) * (1 - coarse_frac) * thickness_m * 1000
}

#' Integrate a per-layer AWC profile (mm) down to a rooting depth (m).
#' Partial last layer is prorated. `layers` must carry top_cm/bot_cm/awc_mm.
#' @return whc, mm
whc_from_profile <- function(layers, root_depth_m) {
  root_cm <- root_depth_m * 100
  frac <- pmin(pmax((root_cm - layers$top_cm) /
    (layers$bot_cm - layers$top_cm), 0), 1)
  sum(layers$awc_mm * frac, na.rm = TRUE)
}

# =============================================================================
# SSPM reader (chunk-oriented). Filenames/paths TBC -> `sspm_layer_paths` is the
# single contract to fill once the on-disk SSPM layout is known.
# =============================================================================

#' The SSPM layer inventory is produced by 2-forcing-soil-download.r as a data.table with
#' columns property ("sand"/"clay"/"OC"), depth_cm (0/30/60/100) and local_path. This
#' reader consumes that inventory directly, so filenames live in the download script only.

#' Extract SSPM properties at model coords and return per-layer AWC (mm).
#' The SSPM tiles are EPSG:4326 (WGS84); model coords are EPSG:2056, so we build a
#' SpatVector in 2056 and let terra reproject on extract (NoData -3.4e38 -> NA).
#' @param sspm_files inventory: data.table(property, depth_cm, local_path)
#' @param coords data.table with id_coord, lon, lat (EPSG:2056 E/N)
#' @return data.table(id_coord, top_cm, bot_cm, thickness_m, awc_mm)
sspm_awc_profile <- function(sspm_files, coords, coarse_frac = 0) {
  pts <- terra::vect(as.data.frame(coords[, c("lon", "lat")]),
    geom = c("lon", "lat"), crs = "EPSG:2056")
  read_prop <- function(prop, depth) {
    p <- sspm_files[property == prop & depth_cm == depth, local_path]
    if (length(p) != 1) stop("expected one SSPM file for ", prop, " @", depth, "cm")
    terra::extract(terra::rast(p), pts, ID = FALSE)[[1]]
  }
  out <- lapply(seq_len(nrow(SSPM_LAYERS)), function(i) {
    lyr <- SSPM_LAYERS[i]
    # layer mean = mean of the horizon values bounding the interval
    sand <- (read_prop("sand", lyr$top_cm) + read_prop("sand", lyr$bot_cm)) / 2
    clay <- (read_prop("clay", lyr$top_cm) + read_prop("clay", lyr$bot_cm)) / 2
    oc <- (read_prop("OC", lyr$top_cm) + read_prop("OC", lyr$bot_cm)) / 2
    data.table(
      id_coord = coords$id_coord,
      top_cm = lyr$top_cm, bot_cm = lyr$bot_cm, thickness_m = lyr$thickness_m,
      awc_mm = awc_layer_mm(sand, clay, oc * OM_PER_OC, lyr$thickness_m,
        coarse_frac = coarse_frac)
    )
  })
  rbindlist(out)
}

# =============================================================================
# Sanity checks (pure PTF; run on source, no data needed)
# =============================================================================
local({
  # a loam: FC ~ 0.2-0.3, WP ~ 0.05-0.15, AWC positive (PTF-reproduction invariants;
  # note this PTF does not guarantee a loam>sand AWC ordering, so we don't assert it)
  h <- soil_hydro_fc_wp(sand = 40, clay = 20, OM = 2)
  stopifnot(h$fc > 0.15, h$fc < 0.45, h$wp > 0.02, h$wp < 0.25, h$fc > h$wp)
  # per-metre AWC stays in a physically plausible band for both textures
  awc_sand <- awc_layer_mm(90, 5, 1, 1)
  awc_loam <- awc_layer_mm(40, 20, 2, 1)
  stopifnot(awc_sand > 0, awc_sand < 400, awc_loam > 0, awc_loam < 400)
  # profile integration: 3 layers of 100 mm-ish, root depth prorates the last
  prof <- data.table(top_cm = c(0, 30, 60), bot_cm = c(30, 60, 100),
    awc_mm = c(30, 30, 40))
  stopifnot(
    abs(whc_from_profile(prof, 1.0) - 100) < 1e-9, # full profile
    abs(whc_from_profile(prof, 0.30) - 30) < 1e-9, # first layer only
    abs(whc_from_profile(prof, 0.80) - (30 + 30 + 20)) < 1e-9 # half of last
  )
  # coarse fragments reduce AWC
  stopifnot(awc_layer_mm(40, 20, 2, 1, coarse_frac = 0.5) < awc_loam)
  message("2-forcing-soil-whc.r: sanity checks passed.")
})
