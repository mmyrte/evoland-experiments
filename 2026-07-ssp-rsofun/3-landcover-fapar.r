#' Purpose: turn the WASIM [landuse_table] into daily fAPAR / albedo / rooting depth
#' date: 2026-07-03
#' auth: jan.hartman@ethz.ch
#'
#' fAPAR is rsofun's land-cover coupling handle. Per the project decision we populate it
#' "the WASIM way": each WASIM land-use class carries 12 monthly values (at mid-month
#' Julian days) for LAI, VCF, Albedo and RootDepth. We reconstruct the daily LAI/VCF the
#' same way WASIM does (linear interpolation between sample days) and form
#'
#'     fAPAR(doy) = VCF(doy) * (1 - exp(-k_extinct * LAI(doy)))      # Beer-Lambert
#'
#' with k_extinct = 0.3 from [multilayer_landuse]. Albedo is interpolated likewise (feeds
#' rsofun net radiation); RootDepth's growing-season maximum gives the rooting depth used
#' with the soil AWC profile (2-forcing-soil-whc.r) to form the scalar `whc`.
#'
#' Source table: 2026-07-ssp-rsofun/wasim_control_sample.txt (19 [landuse_table] entries,
#' 17 [multilayer_landuse] combinations). Land cover enters rsofun in WASIM's own
#' classification (project decision), so each map class maps 1:1 to an entry here.
#'
#' NOTE (AltDep): each entry carries an altitude-dependence array that shifts phenology
#' up-slope. The exact WASIM shift formula is not yet transcribed, so `apply_altdep` is
#' FALSE by default (lowland phenology) -- see README open items before enabling.

library(data.table)

WASIM_CONTROL <- file.path("2026-07-ssp-rsofun", "wasim_control_sample.txt")
K_EXTINCT <- 0.3 # Beer-Lambert extinction, from [multilayer_landuse]
DOY <- 1:365

# =============================================================================
# Parsing
# =============================================================================

#' Read a control-file section body ("[name] ... up to the next [section] or EOF"),
#' with inline "#" comments stripped.
.read_section <- function(path, name) {
  lines <- sub("#.*$", "", readLines(path, warn = FALSE)) # drop inline comments
  starts <- grep("^\\s*\\[", lines)
  hit <- grep(paste0("^\\s*\\[", name, "\\]"), lines)
  if (length(hit) != 1) stop("section [", name, "] not found uniquely in ", path)
  nxt <- starts[starts > hit]
  end <- if (length(nxt)) min(nxt) - 1L else length(lines)
  paste(lines[(hit + 1L):end], collapse = "\n")
}

#' Parse [landuse_table] -> data.table with one row per class and list-columns of the
#' 12-value (or scalar) arrays juldays/albedo/lai/vcf/rootdepth/altdep.
parse_landuse_table <- function(path = WASIM_CONTROL) {
  body <- .read_section(path, "landuse_table")
  # entries: "<id> <name> { ... }" (no nested braces inside the body)
  m <- gregexpr("(\\d+)\\s+(\\S+)\\s*\\{([^}]*)\\}", body, perl = TRUE)
  entries <- regmatches(body, m)[[1]]
  if (!length(entries)) stop("no [landuse_table] entries parsed from ", path)
  get_arr <- function(txt, param) {
    r <- regmatches(txt, regexpr(
      paste0(param, "\\s*=\\s*([^;]*);"), txt, perl = TRUE
    ))
    if (!length(r)) return(NA_real_)
    v <- sub(paste0(param, "\\s*=\\s*"), "", sub(";\\s*$", "", r))
    as.numeric(strsplit(trimws(v), "\\s+")[[1]])
  }
  rbindlist(lapply(entries, function(e) {
    hdr <- regmatches(e, regexpr("^(\\d+)\\s+(\\S+)", e, perl = TRUE))
    id <- as.integer(sub("\\s.*$", "", hdr))
    nm <- trimws(sub("^\\d+\\s+", "", hdr))
    data.table(
      landuse_id = id, name = nm,
      juldays = list(get_arr(e, "JulDays")),
      albedo = list(get_arr(e, "Albedo")),
      lai = list(get_arr(e, "LAI")),
      vcf = list(get_arr(e, "VCF")),
      rootdepth = list(get_arr(e, "RootDepth")),
      altdep = list(get_arr(e, "AltDep"))
    )
  }))
}

#' Parse [multilayer_landuse] -> data.table(ml_id, name, layers=list(int), k_extinct).
parse_multilayer <- function(path = WASIM_CONTROL) {
  body <- .read_section(path, "multilayer_landuse")
  m <- gregexpr("(\\d+)\\s+(\\S+)\\s*\\{([^}]*)\\}", body, perl = TRUE)
  entries <- regmatches(body, m)[[1]]
  rbindlist(lapply(entries, function(e) {
    hdr <- regmatches(e, regexpr("^(\\d+)\\s+(\\S+)", e, perl = TRUE))
    layers_txt <- regmatches(e, regexpr("Landuse_Layers\\s*=\\s*([^;]*);", e, perl = TRUE))
    layers <- as.integer(strsplit(trimws(sub("Landuse_Layers\\s*=\\s*", "",
      sub(";\\s*$", "", layers_txt))), "\\s*,\\s*")[[1]])
    layers <- layers[layers > 0] # drop -9999 padding
    kx <- regmatches(e, regexpr("k_extinct\\s*=\\s*([0-9.]+)", e, perl = TRUE))
    data.table(
      ml_id = as.integer(sub("\\s.*$", "", hdr)),
      name = trimws(sub("^\\d+\\s+", "", hdr)),
      layers = list(layers),
      k_extinct = as.numeric(sub("k_extinct\\s*=\\s*", "", kx))
    )
  }))
}

# =============================================================================
# Daily reconstruction
# =============================================================================

#' Cyclic linear interpolation of monthly sample values to daily (doy 1..365).
#' Scalars (single-value classes, JulDays=365) become a constant series.
interp_cyclic <- function(juldays, vals, doy = DOY) {
  if (length(vals) == 1L) return(rep(vals, length(doy)))
  x <- c(juldays - 365, juldays, juldays + 365)
  y <- c(vals, vals, vals)
  approx(x, y, xout = doy, method = "linear", rule = 2)$y
}

#' Daily fAPAR / albedo / rootdepth for one single-landuse class.
#' @return data.table(doy, fapar, albedo, rootdepth)
landuse_daily <- function(tbl, id, k_extinct = K_EXTINCT) {
  row <- tbl[landuse_id == id]
  if (!nrow(row)) stop("landuse_id ", id, " not in table")
  jd <- row$juldays[[1]]
  lai_d <- interp_cyclic(jd, row$lai[[1]])
  vcf_d <- interp_cyclic(jd, row$vcf[[1]])
  data.table(
    doy = DOY,
    fapar = pmin(pmax(vcf_d * (1 - exp(-k_extinct * lai_d)), 0), 1),
    albedo = interp_cyclic(jd, row$albedo[[1]]),
    rootdepth = interp_cyclic(jd, row$rootdepth[[1]])
  )
}

#' Big-leaf daily fAPAR for a MULTILAYER class: effective LAI = sum over its layers,
#' VCF from the uppermost layer. @return data.table(doy, fapar, albedo, rootdepth)
landuse_daily_multilayer <- function(tbl, ml, id) {
  mrow <- ml[ml_id == id]
  if (!nrow(mrow)) stop("ml_id ", id, " not in multilayer table")
  layers <- mrow$layers[[1]]
  k <- if (is.na(mrow$k_extinct)) K_EXTINCT else mrow$k_extinct
  lai_tot <- Reduce(`+`, lapply(layers, function(l) {
    r <- tbl[landuse_id == l]
    interp_cyclic(r$juldays[[1]], r$lai[[1]])
  }))
  top <- tbl[landuse_id == layers[1]]
  vcf_d <- interp_cyclic(top$juldays[[1]], top$vcf[[1]])
  data.table(
    doy = DOY,
    fapar = pmin(pmax(vcf_d * (1 - exp(-k * lai_tot)), 0), 1),
    albedo = interp_cyclic(top$juldays[[1]], top$albedo[[1]]),
    rootdepth = interp_cyclic(top$juldays[[1]], top$rootdepth[[1]])
  )
}

#' Growing-season maximum rooting depth (m) per class -> used to integrate soil AWC.
rootdepth_rep <- function(tbl, id) max(tbl[landuse_id == id]$rootdepth[[1]])

#' Long daily table over all single-landuse classes: the artifact 4-run-rsofun.r joins
#' by (landuse_id, doy) to attach fapar/albedo, and by landuse_id for rooting depth.
build_landuse_daily_table <- function(path = WASIM_CONTROL) {
  tbl <- parse_landuse_table(path)
  rbindlist(lapply(tbl$landuse_id, function(id) {
    cbind(landuse_id = id, name = tbl[landuse_id == id]$name, landuse_daily(tbl, id))
  }))
}

# =============================================================================
# Sanity checks
# =============================================================================
local({
  # pure interpolation: constant scalar, and monotone ramp reproduced at nodes
  stopifnot(all(interp_cyclic(365, 0.15) == 0.15))
  jd <- c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
  ramp <- interp_cyclic(jd, 1:12)
  stopifnot(abs(ramp[15] - 1) < 1e-6, abs(ramp[196] - 7) < 1e-6)

  if (file.exists(WASIM_CONTROL)) {
    tbl <- parse_landuse_table()
    stopifnot(nrow(tbl) == 19, all(c(5, 12, 13) %in% tbl$landuse_id))
    conif <- landuse_daily(tbl, 12) # Nadelwald, LAI up to 10
    grass <- landuse_daily(tbl, 5) # Intensiv-Gruenland, LAI up to 4
    stopifnot(
      all(conif$fapar >= 0 & conif$fapar <= 1),
      max(conif$fapar) > max(grass$fapar), # denser canopy -> higher fAPAR
      rootdepth_rep(tbl, 12) >= rootdepth_rep(tbl, 5) # forest roots deeper
    )
    ml <- parse_multilayer()
    stopifnot(nrow(ml) >= 16, all(lengths(ml$layers) >= 1))
    message("3-landcover-fapar.r: sanity checks passed (parsed ", nrow(tbl),
      " classes, ", nrow(ml), " multilayer combos).")
  } else {
    message("3-landcover-fapar.r: pure checks passed (WASIM control file not on path).")
  }
})
