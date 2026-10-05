#' ---
#' title: "Plum Island Ecosystems benchmark data"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' The Plum Island Ecosystems (PIE) case ships with
#' [lulcc](https://github.com/simonmoulds/lulcc) (Moulds et al. 2015, GMD): three observed
#' land use maps (1985, 1991, 1999; forest, built, other) and three explanatory factors on a
#' 434 × 497 grid of ~100 m cells, 113 563 of them inside the study area. Small enough that
#' every tool runs in minutes, and published with an open tool, which makes it a fair common
#' ground.
#'
#' This step writes the data once as GeoTIFFs, which every comparator then reads, so no
#' tool sees anything the others don't.
#'
#' - `lu_{1985,1991,1999}.tif`: land use, 1 = forest, 2 = built, 3 = other, NA outside.
#' - `ef_001.tif` elevation, `ef_002.tif` slope, `ef_003.tif` distance to built land in 1985
#'   (Moulds et al. 2015, Sect. 3).
#'
#' The lulcc rasters have a slightly anisotropic resolution (99.92 × 99.95 m). evoland's
#' coordinate table assumes square cells, so the grid is relabelled to exactly 100 m by
#' moving the upper right corner (by 39 m and 20 m). No values are resampled: cell (i, j) is
#' the same in every file and every tool. The CRS is NAD83 / Massachusetts Mainland, which the
#' lulcc rasters carry as a PROJ string; it is set as EPSG:26986, which evoland needs.

#| label: setup
#| output: false
library(terra)
data("pie", package = "lulcc")

source("2026-09-model-comparison/common.r")
out_dir <- data_dir
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

#| label: write
to_terra_100m <- function(r) {
  x <- terra::rast(r)
  e <- terra::ext(x)
  terra::ext(x) <- terra::ext(
    e$xmin,
    e$xmin + 100 * terra::ncol(x),
    e$ymin,
    e$ymin + 100 * terra::nrow(x)
  )
  terra::crs(x) <- "EPSG:26986"
  x
}

years <- c(1985, 1991, 1999)
lu <- terra::rast(lapply(paste0("lu_pie_", years), \(n) to_terra_100m(pie[[n]])))
names(lu) <- paste0("lu_", years)
ef <- terra::rast(lapply(paste0("ef_00", 1:3), \(n) to_terra_100m(pie[[n]])))
names(ef) <- paste0("ef_00", 1:3)

# 080-scaling: tile the grid k x k, mirroring every other copy so that the edges join up
tile_mirrored <- function(x, k) {
  if (k == 1L) {
    return(x)
  }
  row_of_tiles <- do.call(terra::merge, lapply(seq_len(k), function(j) {
    tile <- if (j %% 2 == 0) terra::flip(x, "horizontal") else x
    terra::shift(tile, dx = (j - 1) * terra::xmax(x) - (j - 1) * terra::xmin(x))
  }))
  do.call(terra::merge, lapply(seq_len(k), function(i) {
    tile <- if (i %% 2 == 0) terra::flip(row_of_tiles, "vertical") else row_of_tiles
    terra::shift(tile, dy = (i - 1) * (terra::ymax(x) - terra::ymin(x)))
  }))
}
lu <- tile_mirrored(lu, pie_scale)
ef <- tile_mirrored(ef, pie_scale)
names(lu) <- paste0("lu_", years)
names(ef) <- paste0("ef_00", 1:3)

# the study area: cells with land use in all years; predictors are masked to it too
study_area <- !is.na(sum(lu))
ef <- terra::mask(ef, study_area, maskvalues = FALSE)

for (i in seq_along(years)) {
  terra::writeRaster(
    lu[[i]],
    file.path(out_dir, paste0("lu_", years[i], ".tif")),
    datatype = "INT1U",
    NAflag = 255,
    overwrite = TRUE
  )
}
for (n in names(ef)) {
  terra::writeRaster(
    ef[[n]],
    file.path(out_dir, paste0(n, ".tif")),
    datatype = "FLT4S",
    NAflag = -9999,
    overwrite = TRUE
  )
}

#' # Demand handed to every tool
#'
#' Observed 1991 → 1999 transition counts and the class totals per year, written here (not by
#' an evoland step) so that every tool can run on its own.

#| label: demand
dir.create(outputs_dir, showWarnings = FALSE, recursive = TRUE)
lu_values <- data.table::as.data.table(terra::values(lu))
data.table::setnames(lu_values, as.character(years))
lu_values <- lu_values[!is.na(`1985`) & !is.na(`1991`) & !is.na(`1999`)]
data.table::fwrite(
  lu_values[`1991` != `1999`, .(count = .N), by = .(id_lulc_anterior = `1991`, id_lulc_posterior = `1999`)][
    order(id_lulc_anterior, id_lulc_posterior)
  ],
  file.path(outputs_dir, "demand_transitions_1991_1999.csv")
)
data.table::fwrite(
  data.table::rbindlist(lapply(as.character(years), function(y) {
    lu_values[, .N, by = .(id_lulc = get(y))][, year := as.integer(y)]
  }))[order(year, id_lulc), .(year, id_lulc, N)],
  file.path(outputs_dir, "demand_class_totals.csv")
)

#' # Observed change

#| label: crosstab
lu
ef
crosstab_period <- function(a, b) {
  table(
    anterior = terra::values(lu[[a]], mat = FALSE),
    posterior = terra::values(lu[[b]], mat = FALSE)
  )
}
#' Calibration period, 1985 → 1991:
crosstab_period(1, 2)
#' Validation period, 1991 → 1999:
crosstab_period(2, 3)

#| label: plot
#| fig-asp: 0.35
plot(
  lu,
  nc = 3,
  col = data.frame(value = 1:3, color = c("#91B690", "#5D5D8C", "#EB9486"))
)
plot(ef, nc = 3)
