#' ---
#' title: "Synthetic benchmark data"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' The PIE maps carry little signal: about 4 % of cells change, much of it scattered, and every
#' pairing of the benchmark lands within a factor of 2.5 of random allocation. With
#' `PIE_DATASET=synthetic`, the whole benchmark runs instead on a synthetic landscape with a
#' known change process, so the comparison of estimators and allocators is not drowned out by
#' the lack of anything to learn.
#'
#' The data have the format of `000-pie-data.r`, so every step runs unchanged:
#'
#' - `lu_{1985,1991,1999}.tif`: 1 = forest, 2 = built, 3 = other (the years only label three
#'   periods: an initial map and two steps of the process);
#' - `ef_001.tif` accessibility, `ef_002.tif` site quality, `ef_003.tif` distance to built land
#'   in 1985;
#' - 100 m cells in EPSG:26986, `n_row` × `n_col` cells, all inside the study area.
#'
#' The process is the one of Fig. 2 (`2026-09-paper-figures/000-synthetic-process.r`), with
#' its four logistic transitions on accessibility, site quality and the share of each class in
#' the 5 × 5 neighbourhood, but on PIE's three classes and without the fixed lake, which
#' CLUMondo cannot hold:
#'
#' | transition | logit of its probability |
#' | --- | --- |
#' | other → built | −5.5 + 7 · share built + 3 · accessibility |
#' | forest → built | −7 + 6 · share built + 3 · accessibility |
#' | forest → other | −5 + 4 · (1 − site quality) + 3 · share other |
#' | other → forest | −6 + 4 · site quality · (1 − accessibility) + 3 · share forest |
#'
#' Every estimator sees the two drivers and the distance to built land; evoland's learners also
#' get their neighbourhood counts, Dinamica's Weights of Evidence its distance-to-class maps.

#| label: setup
#| output: false
library(data.table)
library(terra)

source("2026-09-model-comparison/common.r")
only_for_dataset("synthetic")
source("2026-09-paper-figures/000-synthetic-process.r")
out_dir <- data_dir
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

n_row <- 300L
n_col <- 340L
set.seed(2026L)

#| label: landscape
template <- terra::rast(
  nrows = n_row,
  ncols = n_col,
  xmin = 200000,
  xmax = 200000 + 100 * n_col,
  ymin = 900000,
  ymax = 900000 + 100 * n_row,
  crs = "EPSG:26986"
)
drivers <- make_drivers(template)

# the initial map of Fig. 2 has classes forest 1, arable 2, urban 3, lake 4; here the lake
# becomes arable ("other") and the codes follow PIE: forest 1, built 2, other 3
initial <- make_landscape(template, drivers)
to_pie <- function(map) terra::classify(map, rbind(c(1, 1), c(2, 3), c(3, 2), c(4, 3)))
from_pie <- function(map) terra::classify(map, rbind(c(1, 1), c(2, 3), c(3, 2)))

# two steps of the process on the PIE codes, through the Fig. 2 codes it is written in
lu_1985 <- to_pie(initial)
lu_1991 <- to_pie(step_process(from_pie(lu_1985), drivers))
lu_1999 <- to_pie(step_process(from_pie(lu_1991), drivers))
lu <- c(lu_1985, lu_1991, lu_1999)
years <- c(1985, 1991, 1999)
names(lu) <- paste0("lu_", years)

dist_built <- terra::distance(terra::ifel(lu_1985 == 2, 1, NA))
ef <- c(drivers$accessibility, drivers$site_quality, dist_built)
names(ef) <- paste0("ef_00", 1:3)

#| label: write
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
#' As in `000-pie-data.r`: the observed 1991 → 1999 transition counts and the class totals.

#| label: demand
dir.create(outputs_dir, showWarnings = FALSE, recursive = TRUE)
lu_values <- as.data.table(terra::values(lu))
setnames(lu_values, as.character(years))
fwrite(
  lu_values[
    `1991` != `1999`,
    .(count = .N),
    by = .(id_lulc_anterior = `1991`, id_lulc_posterior = `1999`)
  ][order(id_lulc_anterior, id_lulc_posterior)],
  file.path(outputs_dir, "demand_transitions_1991_1999.csv")
)
fwrite(
  rbindlist(lapply(as.character(years), function(y) {
    lu_values[, .N, by = .(id_lulc = get(y))][, year := as.integer(y)]
  }))[order(year, id_lulc), .(year, id_lulc, N)],
  file.path(outputs_dir, "demand_class_totals.csv")
)

#' # Observed change

#| label: crosstab
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
#' Share of cells that change, per period:
c(
  calibration = mean(values(lu[[1]]) != values(lu[[2]])),
  validation = mean(values(lu[[2]]) != values(lu[[3]]))
)

#| label: plot
#| fig-asp: 0.35
plot(lu, nc = 3, col = lulc_colours)
plot(ef, nc = 3)
