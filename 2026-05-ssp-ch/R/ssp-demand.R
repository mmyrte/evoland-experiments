# SSP-CH land-use demand: per-scenario class-area targets.
#
# Sourced by 07-transition-rates-1-solver.qmd and 07-transition-rates-2-legacy.qmd.
# `_quarto.yml` sets execute-dir: project, so both source it by the same relative path.
#
# Source: NCCS-SSP-scenarios/Tools/NCCS_simulation_LULC_areas.xlsx, sheet "shhet1".
# Embedded rather than read from that repo: it is a read-only reference checkout, not a
# declared dependency, and a relative path into it makes this pipeline unrunnable for anyone
# who cloned only this repo. 50 rows, and it does not change.
#
# init_area and final_2060 are cells on the ORIGINAL 4,129,078-cell grid. They are only ever
# used as shares -- see ssp_demand_targets() -- because that grid is not ours.

ssp_demand <- data.table::rowwiseDT(
  ssp   =, lulc_name      =, init_area =, final_2060        =, shape             =,
  "SSP0", "arable",             388383,     547256.829397, "constant change",
  "SSP0", "perm_crops",          47968,      54440.642884, "instant growth",
  "SSP0", "grassland",          539423,     477405.277463, "instant decline",
  "SSP0", "shrubland",          276534,     262094.913500, "constant change",
  "SSP0", "static",             757680,     768414.626253, "constant change",
  "SSP0", "closed_forest",     1112032,    1069149.876130, "instant decline",
  "SSP0", "open_forest",        201359,     201660.614662, "delayed decline",
  "SSP0", "urban",              226030,     215221.265030, "constant change",
  "SSP0", "alp_past",           476677,     472617.178681, "constant change",
  "SSP0", "glacier",            102992,      60816.776000, "constant change",
  "SSP1", "arable",             388383,     287152.337251, "constant change",
  "SSP1", "perm_crops",          47968,      33152.064878, "instant decline",
  "SSP1", "grassland",          539423,     598581.335861, "constant change",
  "SSP1", "shrubland",          276534,     226600.809006, "constant change",
  "SSP1", "static",             757680,     869979.246531, "constant change",
  "SSP1", "closed_forest",     1112032,    1160184.140409, "instant growth",
  "SSP1", "open_forest",        201359,     210078.069486, "constant change",
  "SSP1", "urban",              226030,     250157.528679, "instant growth",
  "SSP1", "alp_past",           476677,     432375.691898, "constant change",
  "SSP1", "glacier",            102992,      60816.776000, "constant change",
  "SSP3", "arable",             388383,     387310.843184, "delayed growth",
  "SSP3", "perm_crops",          47968,      49903.953972, "constant change",
  "SSP3", "grassland",          539423,     475375.306447, "constant change",
  "SSP3", "shrubland",          276534,     299976.696228, "constant change",
  "SSP3", "static",             757680,     734011.777248, "constant change",
  "SSP3", "closed_forest",     1112032,    1243878.048705, "instant growth",
  "SSP3", "open_forest",        201359,     210843.291999, "delayed growth",
  "SSP3", "urban",              226030,     223695.617728, "constant change",
  "SSP3", "alp_past",           476677,     443265.688489, "instant decline",
  "SSP3", "glacier",            102992,      60816.776000, "constant change",
  "SSP4", "arable",             388383,     370417.902296, "constant change",
  "SSP4", "perm_crops",          47968,      21781.625609, "instant decline",
  "SSP4", "grassland",          539423,     489773.507063, "delayed decline",
  "SSP4", "shrubland",          276534,     365349.034012, "delayed growth",
  "SSP4", "static",             757680,     764899.163336, "delayed growth",
  "SSP4", "closed_forest",     1112032,    1226883.113600, "instant growth",
  "SSP4", "open_forest",        201359,     257254.641767, "constant change",
  "SSP4", "urban",              226030,     247183.434779, "instant growth",
  "SSP4", "alp_past",           476677,     324718.801539, "constant change",
  "SSP4", "glacier",            102992,      60816.776000, "constant change",
  "SSP5", "arable",             388383,     413875.731983, "delayed growth",
  "SSP5", "perm_crops",          47968,      38462.336089, "constant change",
  "SSP5", "grassland",          539423,     684718.364390, "instant growth",
  "SSP5", "shrubland",          276534,     231447.842873, "constant change",
  "SSP5", "static",             757680,     886023.625341, "constant change",
  "SSP5", "closed_forest",     1112032,     926495.571348, "constant change",
  "SSP5", "open_forest",        201359,     198978.907634, "constant change",
  "SSP5", "urban",              226030,     282183.101337, "instant growth",
  "SSP5", "alp_past",           476677,     406075.743005, "constant change",
  "SSP5", "glacier",            102992,      60816.776000, "constant change"
)

# The grid the demand was elicited on, for the guard in ssp_demand_targets().
SSP_DEMAND_SOURCE_TOTAL <- 4129078L

#' Per-scenario class-area targets, in cells.
#'
#' Returned as the absolute values from the source workbook, deliberately unscaled. The original
#' was elicited on a 4,129,078-cell grid and this pipeline's is 4,129,079 -- a difference of one
#' cell -- so normalising to shares and multiplying back out would be arithmetic that changes
#' nothing while obscuring where the numbers came from.
#'
#' `observed_total` is not used to rescale; it is only checked, so that a grid which *is*
#' materially different fails loudly instead of silently inheriting targets that do not fit it.
#' See the note in 07-transition-rates-1-solver.qmd if you hit that error.
#'
#' Also normalises `shape`, which needs it -- one row (SSP0 / Open_Forest) reads
#' "delayed decline" in lowercase where every other row is title case.
#'
#' @param lulc_meta A [lulc_meta_t]; supplies the id_lulc crosswalk.
#' @param observed_total Optional integer, our own cell count at the anchor period. When given,
#' it is checked against the grid the demand was elicited on.
#' @param tolerance Fractional deviation tolerated before erroring. The default of 0.001 allows
#' ~4,100 cells, far above the 1-cell difference and far below a genuine change of resolution.
#' @return data.table(ssp, id_lulc, area, shape)
ssp_demand_targets <- function(lulc_meta, observed_total = NULL, tolerance = 0.001) {
  stopifnot(
    "demand class names do not match lulc_meta_t" =
      setequal(unique(ssp_demand$lulc_name), lulc_meta$name)
  )

  if (!is.null(observed_total)) {
    deviation <- abs(observed_total - SSP_DEMAND_SOURCE_TOTAL) / SSP_DEMAND_SOURCE_TOTAL
    if (deviation > tolerance) {
      stop(
        "This grid has ", observed_total, " cells; the demand was elicited on ",
        SSP_DEMAND_SOURCE_TOTAL, " (", round(100 * deviation, 2), "% apart).\n",
        "The targets are absolute cell counts and are used unscaled, which is only valid ",
        "while the two grids match.\n",
        "For a coarser or otherwise different grid, normalise on the source grid and ",
        "rehydrate on yours:\n",
        "  share <- final_2060 / sum(final_2060)   # per scenario; sums to 1 exactly\n",
        "  target <- share * observed_total\n",
        "See the note in 07-transition-rates-1-solver.qmd.",
        call. = FALSE
      )
    }
  }

  d <- data.table::copy(ssp_demand)
  d[lulc_meta[, .(lulc_name = name, id_lulc)], on = "lulc_name", id_lulc := i.id_lulc]

  d[, .(ssp, id_lulc, area = final_2060, shape = tolower(trimws(shape)))]
}

#' Our observed initial state at the anchor period, in cells.
#' @param db An [evoland_db] instance.
#' @param anchor_period Integer period ID to count from.
#' @return data.table(id_lulc, area)
ssp_demand_init_area <- function(db, anchor_period) {
  db$get_query(glue::glue(
    "select id_lulc, count(*)::int as area
     from {db$get_read_expr('lulc_data_t')}
     where id_period = {anchor_period}
     group by id_lulc"
  )) |> data.table::as.data.table()
}

#' The original study's own initial shares, for comparison against ours.
#'
#' A large discrepancy for a class means our NOAS04 re-derivation does not mean quite the same
#' thing as the original aggregation, and the demand is being rehydrated onto a different
#' concept. Worth looking at before trusting any target.
#'
#' @param lulc_meta A [lulc_meta_t]; supplies the id_lulc crosswalk.
#' @return data.table(id_lulc, their_share)
ssp_demand_init_shares <- function(lulc_meta) {
  d <- unique(ssp_demand[ssp == ssp_demand$ssp[1L], .(lulc_name, init_area)])
  d[lulc_meta[, .(lulc_name = name, id_lulc)], on = "lulc_name", id_lulc := i.id_lulc]
  d[, .(id_lulc, their_share = init_area / sum(init_area))]
}
