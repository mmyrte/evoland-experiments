# Spatially aware gap filling for predictor rasters, used by the 02x ingestion steps.
#
# A source rarely covers every cell of the model domain: DHM25 and the CH2025 grids stop at or
# near the border, the soil map has no values on water, rock and sealed ground, a cell centre on
# a border can fall outside every canton polygon. Those cells used to be NA in `pred_data_t` and
# were then patched with a national mean (`029`, retired). A national mean puts lowland climate
# on a glacier and 0 % clay on a lake shore.
#
# `fill_gaps()` instead fills each gap from its own surroundings, at the scale of the gap
# (a "push-pull" or pyramid fill): the raster is repeatedly coarsened by 2 (mean, or mode for
# categorical data) until no gap is left, and each gap is then filled from the next-coarser
# level on the way back up. A one-cell gap takes the mean of its 2x2 neighbourhood; a lake
# takes the smoothed values around its shore. It is exact where the source has data, and needs
# O(log n) passes rather than one per cell of gap width.
#
# What it cannot do is know the physics: filling elevation across a border is harmless, filling
# soil organic carbon over a glacier is a convention. The filled share is reported per
# predictor by `report_fill()` so it stays visible.

fill_gaps <- function(r, categorical = FALSE) {
  if (terra::nlyr(r) > 1L) {
    return(terra::rast(lapply(seq_len(terra::nlyr(r)), function(i) {
      fill_gaps(r[[i]], categorical)
    })))
  }
  n_missing <- terra::global(is.na(r), "sum")[[1L]]
  if (n_missing == 0L || n_missing == terra::ncell(r) || terra::ncell(r) <= 1L) {
    return(r)
  }
  coarse <- terra::aggregate(
    r,
    fact = 2L,
    fun = if (categorical) "modal" else "mean",
    na.rm = TRUE
  )
  coarse <- fill_gaps(coarse, categorical)
  pulled <- terra::resample(coarse, r, method = "near")
  if (!categorical) {
    # bilinear avoids blocky fills; it is NA along the outer edge, where `near` takes over
    pulled <- terra::cover(terra::resample(coarse, r, method = "bilinear"), pulled)
  }
  terra::cover(r, pulled)
}

# The coordinates predictors need values at: every cell carrying land use in the observed
# record. Cells of the rectangular coords_t outside Switzerland are left out, so a filled raster
# does not write values for the whole rectangle.
domain_coords <- function(db) {
  coords <- db$coords_minimal
  domain_ids <- db$get_query(
    "select distinct id_coord from {lulc_read_expr}",
    lulc_read_expr = db$get_read_expr("lulc_data_t"),
    as_atomic = TRUE
  )
  domain <- coords[id_coord %in% domain_ids]
  data.table::setattr(domain, "epsg", attr(coords, "epsg"))
  data.table::setattr(domain, "resolution", attr(coords, "resolution"))
  domain
}

# Fill, extract at the domain, and check that every domain cell got a value. `raw` is the
# unfilled raster, used only to report how much was filled.
extract_filled <- function(raw, domain, categorical = FALSE) {
  filled <- fill_gaps(raw, categorical)
  extracted <- evoland::extract_using_coords_t(filled, domain)
  n_raw <- nrow(evoland::extract_using_coords_t(raw, domain))
  stopifnot(
    "gap filling left domain cells without a value" =
      nrow(extracted) == nrow(domain) * terra::nlyr(raw)
  )
  attr(extracted, "filled_share") <- 1 - n_raw / nrow(extracted)
  extracted
}

report_fill <- function(extracted, name) {
  message(sprintf(
    "%s: %.2f %% of domain cells gap-filled",
    name,
    100 * attr(extracted, "filled_share")
  ))
}
