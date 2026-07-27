library(evoland)
library(terra)
library(data.table)
db <- evoland_db$new(path = "ssp-ch.evolanddb")
coords_minimal <- db$coords_minimal

# check for pr_DJF, pr_JJA, pr_MAM, pr_SON
db$pred_meta_t
dat <- db$fetch(
  table_name = "pred_data_t",
  cols = c("id_pred", "id_coord", "value"), # assuming that id_run = 0 and id_period = 0
  where = "id_pred in (11, 12, 13, 14)"
)

precip_rast <- tabular_to_raster(
  dat,
  coords_minimal,
  value_col = "value"
)

precip_rast |>
  terra::writeRaster("precip_check.tif")
