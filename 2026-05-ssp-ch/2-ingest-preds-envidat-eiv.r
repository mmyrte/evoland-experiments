#' Purpose: ingest Descombes et al. 2020 ecological indicator values (EIVs)
#' date: 2026-06-23
#' auth: jhartman@ethz.ch
#'
#' Descombes, P., Walthert, L., Baltensweiler, A., Meuli, R. G., Karger, D. N.,
#' Ginzler, C., Zurell, D., & Zimmermann, N. E. (2020). Spatial modelling of
#' ecological indicator values improves predictions of plant distributions in
#' complex landscapes. Ecography, 43(10), 1448-1463.
#' https://doi.org/10.1111/ecog.05117
#' Data: https://doi.org/10.16904/ENVIDAT.153
#'
#' EIV operationalisation:
#' Landolt, E. et al. (2010). Flora indicativa. ISBN 9783258074610

library(data.table)
library(evoland)

db <- evoland_db$new(path = "ssp-ch.evolanddb")
coords_minimal <- db$coords_minimal
extent_wide <- db$extent |> terra::extend(1000)

sources_eiv <-
  list(
    list(
      url = "https://www.envidat.ch/dataset/4ab13d14-6f96-41fd-96b0-b3ea45278b3d/resource/81c046c3-8d1d-45bc-a833-7d8240cebd12/download/predictors_description.xlsx",
      md5sum = NA_character_
    ),
    list(
      url = "https://www.envidat.ch/dataset/4ab13d14-6f96-41fd-96b0-b3ea45278b3d/resource/e0faab13-0d1b-492a-8539-5370d48b9e35/download/predictors.zip",
      md5sum = NA_character_
    )
  ) |>
  data.table::rbindlist() |>
  download_and_verify()

zip_path <- sources_eiv[["local_path"]][[2L]]

# EIV codes follow Landolt et al. 2010:
#   R=soil pH, N=nutrients, F=moisture, W=moisture variability,
#   D=aeration, H=humus, L=light, K=continentality
# Filenames confirmed from unzip(zip_path, list = TRUE).
eiv_specs <- list(
  soil_ph = list(
    zip_file = "Predictors/SPEEDMIND_SoilR.tif",
    levels = c(1, 2, 3, 4, 5),
    unit = "1-5",
    pretty_name = "Soil pH (EIV-R)",
    description = paste0(
      "Gradient from acidic soils (1) to carbonate-containing alkaline soils (5). ",
      "Landolt EIV-R. Mapped by Descombes et al. 2020, doi:10.1111/ecog.05117"
    )
  ),
  soil_nutrients = list(
    zip_file = "Predictors/SPEEDMIND_SoilN.tif",
    levels = c(1, 2, 3, 4, 5),
    unit = "1-5",
    pretty_name = "Soil nutrients (EIV-N)",
    description = paste0(
      "Gradient from nutrient-poor soils (1) to nutrient-rich soils (5), mainly nitrogen. ",
      "Landolt EIV-N. Mapped by Descombes et al. 2020, doi:10.1111/ecog.05117"
    )
  ),
  soil_moisture = list(
    zip_file = "Predictors/SPEEDMIND_SoilF.tif",
    levels = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
    unit = "1-5",
    pretty_name = "Soil moisture (EIV-F)",
    description = paste0(
      "Gradient from very dry soils (1) to plants growing in water (5). ",
      "Landolt EIV-F. Mapped by Descombes et al. 2020, doi:10.1111/ecog.05117"
    )
  ),
  soil_moisture_variability = list(
    zip_file = "Predictors/SPEEDMIND_SoilW.tif",
    levels = c(1, 2, 3),
    unit = "1-3",
    pretty_name = "Soil moisture variability (EIV-W)",
    description = paste0(
      "Gradient from low intraannual variability in soil moisture (1) ",
      "to high intraannual variability (3). ",
      "Landolt EIV-W. Mapped by Descombes et al. 2020, doi:10.1111/ecog.05117"
    )
  ),
  soil_aeration = list(
    zip_file = "Predictors/SPEEDMIND_SoilD.tif",
    levels = c(1, 3, 5),
    unit = "1/3/5",
    pretty_name = "Soil aeration (EIV-D)",
    description = paste0(
      "Gradient from waterlogged/low-aerated soils (1) to soils rich in rocks or sand ",
      "with larger distance to the water table (5). ",
      "Landolt EIV-D. Mapped by Descombes et al. 2020, doi:10.1111/ecog.05117"
    )
  ),
  soil_humus = list(
    zip_file = "Predictors/SPEEDMIND_SoilH.tif",
    levels = c(1, 3, 5),
    unit = "1/3/5",
    pretty_name = "Soil humus (EIV-H)",
    description = paste0(
      "Gradient from humus-poor soils (1) to humus-rich soils (5). ",
      "Landolt EIV-H. Mapped by Descombes et al. 2020, doi:10.1111/ecog.05117"
    )
  ),
  light_100m = list(
    zip_file = "Predictors/SPEEDMIND_SoilL.tif",
    levels = c(1, 2, 3, 4, 5),
    unit = "1-5",
    pretty_name = "Light (EIV-L)",
    description = paste0(
      "Gradient from shaded areas (1) to sunny areas (5). ",
      "Landolt EIV-L. Mapped by Descombes et al. 2020, doi:10.1111/ecog.05117"
    )
  ),
  continentality_100m = list(
    zip_file = "Predictors/SPEEDMIND_SoilK.tif",
    levels = c(1, 2, 3, 4, 5),
    unit = "1-5",
    pretty_name = "Continentality (EIV-K)",
    description = paste0(
      "Gradient from atlantic climate (1; high mean air humidity, low temperature variation, ",
      "mild winters) to continental climate (5; low humidity, high variation, cold winters). ",
      "Landolt EIV-K. Mapped by Descombes et al. 2020, doi:10.1111/ecog.05117"
    )
  )
)

snap_to_ordered_factor <- function(x, levels) {
  idx <- vapply(
    x,
    \(v) if (is.na(v)) NA_integer_ else which.min(abs(levels - v)),
    integer(1L)
  )
  factor(levels[idx], levels = levels, ordered = TRUE)
}

for (pred_name in names(eiv_specs)) {
  spec <- eiv_specs[[pred_name]]
  tif_path <- paste0("/vsizip/", zip_path, "/", spec[["zip_file"]])

  r <- terra::rast(tif_path) |>
    terra::project("EPSG:2056", method = "near", res = 100) |>
    terra::crop(extent_wide)
  terra::set.names(r, pred_name)

  pred_data <- extract_using_coords_t(r, coords_minimal)
  pred_data[, value := snap_to_ordered_factor(value, spec[["levels"]])]

  db$add_predictor(
    pred_data_raw = pred_data[, .(id_coord, id_period = 0L, value)],
    name = pred_name,
    fill_value = NA,
    unit = spec[["unit"]],
    pretty_name = spec[["pretty_name"]],
    orig_format = "93m Mercator GeoTIFF, nearest-neighbour resampled to EPSG:2056 100m",
    description = spec[["description"]],
    sources = sources_eiv[, .(url, md5sum)]
  )
}
