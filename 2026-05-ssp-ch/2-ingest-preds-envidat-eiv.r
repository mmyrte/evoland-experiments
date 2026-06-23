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
#'
#' All SPEEDMIND_Soil* layers are continuous community-weighted means (CWMs) of the
#' underlying Landolt EIV classes across plant communities, not discrete class indices.
#' SoilR is an exception: it is stored as modelled soil pH (approx. 4.7-7.1) rather
#' than the Landolt R class index (1-5).

library(data.table)
library(evoland)

db <- evoland_db$new(path = "ssp-ch.evolanddb")
coords_minimal <- db$coords_minimal
extent_wide <- db$extent |> terra::extend(1000)

sources_eiv <-
  list(
    list(
      url = "https://www.envidat.ch/dataset/4ab13d14-6f96-41fd-96b0-b3ea45278b3d/resource/81c046c3-8d1d-45bc-a833-7d8240cebd12/download/predictors_description.xlsx",
      md5sum = "9a49a27141863f37a5c39c87509f20c7"
    ),
    list(
      url = "https://www.envidat.ch/dataset/4ab13d14-6f96-41fd-96b0-b3ea45278b3d/resource/e0faab13-0d1b-492a-8539-5370d48b9e35/download/predictors.zip",
      md5sum = "a8e3bd3a7e929e48a73e7df293ea735d"
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
    unit = "pH",
    pretty_name = "Soil pH (EIV-R derived)",
    description = paste0(
      "Modelled soil pH (approx. 4.7-7.1 for Switzerland); derived from Landolt EIV-R ",
      "but stored as pH units, not the 1-5 class index. ",
      "Mapped by Descombes et al. 2020, doi:10.1111/ecog.05117"
    )
  ),
  soil_nutrients = list(
    zip_file = "Predictors/SPEEDMIND_SoilN.tif",
    unit = "Landolt N CWM",
    pretty_name = "Soil nutrients (EIV-N)",
    description = paste0(
      "Community-weighted mean of Landolt EIV-N across plant communities; ",
      "gradient from nutrient-poor (1) to nutrient-rich (5), mainly nitrogen. ",
      "Mapped by Descombes et al. 2020, doi:10.1111/ecog.05117"
    )
  ),
  soil_moisture = list(
    zip_file = "Predictors/SPEEDMIND_SoilF.tif",
    unit = "Landolt F CWM",
    pretty_name = "Soil moisture (EIV-F)",
    description = paste0(
      "Community-weighted mean of Landolt EIV-F across plant communities; ",
      "gradient from very dry soils (1) to plants growing in water (5). ",
      "Mapped by Descombes et al. 2020, doi:10.1111/ecog.05117"
    )
  ),
  soil_moisture_variability = list(
    zip_file = "Predictors/SPEEDMIND_SoilW.tif",
    unit = "Landolt W CWM",
    pretty_name = "Soil moisture variability (EIV-W)",
    description = paste0(
      "Community-weighted mean of Landolt EIV-W across plant communities; ",
      "gradient from low intraannual variability in soil moisture (1) to high (3). ",
      "Mapped by Descombes et al. 2020, doi:10.1111/ecog.05117"
    )
  ),
  soil_aeration = list(
    zip_file = "Predictors/SPEEDMIND_SoilD.tif",
    unit = "Landolt D CWM",
    pretty_name = "Soil aeration (EIV-D)",
    description = paste0(
      "Community-weighted mean of Landolt EIV-D across plant communities; ",
      "gradient from waterlogged/low-aerated soils (1) to soils rich in rocks or sand (5). ",
      "Mapped by Descombes et al. 2020, doi:10.1111/ecog.05117"
    )
  ),
  soil_humus = list(
    zip_file = "Predictors/SPEEDMIND_SoilH.tif",
    unit = "Landolt H CWM",
    pretty_name = "Soil humus (EIV-H)",
    description = paste0(
      "Community-weighted mean of Landolt EIV-H across plant communities; ",
      "gradient from humus-poor soils (1) to humus-rich soils (5). ",
      "Mapped by Descombes et al. 2020, doi:10.1111/ecog.05117"
    )
  ),
  light_100m = list(
    zip_file = "Predictors/SPEEDMIND_SoilL.tif",
    unit = "Landolt L CWM",
    pretty_name = "Light (EIV-L)",
    description = paste0(
      "Community-weighted mean of Landolt EIV-L across plant communities; ",
      "gradient from shaded areas (1) to sunny areas (5). ",
      "Mapped by Descombes et al. 2020, doi:10.1111/ecog.05117"
    )
  ),
  continentality_100m = list(
    zip_file = "Predictors/SPEEDMIND_SoilK.tif",
    unit = "Landolt K CWM",
    pretty_name = "Continentality (EIV-K)",
    description = paste0(
      "Community-weighted mean of Landolt EIV-K across plant communities; ",
      "gradient from atlantic climate (1; high humidity, mild winters) to ",
      "continental climate (5; low humidity, cold winters). ",
      "Mapped by Descombes et al. 2020, doi:10.1111/ecog.05117"
    )
  )
)

for (pred_name in names(eiv_specs)) {
  spec <- eiv_specs[[pred_name]]
  tif_path <- paste0("/vsizip/", zip_path, "/", spec[["zip_file"]])

  r <- terra::rast(tif_path) |>
    terra::project("EPSG:2056", method = "bilinear", res = 100) |>
    terra::crop(extent_wide)
  terra::set.names(r, pred_name)

  pred_data <- extract_using_coords_t(r, coords_minimal)

  db$add_predictor(
    pred_data_raw = pred_data[, .(id_coord, id_period = 0L, value)],
    name = pred_name,
    fill_value = NA,
    unit = spec[["unit"]],
    pretty_name = spec[["pretty_name"]],
    orig_format = "93m Mercator GeoTIFF, bilinear resampled to EPSG:2056 100m",
    description = spec[["description"]],
    sources = sources_eiv[, .(url, md5sum)]
  )
}
