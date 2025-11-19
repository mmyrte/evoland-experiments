#' purpose: ingest local geotiffs from valpar
#' date: 2025-10-31
#' auth: jhartman@ethz.ch
library(data.table)
indicators_1985 <- rowwiseDT(
  name=, path=, # nolint
  #' The first set is inherited from:
  #' Descombes, P., Walthert, L., Baltensweiler, A., Meuli, R. G., Karger, D. N., Ginzler, C.,
  #' Zurell, D., & Zimmermann, N. E. (2020b). Spatial modelling of ecological indicator values
  #' improves predictions of plant distributions in complex landscapes. Ecography, 43(10),
  #' 1448–1463.
  #' [pdf, html] https://doi.org/10.1111/ecog.05117
  #' [XLSX,geotiff] https://doi.org/10.16904/ENVIDAT.153
  #'
  #' The actual operationalisation stems from
  #' https://www.dora.lib4ri.ch/wsl/islandora/object/wsl%3A9966 - would have to get it out of a
  #' actual physical library - gasp
  #' Landolt et al. 2010
  #' Flora indicativa. Ökologische Zeigerwerte und biologische Kennzeichen zur Flora der Schweiz und
  #' der Alpen. Ecological indicators values and biological attributes of the flora of Switzerland
  #' and the Alps
  #' ISBN : 9783258074610
  #' EIV-R Soil pH 1, 2, 3, 4, 5
  #' Gradient from acidic soils (1) to carbonate containing alkaline soils (5)
  "soil_ph", "preds/prepared/layers/biophysical/soil_ph.tif",
  #' EIV-N Soil nutrients 1, 2, 3, 4, 5
  #' Gradient from nutrient-poor soils (1) to nutrient-rich soils (5), mainly nitrogen
  "soil_nutrients", "preds/prepared/layers/biophysical/soil_nutrients.tif",
  #' EIV-F Soil moisture 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5
  #' Gradient from very dry soils (1) to plants growing in water (5)
  "soil_moisture", "preds/prepared/layers/biophysical/soil_moisture.tif",
  #' EIV-W Soil moisture variability 1, 2, 3
  #' Gradient from low intraannual variability in soil moisture (1) to soils with a high intraannual
  #' variability in soil moisture (3)
  "soil_moisture_variability", "preds/prepared/layers/biophysical/soil_moisture_variability.tif",
  #' EIV-D Soil aeration 1, 3, 5
  #' Gradient from waterlogged/low aerated soils (1) to soils rich in rocks or sand with larger
  #' distance to the water table (5)
  "soil_aeration", "preds/prepared/layers/biophysical/soil_aeration.tif",
  #' EIV-H Soil humus 1, 3, 5
  #' Gradient from humus-poor soils (1) to humus-rich soils (5)
  "soil_humus", "preds/prepared/layers/biophysical/soil_humus.tif",
  #' EIV-L Light 1, 2, 3, 4, 5
  #' Gradient from shaded (1) to sunny areas (5)
  "light_100m", "preds/prepared/layers/topographic/light_100m.tif",
  #' EIV-K Continentality 1, 2, 3, 4, 5
  #' Gradient from atlantic climate (1; high mean air humidity, low variations in temperature and
  #' relatively mild winters) to continental climate (5; low mean air humidity, high variations in
  #' temperature and cold winters)
  "continentality_100m", "preds/prepared/layers/climatic/continentality_100m.tif",

  #' See 2-ingest-preds-dem.r for DHM25 version
  #' Derived Originally SwissAlti3D? Came via valpar.ch, and thence via Speedmind?
  "elevation_mean_100m", "preds/prepared/layers/topographic/elevation_mean_100m.tif",
  "aspect_mean_100m", "preds/prepared/layers/topographic/aspect_mean_100m.tif",
  "slope_mean_100m", "preds/prepared/layers/topographic/slope_mean_100m.tif",
  "hillshade_mean_100m", "preds/prepared/layers/topographic/hillshade_mean_100m.tif",

  #' See 2-ingest-preds-sonbase.r
  #' Based on SONBASE, road/rail noise emission estimates.
  #' Considering dropping these, since they are strongly correlated with infrastructure and economic
  #' indicators, and are missing air travel and industrial emissions
  "noise_mean_100m", "preds/prepared/layers/transport/noise_mean_100m.tif",

  #' Apparently based on swissTLM3D - should be recomputed based on that
  "distance_to_roads_mean_100m", "preds/prepared/layers/transport/distance_to_roads_mean_100m.tif",

  #' Seemingly computed on VECTOR25 Hydrographic network GWN07
  #' https://www.geocat.ch/geonetwork/srv/api/records/0351bc2e-3cdc-4e8a-b422-0142e494e7b4
  "distance_to_lakes_mean_100m", "preds/prepared/layers/hydrological/distance_to_lakes_mean_100m.tif",
  "distance_to_rivers_mean_100m", "preds/prepared/layers/hydrological/distance_to_rivers_mean_100m.tif",

  # see 2-ingest-preds-pop.r
  "muni_pop", "preds/prepared/layers/socio_economic/population/muni_pop_1985.tif",

  # see 2-ingest-preds-statent.r
  "avg_chg_fte_sec1", "preds/prepared/socio_economic/employment/avg_chg_fte_1985_1997_sec1.tif",
  "avg_chg_fte_sec2", "preds/prepared/socio_economic/employment/avg_chg_fte_1985_1997_sec2.tif",
  "avg_chg_fte_sec3", "preds/prepared/socio_economic/employment/avg_chg_fte_1985_1997_sec3.tif",

  # derive from chelsa
  "average_avg_ann_temp", "preds/prepared/layers/climatic/average_avg_ann_temp_1985_1997.tif",
  "average_avg_precip", "preds/prepared/layers/climatic/average_avg_precip_1985_1997.tif",
  "average_sum_gdays_0deg", "preds/prepared/layers/climatic/average_sum_gdays_0deg_1985_1997.tif",
  "average_sum_gdays_3deg", "preds/prepared/layers/climatic/average_sum_gdays_3deg_1985_1997.tif",
  "average_sum_gdays_5deg", "preds/prepared/layers/climatic/average_sum_gdays_5deg_1985_1997.tif"
)

# ingest data of unclear provenance
local_pred_specs <- list(
  distance_lakes = list(
    unit = "m",
    pretty_name = "Distance to nearest lake",
    orig_format = "100m raster",
    description = "Somehow derived from https://www.geocat.ch/geonetwork/srv/api/records/0351bc2e-3cdc-4e8a-b422-0142e494e7b4",
    sources = list(
      list(
        url = "file:///Users/jhartman/Documents/evoland-ch-data/preds/prepared/layers/hydrological/distance_to_lakes_mean_100m.tif",
        md5sum = "6243f66ffa8b973ee0da35c7e7adb62d"
      )
    )
  ),
  distance_rivers = list(
    unit = "m",
    pretty_name = "Distance to nearest river",
    orig_format = "100m raster",
    description = "Somehow derived from https://www.geocat.ch/geonetwork/srv/api/records/0351bc2e-3cdc-4e8a-b422-0142e494e7b4",
    sources = list(
      list(
        url = "file:///Users/jhartman/Documents/evoland-ch-data/preds/prepared/layers/hydrological/distance_to_rivers_mean_100m.tif",
        md5sum = "a4e45961eeeb681264199c7b4316cd53"
      )
    )
  ),
  distance_roads = list(
    unit = "m",
    pretty_name = "Distance to nearest road",
    orig_format = "100m raster",
    description = "Somehow derived from https://www.geocat.ch/geonetwork/srv/api/records/0351bc2e-3cdc-4e8a-b422-0142e494e7b4",
    sources = list(
      list(
        url = "file:///Users/jhartman/Documents/evoland-ch-data/preds/prepared/layers/transport/distance_to_roads_mean_100m.tif",
        md5sum = NA_character_
      )
    )
  ),
  elevation = list(
    unit = "masl",
    pretty_name = "Elevation in metres above sealevel",
    orig_format = "100m raster",
    description = "Originally swissAlti3D? Came via valpar.ch, and thence via Speedmind?",
    sources = list(
      list(
        url = "file:///Users/jhartman/Documents/evoland-ch-data/preds/prepared/layers/topographic/elevation_mean_100m.tif",
        md5sum = NA_character_
      )
    )
  ),
  aspect = list(
    unit = "deg",
    pretty_name = "Aspect in degrees, likely using raster::terrain",
    orig_format = "100m raster",
    description = "Originally swissAlti3D? Came via valpar.ch, and thence via Speedmind?",
    sources = list(
      list(
        url = "file:///Users/jhartman/Documents/evoland-ch-data/preds/prepared/layers/topographic/aspect_mean_100m.tif",
        md5sum = NA_character_
      )
    )
  ),
  slope = list(
    unit = "deg",
    pretty_name = "Slope in degrees, like using raster::terrain",
    orig_format = "100m raster",
    description = "Originally swissAlti3D? Came via valpar.ch, and thence via Speedmind?",
    sources = list(
      list(
        url = "file:///Users/jhartman/Documents/evoland-ch-data/preds/prepared/layers/topographic/slope_mean_100m.tif",
        md5sum = NA_character_
      )
    )
  ),
  hillshade = list(
    unit = "unitless float",
    pretty_name = "Hillshade in 0-256, possibly from raster::hillShade. Direction from North-West? Azimuth?",
    orig_format = "100m raster",
    description = "Originally swissAlti3D? Came via valpar.ch, and thence via Speedmind?",
    sources = list(
      list(
        url = "file:///Users/jhartman/Documents/evoland-ch-data/preds/prepared/layers/topographic/hillshade_mean_100m.tif",
        md5sum = NA_character_
      )
    )
  )
)

devtools::load_all("~/github-repos/evoland-plus/")

local_pred_sources <-
  local_pred_specs |>
  pluck_wildcard(NA, "sources") |>
  purrr::map(data.table::rbindlist) |>
  data.table::rbindlist() |>
  download_and_verify()

db <- evoland_db$new(path = "fullch.evolanddb")
coords_minimal <- db$coords_minimal
periods <- db$periods_t
extent_wide <- db$extent |> terra::extend(1000)

pred_data_long <-
  purrr::map2(
    local_pred_sources$local_path,
    names(local_pred_specs),
    \(x, name) {
      r <- terra::rast(x) |>
        terra::crop(extent_wide)
      terra::set.names(r, name)
      extract_using_coords_t(r, db$coords_minimal)
    }
  ) |>
  data.table::rbindlist()
pred_data_long[, id_period := 0L]

for (pred_name in names(local_pred_specs)) {
  db$add_predictor(
    pred_spec = local_pred_specs[pred_name],
    pred_data = pred_data_long[
      layer == pred_name,
      .(id_coord, id_period, value)
    ],
    pred_type = "float"
  )
}
