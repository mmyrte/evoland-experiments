devtools::load_all("~/github-repos/evoland-plus/")

db <- evoland_db$new(path = "smaller.evolanddb")
coords_minimal <- db$coords_minimal

sonbase_spec <- list(
  noise = list(
    unit = "dBa",
    pretty_name = "Maximum noise exposure",
    orig_format = "10m*10m raster",
    description = "The maximum across all sonBASE noise exposure categories, i.e. daytime & nighttime road & rail exposure",
    sources = list(
      list(
        url = "https://data.geo.admin.ch/ch.bafu.laerm-strassenlaerm_tag/laerm-strassenlaerm_tag/laerm-strassenlaerm_tag_2056.tif",
        md5sum = "09791808dbf12fcde82182e70f2ebdfb"
      ),
      list(
        url = "https://data.geo.admin.ch/ch.bafu.laerm-strassenlaerm_nacht/laerm-strassenlaerm_nacht/laerm-strassenlaerm_nacht_2056.tif",
        md5sum = "6e79dc1d353751084e21dc6b61778b99"
      ),
      list(
        url = "https://data.geo.admin.ch/ch.bafu.laerm-bahnlaerm_nacht/laerm-bahnlaerm_nacht/laerm-bahnlaerm_nacht_2056.tif",
        md5sum = "161df62f9a2a29c9120380f965aa19ba"
      ),
      list(
        url = "https://data.geo.admin.ch/ch.bafu.laerm-bahnlaerm_tag/laerm-bahnlaerm_tag/laerm-bahnlaerm_tag_2056.tif",
        md5sum = "6016b9ca4c974cb982fbe18112f201fe"
      )
    )
  )
)

sonbase_sources <-
  sonbase_spec$noise$sources |>
  data.table::rbindlist() |>
  download_and_verify()

# data is at 10m
# extent plus 1km, given metres for unit; enough for all resampling strategies
minimal_extent <-
  purrr::map(
    sonbase_sources$local_path,
    \(x) terra::rast(x) |> terra::ext()
  ) |>
  c(terra::extend(db$extent, 100)) |> # if the extent of the coords is less, use that
  purrr::map(as.list) |>
  dplyr::bind_rows() |>
  dplyr::summarise(
    xmin = max(xmin),
    xmax = min(xmax),
    ymin = max(ymin),
    ymax = min(ymax)
  ) |>
  unlist() |>
  terra::ext()

sonbase_max <-
  purrr::map(
    sonbase_sources$local_path,
    \(x) terra::rast(x) |> terra::crop(minimal_extent)
  ) |>
  terra::rast() |>
  max(na.rm = TRUE) |>
  terra::resample(10, method = "bilinear") |> # downsample factor 10: to hectare
  extract_using_coords_t(coords_minimal)

db$add_predictor(
  pred_spec = sonbase_spec,
  pred_data = sonbase_max[,
    .(id_coord, id_period = 0L, value)
  ],
  pred_type = "float"
)
