devtools::load_all("~/github-repos/evoland-plus")

# TODO set bioregions
# https://data.geo.admin.ch/ch.bafu.biogeographische_regionen/biogeographische_regionen/biogeographische_regionen_2056.shp.zip
# TODO introduce new land use class "deglaciated area" based on glacier inventory
# this is also linked to the inclusion threshold, which might not be compatible with these very
# small areas of interest

db <- evoland_db$new(path = "fullch.evolanddb")
lulc_files <-
  data.frame(
    url = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/32376216/appendix",
    md5sum = "c32937eb4a11fc9c5c58c66e9830360a"
  ) |>
  download_and_verify(target_dir = getOption("evoland.cachedir"))

db$commit(
  data.frame(
    key = c("lulc_data_url", "lulc_data_md5sum", "lulc_data_provider"),
    value = c(lulc_files$url, lulc_files$md5sum, "BFS Arealstatistik")
  ),
  table_name = "reporting_t",
  mode = "append"
)

zippath <- file.path(
  getOption("evoland.cachedir"),
  lulc_files$md5sum,
  lulc_files$local_filename
)

# find singular csv
csv_file <-
  unzip(zippath, list = TRUE) |>
  purrr::pluck("Name") |>
  stringi::stri_subset_fixed(".csv")

csv_con <- unz(zippath, csv_file, open = "r")
arealstat_dt <-
  readLines(csv_con) |>
  data.table::fread(
    text = _,
    # selecting only years 1985-2018 for now; Arealstatistik 2025 is not yet finished
    select = c(
      "E_COORD",
      "N_COORD",
      "AS85_72",
      "AS97_72",
      "AS09_72",
      "AS18_72"
      # "AS25_72"
    )
  )
close(csv_con)

db$lulc_meta_t <- create_lulc_meta_t(
  list(
    closed_forest = list(
      pretty_name = "Dense Forest",
      description = "Normal forest; Forest strips; Afforestations; Felling areas; Brush forest",
      src_classes = c(50:53, 57L)
    ),
    arable = list(
      pretty_name = "Arable Land",
      src_classes = 41L
    ),
    urban = list(
      pretty_name = "Urban areas",
      description = "Industrial and commercial buildings; Surroundings of industrial and commercial buildings; One- and two-family houses; Surroundings of one- and two-family houses; Terraced houses; Surroundings of terraced houses; Blocks of flats; Surroundings of blocks of flats; Public buildings; Surroundings of public buildings; Agricultural buildings; Surroundings of agricultural buildings; Unspecified buildings; Surroundings of unspecified buildings; Parking areas; Construction sites; Unexploited urban areas; Public parks; Sports facilities; Golf courses; Camping areas; Garden allotments; Cemeteries",
      src_classes = c(1:14, 19L, 29:36)
    ),
    static = list(
      pretty_name = "Static / immutable classes",
      description = "Motorways; Green motorway environs; Roads and paths; Green road environs;  Sealed railway areas; Green railway environs;  Airports; Airfields, green airport environs;  Energy supply plants; Waste water treatment plants; Other supply or waste treatment plants; Dumps; Quarries, mines;  Lakes; Rivers; Flood protection structures; Avalanche and rockfall barriers;  Wetlands; Alpine sports facilities; Rocks; Screes, sand; Landscape interventions",
      src_classes = c(15:18, 20:28, 61:63, 66:71)
    )
  )
)

# lulc_meta_long_v provides an "unrolled" aka "unnested longer" form
# each src_class is related to one id_lulc
lulc_meta_long_dt <- db$lulc_meta_long_v[, .(id_lulc, src_class)]

# longer form: arealstatistik AS replaced by id_lulc
# data.table has form that can be coerced to multilayer terra::rast using type = "xylz"
lulc_dt <-
  data.table::melt(
    # pivot longer with year from regex and coords as ID columns
    arealstat_dt,
    id.vars = c("E_COORD", "N_COORD"),
    value.name = "src_class",
    measure.vars = data.table::measure(
      year = as.integer, # match group
      pattern = "AS([0-9]{2})_72"
    )
  )[,
    year := ifelse(year > 84L, year + 1900L, year + 2000L) # two-digit year to four digit
  ][
    lulc_meta_long_dt,
    .(
      x = E_COORD,
      y = N_COORD,
      year,
      id_lulc
    ),
    on = "src_class",
    nomatch = NULL
  ]

# setting a key on a data.table pre-sorts it
data.table::setkey(lulc_dt, year, x, y)

r <-
  terra::rast(
    lulc_dt,
    type = "xylz",
    crs = "EPSG:2056"
  ) |>
  terra::as.int() # necessary because terra::rast casts to numeric?

minimal_coords_t <- db$coords_minimal
id_coord_yr_lulc_dt <-
  terra::extract(
    x = r,
    y = as.matrix(minimal_coords_t[, .(lon, lat)]),
    method = "simple"
  ) |>
  data.table::as.data.table() |>
  cbind(id_coord = minimal_coords_t[["id_coord"]]) |>
  data.table::melt(
    id.vars = "id_coord",
    measure.vars = data.table::measure(
      year = as.integer,
      pattern = "([0-9]{4})"
    ),
    value.name = "id_lulc"
  ) |>
  na.omit(cols = "id_lulc")

id_coord_yr_lulc_dt[,
  year := lubridate::make_date(year)
]

lulc_data_t <-
  as_lulc_data_t(
    id_coord_yr_lulc_dt[
      db$periods_t,
      .(
        id_coord,
        id_lulc,
        id_period
      ),
      on = .(
        # left closed interval
        year >= start_date,
        year < end_date
      ),
      nomatch = NULL
    ]
  )

db$lulc_data_t <- lulc_data_t

id_coord_keep <- lulc_data_t[, id_coord]
db$commit(
  x = db$coords_t[id_coord %in% id_coord_keep],
  table_name = "coords_t",
  mode = "overwrite"
)
