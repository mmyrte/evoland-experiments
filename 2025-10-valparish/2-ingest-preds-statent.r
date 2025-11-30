devtools::load_all("~/github-repos/evoland-plus/")

db <- evoland_db$new(path = "smaller.evolanddb")
coords_minimal <- db$coords_minimal

sources_detailed <- tibble::tribble(
  ~nomenclature, ~source,   ~year,                     ~url,                                                              ~md5sum,
  "noga2008",    "statent", c(2022),                   "https://dam-api.bfs.admin.ch/hub/api/dam/assets/32258837/master", "279f668e1ccf14c286c29f2a0dd212ce",
  "noga2008",    "statent", c(2021),                   "https://dam-api.bfs.admin.ch/hub/api/dam/assets/27245297/master", "e84e0a07c15c087893c1553e34427571",
  "noga2008",    "statent", c(2020),                   "https://dam-api.bfs.admin.ch/hub/api/dam/assets/23245467/master", "9575ba197422f85a32c238d1a2ec0510",
  "noga2008",    "statent", c(2019),                   "https://dam-api.bfs.admin.ch/hub/api/dam/assets/20124116/master", "6cc7a38827b831eaaa7ceed9139f834d",
  "noga2008",    "statent", c(2018),                   "https://dam-api.bfs.admin.ch/hub/api/dam/assets/20124094/master", "57427b5dc1bfe466cc9f89e747e8b806",
  "noga2008",    "statent", c(2017),                   "https://dam-api.bfs.admin.ch/hub/api/dam/assets/20124095/master", "3b92b89a107e7e1f8774f91c4360c200",
  "noga2008",    "statent", c(2016),                   "https://dam-api.bfs.admin.ch/hub/api/dam/assets/20124092/master", "90492ff1721835f0d2912197fd655e00",
  "noga2008",    "statent", c(2015),                   "https://dam-api.bfs.admin.ch/hub/api/dam/assets/20124090/master", "aa9364e17b12f7cb5bfb06b2147d0e1a",
  "noga2008",    "statent", c(2014),                   "https://dam-api.bfs.admin.ch/hub/api/dam/assets/20124091/master", "96fc8fa74d0585fca068d25f825303be",
  "noga2008",    "statent", c(2013),                   "https://dam-api.bfs.admin.ch/hub/api/dam/assets/20124099/master", "303bc94ec647f5cd18eb02907f14f5f5",
  "noga2008",    "statent", c(2012),                   "https://dam-api.bfs.admin.ch/hub/api/dam/assets/20124098/master", "12ea7e8741aa8f62b6679a044e04e7dd",
  "noga2008",    "statent", c(2011),                   "https://dam-api.bfs.admin.ch/hub/api/dam/assets/20124093/master", "603ac5655ededc28114aeac803cb3dd9",
  "noga2008",    "bc",      c(2008),                   "https://dam-api.bfs.admin.ch/hub/api/dam/assets/14607231/master", "2ed5b1d1f57d348d25817885ffdf1843",
  "noga2002",    "bc",      c(1996, 2000, 2005),       "https://dam-api.bfs.admin.ch/hub/api/dam/assets/14607232/master", "8b06a661fbfb22e1128b65ae0f41ae66",
  "noga2002",    "bc",      c(1995, 1998, 2001, 2005), "https://dam-api.bfs.admin.ch/hub/api/dam/assets/14607233/master", "44445be873a129d2c26b5701c6a32505",
)

sources_lst <- purrr::pmap(sources_detailed, \(url, md5sum, ...) list(url = url, md5sum = md5sum))

spec <- list(
  fte_sec1 = list(
    unit = "FTE",
    pretty_name = "Full Time Equivalents for the primary sector",
    orig_format = "hectare csv",
    description = "The primary sector consists of agriculture, forestry, and raw materials",
    sources = sources_lst
  ),
  fte_sec2 = list(
    unit = "FTE",
    pretty_name = "Full Time Equivalents for the secondary sector",
    orig_format = "hectare csv",
    description = "The secondary sector is industry",
    sources = sources_lst
  ),
  fte_sec3 = list(
    unit = "FTE",
    pretty_name = "Full Time Equivalents for the tertiary sector",
    orig_format = "hectare csv",
    description = "The tertiary sector describes all other work",
    sources = sources_lst
  )
)

sources_downloaded <-
  sources_detailed |>
  download_and_verify() |>
  dplyr::left_join(sources_detailed, by = c("url", "md5sum")) |>
  tibble::as_tibble()

to_read <-
  sources_downloaded$local_path |>
  rlang::set_names() |> # set names to self for map/bind_rows
  purrr::map(zip::zip_list) |>
  dplyr::bind_rows(.id = "zip_name") |>
  dplyr::select(zip_name, filename) |>
  dplyr::filter(
    # BC files are named *CSV, STATENT files are named STATENT(_N08)?_YYYY.csv
    stringr::str_detect(filename, "STATENT(_N08)?_\\d{4}.csv$|CSV$")
  ) |>
  dplyr::mutate(
    is_statent = stringr::str_detect(filename, "STATENT"),
    year = dplyr::if_else(
      is_statent,
      stringr::str_extract(filename, "STATENT(\\d{4})", group = 1L) |>
        as.integer(),
      stringr::str_extract(filename, "(bz|BZ)(\\d{2})(_AST|_agr)", group = 2L) |>
        as.integer() |>
        (function(x) ifelse(x < 90L, 2000L + x, 1900L + x))()
    )
  )


if (FALSE) {
  # Get the variable IDs for the number of Full Time Equivalents in each sector
  # Statent uses NOGA2008
  statent_meta_source <- download_and_verify(list(
    # be-b-00.03-22-STATENT_N08-var-v34-tab.xlsx
    url = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/36073025/master",
    md5sum = "71d447124d9ab5d41cdbcb5788ca3475"
  ))
  statent_variables <-
    readxl::read_excel(
      statent_meta_source$local_path,
      col_names = c("varname", "noga2008", "description"),
      range = "A9:C671"
    ) |>
    tidyr::drop_na(varname) |>
    dplyr::filter(
      stringr::str_detect(
        description,
        r"{Vollzeitäquivalente Sektor|Meterkoordinate}"
      )
    )
}


# read STATENT into rasters
read_extract_statent <- function(zip_name, filename, year, coords_t, ...) {
  csv_con <- unz(zip_name, filename, open = "r")
  on.exit(close(csv_con))
  message("working on ", zip_name)

  csv_con |>
    readLines() |>
    data.table::fread(
      text = _,
      select = c(
        "E_KOORD",
        "N_KOORD",
        "B08VZATS1",
        "B08VZATS2",
        "B08VZATS3"
      )
    ) |>
    terra::rast(crs = "EPSG:2056") |>
    extract_using_coords_t(coords_t) |>
    cbind(year = year)
}

s <-
  to_read |>
  dplyr::filter(is_statent) |>
  purrr::pmap(read_extract_statent, coords_t = coords_minimal) |>
  data.table::rbindlist()


# now for the businecess census (BC) data
read_extract_bc <- function(zip_name, filename, year, coords_t, ...) {
  csv_con <- unz(zip_name, filename, open = "r")
  names_present <-
    csv_con |>
    readLines(1) |>
    data.table::fread(text = _) |>
    names()
  close(csv_con)
  csv_con <- unz(zip_name, filename, open = "r")
  on.exit(close(csv_con))
  message("working on ", zip_name)

  selectcols <- c(
    "X",
    "Y",
    # across BC, the variables match "VZA.*\d" - they don't follow their respective NOGA
    # consistently, but essentially, the number at the end of the string is the sector
    # normal statent: "B08VZATS1",
    # 2008 statent:  "B0808VZAS1",
    # business census: "L05VZAS1", where 05 stands for 2005
    stringr::str_subset(names_present, "VZA")
  )

  csv_con |>
    readLines() |>
    data.table::fread(text = _, select = selectcols) |>
    terra::rast(crs = "EPSG:21781") |> # nowhere explicitly defined, but fits
    terra::project("epsg:2056") |>
    extract_using_coords_t(coords_t) |>
    cbind(year = year)
}

b <-
  to_read |>
  dplyr::filter(!is_statent) |>
  purrr::pmap(read_extract_bc, coords_t = coords_minimal) |>
  data.table::rbindlist()

bc_statent <- rbind(s, b)
bc_statent[, year := lubridate::make_date(year = year)]
bc_statent[, sector := stringr::str_sub(layer, -2, -1)]

periods <- db$periods_t
bc_statent_ingest <- bc_statent[
  periods,
  .(
    id_coord,
    id_period,
    sector,
    value
  ),
  on = .(
    year >= start_date,
    year < end_date
  ),
  nomatch = NULL
][,
  # see calibration_predictor_prep.r in old codebase:
  # there was interpolation logic on the change rate per labour market region
  # this has the advantage that we can extrapolate easily, but has the disadvantage of
  # losing out on features like "there's a farm here"
  .(value = mean(value)),
  by = .(id_coord, id_period, sector)
]

db$add_predictor(
  pred_spec = spec["fte_sec1"],
  pred_data = bc_statent_ingest[sector == "S1", .(id_coord, id_period, value)],
  pred_type = "float"
)
db$add_predictor(
  pred_spec = spec["fte_sec2"],
  pred_data = bc_statent_ingest[sector == "S2", .(id_coord, id_period, value)],
  pred_type = "float"
)
db$add_predictor(
  pred_spec = spec["fte_sec3"],
  pred_data = bc_statent_ingest[sector == "S3", .(id_coord, id_period, value)],
  pred_type = "float"
)
