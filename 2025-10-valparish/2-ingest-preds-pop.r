devtools::load_all("~/github-repos/evoland-plus/")

spec <- list(
  pop = list(
    unit = "mean_pop",
    pretty_name = "Mean Population over a given period",
    orig_format = "number per commune per year",
    description = "The FSO statistic for each commune's inhabitants is averaged over each period",
    sources = list(
      list(
        # this is the Demographic balance by institutional units
        # this could be replaced by a call to the pxweb API
        # maybe use the pxweb package and this URL
        # "https://www.pxweb.bfs.admin.ch/api/v1/en/px-x-0102020000_201/px-x-0102020000_201.px"
        url = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/23164063/master",
        md5sum = "528209137ea5c74a30e477a1ddc3da84"
      ),
      list(
        # swissboundaries3d
        url = "https://data.geo.admin.ch/ch.swisstopo.swissboundaries3d/swissboundaries3d_2021-07/swissboundaries3d_2021-07_2056_5728.shp.zip",
        md5sum = "6d217867b7bf6dfb2927b12f12481a8b"
      )
    )
  )
)

sources <-
  spec$pop$sources |>
  data.table::rbindlist() |>
  download_and_verify()

# read in PX data from http and convert to DF
px_data <- as.data.frame(pxR::read.px(
  sources$local_path[1]
))

# subset to desired rows based on conditions:
# Total population on 1st of January;
# Total populations (Swiss and foreigners)
# Total population (Men and Women)
raw_mun_popdata <- px_data[
  px_data$Demografische.Komponente == "Bestand am 31. Dezember" &
    px_data$Staatsangehörigkeit..Kategorie. == "Staatsangehörigkeit (Kategorie) - Total" &
    px_data$Geschlecht == "Geschlecht - Total",
]

# Identify municipalities records by matching on the numeric contained in their name
raw_mun_popdata <- raw_mun_popdata[
  grepl(
    ".*?([0-9]+).*",
    raw_mun_popdata$Kanton.......Bezirk........Gemeinde.........
  ),
  c(4:6)
]
names(raw_mun_popdata) <- c("Name_Municipality", "Year", "Population")
raw_mun_popdata <- raw_mun_popdata |>
  tidyr::pivot_wider(
    names_from = "Year",
    values_from = "Population"
  )
# Remove the periods in the name column
raw_mun_popdata$Name_Municipality <- gsub(
  "[......]",
  "",
  as.character(raw_mun_popdata$Name_Municipality)
)

# Seperate BFS number from name
raw_mun_popdata$BFS_NUM <- as.numeric(gsub(
  ".*?([0-9]+).*",
  "\\1",
  raw_mun_popdata$Name_Municipality
))

# Remove BFS number from name
raw_mun_popdata$Name_Municipality <- gsub(
  "[[:digit:]]",
  "",
  raw_mun_popdata$Name_Municipality
)

# subset to only municipalities existing in 2021
raw_mun_popdata <- raw_mun_popdata[raw_mun_popdata$`2021` > 0, ]

hoheitsgebiet_path <- paste0(
  "/vsizip/",
  sources$local_path[2],
  # TODO check that 1_3 and 1_5 are interchangeable
  "/swissBOUNDARIES3D_1_5_TLM_HOHEITSGEBIET.shp"
)

muni_shp <- terra::vect(hoheitsgebiet_path)

# filter out non-swiss municipalities
muni_shp <- muni_shp[
  muni_shp$ICC == "CH" &
    muni_shp$OBJEKTART == "Gemeindegebiet",
]

# Import data of municipality mutations from FSO web service using conditions:
# 1. Mutations between 01/01/1981 and 01/05/2022
# 2. Name change mutations only (other mutations captured by shapefile)

# scrape content from html address
content <- rvest::read_html(paste0(
  "https://www.agvchapp.bfs.admin.ch/de/mutated-communes/results?",
  "EntriesFrom=01.01.1981&EntriesTo=01.05.2022&NameChange=True"
))
muni_mutations <- rvest::html_table(content, fill = TRUE)[[1]]

# remove 1st row because it contains additional column names
muni_mutations <- muni_mutations[-1, ]

# rename columns
colnames(muni_mutations) <- c(
  "Mutation_Number",
  "Pre_canton_ID",
  "Pre_District_num",
  "Pre_BFS_num",
  "Pre_muni_name",
  "Post_canton_ID",
  "Post_district_num",
  "Post_BFS_num",
  "Post_muni_name",
  "Change_date"
)

# identify which municipalities have mutations associated with them
mutation_index <- match(raw_mun_popdata$BFS_NUM, muni_mutations$Pre_BFS_num)

# change municpality BFS number in population table according to the mutation for
# each row in the pop df if there is an NA in the mutation index do not replace the
# BFS number If there is not an NA then replace with the new BFS number of the
# mutation table.

for (i in seq_len(nrow(raw_mun_popdata))) {
  if (!is.na(mutation_index[i])) {
    raw_mun_popdata$BFS_NUM[[i]] <- muni_mutations[[
      mutation_index[[i]],
      "Post_BFS_num"
    ]]
  }
}

# If after introducing the mutations we have multiple rows with the same BFS numbers
# then we need to combine their populations values as these indicate municipalities merging

if (length(unique(raw_mun_popdata$BFS_NUM)) != nrow(raw_mun_popdata)) {
  # get the indices of columns that represent the years
  Time_points <- na.omit(as.numeric(gsub(
    ".*?([0-9]+).*",
    "\\1",
    colnames(raw_mun_popdata)
  )))

  # create a empty df for results
  Muni_pop_final <- as.data.frame(matrix(
    ncol = length(Time_points),
    nrow = length(unique(raw_mun_popdata$BFS_NUM))
  ))
  colnames(Muni_pop_final) <- Time_points

  # Add column for BFS number
  Muni_pop_final$BFS_NUM <- sort(unique(raw_mun_popdata$BFS_NUM))

  # loop over date cols and rows summing values where BFS number is non-unique
  for (j in Time_points) {
    for (i in seq_along(unique(raw_mun_popdata$BFS_NUM))) {
      Muni_pop_final[i, paste(j)] <- sum(
        raw_mun_popdata[
          raw_mun_popdata$BFS_NUM == Muni_pop_final[i, "BFS_NUM"],
          paste(j)
        ]
      )
    }
  }
  # replace old data with revised data
  raw_mun_popdata <- Muni_pop_final
} # close if statement

### Create historic municipality population rasters

db <- evoland_db$new(path = "valparish.evolanddb")
coords_minimal <- db$coords_minimal
periods <- db$periods_t


# taking mean number of inhabitants, assuming regular time series
# would skew the representation slightly if a group of years were overrepresented
# not worth checking for now
agg_mun_popdata <-
  raw_mun_popdata |>
  tibble::as_tibble() |>
  dplyr::select(bfs_num = BFS_NUM, tidyselect::matches("[0-9]{4}")) |>
  tidyr::pivot_longer(
    cols = -bfs_num,
    names_to = "year",
    names_transform = as.integer,
    values_to = "no_inhabitants"
  ) |>
  dplyr::mutate(year = lubridate::make_date(year = year)) |>
  dplyr::inner_join(
    periods,
    by = dplyr::join_by(
      year >= start_date,
      year < end_date
    )
  ) |>
  dplyr::group_by(id_period, bfs_num) |>
  dplyr::summarise(
    mean_inhabitants = mean(no_inhabitants),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = id_period,
    values_from = mean_inhabitants
  )


munis_pops <- terra::merge(
  x = muni_shp[, c("BFS_NUMMER")],
  by.x = "BFS_NUMMER",
  y = agg_mun_popdata,
  by.y = "bfs_num"
)

predvals <- extract_using_coords_t(
  munis_pops[, -1],
  coords_minimal
)


db$add_predictor(
  pred_spec = spec,
  pred_data = predvals[,
    .(id_coord, id_period = as.integer(attribute), value)
  ],
  pred_type = "float"
)
