#' Purpose: download the Swiss Soil Property Map (SSPM) mean layers to the evoland cache
#' date: 2026-07-03
#'
#' Source: Gupta, Hasler & Alewell (2024), "Mining soil data of Switzerland" —
#' Swiss Soil Property Map, Zenodo record 7821650 (doi:10.1016/j.geodrs.2023.e00747).
#' 30 m GeoTIFFs, EPSG:4326, NoData -3.4e38, one file per {property}_{depth}_{stat}.
#'
#' We fetch only the MEAN layers of sand, clay and OC at 0/30/60/100 cm (12 files,
#' ~510 MB each, ~6 GB total) — the inputs to the pedotransfer whc derivation in
#' 2-forcing-soil-whc.r. The `error` (uncertainty) layers are left for a later
#' soil-uncertainty id_run. N and P are not used.
#'
#' Mirrors the download pattern of 2026-05-ssp-ch/2-ingest-preds-dem.r: build a
#' url+md5sum table and hand it to download_and_verify(), which caches each file under
#' {cachedir}/{md5sum}/{filename} and returns the local paths (md5-verified).

# TODO this should be implemented in the same pattern as with
# 2026-05-ssp-ch/2-ingest-preds-ch2025-1-download.r
# and
# 2026-05-ssp-ch/2-ingest-preds-ch2025-2-etl.r

library(data.table)
library(evoland)

zenodo_record <- "7821650"

# {property, depth_cm} -> md5sum for the mean layers (from the Zenodo record listing).
sspm_mean_md5 <- data.table::fread(
  text = "
property depth_cm md5sum
sand 0   f1f714315b2180d062144bb0bed664fe
sand 30  2c7e0c6208c6cdcab312c9dc99e434b2
sand 60  9cb7031f5eae28dab9d31ca88932134b
sand 100 59a3e79208af5420300434aafdd68e47
clay 0   9d2739c0fd5347fa927098f9927cb952
clay 30  34fa92871bb7708f4ff0899930ebb580
clay 60  cfbc5cd5ec4787efe75d8cc4311807de
clay 100 b395942a5fdd0196fb371b2058197f65
OC   0   2984b33c24a695e95e133447a9014d41
OC   30  f5bde0ba345558da5068d004922f2997
OC   60  d80401aea525190d6d2e8cf095dbbc07
OC   100 40792da49a13a221fcb5f70f53cc7a30
",
  colClasses = list(character = "property", integer = "depth_cm", character = "md5sum")
)

sspm_mean_md5[, filename := sprintf("%s_%dcm_mean_30m.tif", property, depth_cm)]
sspm_mean_md5[,
  url := sprintf(
    "https://zenodo.org/records/%s/files/%s?download=1",
    zenodo_record,
    filename
  )
]

message(
  "Downloading ",
  nrow(sspm_mean_md5),
  " SSPM mean layers (~",
  round(nrow(sspm_mean_md5) * 0.51, 1),
  " GB) to the evoland cache..."
)

downloaded <- download_and_verify(
  sspm_mean_md5[, .(url, md5sum)],
  target_dir = getOption("evoland.cachedir")
)

# Re-attach property/depth so downstream (2-forcing-soil-whc.r) can index by them.
sspm_files <- merge(
  sspm_mean_md5[, .(md5sum, property, depth_cm, filename)],
  downloaded,
  by = "md5sum"
)

message("Done. SSPM layers cached:")
print(sspm_files[, .(property, depth_cm, local_path)])
