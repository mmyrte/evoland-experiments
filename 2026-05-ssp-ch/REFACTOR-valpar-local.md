# Refactor: `2-ingest-preds-valpar-local.r`

Goal: replace all `file://` URIs with reproducible HTTP/HTTPS sources, one script per distinct data source.
Scope: all predictors in `indicators_1985` and `local_pred_specs`, minus those that already have a dedicated ingest script.

---

## Full predictor inventory

### Already have dedicated scripts — skip

| Predictor              | Script                       |
| ---------------------- | ---------------------------- |
| `noise_mean_100m`      | `2-ingest-preds-sonbase.r` ✓ |
| `muni_pop`             | `2-ingest-preds-pop.r` ✓     |
| `avg_chg_fte_sec1/2/3` | `2-ingest-preds-statent.r` ✓ |

### Need new scripts

| Predictor                      | In `indicators_1985` | In `local_pred_specs`    | Data source                  | New script                     |
| ------------------------------ | -------------------- | ------------------------ | ---------------------------- | ------------------------------ |
| `soil_ph`                      | ✓                    | —                        | EnviDat 10.16904/envidat.153 | `2-ingest-preds-envidat-eiv.r` |
| `soil_nutrients`               | ✓                    | —                        | EnviDat 10.16904/envidat.153 | `2-ingest-preds-envidat-eiv.r` |
| `soil_moisture`                | ✓                    | —                        | EnviDat 10.16904/envidat.153 | `2-ingest-preds-envidat-eiv.r` |
| `soil_moisture_variability`    | ✓                    | —                        | EnviDat 10.16904/envidat.153 | `2-ingest-preds-envidat-eiv.r` |
| `soil_aeration`                | ✓                    | —                        | EnviDat 10.16904/envidat.153 | `2-ingest-preds-envidat-eiv.r` |
| `soil_humus`                   | ✓                    | —                        | EnviDat 10.16904/envidat.153 | `2-ingest-preds-envidat-eiv.r` |
| `light_100m`                   | ✓                    | —                        | EnviDat 10.16904/envidat.153 | `2-ingest-preds-envidat-eiv.r` |
| `continentality_100m`          | ✓                    | —                        | EnviDat 10.16904/envidat.153 | `2-ingest-preds-envidat-eiv.r` |
| `elevation_mean_100m`          | ✓ (ref: `dem.r`)     | ✓ (as `elevation`)       | swissALTI3D tiles (STAC)     | `2-ingest-preds-dem.r`         |
| `aspect_mean_100m`             | ✓ (ref: `dem.r`)     | ✓ (as `aspect`)          | derived from DEM             | `2-ingest-preds-dem.r`         |
| `slope_mean_100m`              | ✓ (ref: `dem.r`)     | ✓ (as `slope`)           | derived from DEM             | `2-ingest-preds-dem.r`         |
| `hillshade_mean_100m`          | ✓ (ref: `dem.r`)     | ✓ (as `hillshade`)       | derived from DEM             | `2-ingest-preds-dem.r`         |
| `distance_to_lakes_mean_100m`  | ✓                    | ✓ (as `distance_lakes`)  | swissTLM3D                   | `2-ingest-preds-swisstlm3d.r`  |
| `distance_to_rivers_mean_100m` | ✓                    | ✓ (as `distance_rivers`) | swissTLM3D                   | `2-ingest-preds-swisstlm3d.r`  |
| `distance_to_roads_mean_100m`  | ✓                    | ✓ (as `distance_roads`)  | swissTLM3D                   | `2-ingest-preds-swisstlm3d.r`  |
| `average_avg_ann_temp`         | ✓                    | —                        | CHELSA V2.1 monthly `tas`    | `2-ingest-preds-chelsa.r`      |
| `average_avg_precip`           | ✓                    | —                        | CHELSA V2.1 monthly `pr`     | `2-ingest-preds-chelsa.r`      |
| `average_sum_gdays_0deg`       | ✓                    | —                        | derived from CHELSA `tas`    | `2-ingest-preds-chelsa.r`      |
| `average_sum_gdays_3deg`       | ✓                    | —                        | derived from CHELSA `tas`    | `2-ingest-preds-chelsa.r`      |
| `average_sum_gdays_5deg`       | ✓                    | —                        | derived from CHELSA `tas`    | `2-ingest-preds-chelsa.r`      |

**Note:** `indicators_1985` and `local_pred_specs` share four overlapping predictors
(elevation/aspect/slope/hillshade and three distance layers). They reference the same
underlying files; a single script per source handles both.

---

## Source 1 — EnviDat (EIV biophysical indicators)

**Dataset:** Descombes et al. 2020, _Ecography_, doi:10.1111/ecog.05117  
**EnviDat DOI:** 10.16904/envidat.153  
**License:** ODbL with Database Contents License (DbCL)

All GeoTIFF layers are bundled in a single ~975 MB ZIP. No per-file direct download; no
published md5sum.

| Resource                     | URL                                                                                                                                                      |
| ---------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Description XLSX (15 KB)     | `https://www.envidat.ch/dataset/4ab13d14-6f96-41fd-96b0-b3ea45278b3d/resource/81c046c3-8d1d-45bc-a833-7d8240cebd12/download/predictors_description.xlsx` |
| All predictors ZIP (~975 MB) | `https://www.envidat.ch/dataset/4ab13d14-6f96-41fd-96b0-b3ea45278b3d/resource/e0faab13-0d1b-492a-8539-5370d48b9e35/download/predictors.zip`              |

Raster specs: **continuous float (community-weighted means)**, 93 m resolution, Mercator
projection (`+proj=merc +ellps=WGS84 ...`). Must reproject to EPSG:2056 and resample to
100 m in-script using bilinear interpolation.

**Important:** the SPEEDMIND layers are _not_ discrete class indices as implied by the
Landolt scale descriptions. They are community-weighted means (CWMs) interpolated
continuously across space:

| Predictor                   | Observed range | Note                                                   |
| --------------------------- | -------------- | ------------------------------------------------------ |
| `soil_ph`                   | 4.7 – 7.1      | Stored as actual pH units, not Landolt 1–5 class index |
| `soil_nutrients`            | 1.7 – 3.9      | CWM of Landolt N (1–5)                                 |
| `soil_moisture`             | 1.3 – 4.7      | CWM of Landolt F (1–5)                                 |
| `soil_moisture_variability` | 1.1 – 2.9      | CWM of Landolt W (1–3)                                 |
| `soil_aeration`             | 1.0 – 4.5      | CWM of Landolt D (1/3/5)                               |
| `soil_humus`                | 1.7 – 4.9      | CWM of Landolt H (1/3/5)                               |
| `light_100m`                | 1.8 – 4.9      | CWM of Landolt L (1–5)                                 |
| `continentality_100m`       | 2.4 – 4.3      | CWM of Landolt K (1–5)                                 |

Filenames inside ZIP confirmed via `unzip(zip_path, list = TRUE)`:  
`Predictors/SPEEDMIND_Soil{R,N,F,W,D,H,L,K}.tif`

md5sums confirmed from cache (populated by `download_and_verify` on first run):

- XLSX: `9a49a27141863f37a5c39c87509f20c7`
- ZIP: `a8e3bd3a7e929e48a73e7df293ea735d`

---

## Source 2 — DHM25 (DEM + terrain derivatives)

DHM25 (`ch.swisstopo.digitales-hoehenmodell_25`) is not in the swisstopo STAC API but
is available as a single whole-Switzerland ASCII grid ZIP (~25 m resolution, LV03/LN02,
~1980s survey vintage).

**Download URL:**

```
https://cms.geo.admin.ch/ogd/topography/DHM25_MM_ASCII_GRID.zip
```

No md5sum published. The ZIP contains ASCII grid tiles in LV03 (EPSG:21781); the script
must mosaic, reproject to EPSG:2056, and resample to 100 m.

Derivatives computed in-script:

- `elevation` → direct resample to 100 m
- `slope` → `terra::terrain(r, "slope", unit = "degrees")`
- `aspect` → `terra::terrain(r, "aspect", unit = "degrees")`
- `hillshade` → `terra::shade(...)`

**Optional expansion note (to be placed in the script):** For terrain roughness, maximum
slope, or other hectare-level terrain metrics at higher resolution, swissALTI3D (2 m COG
tiles, STAC collection `ch.swisstopo.swissalti3d`) would be the appropriate source. This
is deferred due to the large data volume (~44 GB, ~44k tiles).

---

## Source 3 — swissTLM3D (hydrology + roads)

swissTLM3D is the official swisstopo topographic landscape model and the successor to
VECTOR25 / GWN07. GWN07 itself (`ch.swisstopo.vec25-gewaessernetz_referenz`) is only
available via WMS/WMTS — no public direct-download URL exists.

**Latest release download (whole-Switzerland GeoPackage, EPSG:2056):**

```
https://data.geo.admin.ch/ch.swisstopo.swisstlm3d/swisstlm3d_2026-02-24/swisstlm3d_2026-02-24_2056_5728.gpkg.zip
```

STAC browser: <https://data.geo.admin.ch/browser/index.html#/collections/ch.swisstopo.swisstlm3d>

Layers of interest:

| Layer                             | Use                                           |
| --------------------------------- | --------------------------------------------- |
| `TLM_STEHENDES_GEWAESSER_FLAECHE` | Lake polygons → `distance_to_lakes_mean_100m` |
| `TLM_FLIESSGEWAESSER`             | River lines → `distance_to_rivers_mean_100m`  |
| `TLM_STRASSE`                     | Road lines → `distance_to_roads_mean_100m`    |

Distance rasters derived with `terra::distance()` after rasterizing the vector layers.

---

## Source 4 — CHELSA V2.1 (climate)

Monthly ERA5-Land downscaled climate data, freely downloadable without registration.

**URL pattern — monthly mean temperature (`tas`, in Kelvin):**

```
https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/tas/CHELSA_tas_{MM}_{YYYY}_V.2.1.tif
```

**URL pattern — monthly precipitation (`pr`, in kg m⁻² month⁻¹ = mm/month):**

```
https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/pr/CHELSA_pr_{MM}_{YYYY}_V.2.1.tif
```

Where `MM` = zero-padded month `01`–`12` and `YYYY` = `1985`–`1997`.

Files needed for 1985–1997:

- `tas`: 13 × 12 = **156 files** (~128 MB each, ~20 GB total)
- `pr`: 13 × 12 = **156 files** (~800 MB each, ~125 GB total)

**Download approach:** GDAL `/vsicurl/` range requests against the ZHDK S3-compatible
object store. Each file is global (~128 MB for `tas`, ~800 MB for `pr`); cropping to
Switzerland reads only ~1–2 MB per file, so 312 requests total.

```r
url <- "/vsicurl/https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/tas/CHELSA_tas_01_1985_V.2.1.tif"
r <- terra::rast(url) |> terra::crop(extent_wide_wgs84)
```

Derived indicators:

- `average_avg_ann_temp`: mean over 12 monthly means per year, then mean over 1985–1997; convert Kelvin → °C
- `average_avg_precip`: sum over 12 months per year, then mean over 1985–1997
- `average_sum_gdays_{T}deg`: per month, `max(0, T_celsius - threshold) × days_in_month`, sum over year, average over 1985–1997

Precomputed GDD climatologies exist in CHELSA for 0 °C, 5 °C, 10 °C only (not 3 °C), and
only as 1981–2010 averages — not suitable here.

**SUPERSEDED for the SSP runs.** Climate is now sourced from MeteoSwiss CH2025 (Source 5),
mapping global warming levels onto land-use time steps under each SSP scenario. The CHELSA
script is retained only as a reference / fallback for a 1985–1997 historical baseline.

---

## Source 5 — MeteoSwiss CH2025 (climate, GWL-indexed projections + observations)

CH2025 aggregate netCDFs from the MeteoSwiss web atlas. There is no documented download
API, so the URL pattern was reverse-engineered from the atlas
(`2-ingest-preds-ch2025-urls.md`) and candidate URLs are **probed** (HEAD, status < 400)
before downloading only the live ones.

Atlas: <https://www.meteoschweiz.admin.ch/service-und-publikationen/applikationen/ext/climate-ch2025-maps.html>
Endpoint: `https://service.meteoswiss.ch/pbbackend/api/v1/products/{product}/realizations?…&mediaType=application%2Fnetcdf`

Two product families share one indicator set (six climate categories):

| Family | Suffix | Indexed by                                                | Dimensions probed                                                   |
| ------ | ------ | --------------------------------------------------------- | ------------------------------------------------------------------- |
| `-gwl` | proj.  | global warming level × uncertainty quantile (annual only) | GWL ∈ {1.5, 2.0, 3.0}; q ∈ {q5, q50, q95}                           |
| `-obs` | obs.   | reference period × time of year (no uncertainty quantile) | period ∈ {1961-1990, 1991-2020}; ToY ∈ {DJF, MAM, JJA, SON, yearly} |

Categories → `scenarioIndicator`s (`climate-ch2025-maps-{category}-{gwl|obs}`):

| Category      | Indicators                                                  |
| ------------- | ----------------------------------------------------------- |
| `cold`        | `ID`, `HDD`, `HED`                                          |
| `heat`        | `HD`, `HW2`, `HW3`, `HW4`, `CoDD`, `COD`, `VHD`, `SD`, `TN` |
| `precip`      | `pr`, `PR20`, `PR40`, `PR60`                                |
| `snow`        | `SNFD`                                                      |
| `temperature` | `tas`, `tasmax`, `tasmin`                                   |
| `drought`     | `CDD`                                                       |

21 indicators × (189 GWL + 210 OBS) = **399 candidate URLs** before probing.

Raster specs: each netCDF is **already CH1903+ / LV95 (EPSG:2056)** on a **1 km grid**,
single variable/layer = the indicator. No reprojection needed; extracted at the model
coords with `extract_using_coords_t` (nearest 1 km cell). Requests are throttled
(`httr2::req_throttle`, ~2 req/s, host-level bucket shared by probe + download).

Split into two scripts:

- `…-1-download.r` — Stage 1 (probe) + Stage 2 (download). Writes only to the cache.
- `…-2-etl.r` — Stage 3 (extract) + Stage 4 (load). Recovers all dimensions from the
  cached file names (nothing passed in memory between scripts) and reads → transforms →
  writes **one file at a time** (a single long table over ~4M coords × 67 files is too
  large to hold). Per-indicator metadata follows the `2-ingest-preds-swisstlm3d.r`
  pattern (one spec per indicator, tied to files by the indicator token).

**Data model (pred_data_t dimensions):**

- `name = {indicator}_{time_of_year}` (e.g. `tas_yearly`, `pr_DJF`). A predictor is one
  climate variable and must stay consistent across historic (observed) and future
  (extrapolated) periods, so the **reference period is not part of the name**.
- `id_period` carries the temporal progression: the observed reference period (obs) and,
  deferred, the progression through global warming levels (`-gwl`).
- `id_run` carries quantile × SSP scenario (relevant for the deferred `-gwl` runs).

**Observations (`-obs`) — implemented in `…-2-etl.r`:** ingested at `id_period = 0`
(baseline) under the active `id_run` (= 0) via `db$add_predictor` (idempotent upsert by
`name`). Because the name drops the reference period, only one reference period can occupy
`id_period 0`; we use the WMO normal **1991-2020**, which covers all 21 indicators
(1961-1990 covers only 14, a subset). That yields **41 predictors** (21 yearly + 5 seasonal
indicators × 4 seasons). 1961-1990 stays cached for a later historic `id_period` if wanted.

**Deferred — projections (`-gwl`):** when appending these, write directly to
`pred_data_t` keyed by the `id_pred` already in `pred_meta_t`, after:

1. Crosswalk `gwl` → `id_period` per SSP scenario (extrapolated periods 5–8 span
   2025–2060).
2. Encode quantile × scenario as `id_run`.
3. Decide which indicators actually feed the transition model (vs. the CHELSA-derived
   predictors they replace).

**TODO / out of scope:** SSP5-8.5 late-century (~2071–2100, ~5–6 °C) exceeds the highest
GWL probed (GWL3.0) and cannot be mapped to an existing CH2025 aggregate.

---

## Status

| Script                         | Status                                                                                                  |
| ------------------------------ | ------------------------------------------------------------------------------------------------------- |
| `2-ingest-preds-envidat-eiv.r` | ✅ complete; continuous floats (CWMs), bilinear resample; md5sums confirmed                             |
| `2-ingest-preds-dem.r`         | ✅ written; DHM25 ZIP, LV03→EPSG:2056 100m, bilinear; swissALTI3D expansion noted                       |
| `2-ingest-preds-swisstlm3d.r`  | ✅ written; single GPKG ZIP, rasterize→distance for all three layers                                    |
| `2-ingest-preds-chelsa.r`           | 🟡 superseded by CH2025 for SSP runs; kept as 1985–1997 baseline reference                         |
| `2-ingest-preds-ch2025-1-download.r` | ✅ probe + throttled download of all 399 candidates to cache                                      |
| `2-ingest-preds-ch2025-2-etl.r`      | 🟡 41 obs predictors (name `{indicator}_{toy}`, 1991-2020 → id_period 0) via add_predictor; -gwl deferred |
