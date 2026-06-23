# Refactor: `2-ingest-preds-valpar-local.r`

Goal: replace all `file://` URIs with reproducible HTTP/HTTPS sources, one script per distinct data source.
Scope: all predictors in `indicators_1985` and `local_pred_specs`, minus those that already have a dedicated ingest script.

---

## Full predictor inventory

### Already have dedicated scripts — skip

| Predictor | Script |
|---|---|
| `noise_mean_100m` | `2-ingest-preds-sonbase.r` ✓ |
| `muni_pop` | `2-ingest-preds-pop.r` ✓ |
| `avg_chg_fte_sec1/2/3` | `2-ingest-preds-statent.r` ✓ |

### Need new scripts

| Predictor | In `indicators_1985` | In `local_pred_specs` | Data source | New script |
|---|---|---|---|---|
| `soil_ph` | ✓ | — | EnviDat 10.16904/envidat.153 | `2-ingest-preds-envidat-eiv.r` |
| `soil_nutrients` | ✓ | — | EnviDat 10.16904/envidat.153 | `2-ingest-preds-envidat-eiv.r` |
| `soil_moisture` | ✓ | — | EnviDat 10.16904/envidat.153 | `2-ingest-preds-envidat-eiv.r` |
| `soil_moisture_variability` | ✓ | — | EnviDat 10.16904/envidat.153 | `2-ingest-preds-envidat-eiv.r` |
| `soil_aeration` | ✓ | — | EnviDat 10.16904/envidat.153 | `2-ingest-preds-envidat-eiv.r` |
| `soil_humus` | ✓ | — | EnviDat 10.16904/envidat.153 | `2-ingest-preds-envidat-eiv.r` |
| `light_100m` | ✓ | — | EnviDat 10.16904/envidat.153 | `2-ingest-preds-envidat-eiv.r` |
| `continentality_100m` | ✓ | — | EnviDat 10.16904/envidat.153 | `2-ingest-preds-envidat-eiv.r` |
| `elevation_mean_100m` | ✓ (ref: `dem.r`) | ✓ (as `elevation`) | swissALTI3D tiles (STAC) | `2-ingest-preds-dem.r` |
| `aspect_mean_100m` | ✓ (ref: `dem.r`) | ✓ (as `aspect`) | derived from DEM | `2-ingest-preds-dem.r` |
| `slope_mean_100m` | ✓ (ref: `dem.r`) | ✓ (as `slope`) | derived from DEM | `2-ingest-preds-dem.r` |
| `hillshade_mean_100m` | ✓ (ref: `dem.r`) | ✓ (as `hillshade`) | derived from DEM | `2-ingest-preds-dem.r` |
| `distance_to_lakes_mean_100m` | ✓ | ✓ (as `distance_lakes`) | swissTLM3D | `2-ingest-preds-swisstlm3d.r` |
| `distance_to_rivers_mean_100m` | ✓ | ✓ (as `distance_rivers`) | swissTLM3D | `2-ingest-preds-swisstlm3d.r` |
| `distance_to_roads_mean_100m` | ✓ | ✓ (as `distance_roads`) | swissTLM3D | `2-ingest-preds-swisstlm3d.r` |
| `average_avg_ann_temp` | ✓ | — | CHELSA V2.1 monthly `tas` | `2-ingest-preds-chelsa.r` |
| `average_avg_precip` | ✓ | — | CHELSA V2.1 monthly `pr` | `2-ingest-preds-chelsa.r` |
| `average_sum_gdays_0deg` | ✓ | — | derived from CHELSA `tas` | `2-ingest-preds-chelsa.r` |
| `average_sum_gdays_3deg` | ✓ | — | derived from CHELSA `tas` | `2-ingest-preds-chelsa.r` |
| `average_sum_gdays_5deg` | ✓ | — | derived from CHELSA `tas` | `2-ingest-preds-chelsa.r` |

**Note:** `indicators_1985` and `local_pred_specs` share four overlapping predictors
(elevation/aspect/slope/hillshade and three distance layers). They reference the same
underlying files; a single script per source handles both.

---

## Source 1 — EnviDat (EIV biophysical indicators)

**Dataset:** Descombes et al. 2020, *Ecography*, doi:10.1111/ecog.05117  
**EnviDat DOI:** 10.16904/envidat.153  
**License:** ODbL with Database Contents License (DbCL)

All GeoTIFF layers are bundled in a single ~975 MB ZIP. No per-file direct download; no
published md5sum.

| Resource | URL |
|---|---|
| Description XLSX (15 KB) | `https://www.envidat.ch/dataset/4ab13d14-6f96-41fd-96b0-b3ea45278b3d/resource/81c046c3-8d1d-45bc-a833-7d8240cebd12/download/predictors_description.xlsx` |
| All predictors ZIP (~975 MB) | `https://www.envidat.ch/dataset/4ab13d14-6f96-41fd-96b0-b3ea45278b3d/resource/e0faab13-0d1b-492a-8539-5370d48b9e35/download/predictors.zip` |

Raster specs: 93 m resolution, Mercator projection (`+proj=merc +ellps=WGS84 ...`). Must
reproject to EPSG:2056 and resample to 100 m in-script.

**Exact filenames inside the ZIP are not yet confirmed** — the XLSX describes them. The
expected names based on the paper and field names in `indicators_1985`:

| Predictor | Expected filename in ZIP |
|---|---|
| `soil_ph` | `EIV_R.tif` or similar |
| `soil_nutrients` | `EIV_N.tif` |
| `soil_moisture` | `EIV_F.tif` |
| `soil_moisture_variability` | `EIV_W.tif` |
| `soil_aeration` | `EIV_D.tif` |
| `soil_humus` | `EIV_H.tif` |
| `light_100m` | `EIV_L.tif` |
| `continentality_100m` | `EIV_K.tif` |

> **Please check:** do you know the actual filenames, or should the script download the
> XLSX first and parse it to determine which TIF corresponds to which EIV?

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

| Layer | Use |
|---|---|
| `TLM_STEHENDES_GEWAESSER_FLAECHE` | Lake polygons → `distance_to_lakes_mean_100m` |
| `TLM_FLIESSGEWAESSER` | River lines → `distance_to_rivers_mean_100m` |
| `TLM_STRASSE` | Road lines → `distance_to_roads_mean_100m` |

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

---

## Status

| Script | Status |
|---|---|
| `2-ingest-preds-envidat-eiv.r` | ✅ written; filenames confirmed as `Predictors/SPEEDMIND_Soil{R,N,F,W,D,H,L,K}.tif` |
| `2-ingest-preds-dem.r` | ⏳ ready to write; DHM25 ZIP confirmed |
| `2-ingest-preds-swisstlm3d.r` | ⏳ ready to write |
| `2-ingest-preds-chelsa.r` | ⏳ ready to write; /vsicurl/ confirmed |
