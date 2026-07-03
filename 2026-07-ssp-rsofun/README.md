# SSP-CH × rsofun: process-based land-use suitability predictors

**Status:** planning · **Date:** 2026-07-03

## 1. Purpose and relationship to `2026-05-ssp-ch`

`2026-05-ssp-ch/` implements the SSP-CH scenarios with a **purely empirical/statistical**
land-use model: land-use transitions are estimated from static and climatological
predictors (Arealstatistik LULC history + CH2025 climate indicators + terrain +
population/accessibility covariates), with **no process-based feedback** between land
cover and the biophysical environment. That experiment is retained as-is and is the
**baseline** for this one.

This experiment (`2026-07-ssp-rsofun/`) **extends** the baseline by adding a new class
of predictors: **process-based land-use suitability indicators emitted by
[rsofun](https://github.com/mmyrte/rsofun)** (P-model + SPLASH water balance). It
reuses everything from the baseline — the `ssp-ch.evolanddb` schema, the ~4.1M
hectare `coords_t`, the decadal `periods_t` (P10Y, 1985–2060), the LULC ingestion, and
the empirical predictors — and layers rsofun-derived predictors into the same
`pred_data_t` (keyed by `id_coord`, `id_period`, `id_run`) via `db$add_predictor`.

### Scientific hypothesis

We expect **feedbacks between land-use change and hydrological/vegetation state**: a
transition (e.g. forest → grassland, or cropland abandonment → shrub encroachment)
alters the local water and energy balance (interception, transpiring leaf area,
albedo, rooting depth), which changes soil-moisture/water-stress and productivity,
which in turn changes the **suitability of the next transition**. The empirical
baseline cannot represent this; a process-based model forced by the evolving land
cover can.

### Why rsofun (and not WASIM yet, and not bare SPLASH)

The eventual target for this feedback is **WASIM** (fully distributed, land-use-aware
via its `[landuse_table]`, lateral routing across 17 nested basins). WASIM is too
expensive to stand up before mid-August: it requires daily→hourly disaggregation of
station series and watershed-specific runs in upstream→downstream DAG order.

rsofun is the right **interim stand-in** because its P-model couples to land cover
through exactly the handles WASIM uses:

- **fAPAR** (fraction of absorbed PAR ≈ canopy density) — a required daily forcing.
- **whc** (soil water-holding capacity / rooting) — a site parameter; water-stress
  threshold θ\* = 0.6·whc.

The P-model uses **SPLASH internally** for the water balance (soil moisture, AET, PET),
so this is continuous with the `rsplash` work — but unlike bare SPLASH (which is
land-cover-blind: albedo is a global constant, no LAI/transpiration), rsofun lets
vegetation state drive the fluxes. See `docs/` cross-reference to the WASIM land-use
table below.

**Captured** by rsofun: the dominant *vertical* land-cover ↔ water/energy feedback
(fAPAR/LAI, albedo, rooting → transpiration, soil moisture, water stress, GPP).
**Deferred to WASIM** (knowingly out of scope here): lateral/topographic routing across
the basin DAG, canopy interception loss, aerodynamic/roughness effects, wet-side
(aeration) stress, and multi-layer canopies.

## 2. Coupling architecture

rsofun sits **outside** evoland's inner allocation loop, as a decadal forcing generator:

```
CH2025 climate (per decade, per SSP/ensemble)  ┐
swissALTI3D/DHM25 terrain (static)             ├─► rsofun P-model ─► decadal suitability
Swiss Soil Property Map → whc (static)         │      (per pixel)     predictors (GPP, α,
evoland land cover → fAPAR, whc mod, rootdepth ┘                      wscal, soil moisture)
        ▲                                                                     │
        └──────────── next decade's land cover ◄── evoland transition + allocation ◄┘
```

The loop is closed **at decadal cadence**: within a decade the rsofun predictors are
static; when evoland reallocates land cover for the next decade, fAPAR/whc/rootdepth
change, rsofun is re-run, and the updated suitability predictors feed the next
transition step. rsofun is **never** in the per-cell allocation inner loop, so its cost
is amortised to one run per (decade × SSP × ensemble member).

## 3. Input data required

### 3.1 Climate — CH2025 daily gridded (the binding constraint)

Available on the target machine: **CH2025 daily gridded climatologies**, 30-year runs,
up to 30 ensemble members, delivering **only `pr`, `tas`, `tasmax`, `tasmin`**. The
P-model needs more than that, so several forcings must be **derived** or **sourced
separately**. This is the single most important open item.

| rsofun P-model forcing | In CH2025? | Provenance / derivation | Risk |
|---|---|---|---|
| `temp` (tas) | ✓ | direct | — |
| `tmin` / `tmax` | ✓ | tasmin / tasmax | — |
| `prec` (rain+snow) | ✓ | `pr`; SPLASH splits rain/snow by temperature | — |
| `vpd` | ✗ | derive from tmin/tmax/tas: VPD = ē_sat(tmax,tmin) − e_a, with dewpoint ≈ tmin | **Alpine dry-air bias** in the dewpoint≈tmin assumption — validate vs MeteoSwiss RH stations |
| `ppfd` (shortwave/PAR) | ✗ | **major gap.** Option A: Hargreaves/Bristow-Campbell — Rs ∝ √(tmax−tmin)·Ra, with extraterrestrial Ra(lat, DOY) (the `rsplash` SOLAR module already computes Ra + topographic corrections). Option B: source a radiation product (CM SAF SARAH-3 satellite, or MeteoSwiss operational global-radiation grids, or ERA5-Land `ssrd`). | High: √ΔT estimation is uncertain; recommend A for prototyping, validate, swap to B for production |
| `netrad` | ✗ | computed inside `waterbal_splash` from PPFD + temperature (longwave empirical) — **verify** it is optional in the rsofun forcing spec; otherwise derive alongside ppfd | verify |
| `patm` | ✗ | from DEM elevation via barometric formula (`calc_patm(elv)`) | low |
| `co2` | ✗ | SSP concentration pathway, annual global mean (Meinshausen et al. 2020) — one series per SSP | low |
| `ccov` (cloud) | ✗ | only needed if PPFD is derived from cloudiness; **N/A** on the Hargreaves route | — |
| `fapar` | ✗ | **land-cover coupling handle** — see §3.3 | central |

Two genuine gaps: **shortwave radiation/PPFD** (absent from CH2025 entirely) and
**fAPAR**. Everything else is either present or cheaply derivable.

### 3.2 Soil — Swiss Soil Property Map (Gupta et al. 2024)

[Swiss Soil Property Map (SSPM)](https://doi.org/10.1016/j.geodrs.2023.e00747), QRF-based,
**30 m**, four depths (**0, 30, 60, 100 cm**), delivering **sand, clay** (→ silt),
**organic carbon (OC)**, N, P, each with a 90% prediction interval.

To get the rsofun/SPLASH **`whc`** we run a pedotransfer function on texture + organic
matter, exactly as `rsplash::soil_hydro` already does (Saxton–Rawls / Balland PTF →
field capacity, wilting point, saturation, Ksat), then integrate plant-available water
(FC − WP) over the rooting depth.

Missing inputs the SSPM does **not** provide, and how to fill them:

- **Bulk density** — derive via the same PTF from texture + OM (Balland et al. 2008;
  `soil_hydro` estimates BD when not supplied).
- **Coarse fragments / gravel** — not mapped nationally; assume a class/regional
  default or source separately (flag as an assumption).
- **Depth to bedrock / soil thickness** — cap at the 100 cm SSPM profile, or bring in
  Pelletier et al. (2016) soil+sediment thickness (already a covariate in the SSPM
  paper) for the rooting-depth ceiling.

Aggregate the SSPM 30 m properties to the 100 m model grid (area-weighted mean of
texture/OM; propagate the PI as an uncertainty layer if we want a soil-uncertainty
`id_run`).

### 3.3 Land cover — evoland SSP output → fAPAR / whc / rooting

The SSP-CH LULC schema (`2026-05-ssp-ch/1-ingest-lulc-data.r`) aggregates Arealstatistik
NOAS04 (72 categories) into **9 classes**: `arable`, `perm_crops`, `grassland`,
`alp_past`, `closed_forest`, `open_forest`, `shrubland`, `urban`, `static`. The seven
vegetated classes are the ones that carry a meaningful fAPAR/rooting signature; `urban`
and `static` get minimal-vegetation defaults.

For each class we author a **class → biophysical-parameter crosswalk** (the interim
equivalent of WASIM's `[landuse_table]`):

| evoland class | fAPAR (seasonal) | rooting depth → whc modifier | albedo | notes |
|---|---|---|---|---|
| arable | crop phenology, low winter | shallow–medium | moderate | strong seasonal cycle |
| perm_crops | orchard/vineyard | medium | moderate | |
| grassland | high growing-season | shallow | moderate | |
| alp_past | short season, snow-limited | shallow | moderate–high | elevation-modulated |
| closed_forest | high, evergreen/deciduous split | deep | low | highest LAI |
| open_forest | intermediate | deep | low–moderate | |
| shrubland | low–moderate | medium | moderate | |
| urban / static | ~0 vegetated fraction | n/a | high/variable | minimal-veg defaults |

Two ways to populate fAPAR (decide in Phase 0):

1. **Remote-sensing climatology per class × bioregion** — MODIS/Copernicus/Sentinel
   fAPAR, composited by class and biogeographic region (see the `static` biogeographic
   regions TODO in `1-ingest-lulc-data.r`). Preferred for the observed baseline.
2. **LAI → fAPAR mechanistically** — fAPAR = VCF·(1 − exp(−k·LAI)) (Beer–Lambert). This
   is the **same equation WASIM uses** (`k_extinct` in its `[multilayer_landuse]` table),
   so a class→LAI phenology table authored here is directly reusable for WASIM later.

### 3.4 Terrain — already ingested

`2026-05-ssp-ch/2-ingest-preds-dem.r` provides 100 m **elevation, slope, aspect** from
DHM25 (EPSG:2056). Elevation → `patm` and radiation; slope/aspect → SPLASH topographic
radiation corrections; latitude from the grid. swissALTI3D (2 m) is noted there as the
higher-resolution upgrade if hectare-scale terrain roughness is wanted.

### 3.5 CO₂

SSP concentration pathway, annual global-mean CO₂ per scenario (one short series each).

## 4. WASIM consistency (make the interim reusable, not throwaway)

The eventual WASIM `[landuse_table]` parametrises each class with Albedo, canopy
resistance (rsc), **LAI**, VCF, **RootDepth**, z0, and phenology `JulDays` arrays
(WASIM manual §4.10 / §2.8.3). The evoland↔WASIM class mapping already exists in
`mmyrte/wasim-forclim-evoland-experiments/AS0409_17Cat_to_WaSiM_LansuseTable.rmp`
(Arealstatistik 04/09 17-category → WASIM landuse IDs). **Author the class→parameter
crosswalk once**, with the columns WASIM needs (LAI phenology, k_extinct, albedo,
RootDepth/RootDist, VCF); derive the rsofun handles from it:

- LAI + k_extinct → **fAPAR** (identical Beer–Lambert conversion)
- RootDepth + soil PTF → **whc**
- albedo → net-radiation input

That way the interim rsofun run is a genuine stepping stone: the same table exports both
an rsofun forcing set and a WASIM `landuse_table` stub. **Open reconciliation item:** the
baseline uses a 9-class schema; WASIM uses the 17-category schema — the crosswalk must
bridge both (the `.rmp` is the reference for the WASIM side).

## 5. Outputs — decadal suitability predictors

Per pixel, aggregate rsofun daily/monthly outputs to **decadal** statistics (matching
`periods_t` P10Y), and ingest via `db$add_predictor` keyed by `id_coord`, `id_period`
(decade / GWL), `id_run` (SSP × quantile), mirroring the deferred `-gwl` ingestion
described in `2-ingest-preds-ch2025-2-etl.r`:

- **α = AET/PET** (Cramer–Prentice moisture index) — canonical plant-available-moisture
- **`wscal`** — P-model water-stress scalar
- **soil moisture** (growing-season mean, and driest-month for extremes)
- **GPP** — productivity suitability
- optionally net radiation / PPFD, snow (SWE, snow-cover duration)

Report decadal means/sums **and** variability/extremes (the tails often drive suitability).

## 6. Performance & the C++/Rcpp question

Scale: ~4.1M hectare pixels × ~30 yr × 365 d ≈ 4.5×10¹⁰ cell-days per member, × up to
30 members ≈ 1.3×10¹² cell-days. rsofun's biophysics is compiled **Fortran** (P-model ≈
9 `*_pmodel.mod.f90` modules + `waterbal_splash.mod.f90`, bridged by `wrappersc.c`); the
Fortran itself is fast. The likely bottleneck is **R↔Fortran marshalling per site**
(`runread_pmodel_f` is called one gridcell at a time, rebuilding nested forcing frames),
not the numerics.

**Phased plan — do not rewrite before the science is validated (the translation is the
highest-risk item):**

- **Phase 0 — prototype coupling** with the stock forked rsofun on a small AOI (one
  bioregion / catchment): build the forcing (§3), the class→fAPAR/whc crosswalk (§4),
  run the closed decadal loop, sanity-check that the feedback signal is real and
  large enough to matter (a Budyko back-of-envelope can pre-screen this).
- **Phase 1 — scale by embarrassing parallelism** over pixels: chunk the 4.1M coords
  across cores/nodes (`future`/`mirai` or a Slurm array), stock rsofun per chunk.
  **Profile** to confirm where time goes.
- **Phase 2 — optimise only if Phase 1 profiling demands it.** Two options:
    - (a) **Batch driver** in Fortran/C that loops sites internally without per-site R
    round-trips — low risk, reuses the already-validated biophysics. Likely sufficient.
    - (b) **Full Rcpp/C++ SoA-vectorised rewrite** across locations for maximum throughput
    and headless HPC use (no R in the hot path). Translate **only** `waterbal_splash` +
    the P-model `gpp`/`photosynth`/`plant` modules; **skip BiomeE** (not needed for the
    forced-fAPAR design).
    - Either path requires the workflow the user described: a **golden-master test
    harness** capturing stock-rsofun outputs on a representative pixel sample →
    port → assert **numerical consistency within tolerance** → then optimise.
- **Spin-up:** warm-start each decade from the previous decade's end state (the loop is
  sequential in time) instead of re-spinning every pixel every decade.

## 7. Proposed pipeline (mirrors the `2026-05` numbering convention)

```
0-setup-db.r                 # reuse/attach ssp-ch.evolanddb (baseline); add rsofun run(s)
1-forcing-climate.r          # CH2025 daily → derive vpd, ppfd (Hargreaves+SOLAR), patm, co2
2-forcing-soil-whc.r         # SSPM (Gupta 2024) → PTF (soil_hydro) → whc, rooting ceiling
3-landcover-crosswalk.r      # evoland 9-class → fAPAR/whc-mod/rootdepth (WASIM-consistent)
4-run-rsofun.r               # per-pixel P-model over decades; warm-start; chunked parallel
5-aggregate-indicators.r     # daily → decadal α, wscal, soil moisture, GPP → add_predictor
6-couple-decadal-loop.r      # feed predictors to transition model; re-run per decade
```

## 8. Open questions / decisions

1. **Radiation source** — Hargreaves-derived Rs (self-contained) vs a satellite/ERA5
   product. Blocks §3.1; pick in Phase 0.
2. **VPD from tmin/tmax** — quantify the Alpine dewpoint≈tmin bias against station RH.
3. **fAPAR source** — RS climatology vs mechanistic LAI→fAPAR (the latter buys WASIM
   reuse).
4. **Coarse fragments & soil depth** — assumption vs Pelletier (2016) ingest.
5. **Ensemble handling** — run all ≤30 CH2025 members (→ predictor uncertainty via
   `id_run`) or a representative subset for the prototype.
6. **Rewrite trigger** — only after Phase 1 profiling; decide batch-driver vs full Rcpp.
7. **9-class ↔ 17-category** reconciliation against the WASIM `.rmp`.

## References

- Sandoval, Prentice & Nóbrega (2024). SPLASH v.2.0. *Geosci. Model Dev.* 17, 4229–4309.
  <https://doi.org/10.5194/gmd-17-4229-2024>
- Stocker et al. (2020). P-model v1.0. *Geosci. Model Dev.* 13, 1545–1581.
  <https://doi.org/10.5194/gmd-13-1545-2020>
- Gupta, Hasler & Alewell (2024). Swiss Soil Property Map. *Geoderma Regional* 36, e00747.
  <https://doi.org/10.1016/j.geodrs.2023.e00747>
- rsofun (fork): <https://github.com/mmyrte/rsofun>
- WASIM documentation (2025), §2.8.3, §4.10 (land-use & soil parameterisation) 
  <https://www.wasim.ch/downloads/doku/wasim/wasim_2025_en.pdf>
