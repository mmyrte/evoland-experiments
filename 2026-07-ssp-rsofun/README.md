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

### Two model tracks

Both rsofun models are in scope, because the point of the coupling is **ecosystem
productivity** as a land-use-change indicator:

1. **P-model** (fAPAR-forced) — the primary track. Land cover enters diagnostically via
   fAPAR + whc; emits GPP + the SPLASH water balance. Light, and the natural fit for the
   forced-land-cover design.
2. **BiomeE** (prognostic) — the productivity track. Cohort-explicit forest demography;
   emits NPP, biomass, prognostic LAI, and stand dynamics. Note the design difference: it
   is **not** fAPAR-forced — it is *initialised* with a PFT per land-cover class and
   simulates vegetation forward, so it delivers richer productivity/succession indicators
   at higher cost. Run in parallel to the P-model, not in series.

### Decisions locked (2026-07-03)

- **Radiation:** Hargreaves (√(tmax−tmin)·Ra) for shortwave/PPFD — chosen for
  self-containedness. Prototype the estimate in R; **migrate into the rsofun Fortran**
  (SPLASH/SOLAR already computes Ra) once validated, so PPFD becomes internal.
- **fAPAR:** the WASIM way — mechanistic LAI→fAPAR from the WASIM `[landuse_table]`
  monthly arrays (see §3.3). Guarantees cross-model consistency.
- **BiomeE:** included (productivity track, above).
- **Compute:** keep the stock R↔Fortran (`runread_pmodel_f`) path for now; the per-site
  marshalling over 4.1M locations is expected to bottleneck, but the rewrite is deferred
  (see §6).

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
| `rain` **and** `snow` | ✓ | **two separate required columns** — the P-model interface does **not** split precip internally, so split `pr` in preprocessing (temperature threshold / Kienzle sigmoid, à la SPLASH) into rain + snow (mm) | low |
| `vpd` | ✗ | derive from tmin/tmax/tas: VPD = ē_sat(tmax,tmin) − e_a, with dewpoint ≈ tmin (Pa) | **Alpine dry-air bias** in the dewpoint≈tmin assumption — validate vs MeteoSwiss RH stations |
| `ppfd` (mol m⁻² d⁻¹) | ✗ | **DECIDED: Hargreaves** — Rs = k·√(tmax−tmin)·Ra, Ra(lat, DOY); PPFD = Rs·0.5·4.57. Migrate into the Fortran later (SOLAR already computes Ra). | k (≈0.16 interior / 0.19 coastal) needs a CH tuning; validate vs MeteoSwiss/CM SAF |
| `fsun` (sunshine frac) | ✗ | **REQUIRED regardless of ppfd** — `waterbal_splash` needs it for τ and net-longwave (`run_pmodel_f_bysite.R:322`, `waterbal_splash.mod.f90:201/213`). Derive from the Hargreaves transmissivity τ = Rs/Ra via Ångström–Prescott: fsun = (τ − a)/b. One derivation yields both ppfd and fsun. | ties to the same k tuning |
| `netrad` | — | **NOT needed** — currently *ignored* as forcing; SPLASH computes net radiation internally (`run_pmodel_f_bysite.R:319`, docstring). Do not supply. | — |
| `patm` | ✗ | from DEM elevation via barometric formula (`calc_patm(elv)`), Pa | low |
| `co2` | ✗ | SSP concentration pathway, annual global mean (Meinshausen et al. 2020) — one series per SSP, ppm | low |
| `fapar` | ✗ | **land-cover coupling handle** — WASIM LAI→fAPAR, see §3.3 | central |

Mandatory columns the interface NA-checks: `temp, rain, vpd, snow, co2, fapar, patm,
tmin, tmax`, plus one of {`ppfd`, `fsun`} — but `fsun` is needed anyway (above), so we
supply both. `tsoil` is optional (unused here). Verified against the fork at
`R/run_pmodel_f_bysite.R:205-247`.

Two genuine gaps remain: **shortwave (Hargreaves-derived → ppfd + fsun)** and **fAPAR**.
Everything else is present or cheaply derivable — and `netrad` drops out entirely.

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

### 3.3 Land cover — WASIM classification → fAPAR / whc / rooting

**DECIDED (2026-07-03): use the WASIM land-use classification directly**, *not* the
9-class SSP-CH schema. The land-cover state feeding rsofun is expressed in WASIM's
categories (the 17-category `[multilayer_landuse]` / 19-entry `[landuse_table]`), so each
class maps 1:1 to its own `[landuse_table]` entry — no semantic aggregation or
evoland-9→WASIM crosswalk guesswork. The SSP-CH land cover is brought into this scheme
via the existing `AS0409_17Cat_to_WaSiM_LansuseTable.rmp`. The **9-class SSP-CH labelling
is backported later, without rsofun** (a relabelling of the same pixels), so nothing is
lost by deviating here.

Consequence: `3-landcover-crosswalk.r` becomes a straight **parser** of the
`[landuse_table]` (now committed at `2026-07-ssp-rsofun/wasim_control_sample.txt`) into a
per-class daily table of fAPAR / albedo / whc, keyed by WASIM land-use ID — no mapping
decisions, and one source of truth shared with WASIM.

**Populate fAPAR the WASIM way.** The WASIM `[landuse_table]` (verified in
`wasim_control_sample.txt`, 19 single land-use classes) gives, per class, **12 monthly
values** (at mid-month Julian days 15, 46, 74, … 349) for `LAI`, `VCF`, `Albedo`,
`RootDepth`, and `rsc`, with `k_extinct = 0.3` from `[multilayer_landuse]`. We build

    fAPAR_month = VCF · (1 − exp(−k_extinct · LAI))      # Beer–Lambert, k_extinct = 0.3

per class, interpolate the 12 monthly points to daily (linear — the same reconstruction
WASIM does internally between sample days), and pass that as the rsofun `fapar` forcing.
Albedo (monthly) feeds net radiation; RootDepth × soil PAWC feeds `whc`. This reuses
WASIM's exact phenology, so the interim and eventual WASIM runs share one source of
truth.

Handling notes when consuming the WASIM arrays in rsofun:

- **Monthly → daily:** linear interpolation across the mid-month JulDays (cyclic at
  year boundary).
- **Elevation (`AltDep`):** each WASIM entry carries an `AltDep` altitude-dependence term
  that shifts phenology up-slope. This matters across Switzerland's elevation gradient
  (delayed/compressed alpine growing season). Apply it using the 100 m DEM; make it a
  toggle so we can test its impact.
- **Multi-layer classes:** WASIM stacks canopies (e.g. `Wald` = tree layer + understory).
  The P-model is big-leaf/single-layer, so use an **effective LAI** (sum of layers) →
  single fAPAR.
- **`rsc` is ignored:** the P-model derives stomatal conductance endogenously (optimality),
  so WASIM's canopy resistance is not used — fewer free parameters, by design.
- **`whc` is static, RootDepth is monthly:** collapse RootDepth to one representative
  value per class (growing-season max) for the single `whc`.

The 19 `[landuse_table]` classes are used as-is (IDs → names, peak growing-season LAI):
1 teilversiegelte_Flaechen (1) · 2 versiegelte_Flaechen (1) · 3 vegetationslose_Flaechen
(0.5) · 4 spaerliche_Vegetation (2) · 5 Intensiv-Gruenland (4) · 6 Extensiv-Gruenland (3)
· 7 Intensiv-Ackerland (5) · 8 Extensiv-Ackerland (4) · 9 Heidevegetation (4) · 10
Busch-Kraut-Vegetation (5) · 11 Laubwald (8) · 12 Nadelwald (10) · 13 Mischwald (8) · 14
locker_baumbestanden (4) · 15 Moore_Suempfe (4) · 16 Wasserflaechen (1) · 17 horticulture
(5) · 22 Eisflaechen (1) · 23 Firnflaechen (1). Forest type (Laub/Nadel/Misch) is a
first-class distinction here, resolved by the input land-cover map rather than a default.

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
- **GPP** (P-model) — productivity suitability
- **NPP, biomass, prognostic LAI** (BiomeE) — ecosystem-productivity / succession
  indicators, the motivation for including BiomeE
- optionally net radiation / PPFD, snow (SWE, snow-cover duration)

Report decadal means/sums **and** variability/extremes (the tails often drive suitability).

## 6. Performance & the C++/Rcpp question

Scale: ~4.1M hectare pixels × ~30 yr × 365 d ≈ 4.5×10¹⁰ cell-days per member, × up to
30 members ≈ 1.3×10¹² cell-days. rsofun's biophysics is compiled **Fortran** (P-model ≈
9 `*_pmodel.mod.f90` modules + `waterbal_splash.mod.f90`, bridged by `wrappersc.c`); the
Fortran itself is fast. The likely bottleneck is **R↔Fortran marshalling per site**
(`runread_pmodel_f` is called one gridcell at a time, rebuilding nested forcing frames),
not the numerics. **DECIDED:** keep the stock R↔Fortran path for now and accept the
per-site overhead; the rewrite stays deferred until Phase 1 profiling quantifies it.

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
    and headless HPC use (no R in the hot path). Translate `waterbal_splash` + the
    P-model `gpp`/`photosynth`/`plant` modules first; BiomeE is heavier (cohort linked
    lists) and would be a later, separate translation effort.
    - Either path requires the workflow the user described: a **golden-master test
    harness** capturing stock-rsofun outputs on a representative pixel sample →
    port → assert **numerical consistency within tolerance** → then optimise.
- **Spin-up:** warm-start each decade from the previous decade's end state (the loop is
  sequential in time) instead of re-spinning every pixel every decade.

## 7. Proposed pipeline (mirrors the `2026-05` numbering convention)

```
0-setup-db.r                 # reuse/attach ssp-ch.evolanddb (baseline); add rsofun run(s)
1-forcing-climate.r          # CH2025 daily → vpd, ppfd+fsun (Hargreaves), rain/snow, patm, co2
2-forcing-soil-whc.r         # SSPM (Gupta 2024) → PTF (soil_hydro) → whc, rooting ceiling
3-landcover-fapar.r          # parse WASIM [landuse_table] → daily fAPAR/albedo/whc per class
4-run-rsofun.r               # per-pixel P-model (+BiomeE) over decades; warm-start; chunked
5-aggregate-indicators.r     # daily → decadal α, wscal, soil moisture, GPP → add_predictor
6-couple-decadal-loop.r      # feed predictors to transition model; re-run per decade
```

## 8. Open questions / decisions

Resolved 2026-07-03: radiation = Hargreaves (→ into Fortran); fAPAR = WASIM LAI→fAPAR;
BiomeE included; keep R↔Fortran; **land cover = WASIM classification directly** (SSP-9
relabelled later, no crosswalk); netrad not required; forcing needs `fsun` + a rain/snow
split (verified against the fork). Remaining:

1. **Daily CH2025 input format** — the baseline ingests CH2025 *summary indicators*; the
   process run needs the *daily* gridded netCDFs (pr/tas/tasmax/tasmin, 30 members) that
   live on the target machine. Need their on-disk layout (paths, netCDF var names, ensemble
   dim) to wire `1-forcing-climate.r` I/O. The derivation core is format-independent and
   can be written now against a defined input contract.
2. **VPD from tmin/tmax** — quantify the Alpine dewpoint≈tmin bias against station RH.
3. **Hargreaves coefficient `k`** — single CH value vs elevation/region tuning; validate
   a subset vs MeteoSwiss/CM SAF radiation.
4. **Coarse fragments & soil depth** — assumption vs Pelletier (2016) ingest.
5. **Which SSPs** — baseline excludes SSP2; confirm SSP1/3/4/5 CO₂ pathways for the runs.
6. **Ensemble handling** — run all ≤30 CH2025 members (→ predictor uncertainty via
   `id_run`) or a representative subset for the prototype.
7. **Rewrite trigger** — only after Phase 1 profiling; decide batch-driver vs full Rcpp.

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
