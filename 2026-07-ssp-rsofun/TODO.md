# TODO — 2026-07-ssp-rsofun

Task tracker for the process-based coupling (MS9 phase 2/3). The design rationale,
data contracts, and decisions live in [`README.md`](README.md); this file tracks
what is built vs. outstanding. See the README's §6 (phased plan) and §8 (open
questions) for detail.

Legend: ⬜ not started · 🟡 in progress / partial · ✅ done

---

## Concluded

- [x] Design & data-contract write-up (`README.md`): coupling architecture,
      CH2025 forcing gaps, SSPM soil → whc, WASIM `[landuse_table]` → fAPAR, output
      predictors, performance plan. Key decisions locked 2026-07-03.
- [x] `1-forcing-climate.r` — CH2025 daily → derived forcings (vpd, ccov via
      Hargreaves, rain/snow split, patm; SPLASH does PPFD internally).
- [x] `2-forcing-soil-1-download.r` — fetch SSPM mean GeoTIFFs (Zenodo 7821650).
- [x] `2-forcing-soil-2-whc.r` — SSPM (PTF / `soil_hydro`) → per-layer AWC profile.
- [x] `3-landcover-fapar.r` — parse WASIM `[landuse_table]` → daily
      fAPAR/albedo/rootdepth per class.
- [x] HPC provisioning docs — `hpc-setup.md`, `spack.yaml`, `spack-available.txt`,
      `install-missing.R`.

---

## Pipeline — remaining scripts (README §7)

- [ ] **`0-setup-db.r`** — attach/reuse `ssp-ch.evolanddb` baseline; register the
      rsofun run(s) (`id_run`).
- [ ] **`4-run-rsofun.r`** — per-pixel P-model (+ BiomeE) over decades; warm-start
      each decade from the previous end state; chunked for parallelism.
- [ ] **`5-aggregate-indicators.r`** — daily → decadal α (AET/PET), `wscal`, soil
      moisture, GPP (+ BiomeE NPP/biomass/LAI) → `db$add_predictor`.
- [ ] **`6-couple-decadal-loop.r`** — feed rsofun predictors to the transition
      model; re-run per decade to close the land-cover ↔ water/energy feedback.

---

## Forcing & data gaps

- [ ] **AltDep elevation phenology.** `3-landcover-fapar.r` parses `AltDep` but the
      WASIM shift formula is not transcribed; `apply_altdep` is off. Transcribe the
      exact WASIM adjustment before enabling (matters across the CH elevation
      gradient). (`3-landcover-fapar.r:28`; README §8.1)
- [ ] **Soil download pattern.** Align `2-forcing-soil-1-download.r` with the shared
      `download_and_verify` ingestion pattern used elsewhere.
      (`2-forcing-soil-1-download.r:17`)
- [ ] **VPD dewpoint bias.** Quantify the Alpine `dewpoint ≈ tmin` dry bias against
      MeteoSwiss RH stations. (`1-forcing-climate.r:64`; README §8.2)
- [ ] **Hargreaves coefficient `k`.** Single CH value vs. elevation/region tuning;
      validate against MeteoSwiss / CM SAF radiation. (README §8.3)
- [ ] **Coarse fragments & soil depth.** Assume defaults vs. ingest Pelletier (2016)
      soil+sediment thickness. (README §8.4)
- [ ] **CO₂ / GWL mapping.** Confirm SSP1/3/4/5 CO₂ pathways and the `{SSP × period →
      GWL, CO₂}` lookup for the runs. (README §3.5, §8.5)
- [ ] **Ensemble handling.** All ≤30 CH2025 members (→ predictor uncertainty via
      `id_run`) vs. a representative subset for the prototype. (README §8.6)

---

## Performance (README §6)

- [ ] **Phase 0 — prototype coupling** on a small AOI (one bioregion/catchment);
      sanity-check the feedback signal is real (Budyko pre-screen).
- [ ] **Phase 1 — scale by embarrassing parallelism** over pixels (Slurm array /
      `mirai`/`future`); **profile** to locate the bottleneck.
- [ ] **Phase 2 — optimise only if profiling demands it:** batch Fortran/C driver
      (low risk) vs. full Rcpp/C++ SoA rewrite. Requires a golden-master test
      harness (stock-rsofun outputs → port → assert numerical consistency).
      (README §8.7)

---

## Related / cross-cutting

- [ ] **Whitebox simple water routing?** Evaluate WhiteboxTools as a lightweight
      *lateral* routing option to partially cover what rsofun/SPLASH omits (routing
      across the basin DAG is otherwise deferred to WASIM). (reminder "whitebox
      simple water routing"; README §1 "Deferred to WASIM")
- [ ] **9-class SSP-CH backport.** Relabel the WASIM-classified pixels into the
      9-class SSP-CH schema (without rsofun) for consistency with the baseline.
      (README §3.3, §4 open reconciliation item)
