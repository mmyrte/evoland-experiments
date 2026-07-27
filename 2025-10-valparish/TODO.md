# TODO — 2025-10-valparish

> **Frozen.** This experiment is reference-only (see [README](README.md)). The
> items below are the inline TODOs left in the scripts, captured here for the
> record. They are **not** scheduled — where they still matter, they have been
> carried forward into `2026-05-ssp-ch/TODO.md`. Do not start new work here.

## Concluded

- [x] Full pipeline wired end-to-end: setup → LULC → predictors → neighbours →
      covariate selection → GLM transition models → transition rates →
      allocation params (`0-`…`7-`).
- [x] Ingest scripts for population, sonBASE noise, STATENT, and ValPar-local
      GeoTIFFs (the latter later refactored to reproducible HTTP sources in
      `2026-05-ssp-ch/`).

## Open notes (carried forward, not scheduled here)

- [ ] **Bioregions.** Set biogeographic regions
      (`ch.bafu.biogeographische_regionen`). → now `2026-05-ssp-ch` "region ID as
      indicator". (`1-ingest-lulc-data.r:5`)
- [ ] **Deglaciated-area land-use class.** New class based on the glacier
      inventory; interacts with the small-area inclusion threshold. → carried to
      `2026-05-ssp-ch`. (`1-ingest-lulc-data.r:7`)
- [ ] **Arealstatistik 2025 vintage.** Only 1985–2018 selected; AS2025 not yet
      finished at time of writing. (`1-ingest-lulc-data.r:44`)
- [ ] **Population classes** `1_3` vs `1_5` interchangeability check.
      (`2-ingest-preds-pop.r:78`)
- [ ] **Covariate cardinality threshold** (`min_cardinality_abs`) not set to a
      justified value. (`4-covariate-selection.r:6`)
- [ ] **Train/test split** (`sample_frac = 0.3`) not justified. → folded into
      `2026-05-ssp-ch` transition-model validation (MS9 phase 3). (`5-transition-modelling.r:9`)
