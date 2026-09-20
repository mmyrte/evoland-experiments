# Model comparison (2026-09) — evoland-plus against other LULCC tools

**Status:** planned. No code yet; this README is the design, [`TODO.md`](TODO.md) the open
work.

Two purposes, served by one worked example:

1. **Illustrate the use of evoland-plus** end to end on a tractable case, in the manner of the
   package vignettes (`evoland.qmd`, `stochastic-allocation-sensitivity.qmd`) but as a complete
   small study rather than a feature tour.
2. **Compare against other land-use-change modelling tools** on the same data, the same
   calibration and validation periods, and the same metrics, so the comparison says something
   about the methods rather than about the inputs.

The SSP-CH experiment is a full-scale application with scenario demand, a large predictor set
and a lot of case-specific reasoning. It is a poor vehicle for either purpose, which is why
this is a separate sub-project.

## Comparators

| Tool | Access | Role in the comparison |
| --- | --- | --- |
| **evoland-plus** | this repo's pin | Reference implementation. mlr3 transition models + CLUMPY or Dinamica allocation. |
| **lulcc** ([simonmoulds/lulcc](https://github.com/simonmoulds/lulcc)) | R package, open | Closest open-source analogue: allocation (CLUE-S, ordered) over statistically estimated suitability. Comparable at both the suitability and allocation stages. |
| **Dinamica EGO** ([dinamicaego.com](https://dinamicaego.com/)) | free, Linux AppImage / Windows | Two configurations: (a) driven through evoland-plus, which already allocates through it (`alloc_dinamica()`, see the package's `install-dinamica` vignette); (b) standalone, using Dinamica's own Weights-of-Evidence estimator — a naive-Bayes transition-potential model — instead of an mlr3 learner. (a) vs. (b) isolates the estimator from the allocator. |
| **TerrSet Land Change Modeler** ([Clark Labs](https://www.clarku.edu/geospatial-analytics/terrset-liberagis-features/land-change-modeler/)) | proprietary, Windows, licence needed | The widely cited commercial reference. Include if a licence and a machine are available; otherwise document the comparison design and leave it out. |

The design the comparison hangs on: **estimator and allocator are separable**, and most of
these tools bundle them. Running Dinamica both ways gives one clean crossing of that split;
lulcc gives a second estimator/allocator pairing.

## Design sketch

- **Extent.** Small enough to run four toolchains by hand and re-run them. One bioregion, one
  canton, or the ValPar.CH extent — undecided.
- **Data.** Arealstatistik LULC history plus a small, uncontroversial predictor set (terrain,
  distance-to-network, one or two climatological covariates), all already ingested by
  `2026-05-ssp-ch/` and reusable from the same sources.
- **Protocol.** Calibrate on an early observed period pair, allocate forward to a later
  observed period, validate against the observation. This is the backcasting design
  `2026-05-ssp-ch/080-validate-backcasting.qmd` implements, and its metrics apply directly.
- **Metrics.** Figure of merit against a random-allocation null, fuzzy similarity of
  differences masked to changed cells, and per-transition AUC where the tool exposes a
  potential surface. Quantity and allocation disagreement should be reported separately.
- **What is not compared.** Scenario demand: each tool takes the same observed transition
  quantities as given, so the comparison is about *where* change is placed, not how much.

## Relationship to the other sub-projects

Reuses ingestion and validation machinery from `2026-05-ssp-ch/` and is independent of the
dormant `2026-07-ssp-rsofun/`. Nothing in SSP-CH depends on this experiment.
