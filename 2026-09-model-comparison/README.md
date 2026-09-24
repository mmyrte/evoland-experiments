# Model comparison (2026-09) — evoland-plus against other LULCC tools

**Status:** planned. No code yet; this README is the design, [`TODO.md`](TODO.md) the open
work.

Two purposes, served by one worked example:

1. **Illustrate the use of evoland-plus** end to end on a tractable case, in the manner of the
   package vignettes (`evoland.qmd`, `stochastic-allocation-sensitivity.qmd`) but as a complete
   small study rather than a feature tour.
2. **Compare against other land-use-change modelling tools** on the same data, the same
   calibration and validation periods, and the same metrics, so the comparison says something about
   the methods rather than about the inputs. Should only be one section of the model description.

The SSP-CH experiment is a full-scale application with scenario demand, a large predictor set
and a lot of case-specific reasoning. It is a poor vehicle for either purpose, which is why
this is a separate sub-project.

## Comparators

| Tool | Access | Role in the comparison |
| --- | --- | --- |
| **evoland-plus** | this repo's pin | Reference implementation. mlr3 transition models + CLUMPY or Dinamica allocation. |
| **lulcc** ([simonmoulds/lulcc](https://github.com/simonmoulds/lulcc)) | R package, open | Closest open-source analogue: allocation (CLUE-S, ordered) over statistically estimated suitability. Comparable at both the suitability and allocation stages. |
| **Dinamica EGO** ([dinamicaego.com](https://dinamicaego.com/)) | free but closed source, Linux AppImage / Windows | Two configurations: (a) driven through evoland-plus, which already allocates through it (`alloc_dinamica()`, see the package's `install-dinamica` vignette); (b) standalone, using Dinamica's own Weights-of-Evidence estimator — a naive-Bayes transition-potential model — instead of an mlr3 learner. (a) vs. (b) isolates the estimator from the allocator. |
| **TerrSet Land Change Modeler** ([Clark Labs](https://www.clarku.edu/geospatial-analytics/terrset-liberagis-features/land-change-modeler/)) | free but closed source, Windows | Recently changed license, still closed source. License indicates commercial use is permissible, but actually understanding the routines via reverse engineering is expressly prohibited. Is scriptable, and may even be runnable via WINE, so we could try |

The design the comparison hangs on: **estimator and allocator are separable**, and most of
these tools bundle them. Running Dinamica both ways gives one clean crossing of that split;
lulcc gives a second estimator/allocator pairing.

## Design sketch for comparison.

- **Extent.** Push it to the point of memory/computational cost. Start with a single canton (Bern)
  then go to the national scale.
- **Data.** Arealstatistik LULC history plus a small, uncontroversial predictor set (terrain,
  distance-to-road, soil organic fraction, precipitation), all already ingested by
  `2026-05-ssp-ch/` and reusable from the same sources. Latest Arealstatistik from
  https://data.geo.admin.ch/ch.bfs.arealstatistik/arealstatistik-zeitreihe/arealstatistik-zeitreihe_2056.csv.zip
- **Protocol.** Calibrate on the first three periods (or the transition from 2. to 3.), allocate
  forward to 4th (last) observed period, validate against the observation. This is the backcasting
  design `2026-05-ssp-ch/080-validate-backcasting.qmd` implements, and its metrics apply directly.
- **Metrics.** Figure of merit against a random-allocation null, fuzzy similarity of
  differences masked to changed cells, and per-transition AUC where the tool exposes a
  potential surface. Quantity and allocation disagreement should be reported separately.
- **What is not compared.** Scenario demand: each tool takes the same observed transition
  quantities as given, so the comparison is about *where* change is placed, not how much.

## Relationship to the other sub-projects

Reuses ingestion and validation machinery from `2026-05-ssp-ch/` and is independent of the
dormant `2026-07-ssp-rsofun/`. Nothing in SSP-CH depends on this experiment.

## Publication Venue

Go with Geoscientific Model Development:
- follow https://www.geoscientific-model-development.net/submission.html and
  https://www.geoscientific-model-development.net/about/manuscript_types.html
- and take inspiration at least from moulds lulcc: https://doi.org/10.5194/gmd-8-3215-2015
- check for other land use change models https://gmd.copernicus.org/model_description_paper.html
  (most seem to relate to land surface modelling at roughly continental or global scales)

Keep Environmental Modelling & Software as backup; if two options are equally desirable for GMD, EMS
should tip the scales in favour of either one of them.
- Follow https://www.sciencedirect.com/journal/environmental-modelling-and-software/publish/guide-for-authors
- Structural inspiration from papers like https://doi.org/10.1016/j.envsoft.2018.07.010 iCLUE 
- or https://doi.org/10.1016/j.envsoft.2026.106917 CLUinPy

## Reproducibility in different environments

The argument here doesn't run along Mazy/Longaretti's statistical argument that the other allocation
routines are biased, it's about the usefulness:

- The CLUE family takes in absolute suitability (CHECK this), which is not the same as estimating transition probability and allocating on top of that (CHECK this); this is a slightly different modelling paradigm. Regarding user friendliness, there's lulcc (depending on deprecated R packages, don't know if it is effective at working with a large domain) and CLUinPy; plus i found some DynaCLUE C++ code that's spicy to compile, don't know about the other clue members.
- The Dinamica allocator is biased because of pruning (thesis ch. 3.8); its license is restrictive; its code is not reachable; although the dinamica virtual machine is a very advanced piece of technology, it does not have the same community effects like an R or python package.
- The IDRISI LCM is apparently biased anyways, plus it's mostly a point & click adventure

Slightly less 

- IDRISI TerrSet liberaGIS LCM
  - Dominant practice: GUI-driven; Mazy et al. have reproduced the LCM logic
  - Executable publication artifact: would require scripting (-> mmyrte/clumpy)
  - Exact replay: doubtful
  - Auditability: low
  - Value: useful as a widely cited black-box comparator
- Dinamica EGO
  - Dominant practice: visual model construction, but backed by a serializable model
  - Executable artifact: .ego/.egoml (xml based)
  - Exact replay: realistic with inputs, seeds, and version
  - Auditability: medium to high
  - Value: strongest established modelling system for your comparison
- CLUMPY
  - Dominant practice: Python scripting
  - Executable artifact: Python program and configuration
  - Exact replay: realistic if dependency and RNG state are pinned
  - Auditability: high
  - Value: experimental reference implementation
- R lulcc
  - Dominant practice: R scripting
  - Executable artifact: R scripts, R Markdown or Quarto, and serialized objects
  - Exact replay: realistic with package lockfiles and seed control
  - Auditability: high
  - Value: strongest statistical and validation workbench
- Python CLUinPy
  - Dominant practice: Python scripting
  - Executable artifact: Python program and configuration
  - Exact replay: realistic if dependency and RNG state are pinned
  - Auditability: high
  - Value: Python reimplementation of the CLUMondo framework
