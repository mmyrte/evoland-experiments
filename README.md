# evoland-experiments

Specific [evoland-plus](https://github.com/ethzplus/evoland-plus) experiments; code not
necessarily suitable for general consumption. Each dated sub-directory is a
self-contained experiment with its own numbered pipeline, `README.md` (analytic purpose)
and `TODO.md` (open work).

## Sub-projects

| Status    | Sub-project                                              | Purpose                                                                                                                                  |
| --------- | -------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------- |
| active    | [`2026-05-ssp-ch/`](2026-05-ssp-ch/)                     | Re-implementation of the [SSP-CH scenarios](https://ssp-ch-szenarien.wsl.ch/en/) on the new evoland-plus. Empirical/statistical transition model, reproducible data sources, elicited SSP land-use demand. [README](2026-05-ssp-ch/README.md) · [TODO](2026-05-ssp-ch/TODO.md) |
| planned   | [`2026-09-model-comparison/`](2026-09-model-comparison/) | Worked illustration of evoland-plus against other LULCC modelling tools (lulcc, TerrSet LCM, Dinamica EGO). [README](2026-09-model-comparison/README.md) · [TODO](2026-09-model-comparison/TODO.md) |
| dormant   | [`2026-07-ssp-rsofun/`](2026-07-ssp-rsofun/)             | Process-based land-use-suitability predictors from [rsofun](https://github.com/mmyrte/rsofun) (P-model + SPLASH) as an interim stand-in for WASIM coupling. Forcing steps written, nothing consumes them. Kept as a design record. [README](2026-07-ssp-rsofun/README.md) · [TODO](2026-07-ssp-rsofun/TODO.md) |
| reference | [`2025-10-valparish/`](2025-10-valparish/)               | Stub that co-evolved with early evoland-plus development. Not expected to run against any current commit. [README](2025-10-valparish/README.md) |

## Cross-cutting threads

Tracked here until they have a home of their own.

- **Interventions interface in evoland-plus.** The intervention machinery should not live in
  the main package; evoland-plus should expose an interface that ad-hoc interventions are
  built on. See `2026-05-ssp-ch/TODO.md` § Interventions for the current thinking.
- **Ecosystem-services package / standardised land-use ⇄ ES interface.** Post-processing
  land-use projections through ES models, possibly a new repo or R/Python package.
  See <https://github.com/ethzplus/evoland-plus-HPC/>.
- **Whitebox simple water routing.** Lightweight lateral routing (e.g. WhiteboxTools) as an
  interim before the full WASIM coupling; the routing gap is documented in
  `2026-07-ssp-rsofun/README.md` §1.

## Environment

Setup via [rv](https://github.com/A2-ai/rv): `rv init; rv sync` installs CRAN packages
and the pinned evoland-plus commit. Currently using `module load R/4.6.1` rain; not yet
set up on Euler.

## Conventions

- **Three-digit numbering: `NNN[d]-slug.{qmd,r}`.** used with globbing/lexical sort.
  - Steps **sharing a number** are serially independent and can be run in parallel.
  - Serial dependence in related scripts takes the next number, e.g.
    `021-ingest-preds-ch2025-download` fetches, `022-ingest-preds-ch2025-etl` consumes
    what it fetched.
  - Multiples of ten mark pipeline phases (`010-` ingest, `030-` neighbours, `060-`
    modelling, …).
- **A trailing `d`** marks a diagnostic which is associated with a pipeline stage, but not
  part of the serial dependence chain. As such it is safe to skip for reproduction, but
  helpful for understanding decisions/thresholds etc.
- **Run a pipeline using `execute-all.sh 'folder/glob*-pattern.qmd'`.**
  - Optional parallelism using
    `./execute-all.sh --workers 4 '2026-05-ssp-ch/0[0-3][0-9]-*.qmd'`;
    this glob would run all stages from 001 to 039 without diagnostics.
- **Quarto pipelines.** Steps are `.qmd` rendered to self-contained HTML, so
  rationale can live beside the code.
  - The repo-root `_quarto.yml` sets `execute-dir: project` (so the root `.Rprofile` and
    relative paths resolve) and `freeze: auto` (only re-render when code changes). Needs
    the Quarto CLI and git-lfs.
- **HTML reports land in `html-reports/`** beside the sources they come from, e.g.
  `2026-05-ssp-ch/html-reports/050-covariate-selection.html`. Quarto writes a report next to
  its `.qmd`, so a post-render hook (`quarto-post-render.sh`, wired up in `_quarto.yml`)
  files it; this happens for `quarto render <file>` and for a whole-project render alike, so
  the step directories hold sources only.
  - Rendered HTML with embedded graphics and richer visualisations (e.g. leaflet) is
    LFS-tracked (`.gitattributes`) and committed ad-hoc; the `_freeze/` cache is git-ignored.
- **State lives in a DuckLake.** Each experiment builds a git-ignored `*.evolanddb`.
- **Data provenance.** Ingest scripts ideally download from public sources and verify
  md5sums via `evoland::download_and_verify()` into the evoland cache, ensuring maximum
  reproducibility.
