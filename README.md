# evoland-experiments

Specific [evoland-plus](https://github.com/ethzplus/evoland-plus) experiments;
code not suitable for general consumption. Each dated sub-directory is a
self-contained experiment with its own numbered R pipeline, `README.md`
(analytic purpose) and `TODO.md` (task tracking).

## Sub-projects

| Status    | Sub-project                                  | Purpose                                                                                                                                                                                                                                                                                                    | Docs                                                                        |
| --------- | -------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------- |
| archived  | [`2025-10-valparish/`](2025-10-valparish/)   | Stub that co-evolved with early evoland-plus development. Kept for reference; not expected to run against any specific evoland-plus commit.                                                                                                                                                                | [README](2025-10-valparish/README.md)                                       |
| active    | [`2026-05-ssp-ch/`](2026-05-ssp-ch/)         | Re-implementation of the [SSP-CH scenarios](https://ssp-ch-szenarien.wsl.ch/en/) on the new evoland-plus, reusing the elicited land-use demand from `NCCS-SSP-scenarios/Tools/NCCS_simulation_LULC_areas.xlsx` but with new, reproducible data sources. **Baseline** (purely empirical/statistical transition model). | [README](2026-05-ssp-ch/README.md) · [TODO](2026-05-ssp-ch/TODO.md)         |
| abandoned | [`2026-07-ssp-rsofun/`](2026-07-ssp-rsofun/) | Would have extended the baseline with **process-based** land-use-suitability predictors from [rsofun](https://github.com/mmyrte/rsofun) (P-model + SPLASH), as an interim stand-in for the eventual WASIM coupling. **Abandoned for now** at the forcing steps; kept as a design record for the WASIM work. | [README](2026-07-ssp-rsofun/README.md) · [TODO](2026-07-ssp-rsofun/TODO.md) |

## Milestone MS9 — SSP scenarios in evoland-plus

The active work is organised as one milestone in three phases, mapped onto the
sub-projects:

- **MS9 phase 1/3 — replicate SSP scenarios in evoland-plus** → `2026-05-ssp-ch/`.
  Reproduce the SSP-CH land-use futures on the new evoland-plus with an empirical
  transition model. This is the baseline everything else builds on.
- **MS9 phase 2/3 — minimal evoland-plus ↔ biophysical coupling** → `2026-07-ssp-rsofun/`,
  **abandoned for now**. Would add process-based (rsofun/SPLASH) suitability predictors and
  close a decadal land-cover ↔ water/energy feedback loop; the forcing steps are written,
  the coupling is not.
- **MS9 phase 3/3 — validate transition models** → primarily `2026-05-ssp-ch/`
  (see its TODO "Transition modelling & validation"). Validate the mlr3-based
  transition models, including **backcasting** against observed Arealstatistik
  periods.

## Beyond MS9

Tracking these here for now.

- **Ecosystem-services package / standardised land-use ⇄ ES interface.** May involve
  setting up a new repo / R or Python package for post-processing land use projections
  using ecosystem services models, see <https://github.com/ethzplus/evoland-plus-HPC/>
  Tracked here until it has a home.
- **Whitebox simple water routing.** A lightweight lateral-routing option (e.g.
  WhiteboxTools) as an interim before the full WASIM coupling — see
  `2026-07-ssp-rsofun/TODO.md` where the routing gap is documented.

## Environment

Environment setup using [rv](https://github.com/A2-ai/rv). Make sure your R installation
is not broken (`module load R/4.5.3` on rain leads to weird s4 methods dispatch errors,
4.6.1 works).

`rv init; rv sync` installs CRAN packages and the pinned evoland-plus commit.

## Conventions

- **Three-digit numbering: `NNN[d]-slug.{qmd,r}`.** Every sub-project is a sequence of
  numbered steps, and the number is the whole ordering mechanism — a plain lexical sort of
  the file names is the run order. All three sub-projects now use it.
  - **Steps that share a number are independent** and may run in any order, or at the same
    time (the five `020-ingest-preds-*`, say). This is the load-bearing part of the
    convention: it is what makes `--workers N` safe.
  - **A step that depends on another in the same family takes the next number.** `020-` are
    the plain predictor ingests, `021-ingest-preds-ch2025-download` fetches, and
    `022-ingest-preds-ch2025-etl` consumes what it fetched. Same for
    `010-forcing-soil-download` → `011-forcing-soil-whc` in the rsofun stub. Never encode
    that dependency in the slug (`…-1-download`, `…-2-etl`) — a shared number claims the
    two are independent, and a parallel run will take you up on it.
  - **Multiples of ten mark the pipeline's phases** (`010-` ingest, `030-` neighbours,
    `060-` modelling, …), which leaves nine free numbers to insert a dependent step into
    without renumbering anything. `001-setup-db` comes before all of it.
  - **A trailing `d` marks a diagnostic** (below), and sorts after its own stage and before
    the next: `020- … < 020d- … < 021-`.
- **Core vs. diagnostic steps.** `NNN-slug.qmd` is a **core** step (mutates the DuckDB /
  produces canonical outputs). `NNNd-slug.qmd` is an **optional diagnostic** for stage
  `NNN` — read-only, renders a verification/visualisation report, safe to skip.
- **Ordering by number beats ordering by family.**
  `2026-05-ssp-ch/051-ingest-preds-ch2025-3-gwl.qmd` belongs to the CH2025 ingest family by
  slug, but projected climate can only be materialised for the *selected* predictor set (the
  full cross product does not fit), so it must run after `050-covariate-selection`. Its
  number says so and its family does not; the number wins. Run it early and it fails loudly
  rather than writing anything.
- **Literate Quarto pipelines.** Steps are `.qmd` rendered to self-contained HTML, so
  rationale lives beside the code. A repo-root `_quarto.yml` sets `execute-dir: project`
  (so the root `.Rprofile` / rv activation and relative paths resolve) and
  `freeze: auto` (expensive core steps execute once — re-rendering a report never
  re-runs the model or re-downloads data). Needs the Quarto CLI + git-lfs on the run
  machine.
- **Running a pipeline: `execute-all.sh`.** One entrypoint, one glob, in stage order:

  ```sh
  ./execute-all.sh --workers 4 '2026-05-ssp-ch/0*.qmd'   # or -j 4; default is 1, i.e. serial
  ```

  It groups the matched files by leading number and runs the groups **strictly in order**,
  up to `N` steps of one group at a time. Concurrency is safe because steps sharing a
  number are independent (above) and the `evoland_db` DuckLake catalog takes concurrent
  writers, so the several `020-ingest-preds-*` ingest at once; `NNNd-*` diagnostics form a
  stage of their own and still run after the steps they report on. Above one worker each
  step's output is buffered and printed as one block when it finishes, so nothing
  interleaves; a failing step stops the pipeline once the steps already running have
  finished. The glob is the only selector — narrow it to a single stage
  (`'…/020-*.qmd'`) or to the diagnostics alone (`'…/*d-*.qmd'`).
- **Reports via git-LFS.** Rendered HTML reports are git-LFS-tracked (`.gitattributes`)
  and committed ad-hoc at checkpoints; the `_freeze/` cache is git-ignored.
- **State lives in DuckDB.** Each experiment builds a `*.evolanddb` (folder of parquet
  files) via the evoland-plus `evoland_db` R6 class; predictors are ingested through
  `db$add_predictor` as a cheap way of ensuring foreign relations (no constraint checks
  as with a properly schematized RDBMS; allows tremendous speedup.)
- **Data provenance.** Ingest scripts download from public HTTP(S) sources and
  verify md5sums via `download_and_verify` into the evoland cache. Detailed
  source documentation lives alongside the scripts (see each sub-project README).
