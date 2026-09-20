# evoland-experiments

Specific [evoland-plus](https://github.com/ethzplus/evoland-plus) experiments;
code not suitable for general consumption. Each dated sub-directory is a
self-contained experiment with its own numbered pipeline, `README.md`
(analytic purpose) and `TODO.md` (open work).

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

Setup via [rv](https://github.com/A2-ai/rv): `rv init; rv sync` installs CRAN packages and the
pinned evoland-plus commit. Make sure your R installation is not broken — `module load R/4.5.3`
on rain gives S4 method dispatch errors; 4.6.1 works.

## Conventions

- **Three-digit numbering: `NNN[d]-slug.{qmd,r}`.** The number is the whole ordering
  mechanism: a lexical sort of the file names is the run order.
  - **Steps sharing a number are independent** and may run in any order or at the same time
    (the several `020-ingest-preds-*`, say). This is what makes `--workers N` safe.
  - **A step depending on another in the same family takes the next number.** `021-ingest-preds-ch2025-download`
    fetches, `022-ingest-preds-ch2025-etl` consumes what it fetched. Never encode that
    dependency in the slug — a shared number claims independence and a parallel run will act
    on that claim.
  - **Multiples of ten mark pipeline phases** (`010-` ingest, `030-` neighbours, `060-`
    modelling, …), leaving nine free numbers to insert a dependent step without renumbering.
    `001-setup-db` comes first.
  - **A trailing `d` marks a diagnostic**, sorting after its own stage and before the next:
    `020- … < 020d- … < 021-`.
- **Core vs. diagnostic steps.** `NNN-slug.qmd` is a **core** step: it mutates the DuckDB or
  produces canonical outputs. `NNNd-slug.qmd` is an **optional diagnostic** for stage `NNN` —
  read-only, renders a verification report, safe to skip.
- **The number wins over the family.** `2026-05-ssp-ch/051-ingest-preds-ch2025-3-gwl.qmd`
  belongs to the CH2025 ingest family by slug but must run after `050-covariate-selection`,
  because projected climate can only be materialised for the selected predictor set. Run it
  early and it fails loudly rather than writing anything.
- **Literate Quarto pipelines.** Steps are `.qmd` rendered to self-contained HTML, so
  rationale lives beside the code. The repo-root `_quarto.yml` sets `execute-dir: project`
  (so the root `.Rprofile` and relative paths resolve) and `freeze: auto` (re-rendering a
  report never re-runs the model or re-downloads data). Needs the Quarto CLI and git-lfs.
- **Running a pipeline: `execute-all.sh`.** One entrypoint, one glob, in stage order:

  ```sh
  ./execute-all.sh --workers 4 '2026-05-ssp-ch/0*.qmd'   # or -j 4; default 1, i.e. serial
  ```

  It groups matched files by leading number and runs the groups strictly in order, up to `N`
  steps of one group at a time. The `evoland_db` DuckLake catalog takes concurrent writers.
  Above one worker each step's output is buffered and printed as one block, so nothing
  interleaves; a failing step stops the pipeline once the steps already running have
  finished. The glob is the only selector — narrow it to one stage (`'…/020-*.qmd'`) or to
  the diagnostics alone (`'…/*d-*.qmd'`).
- **Reports via git-LFS.** Rendered HTML is LFS-tracked (`.gitattributes`) and committed at
  checkpoints; the `_freeze/` cache is git-ignored.
- **State lives in DuckDB.** Each experiment builds a `*.evolanddb` (a folder of parquet
  files) via the evoland-plus `evoland_db` R6 class. Predictors go in through
  `db$add_predictor`, which enforces foreign relations cheaply — no constraint checks as in a
  fully schematised RDBMS, which is where the speedup comes from.
- **Data provenance.** Ingest scripts download from public HTTP(S) sources and verify md5sums
  via `download_and_verify` into the evoland cache. Per-source documentation lives in the
  step documents.
