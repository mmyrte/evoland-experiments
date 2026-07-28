# evoland-experiments

Specific [evoland-plus](https://github.com/ethzplus/evoland-plus) experiments;
code not suitable for general consumption. Each dated sub-directory is a
self-contained experiment with its own numbered R pipeline, `README.md`
(analytic purpose) and `TODO.md` (task tracking).

## Sub-projects

| Status   | Sub-project                                  | Purpose                                                                                                                                                                                                                                                                                                    | Docs                                                                        |
| -------- | -------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------- |
| archived | [`2025-10-valparish/`](2025-10-valparish/)   | Stub that co-evolved with early evoland-plus development. Kept for reference; not expected to run against any specific evoland-plus commit.                                                                                                                                                                | [README](2025-10-valparish/README.md) · [TODO](2025-10-valparish/TODO.md)   |
| active   | [`2026-05-ssp-ch/`](2026-05-ssp-ch/)         | Re-implementation of the [SSP-CH scenarios](https://ssp-ch-szenarien.wsl.ch/en/) on the new evoland-plus, reusing the land-use demand curves from `NCCS-SSP-scenarios/Tools/Transition_Tables.xlsx` but with new, reproducible data sources. **Baseline** (purely empirical/statistical transition model). | [README](2026-05-ssp-ch/README.md) · [TODO](2026-05-ssp-ch/TODO.md)         |
| planning | [`2026-07-ssp-rsofun/`](2026-07-ssp-rsofun/) | Extends the baseline with **process-based** land-use-suitability predictors from [rsofun](https://github.com/mmyrte/rsofun) (P-model + SPLASH), as an interim stand-in for the eventual WASIM coupling.                                                                                                    | [README](2026-07-ssp-rsofun/README.md) · [TODO](2026-07-ssp-rsofun/TODO.md) |

## Milestone MS9 — SSP scenarios in evoland-plus

The active work is organised as one milestone in three phases, mapped onto the
sub-projects:

- **MS9 phase 1/3 — replicate SSP scenarios in evoland-plus** → `2026-05-ssp-ch/`.
  Reproduce the SSP-CH land-use futures on the new evoland-plus with an empirical
  transition model. This is the baseline everything else builds on.
- **MS9 phase 2/3 — minimal evoland-plus ↔ biophysical coupling** → `2026-07-ssp-rsofun/`.
  Add process-based (rsofun/SPLASH) suitability predictors and close a decadal
  land-cover ↔ water/energy feedback loop.
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

- **Numbered pipelines.** Scripts within a sub-project run in numeric order
  (`0-setup-db.r`, `1-…`, `2-…`). Steps sharing a number are independent (e.g.
  the several `2-ingest-preds-*.r`). Run a whole stage with
  `./execute-all.sh '2026-05-ssp-ch/2-*.r'`.
- **State lives in DuckDB.** Each experiment builds a `*.evolanddb` (folder of parquet
  files) via the evoland-plus `evoland_db` R6 class; predictors are ingested through
  `db$add_predictor` as a cheap way of ensuring foreign relations (no constraint checks
  as with a properly schematized RDBMS; allows tremendous speedup.)
- **Data provenance.** Ingest scripts download from public HTTP(S) sources and
  verify md5sums via `download_and_verify` into the evoland cache. Detailed
  source documentation lives alongside the scripts (see each sub-project README).
