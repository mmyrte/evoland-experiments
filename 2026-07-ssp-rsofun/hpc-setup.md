# HPC environment setup (Spack)

Replicating the `evoland-experiments` R environment (rv project `evoland-plus-darwin`,
R 4.5) on an HPC cluster with [Spack](https://spack.io). `spack.yaml` in this folder is
the environment; this file explains how to use it and the trade-offs.

## The R-package name mapping

rv/CRAN names map to Spack package names by **lower-casing and replacing `.` with `-`**,
with the `r-` prefix:

| CRAN | Spack |
|---|---|
| `curl` | `r-curl` |
| `data.table` | `r-data-table` |
| `DBI` | `r-dbi` |
| `R.utils` | `r-r-utils` |
| `R6` | `r-r6` |
| `RcppEigen` | `r-rcppeigen` |

`spack.yaml` was generated from `rv.lock` with exactly this rule, so all 215 CRAN
packages in the lock are listed (the 6 base-R "recommended" packages — KernSmooth, MASS,
Matrix, class, codetools, lattice — are omitted because Spack's `r` bundles them).

## Quick start (pure Spack)

```bash
spack env create evoland 2026-07-ssp-rsofun/spack.yaml
spack env activate evoland
spack concretize            # <-- read the output: any r-* it can't resolve is
                            #     not yet in Spack (see "Missing packages" below)
spack install
```

Then install the packages Spack does **not** carry (from Git), into the active R:

```bash
R -q -e 'remotes::install_github("ethzplus/evoland-plus", ref="b40175fd048c1615ff66e4fa556f6a0cd863b3fe")'
R -q -e 'remotes::install_github("mmyrte/rsofun")'   # P-model / SPLASH / BiomeE
R -q -e 'remotes::install_github("mmyrte/rsplash")'  # if used standalone
```

`rsofun`/`rsplash` compile Fortran and C++, so the R stack must be built with a `gcc`
that provides **gfortran** — set that in the site `compilers.yaml` (e.g. build with
`%gcc@12`). GDAL/GEOS/PROJ/libcurl arrive automatically as dependencies of
`r-sf`/`r-terra`/`r-stars`/`r-curl`; they are also listed explicitly in `spack.yaml` so a
site can force external system providers via `packages.yaml`.

## Recommended: hybrid (Spack toolchain + rv for exact versions)

Pure Spack gives you *a* working R stack, but Spack's package versions will **not** match
`rv.lock` exactly, and a few CRAN packages are not in Spack at all. For reproducible runs
that honour the lockfile, use Spack only for the heavy, compiled foundation and let `rv`
install the exact pinned CRAN packages on top:

```bash
# 1. Spack provides R + toolchain + system libraries (a slim env):
#    r@4.5, gmake, gdal, geos, proj, sqlite, udunits, curl   (+ gfortran-capable gcc)
spack env activate evoland-slim
spack install

# 2. rv installs the exact rv.lock versions into the project library:
rv sync                     # reads rproject.toml + rv.lock, builds against Spack's R
```

This is the most robust path on HPC: Spack handles the parts that are painful to compile
(GDAL stack, R itself, compilers), while `rv` guarantees the exact CRAN versions and pulls
`evoland` from its pinned commit. Keep a `slim` `spack.yaml` variant with just the
foundation specs for this mode.

## Missing packages (resolved against this site's catalog)

Cross-referencing `rv.lock` against `spack-available.txt`: **189 of the 215** CRAN
packages are in this Spack (now the full `spack.yaml` list). **26 are missing** and are
installed on top of the Spack R by `install-missing.R` at their exact locked versions —
the Spack view already provides shared dependencies, so `upgrade = "never"` leaves them
untouched:

```bash
spack env activate evoland && spack install
Rscript 2026-07-ssp-rsofun/install-missing.R          # 15 runtime gaps
Rscript 2026-07-ssp-rsofun/install-missing.R --dev    # + 11 dev/IDE tools
```

- **Runtime gaps (15):** `duckdb` (DB backend), `mirai`+`nanonext` (parallelism), the
  `mlr3` stack `mlr3`/`mlr3filters`/`mlr3measures`/`mlr3misc`/`mlr3viz`/`paradox`/`lgr`
  (evoland's transition model), `PRROC`, `pxR`, `qs2`, `S7`, `otel`.
- **Dev/IDE gaps (11, `--dev`, skip on batch nodes):** `httpgd`, `unigd`,
  `languageserver`, `collections`, `lintr`, `xmlparsedata`, `pak`, `tinytest`,
  `palmerpenguins`, `quarto`, `AsioHeaders`.

So the pure-Spack route **is** viable here — Spack covers the compiled heavy hitters
(`r-duckdb` is the notable one it lacks, hence the source build in the gap step). If even
the gap installs prove painful, fall back to the hybrid (`rv sync`) below.

## Notes for the rsofun run specifically

- The forcing/soil/land-cover scripts (`1-`, `2-`, `3-`) need: `r-data-table`, `r-terra`,
  plus `r-curl`/`httr2` for downloads. `4-run-rsofun.r` additionally needs `rsofun`
  (Git) and, for the grid sweep, `r-mirai`/`r-future`/`r-future-apply`.
- `duckdb` backs the evoland DB (`db$…`); ensure `r-duckdb` (or the rv-installed version)
  resolves — it is a large compiled package.
