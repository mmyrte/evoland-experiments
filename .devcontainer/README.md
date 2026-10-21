# Dev container

Ubuntu 24.04 with R 4.6 and the packages of `rproject.toml` as apt binaries from
[r2u](https://eddelbuettel.github.io/r2u/), Dinamica EGO, quarto, and CLUinPy's Python
environment: everything the sub-projects need, `2026-09-model-comparison/` included.

## What it is built from

- **R and its packages**: `rocker/r2u:24.04`. `install_r_deps.py` reads `rproject.toml` and
  installs every CRAN package listed there as an r2u binary (bspm turns `install.packages()` into
  apt), and evoland-plus at its pinned commit with `remotes`, its dependencies again from r2u.
  Rebuild the container after changing `rproject.toml`.
- **rv is not used in the container.** r2u has current CRAN versions, not those of `rv.lock`, and
  builds against the system libraries it installs alongside (so `force_source` does not apply).
  Without the `rv` binary, the repo's `.Rprofile` warns "rv is not installed" once per session and
  leaves the library paths alone.
- **Dinamica EGO**: copied from your own `ghcr.io/mmyrte/evoland` image, which extracts it from
  the AppImage (its licence forbids redistribution, so it is never downloaded here). The bundled
  `Dinamica` R package goes into the system library, where Dinamica's R bridge finds it.
  `DINAMICA_EGO_8_TEMP_DIR` is deliberately left unset: set for R sessions, it breaks the bridge.
- **CLUinPy** at commit `7d18dfe` in `/opt/CLUinPy`, with a venv at `/opt/cluinpy-venv` that sees
  the system GDAL bindings; `CLUINPY_REPO` and `CLUINPY_PYTHON` point there.
- **quarto** 1.10.18.

To build against another tag of the Dinamica image, set the build arg `DINAMICA_IMAGE`.

## On Apple silicon

Dinamica is distributed for amd64 only, so the whole container is `linux/amd64` and runs under
emulation.

- In Docker Desktop, turn on *Use Rosetta for x86_64/amd64 emulation on Apple Silicon*
  (Settings → General); without it, QEMU emulation is several times slower.
- Give the Docker VM the cores and memory to use (Settings → Resources): on an M3 Pro, 10 CPUs
  and at least 16 GB. The PIE benchmark peaks at about 4 GB per R process; with
  `execute-all.sh --workers 3`, three of them run at once.
- Expect emulated amd64 to be slower per core than native arm64; the gain over the cloud
  container (4 cores) is in running steps side by side.

## Volumes

Many small files on a macOS bind mount are slow, so the PIE benchmark's database and outputs
live in named Docker volumes instead of the repo checkout:

| volume | mounted at |
| --- | --- |
| `evoland-experiments-pie-outputs` | `2026-09-model-comparison/outputs` |
| `evoland-experiments-pie-db` | `2026-09-model-comparison/pie.evolanddb` |

They survive a rebuild of the container; `docker volume rm <name>` starts over. (`010` empties
the database directory rather than removing it, which the mount point would not allow.) The
tables and figures the steps write into `figures/` stay in the checkout.

## Replaying the PIE benchmark

From the repo root, inside the container:

```bash
./execute-all.sh --workers 3 '2026-09-model-comparison/0*.r'
git diff --stat 2026-09-model-comparison/figures
```

`010` rebuilds the database from scratch. The tables in `figures/` are committed from the last
run, so `git diff` shows what a replay changed. Expected: the Dinamica allocations differ
(Dinamica draws a fresh seed per run); everything else should come out identical.
