# Dev container

R 4.6 with [rv](https://github.com/A2-ai/rv), Dinamica EGO, quarto, and CLUinPy's Python
environment: everything the sub-projects need, `2026-09-model-comparison/` included.

## What it is built from

- **R**: `rocker/r-ver:4.6.1`, matching `r_version` in `rproject.toml`. The R packages come from
  `rv sync` (run once the container is created) into `rv/library`, not from the image.
- **Dinamica EGO**: copied from your own `ghcr.io/mmyrte/evoland` image, which extracts it from
  the AppImage (its licence forbids redistribution, so it is never downloaded here). That image
  runs R 4.5, hence the separate R. The bundled `Dinamica` R package goes into the site library,
  where Dinamica's R bridge finds it (it runs `Rscript` outside the repo, without rv).
  `DINAMICA_EGO_8_TEMP_DIR` is deliberately left unset: set for R sessions, it breaks the bridge.
- **CLUinPy** at commit `7d18dfe` in `/opt/CLUinPy`, with a venv at `/opt/cluinpy-venv` that sees
  the system GDAL bindings; `CLUINPY_REPO` and `CLUINPY_PYTHON` point there.

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

Many small files on a macOS bind mount are slow, so three directories live in named Docker
volumes instead of the repo checkout:

| volume | mounted at |
| --- | --- |
| `evoland-experiments-rv-library` | `rv/library` |
| `evoland-experiments-pie-outputs` | `2026-09-model-comparison/outputs` |
| `evoland-experiments-pie-db` | `2026-09-model-comparison/pie.evolanddb` |

They survive a rebuild of the container; `docker volume rm <name>` starts over. The tables and
figures the steps write into `figures/` stay in the checkout.

## Replaying the PIE benchmark

From the repo root, inside the container:

```bash
./execute-all.sh --workers 3 '2026-09-model-comparison/0*.r'
git diff --stat 2026-09-model-comparison/figures
```

`010` rebuilds the database from scratch. The tables in `figures/` are committed from the last
run, so `git diff` shows what a replay changed. Expected: the Dinamica allocations differ
(Dinamica draws a fresh seed per run); everything else should come out identical.
