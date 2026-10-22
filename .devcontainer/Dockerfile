# Dev container for evoland-experiments: Ubuntu 24.04 with R 4.6 and every CRAN package as an
# apt binary from r2u (https://eddelbuettel.github.io/r2u/), Dinamica EGO, quarto, and the
# repository's uv environment (pyproject.toml; CLUinPy for 2026-09-model-comparison).
#
# Dinamica comes from your own ghcr.io/mmyrte/evoland image (its licence forbids redistribution,
# so it is not downloaded here). Dinamica is distributed for amd64 only: on Apple silicon this
# runs under Rosetta.

ARG DINAMICA_IMAGE=ghcr.io/mmyrte/evoland:latest
ARG UV_VERSION=0.8.17
FROM --platform=linux/amd64 ${DINAMICA_IMAGE} AS dinamica
FROM ghcr.io/astral-sh/uv:${UV_VERSION} AS uv

FROM --platform=linux/amd64 rocker/r2u:24.04
ARG DINAMICA_DIR=/opt/dinamica
ARG QUARTO_VERSION=1.10.18

# Tools, and what the Python environment builds on: the GDAL bindings are compiled against the
# system GDAL. The system libraries of the R packages come with their r2u binaries.
RUN apt-get update -qq \
  && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    curl \
    gdal-bin \
    git \
    git-lfs \
    htop \
    libgdal-dev \
    python3-dev \
    shellcheck \
    time \
  && rm -rf /var/lib/apt/lists/*

RUN curl -fsSL -o /tmp/quarto.deb \
    "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb" \
  && apt-get install -y /tmp/quarto.deb \
  && rm /tmp/quarto.deb

# The R packages of rproject.toml, evoland-plus at its pinned commit included. The repo's rv
# setup is not used in here: without the rv binary, the .Rprofile's activation warns once and
# leaves the library paths alone, so R finds these system-wide packages.
COPY rproject.toml /tmp/rproject.toml
COPY .devcontainer/install_rproject_via_r2u.py /tmp/install_rproject_via_r2u.py
RUN python3 /tmp/install_rproject_via_r2u.py /tmp/rproject.toml \
  && rm -rf /tmp/rproject.toml /tmp/install_rproject_via_r2u.py /var/lib/apt/lists/*

# Dinamica EGO, set up as in the ghcr.io/mmyrte/evoland image, with its bundled R package for the
# R bridge (CalculateRExpression)
COPY --from=dinamica ${DINAMICA_DIR} ${DINAMICA_DIR}
RUN Rscript -e 'install.packages(c("Rcpp", "RcppProgress"))' \
  && R CMD INSTALL ${DINAMICA_DIR}/usr/bin/Data/R/Dinamica_*.tar.gz \
  && printf '%s\n' \
    'AlternativePathForR = "/usr/bin/Rscript"' \
    'ClConfig = "0"' \
    'MemoryAllocationPolicy = "1"' \
    'RCranMirror = "https://cloud.r-project.org"' \
    > /root/.dinamica_ego_8.conf
# DINAMICA_EGO_8_TEMP_DIR is left unset on purpose: set for R sessions, it breaks the R bridge
# (evoland-plus, install-dinamica vignette)
ENV LD_LIBRARY_PATH="${DINAMICA_DIR}/usr/lib/" \
  PATH="${PATH}:${DINAMICA_DIR}/usr/bin" \
  DINAMICA_EGO_8_HOME=${DINAMICA_DIR} \
  DINAMICA_EGO_8_INSTALLATION_DIRECTORY=${DINAMICA_DIR}/usr/bin \
  DINAMICA_EGO_CLI=${DINAMICA_DIR}/usr/bin/DinamicaConsole

# The repository's Python environment (pyproject.toml, uv.lock), on the system Python, in
# /opt/venv rather than the workspace's .venv: a venv on the macOS bind mount would be slow.
# `uv sync` in the container keeps it in step with the lock file. CLUinPy itself is not an
# installable package; python-env.r clones it at its pinned commit into the workspace's .cache/
# on first use, so the image needs neither a clone nor CLUINPY_* variables.
COPY --from=uv /uv /uvx /usr/local/bin/
ENV UV_PROJECT_ENVIRONMENT=/opt/venv \
  UV_PYTHON=/usr/bin/python3 \
  UV_PYTHON_DOWNLOADS=never \
  UV_LINK_MODE=copy
COPY pyproject.toml uv.lock /tmp/uv-project/
RUN cd /tmp/uv-project \
  && uv sync --frozen --no-cache \
  && rm -rf /tmp/uv-project
