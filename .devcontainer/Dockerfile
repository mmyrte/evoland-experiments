# Dev container for evoland-experiments: R 4.6 (as rproject.toml asks for) with rv, Dinamica EGO,
# quarto, and the Python environment of CLUinPy (2026-09-model-comparison).
#
# Dinamica comes from your own ghcr.io/mmyrte/evoland image (its licence forbids redistribution,
# so it is not downloaded here); that image runs R 4.5, so the R itself comes from rocker/r-ver.
# Dinamica is distributed for amd64 only: on Apple silicon this runs under Rosetta.

ARG DINAMICA_IMAGE=ghcr.io/mmyrte/evoland:latest
FROM --platform=linux/amd64 ${DINAMICA_IMAGE} AS dinamica

FROM --platform=linux/amd64 rocker/r-ver:4.6.1
ARG DINAMICA_DIR=/opt/dinamica
ARG CLUINPY_COMMIT=7d18dfe
ARG QUARTO_VERSION=1.10.18

# System libraries for the R packages rv builds (sf and terra from source, tidyverse), git,
# Python with GDAL bindings for CLUinPy, and tools for working in the container
RUN apt-get update -qq \
  && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    cmake \
    curl \
    gdal-bin \
    git \
    git-lfs \
    htop \
    libabsl-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgdal-dev \
    libgeos-dev \
    libgit2-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libnetcdf-dev \
    libpng-dev \
    libproj-dev \
    libsqlite3-dev \
    libssl-dev \
    libtiff-dev \
    libudunits2-dev \
    libxml2-dev \
    pkg-config \
    python3-gdal \
    python3-venv \
    shellcheck \
    sqlite3 \
    time \
  && rm -rf /var/lib/apt/lists/*

RUN /rocker_scripts/install_quarto.sh "${QUARTO_VERSION}"

# rv (https://github.com/A2-ai/rv), which the repo's .Rprofile activates
RUN curl -sSL https://raw.githubusercontent.com/A2-ai/rv/refs/heads/main/scripts/install.sh | bash \
  && mv /root/.local/bin/rv /usr/local/bin/rv \
  && rv --version

# Dinamica EGO, set up as in the ghcr.io/mmyrte/evoland image. Its R bridge
# (CalculateRExpression) runs Rscript outside the repo, i.e. without rv, on the site library:
# the bundled Dinamica package goes there.
COPY --from=dinamica ${DINAMICA_DIR} ${DINAMICA_DIR}
RUN install2.r --error --skipinstalled Rcpp RcppProgress \
  && R CMD INSTALL ${DINAMICA_DIR}/usr/bin/Data/R/Dinamica_*.tar.gz \
  && printf '%s\n' \
    'AlternativePathForR = "/usr/local/bin/Rscript"' \
    'ClConfig = "0"' \
    'MemoryAllocationPolicy = "1"' \
    "RCranMirror = \"${CRAN}\"" \
    > /root/.dinamica_ego_8.conf
ENV LD_LIBRARY_PATH="${DINAMICA_DIR}/usr/lib/" \
  PATH="${PATH}:${DINAMICA_DIR}/usr/bin" \
  DINAMICA_EGO_8_HOME=${DINAMICA_DIR} \
  DINAMICA_EGO_8_INSTALLATION_DIRECTORY=${DINAMICA_DIR}/usr/bin \
  DINAMICA_EGO_CLI=${DINAMICA_DIR}/usr/bin/DinamicaConsole
# rocker sets its own PATH for R sessions; keep Dinamica on it. DINAMICA_EGO_8_TEMP_DIR is left
# unset on purpose: set for R sessions it breaks the R bridge (evoland-plus, install-dinamica
# vignette).
RUN echo "PATH=${PATH}" >> "${R_HOME}/etc/Renviron.site"

# CLUinPy (2026-09-model-comparison/040-alloc-cluinpy.r), at the commit the benchmark used, in
# a venv that sees the system GDAL bindings
RUN git clone --quiet https://github.com/nomisthomsen/CLUinPy /opt/CLUinPy \
  && git -C /opt/CLUinPy checkout --quiet ${CLUINPY_COMMIT} \
  && python3 -m venv --system-site-packages /opt/cluinpy-venv \
  && /opt/cluinpy-venv/bin/pip install --quiet --no-cache-dir \
    geopandas joblib numba numpy openpyxl pandas rasterio scikit-learn scipy statsmodels xgboost
ENV CLUINPY_REPO=/opt/CLUinPy \
  CLUINPY_PYTHON=/opt/cluinpy-venv/bin/python
