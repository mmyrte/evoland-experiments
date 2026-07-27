#!/usr/bin/env Rscript
# Install the CRAN packages from rv.lock that this site's Spack does NOT provide (26),
# at their exact locked versions, on top of the Spack-activated R (the Spack view supplies
# the other 189, so shared dependencies are already on .libPaths() and are not rebuilt).
#
# Usage (after `spack env activate <env> && spack install`):
#   Rscript 2026-07-ssp-rsofun/install-missing.R          # runtime gaps only
#   Rscript 2026-07-ssp-rsofun/install-missing.R --dev    # also the dev/IDE tools
#
# Installs into the first writable library on .libPaths(); override with R_LIBS_USER.

options(repos = c(CRAN = "https://stat.ethz.ch/CRAN/"))
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

# runtime gaps: DB backend, parallelism, the mlr3 stack (evoland transition model),
# eval/serialisation, and PX ingestion. name = exact rv.lock version.
runtime <- c(
  duckdb        = "1.5.4",
  nanonext      = "1.9.1",   # dependency of mirai
  mirai         = "2.7.1",
  lgr           = "0.5.2",   # dependency of mlr3
  paradox       = "1.0.1",   # dependency of mlr3
  mlr3misc      = "0.22.0",
  mlr3          = "1.7.1",
  mlr3filters   = "0.9.1",
  mlr3measures  = "1.3.0",
  mlr3viz       = "0.11.0",
  PRROC         = "1.4",
  pxR           = "0.42.8",
  qs2           = "0.2.2",
  S7            = "0.2.2",   # needed by ggplot2 4.x / scales if Spack pulls those versions
  otel          = "0.2.0"
)

# dev / IDE only -- unnecessary on batch compute nodes.
dev <- c(
  AsioHeaders    = "1.30.2-1",
  collections    = "0.3.12",
  httpgd         = "2.1.4",
  unigd          = "0.2.0",
  languageserver = "0.3.18",
  lintr          = "3.3.0-1",
  xmlparsedata   = "1.0.5",
  pak            = "0.10.0",
  tinytest       = "1.4.3",
  palmerpenguins = "0.1.1",
  quarto         = "1.5.1"
)

want <- if ("--dev" %in% commandArgs(TRUE)) c(runtime, dev) else runtime

# upgrade = "never": do not rebuild dependencies already provided by the Spack view.
for (pkg in names(want)) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    message("already present: ", pkg)
    next
  }
  message("installing ", pkg, " ", want[[pkg]])
  remotes::install_version(pkg, version = want[[pkg]], upgrade = "never")
}
message("Done. Missing-package gap filled (", length(want), " requested).")
