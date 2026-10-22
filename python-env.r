# The repository's Python environment, for the R steps that call Python; sourced, not rendered.
#
# The environment is the uv project at the repository root (pyproject.toml, uv.lock): `uv sync`
# makes it in `.venv`, or wherever UV_PROJECT_ENVIRONMENT points (the devcontainer: /opt/venv).

project_python <- function() {
  file.path(Sys.getenv("UV_PROJECT_ENVIRONMENT", ".venv"), "bin", "python")
}

# CLUinPy (nomisthomsen/CLUinPy) cannot be a dependency in pyproject.toml: its setup.py packages
# nothing (find_packages() at the repository root, the code is under src/), and its modules import
# each other by bare name. Its Python dependencies are in pyproject.toml; the code itself is
# cloned at this commit into a git-ignored directory on first use and put on PYTHONPATH by the
# caller (src/ and src/suitability/).
cluinpy_commit <- "7d18dfe1919e5e7a75516ed24c5c78c1d8612cdc"

cluinpy_repo <- function(dir = file.path(".cache", "CLUinPy")) {
  if (!dir.exists(file.path(dir, ".git"))) {
    dir.create(dirname(dir), recursive = TRUE, showWarnings = FALSE)
    status <- system2("git", c("clone", "--quiet", "https://github.com/nomisthomsen/CLUinPy", dir))
    stopifnot("could not clone CLUinPy" = status == 0L)
  }
  head <- system2("git", c("-C", dir, "rev-parse", "HEAD"), stdout = TRUE)
  if (head != cluinpy_commit) {
    system2("git", c("-C", dir, "fetch", "--quiet", "origin"))
    status <- system2("git", c("-C", dir, "checkout", "--quiet", cluinpy_commit))
    stopifnot("could not check out the pinned CLUinPy commit" = status == 0L)
  }
  normalizePath(dir)
}

cluinpy_pythonpath <- function(repo) {
  paste(file.path(repo, "src"), file.path(repo, "src", "suitability"), sep = ":")
}
