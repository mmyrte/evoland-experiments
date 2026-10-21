#!/usr/bin/env python3
"""Install the R packages of rproject.toml from r2u (apt binaries via bspm), and the git
dependencies (evoland-plus at its pinned commit) with remotes, their dependencies again from
r2u. rv's own options (force_source, the repositories) do not apply: r2u builds everything
against the system libraries it installs.

Usage: install_r_deps.py <rproject.toml>
"""

import subprocess
import sys
import tomllib

with open(sys.argv[1], "rb") as f:
    deps = tomllib.load(f)["project"]["dependencies"]

cran, git = [], []
for dep in deps:
    if isinstance(dep, str):
        cran.append(dep)
    elif "git" in dep:
        git.append(dep)
    else:
        cran.append(dep["name"])
# base R packages are not on CRAN
cran = sorted(set(cran) - {"methods", "stats", "utils", "tools", "parallel", "grid"})


def r(code):
    subprocess.run(["Rscript", "-e", code], check=True)


r(f"install.packages(c({', '.join(repr(p) for p in cran + ['remotes'])}))")
# install_git clones with git; install_github would go through the GitHub API and its rate limit
for dep in git:
    ref = dep.get("commit") or dep.get("tag") or dep.get("branch") or "HEAD"
    r(
        f"remotes::install_git({dep['git']!r}, ref = {ref!r}, "
        f"dependencies = {'TRUE' if dep.get('install_suggestions') else 'NA'}, upgrade = 'never')"
    )
# fail the build if anything is missing
r(
    f"missing <- setdiff(c({', '.join(repr(p) for p in cran)}, {', '.join(repr(d['name']) for d in git)}), "
    "rownames(installed.packages())); if (length(missing)) stop('not installed: ', toString(missing))"
)
