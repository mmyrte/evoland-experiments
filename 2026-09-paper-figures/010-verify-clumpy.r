#' ---
#' title: "Verification: evoland's CLUMPY allocators against the reference clumpy"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' Does evoland's C++ CLUMPY reproduce the reference implementation it was translated from,
#' Mazy's Python clumpy (as forked in [mmyrte/clumpy](https://github.com/mmyrte/clumpy))? This
#' step builds on the comparison scripts of that fork (`scripts/comparison/`, branch
#' `claude/upbeat-mayer-2ik3ux`), which matched the pivot test on its own (`must_cpp`), and
#' extends them to the allocator as evoland runs it. Three checks:
#'
#' 1. **uSAM, end to end, bit for bit.** clumpy draws one uniform per cell of each anterior
#'    class in its MuST pivot test (`generalized_allocation_rejection_test`, "GART"). Those
#'    uniforms are replayed into evoland through R's user-supplied random number generator
#'    (`010-verify-clumpy/replay_rng.c`), and evoland's allocator specification
#'    (`alloc_clumpy_spec()$one_period()`, the code `allocate()` calls) allocates the same
#'    landscape. evoland itself is not changed for this. Both maps must agree in every cell,
#'    and evoland must consume exactly the uniforms clumpy drew.
#' 2. **uPAM's patch grower, scenario by scenario.** uPAM is the pivot test of (1) plus the
#'    patch grower and an iteration over the remaining pool (Mazy 2022, Fig. 3.2). The grower
#'    (`grow_patch_cpp`, the function `allocate_clumpy_cpp` calls per pivot) is compared with
#'    clumpy's `Patcher.allocate` on a battery of random scenarios with prescribed patch area
#'    and elongation, so that no random draw is involved. clumpy is run as shipped and in
#'    variants that isolate the known differences of the re-implementation (see below).
#' 3. **Patch areas.** The number of cells a patch gets from clumpy's `GaussianPatcher` and
#'    from evoland's normal area draw, for the same mean and standard deviation.
#'
#' The iteration of uPAM itself cannot be replayed against clumpy: clumpy's iterative
#' allocator (`clumpy.allocation.Unbiased.allocate`) does not run (it refers to the undefined
#' names `features`, `tm` and `mask`), and it differs from Mazy's Fig. 3.2 in what it
#' iterates (it re-estimates the potentials between passes and does not stop a transition at
#' its quota). evoland follows the thesis there. Fig. 2 checks the iteration's outcome
#' statistically (`020-verify-placement.r`).
#'
#' **Requirements.** An evoland.plus source checkout (`EVOLAND_PLUS_SRC`, default
#' `../evoland.plus`; loaded with `pkgload`, its commit is recorded), the repository's uv
#' environment (`python-env.r`; clumpy, ekde and hyperclip as pinned in `pyproject.toml`,
#' whose package code is identical on the fork's branch `claude/upbeat-mayer-2ik3ux`) and a C
#' compiler. A few seconds.
#'
#' Writes `results/verification/`: `clumpy-usam.csv`, `clumpy-patches.csv`,
#' `clumpy-patch-summary.csv`, `clumpy-patch-ties.csv`, `clumpy-areas.csv`, `versions.csv`.

#| label: setup
#| output: false
library(data.table)
source("python-env.r")

evoland_src <- normalizePath(Sys.getenv("EVOLAND_PLUS_SRC", "../evoland.plus"))
pkgload::load_all(evoland_src, quiet = TRUE, export_all = TRUE, helpers = FALSE)
evoland_commit <- system2("git", c("-C", evoland_src, "rev-parse", "HEAD"), stdout = TRUE)

here <- "2026-09-paper-figures/010-verify-clumpy"
res_dir <- "2026-09-paper-figures/results/verification"
dir.create(res_dir, recursive = TRUE, showWarnings = FALSE)
work <- tempfile("verify-clumpy-")
dir.create(work)

python <- project_python()
reference <- function(...) {
  status <- system2(python, c(file.path(here, "clumpy_reference.py"), ...))
  stopifnot("the reference clumpy failed" = status == 0L)
}

# the replaying generator, compiled into the scratch directory
file.copy(file.path(here, "replay_rng.c"), work)
local({
  status <- system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "SHLIB", "-o", file.path(work, "replay_rng.so"), file.path(work, "replay_rng.c")),
    stdout = FALSE
  )
  stopifnot("could not compile replay_rng.c" = status == 0L)
})
dyn.load(file.path(work, "replay_rng.so"))

#' # uSAM: the whole allocator on clumpy's uniforms
#'
#' Each case is a random landscape of `n_side` × `n_side` cells with four classes (1 and 3
#' change, 2 and 4 do not) and a block without data, and four transitions: 1 → 2, 1 → 3,
#' 3 → 1 and 3 → 2, so that the pivot test has to choose between two transitions and the stay.
#' The potentials are random, sum to at most 1 per cell, and are zero on a random share of
#' cells. Patch parameters are mono-pixel (area 1, variance 0), so evoland selects uSAM, as
#' it does in `allocate()`.
#'
#' The allocation context is built in memory, without a database: `ctx` holds what
#' `allocation_context()` would read. The posterior is the result of
#' `alloc_clumpy_spec()$one_period(ctx)`, i.e. evoland's R layer (sparse potentials, grid
#' cells, method selection) and its C++ backend.

#| label: usam
n_side <- 60L
n_cases <- 20L
transitions <- data.table(
  id_trans = 1:4,
  id_lulc_anterior = c(1L, 1L, 3L, 3L),
  id_lulc_posterior = c(2L, 3L, 1L, 2L)
)

usam_case <- function(case) {
  set.seed(case)
  n <- n_side^2
  rc <- data.table(cell = seq_len(n))[, `:=`(
    row = (cell - 1L) %/% n_side + 1L,
    col = (cell - 1L) %% n_side + 1L
  )]
  lulc <- sample(1:4, n, replace = TRUE, prob = c(0.45, 0.15, 0.3, 0.1))
  # a block without data, away from the edges so the grid extent is the full square
  lulc[rc$row %in% 20:29 & rc$col %in% 30:41] <- NA_integer_

  # random potentials: per cell, a share of the change mass for each of the class's two
  # transitions; a quarter of cells get no potential for a transition
  potentials <- rbindlist(lapply(c(1L, 3L), function(u) {
    cells <- which(lulc == u)
    total <- stats::rbeta(length(cells), 0.6, 4) # mass of change, mostly small
    split <- stats::runif(length(cells))
    tr <- transitions[id_lulc_anterior == u, id_trans]
    data.table(
      id_trans = rep(tr, each = length(cells)),
      cell = rep(cells, times = 2L),
      value = c(total * split, total * (1 - split))
    )
  }))
  potentials <- potentials[stats::runif(.N) > 0.25]

  case_dir <- file.path(work, sprintf("usam-%02d", case))
  dir.create(case_dir)
  fwrite(data.table(cell = seq_len(n), lulc = fcoalesce(lulc, -1L)), file.path(case_dir, "landscape.csv"))
  fwrite(transitions[, .(id_trans, from = id_lulc_anterior, to = id_lulc_posterior)], file.path(case_dir, "transitions.csv"))
  fwrite(potentials[, .(id_trans, cell, value = sprintf("%.17g", value))], file.path(case_dir, "potentials.csv"))
  writeLines(as.character(1000L * case), file.path(case_dir, "seed.txt"))
  reference("usam", case_dir, case_dir)
  uniforms <- scan(file.path(case_dir, "uniforms.csv"), quiet = TRUE)
  posterior_clumpy <- fread(file.path(case_dir, "posterior.csv"))[lulc == -1L, lulc := NA]

  # evoland's allocation context for the same case; id_coord is the cell number
  coords <- rc[!is.na(lulc), .(id_coord = cell, lon = (col - 0.5) * 100, lat = (n_side - row + 0.5) * 100)]
  data.table::setattr(coords, "epsg", 2056L)
  data.table::setattr(coords, "resolution", 100)
  ctx <- list(
    anterior = data.table(id_coord = which(!is.na(lulc)), id_lulc = lulc[!is.na(lulc)]),
    viable_trans = transitions,
    quota = transitions[, .(id_trans, rate = 0, n_anterior = 0L, quota = 0L)],
    potentials = potentials[, .(id_trans, id_coord = cell, value)],
    alloc_params = transitions[, .(id_trans, mean_patch_size = 1, patch_size_variance = 0, patch_elongation = NA_real_)],
    grid = coords_grid(coords)
  )
  stopifnot("grid cells must be the raster's cells" = ctx$grid$cells[, all(cell == id_coord)])

  old_kind <- RNGkind()
  RNGkind("user-supplied")
  .C("replay_set", as.double(uniforms), length(uniforms))
  posterior_evoland <- suppressMessages(alloc_clumpy_spec()$one_period(ctx))
  state <- .C("replay_state", pos = 0L, over = 0L)
  RNGkind(old_kind[1])

  both <- posterior_evoland[posterior_clumpy[, .(id_coord = cell, clumpy = lulc)], on = "id_coord", nomatch = NULL]
  data.table(
    case,
    cells = nrow(both),
    changed_clumpy = both[, sum(clumpy != lulc[id_coord])],
    changed_evoland = both[, sum(id_lulc != lulc[id_coord])],
    cells_differing = both[, sum(id_lulc != clumpy)],
    uniforms_drawn_clumpy = length(uniforms),
    uniforms_used_evoland = state$pos,
    uniforms_past_end = state$over,
    gart_fallthrough = as.integer(readLines(file.path(case_dir, "fallthrough.txt")))
  )
}

usam <- rbindlist(lapply(seq_len(n_cases), usam_case))
fwrite(usam, file.path(res_dir, "clumpy-usam.csv"))
knitr::kable(usam)
usam[, .(
  cases = .N,
  cells = sum(cells),
  changed = sum(changed_clumpy),
  cells_differing = sum(cells_differing),
  uniforms_unmatched = sum(uniforms_drawn_clumpy != uniforms_used_evoland) + sum(uniforms_past_end)
)]
stopifnot(
  "uSAM: evoland and clumpy maps differ" = usam[, all(cells_differing == 0L)],
  "uSAM: evoland did not consume exactly clumpy's uniforms" = usam[, all(uniforms_drawn_clumpy == uniforms_used_evoland & uniforms_past_end == 0L)]
)

#' # uPAM: the patch grower
#'
#' The documented differences between evoland's grower and clumpy's (mmyrte/clumpy,
#' `scripts/comparison/PIXEL_PERFECT_PLAN.md`), and how the battery handles them:
#'
#' - **Connectedness ("hollow-fill").** Mazy's step (iv)-(v): a neighbour with 3 of its 4 rook
#'   neighbours already in the patch is added first, picked at random. evoland does not
#'   implement it. clumpy is run with and without it (`nb_of_missing_to_fill = -1` disables it).
#' - **Elongation moments.** Mazy (2022, eqs. 3.I.6-3.I.12) defines the elongation of a patch
#'   from its second moments about its own centroid. To score a candidate cell, clumpy computes
#'   the moments of patch-plus-candidate about the centroid of the patch *without* the
#'   candidate; evoland about the centroid *with* it, as the thesis defines. clumpy is run as
#'   shipped and with the thesis' moments (`_allocate_thesis_moments` in
#'   `clumpy_reference.py`: the shipped function with only those lines changed).
#' - **No neighbour left.** Without aggregation avoidance, clumpy fails a patch that runs out
#'   of neighbours of its class; evoland keeps the patch it has grown. With avoidance on, both
#'   fail. The battery runs both settings and reports them apart.
#' - **Score.** clumpy divides the potential by the distance to the target elongation, evoland
#'   by that distance plus 1e-6. The target elongations (0.31, 0.57, 0.83) are not elongations
#'   a small patch can have exactly, so the distance is never 0.
#' - **Ties.** Both take the first best candidate in row-major order (clumpy the first maximum
#'   of `np.where`'s order, evoland the first of an ordered set). But candidates that tie in
#'   exact arithmetic, e.g. mirror images with the same potential, get elongations that differ
#'   in the last bits, and differently so in the two implementations (clumpy computes central
#'   moments, evoland running raw sums). Such a tie is then decided by round-off. Mazy's
#'   algorithm does not say how to break ties. A fifth of the scenarios has equal potentials
#'   everywhere, so that every choice is decided by elongation alone and such ties are common.
#'
#' Because of the ties, cell-by-cell equality with one implementation is not the right
#' criterion. The decisive check is therefore exact: `clumpy_reference.py ties` enumerates
#' every patch Mazy's grower (thesis moments, no hollow-fill) can produce when each exact tie
#' may go either way, using integer moments and exact fractions, and asks whether evoland's
#' patch is one of them. Without aggregation avoidance, where evoland keeps a patch that runs
#' out of neighbours, the enumeration does the same.
#' Each scenario: a 16 × 16 map, mostly the source class 1, some cells of 2 and 3; a few
#' cells of class 1 already turned into class 2 in this step (other patches, which
#' aggregation avoidance must not touch); random potentials on class 1 (or all equal); a random
#' pivot of class 1; an area of 1-16 cells; an elongation target.

#| label: patches
n_patch <- 2000L
patch_side <- 16L
set.seed(4242)
scenarios <- lapply(seq_len(n_patch), function(i) {
  n <- patch_side^2
  ant <- sample(1:3, n, replace = TRUE, prob = c(0.8, 0.12, 0.08))
  land <- ant
  # cells already allocated to 1 -> 2 in this step: other patches
  land[ant == 1L & stats::runif(n) < 0.03] <- 2L
  equal <- i %% 5L == 0L
  proba <- ifelse(ant == 1L, if (equal) 1 else stats::runif(n, 0.01, 1), 0)
  pivots <- which(ant == 1L & land == 1L)
  list(
    id = i,
    nrow = patch_side,
    ncol = patch_side,
    land = land,
    ant = ant,
    proba = proba,
    pivot = pivots[sample.int(length(pivots), 1L)],
    area = sample.int(16L, 1L),
    elongation = sample(c(0.31, 0.57, 0.83), 1L),
    from = 1L,
    to = 2L,
    avoid_aggregation = i %% 2L == 0L,
    equal_potentials = equal
  )
})
patch_dir <- file.path(work, "patches")
dir.create(patch_dir)
jsonlite::write_json(scenarios, file.path(patch_dir, "scenarios.json"), auto_unbox = TRUE, digits = NA)
reference("patch", patch_dir, patch_dir)
patches_clumpy <- fread(file.path(patch_dir, "patches.csv"), colClasses = list(character = "cells"))

nbr <- raster_neighbors_cpp(patch_side, patch_side)
patches_evoland <- rbindlist(lapply(scenarios, function(sc) {
  cells <- grow_patch_cpp(
    landscape = as.integer(sc$land),
    ant_landscape = as.integer(sc$ant),
    probs = as.numeric(sc$proba),
    nbr_above = nbr$above,
    nbr_below = nbr$below,
    nbr_left = nbr$left,
    nbr_right = nbr$right,
    pivot = sc$pivot,
    target_area = sc$area,
    from_class = sc$from,
    to_class = sc$to,
    elongation = sc$elongation,
    ncol = sc$ncol,
    avoid_aggregation = sc$avoid_aggregation
  )
  data.table(scenario = sc$id, evoland_size = length(cells), evoland_cells = paste(sort(cells), collapse = " "))
}))

scenario_meta <- rbindlist(lapply(scenarios, `[`, c("id", "area", "elongation", "avoid_aggregation", "equal_potentials")))
setnames(scenario_meta, "id", "scenario")
patches <- patches_clumpy[patches_evoland, on = "scenario"][scenario_meta, on = "scenario"]
patches[, same := size == evoland_size & cells == evoland_cells]
fwrite(patches, file.path(res_dir, "clumpy-patches.csv"))

patch_summary <- patches[,
  .(
    scenarios = .N,
    identical = sum(same),
    share_identical = mean(same),
    success_clumpy = sum(size > 0),
    success_evoland = sum(evoland_size > 0),
    only_success_differs = sum(!same & (size > 0) != (evoland_size > 0))
  ),
  keyby = .(variant, avoid_aggregation)
]
fwrite(patch_summary, file.path(res_dir, "clumpy-patch-summary.csv"))
knitr::kable(patch_summary, digits = 3)

#' Is evoland's patch one that Mazy's grower can produce, with ties resolved exactly? Every
#' scenario, both aggregation settings.

#| label: ties
fwrite(patches_evoland[, .(scenario, cells = evoland_cells)], file.path(patch_dir, "evoland.csv"))
reference("ties", patch_dir, patch_dir)
ties <- fread(file.path(patch_dir, "ties.csv"))
patches <- patches[ties, on = "scenario"]
fwrite(patches, file.path(res_dir, "clumpy-patches.csv"))

thesis_variant <- "clumpy, no hollow-fill, thesis moments"
ties_summary <- patches[variant == thesis_variant,
  .(
    scenarios = .N,
    identical_to_clumpy_variant = sum(same),
    # documented: without avoidance, clumpy fails a patch that runs out of neighbours
    differ_no_neighbour_left = sum(!same & size == 0L & evoland_size > 0L & !avoid_aggregation),
    differ_by_exact_ties = sum(
      !same & evoland_reachable & !(size == 0L & evoland_size > 0L & !avoid_aggregation)
    ),
    not_reachable = sum(!evoland_reachable),
    with_exact_ties = sum(n_outcomes > 1L),
    max_outcomes = max(n_outcomes)
  ),
  keyby = .(avoid_aggregation, equal_potentials)
]
fwrite(ties_summary, file.path(res_dir, "clumpy-patch-ties.csv"))
knitr::kable(ties_summary)
stopifnot(
  "uPAM grower: the tie enumeration was cut off" = patches[, !anyNA(evoland_reachable)],
  "uPAM grower: evoland grew a patch that Mazy's grower cannot produce" = patches[, all(evoland_reachable)]
)

#' # Patch areas
#'
#' clumpy's `GaussianPatcher` draws a real-valued area from a normal distribution, clamps it
#' at 1, and its grower adds cells while the patch is smaller than that area, so a patch gets
#' `ceiling(area)` cells. evoland draws from the same normal distribution and rounds to the
#' nearest integer (at least 1). The means differ by about half a cell; evoland's is the
#' prescribed one (up to the clamping at 1, which both share).

#| label: areas
area_cases <- CJ(mean = c(3, 10), sd = c(1, 3))
areas <- rbindlist(lapply(seq_len(nrow(area_cases)), function(i) {
  m <- area_cases$mean[i]
  s <- area_cases$sd[i]
  n <- 100000L
  area_dir <- file.path(work, sprintf("areas-%d", i))
  dir.create(area_dir)
  reference("areas", area_dir, m, s, n, 1L)
  clumpy_cells <- fread(file.path(area_dir, "areas.csv"))$cells
  set.seed(i)
  evoland_cells <- vapply(seq_len(n), function(k) sample_normal_area_cpp(m, s^2), integer(1))
  data.table(
    area_mean = m,
    area_sd = s,
    mean_cells_clumpy = mean(clumpy_cells),
    mean_cells_evoland = mean(evoland_cells),
    sd_cells_clumpy = sd(clumpy_cells),
    sd_cells_evoland = sd(evoland_cells)
  )
}))
fwrite(areas, file.path(res_dir, "clumpy-areas.csv"))
knitr::kable(areas, digits = 3)

#' # Versions

#| label: versions
clumpy_version <- system2(
  python,
  c("-c", shQuote("import importlib.metadata as m; print(m.version('clumpy'))")),
  stdout = TRUE
)
versions <- data.table(
  component = c("evoland.plus", "clumpy (Python)", "R", "Python"),
  version = c(
    evoland_commit,
    clumpy_version,
    R.version.string,
    system2(python, "--version", stdout = TRUE)
  )
)
fwrite(versions, file.path(res_dir, "versions.csv"))
knitr::kable(versions)
