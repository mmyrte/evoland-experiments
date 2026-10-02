#' ---
#' title: "Synthetic process and scores shared by the paper-figure experiments"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' Sourced by `010-skill-attribution.r` and `010-learner-comparison.r`, so that
#' the process is defined in one place. Rendered on its own, it shows one
#' synthetic landscape and its first two steps. `010-fig2-ensembles.r` still
#' carries its own copy of the same process; keep the two in step.

#| label: shared-setup
#| output: false
library(data.table)
library(terra)

#' # Synthetic process
#'
#' As in Fig. 2: two latent drivers, a nuisance field, and four transitions
#' whose per-period probabilities depend on the drivers and on the 5 x 5
#' neighbourhood. `true_probs()` returns the exact categorical probability of
#' each transition, accounting for the sequential draw in `step_process()`:
#' transition j of an anterior class gets min(p_j, max(0, 1 - sum_{i<j} p_i)).

#| label: process
scale01 <- function(x) {
  rng <- terra::global(x, c("min", "max"), na.rm = TRUE)
  (x - rng[1, 1]) / (rng[1, 2] - rng[1, 1])
}
smooth_field <- function(template, sd = 1, w = 7) {
  terra::setValues(template, rnorm(terra::ncell(template), sd = sd)) |>
    terra::focal(w = w, fun = mean, na.rm = TRUE) |>
    scale01()
}
qntl <- function(r, p) stats::quantile(terra::values(r), p, na.rm = TRUE)
plogis_rast <- function(x) 1 / (1 + exp(-x))
neighbour_share <- function(map, class) {
  terra::focal(map == class, w = 5, fun = mean, na.rm = TRUE)
}

make_drivers <- function(template_rast) {
  xy <- terra::crds(template_rast, df = TRUE)
  x_grad <- terra::setValues(
    template_rast,
    (xy$x - min(xy$x)) / (max(xy$x) - min(xy$x))
  )
  y_grad <- terra::setValues(
    template_rast,
    (xy$y - min(xy$y)) / (max(xy$y) - min(xy$y))
  )
  accessibility <- scale01(
    0.55 * (1 - x_grad) + 0.25 * (1 - y_grad) + 0.20 * smooth_field(template_rast, w = 9)
  )
  site_quality <- scale01(
    0.50 * y_grad + 0.35 * smooth_field(template_rast, w = 5) + 0.15 * x_grad
  )
  list(
    accessibility = accessibility,
    site_quality = site_quality,
    random_nuisance = smooth_field(template_rast, sd = 1, w = 3)
  )
}

# per-transition probabilities, in the order step_process() draws them
transition_probs <- function(map, drivers) {
  share_urban <- neighbour_share(map, 3)
  share_arable <- neighbour_share(map, 2)
  share_forest <- neighbour_share(map, 1)
  acc <- drivers$accessibility
  sq <- drivers$site_quality
  list(
    list(from = 2, to = 3, p = plogis_rast(-5.5 + 7 * share_urban + 3 * acc)),
    list(from = 1, to = 3, p = plogis_rast(-7 + 6 * share_urban + 3 * acc)),
    list(
      from = 1,
      to = 2,
      p = plogis_rast(-5 + 4 * (1 - sq) + 3 * share_arable)
    ),
    list(
      from = 2,
      to = 1,
      p = plogis_rast(-6 + 4 * sq * (1 - acc) + 3 * share_forest)
    )
  )
}

step_process <- function(map, drivers) {
  anterior <- terra::values(map, mat = FALSE)
  posterior <- anterior
  draw <- runif(length(anterior))
  cumulative <- numeric(length(anterior))
  for (tr in transition_probs(map, drivers)) {
    p <- terra::values(tr$p, mat = FALSE)
    at_risk <- anterior == tr$from
    flips <- at_risk &
      draw >= cumulative &
      draw < cumulative + p &
      posterior == anterior
    posterior[flips] <- tr$to
    cumulative[at_risk] <- cumulative[at_risk] + p[at_risk]
  }
  terra::setValues(map, posterior)
}

# exact categorical probabilities of the sequential draw, per raster cell
true_probs <- function(map, drivers) {
  anterior <- terra::values(map, mat = FALSE)
  cumulative <- numeric(length(anterior))
  out <- list()
  for (tr in transition_probs(map, drivers)) {
    p <- terra::values(tr$p, mat = FALSE)
    at_risk <- which(anterior == tr$from)
    q <- pmin(p[at_risk], pmax(0, 1 - cumulative[at_risk]))
    cumulative[at_risk] <- cumulative[at_risk] + p[at_risk]
    out[[length(out) + 1L]] <- data.table(
      cell = at_risk,
      id_lulc_anterior = tr$from,
      class = tr$to,
      q = q
    )
  }
  rbindlist(out)
}

make_landscape <- function(template_rast, drivers) {
  urban_score <- scale01(
    0.7 * drivers$accessibility + 0.3 * smooth_field(template_rast, w = 3)
  )
  lake_score <- smooth_field(template_rast, w = 7)
  forest_score <- scale01(
    0.6 * smooth_field(template_rast, w = 5) + 0.4 * drivers$site_quality
  )
  terra::ifel(
    urban_score > qntl(urban_score, 0.88),
    3,
    terra::ifel(
      lake_score > qntl(lake_score, 0.97),
      4,
      terra::ifel(forest_score > qntl(forest_score, 0.45), 1, 2)
    )
  )
}

#' # Scores
#'
#' A forecast is a long table (id_coord, class, p) over the cells that can
#' change; omitted classes get p = 0, and persistence is the remainder.

#| label: scores
brier_tables <- function(maps, eligible, classes, truth) {
  grid <- CJ(id_coord = eligible, class = classes)
  anterior_of <- maps[id_coord %in% eligible, .(id_coord, anterior)]
  observed <- maps[id_coord %in% eligible, .(id_coord, class = observed, o = 1)]
  observed <- observed[grid, on = .(id_coord, class)][is.na(o), o := 0]
  truth <- truth[grid, on = .(id_coord, class)][is.na(q), q := 0]
  list(grid = grid, anterior_of = anterior_of, observed = observed, truth = truth)
}

with_persistence <- function(trans_p, anterior_of) {
  stay <- trans_p[, .(leave = sum(p)), by = id_coord][
    anterior_of,
    on = "id_coord"
  ][is.na(leave), leave := 0][, .(id_coord, class = anterior, p = 1 - leave)]
  rbind(trans_p[, .(id_coord, class, p)], stay)
}

score <- function(f, bt, members = NA_integer_) {
  f <- f[id_coord %in% bt$grid$id_coord, .(p = sum(p)), by = .(id_coord, class)]
  d <- f[bt$grid, on = .(id_coord, class)][is.na(p), p := 0][
    bt$observed,
    on = .(id_coord, class)
  ][bt$truth, on = .(id_coord, class)]
  cell <- d[,
    .(
      realised = sum((p - o)^2),
      distance = sum((p - q)^2),
      irreducible = sum(q * (1 - q)),
      noise = if (is.na(members)) 0 else sum(p * (1 - p)) / (members - 1)
    ),
    by = id_coord
  ]
  cell[, .(
    brier_realised = mean(realised - noise),
    distance_to_truth = mean(distance - noise),
    brier_expected = mean(distance - noise + irreducible)
  )]
}

fom_counts <- function(anterior, observed, simulated) {
  obs_change <- anterior != observed
  sim_change <- anterior != simulated
  hits <- sum(obs_change & sim_change & simulated == observed)
  wrong <- sum(obs_change & sim_change & simulated != observed)
  misses <- sum(obs_change & !sim_change)
  false_alarms <- sum(!obs_change & sim_change)
  hits / (hits + wrong + misses + false_alarms)
}

#' # Example
#'
#' One 30 x 30 landscape and two steps of the process.

#| label: example
# skipped when another step sources this file for its functions
if (!isTRUE(getOption("synthetic_process.source_only"))) {
  set.seed(1337)
  example_template <- terra::rast(
    crs = "EPSG:2056",
    extent = terra::ext(c(xmin = 0, xmax = 3000, ymin = 0, ymax = 3000)),
    resolution = 100
  )
  example_drivers <- make_drivers(example_template)
  example_1 <- make_landscape(example_template, example_drivers)
  example_2 <- step_process(example_1, example_drivers)
  example_3 <- step_process(example_2, example_drivers)
  plot(
    c(example_1, example_2, example_3) |> setNames(paste("period", 1:3)),
    nc = 3,
    col = data.frame(
      value = 1:4,
      color = c("#91B690", "#EB9486", "#F3DE8A", "#CBC6D2")
    )
  )
}
