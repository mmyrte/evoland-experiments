#' ---
#' title: "PIE benchmark: one set of potentials per estimator, for every allocator"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' The crossing hinges on this step: each estimator's surface ends up in two forms that every
#' allocator can read, so that any estimator can be paired with any allocator.
#'
#' 1. **Transition potentials** in evoland's `trans_pot_t`, under the estimator's run. evoland's
#'    learners (logistic regression, random forest, naive Bayes) wrote theirs in `010`. Here
#'    Dinamica's Weights of Evidence probability maps (`015`) are written into run 3000 the same
#'    way, so CLUMPY, Dinamica and the greedy allocator (`030`) read all four alike.
#' 2. **Class probability maps** for the allocators that work on class totals rather than
#'    transitions (CLUE-S, Ordered, CLUMondo; `040`). They take a suitability per class and
#'    cell. Here that is the estimator's probability that the cell is of class k in 1999:
#'    from a cell of class i, the adjusted potential of i → k (scaled to the demanded rate,
#'    row-closed by `adjusted_trans_pot_v()`) for k ≠ i, and one minus their sum for staying in
#'    i. One GeoTIFF per estimator, band k for class k, in `outputs/suitability/`.
#'
#' The class-total allocators thus get the same information as the transition allocators,
#' including the demand split between anterior classes, in the form they are built for.

#| label: setup
#| output: false
library(evoland)
library(data.table)
source("2026-09-model-comparison/common.r")
db <- evoland_db$new(path = db_path)
maps_dir <- file.path(outputs_dir, "maps")
suitability_dir <- file.path(outputs_dir, "suitability")
dir.create(suitability_dir, recursive = TRUE, showWarnings = FALSE)

db$id_run <- 0L
viable <- db$trans_meta_t[is_viable == TRUE][order(id_lulc_anterior, id_lulc_posterior)]
viable
anterior <- db$fetch("lulc_data_t", where = "id_run = 0 and id_period = 2")[,
  .(id_coord, id_lulc_anterior = id_lulc)
]

#' # Weights of Evidence into `trans_pot_t`

#| label: woe
if ("woe" %in% estimators$estimator) {
  # Dinamica writes one layer per transition, in the order of the transition set in
  # probabilities.ego, which is the order of `viable`
  woe <- terra::rast(file.path(maps_dir, "dinamica-native", "probabilities.tif"))
  # numeric layer names do not survive extraction, hence the prefixes
  names(woe) <- paste0("trans_", viable$id_trans)
  woe_pot <- extract_using_coords_t(woe, db$coords_t)[, .(
    id_trans = as.integer(sub("trans_", "", as.character(layer))),
    id_coord,
    value
  )]
  # a transition's potential only matters in the cells of its anterior class
  woe_pot <- woe_pot[viable[, .(id_trans, id_lulc_anterior)], on = "id_trans"][
    anterior,
    on = .(id_coord, id_lulc_anterior),
    nomatch = NULL
  ][!is.na(value)]
  id_run_woe <- estimators[estimator == "woe", id_run]
  db$id_run <- id_run_woe
  db$commit(
    as_trans_pot_t(woe_pot[, .(
      id_run = id_run_woe,
      id_trans,
      id_period_post = 3L,
      id_coord,
      value
    )]),
    "trans_pot_t",
    method = "upsert"
  )
  woe_pot[, .(n = .N, min = min(value), mean = mean(value), max = max(value)), by = id_trans]
}

#' # Class probability maps

#| label: class-probabilities
#| fig-asp: 0.35
class_probabilities <- function(id_run) {
  db$id_run <- id_run
  adjusted <- db$adjusted_trans_pot_v(3L)[
    viable[, .(id_trans, id_lulc_anterior, id_lulc_posterior)],
    on = "id_trans",
    nomatch = NULL
  ]
  # every cell, every class: zero unless a viable transition leads there
  cells <- CJ(id_coord = anterior$id_coord, id_lulc = lulc_classes)[anterior, on = "id_coord"]
  cells[
    adjusted,
    on = .(id_coord, id_lulc_anterior, id_lulc = id_lulc_posterior),
    p := i.value
  ][is.na(p), p := 0]
  # staying in the anterior class takes what the transitions leave
  cells[, p_change := sum(p[id_lulc != id_lulc_anterior]), by = id_coord]
  cells[id_lulc == id_lulc_anterior, p := pmax(0, 1 - p_change)]
  cells[, .(id_coord, id_lulc, p)]
}

for (i in seq_len(nrow(estimators))) {
  probs <- class_probabilities(estimators$id_run[i])
  stopifnot(probs[, all(abs(sum(p) - 1) < 1e-6), by = id_coord]$V1)
  stack <- terra::rast(lapply(lulc_classes, function(k) {
    tabular_to_raster(
      probs[id_lulc == k, .(id_coord, value = p)],
      db$coords_minimal,
      value_col = "value"
    )
  }))
  names(stack) <- names(lulc_classes)
  terra::writeRaster(
    stack,
    file.path(suitability_dir, paste0(estimators$estimator[i], ".tif")),
    overwrite = TRUE
  )
  terra::plot(stack, nc = 3, main = paste(estimators$label[i], names(lulc_classes)))
  # the expected number of cells per class equals the demanded 1999 totals
  print(probs[, .(expected_1999 = round(sum(p))), by = id_lulc][,
    estimator := estimators$estimator[i]
  ][])
}
