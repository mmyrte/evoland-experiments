# Allocation-stage interventions, replicating NCCS-SSP-scenarios'
# `Scripts/Functions/implement_spatial_interventions.R` on evoland's transition potentials.
#
# The specification is `interventions/allocation-interventions.csv`, a translation of
# `NCCS-SSP-scenarios/Tools/SSP*_interventions.yml` into ssp-ch's class names and mask names;
# see `052-ingest-intrv-masks.qmd` for the translation and what was deliberately changed.
# The masks live in the `intervention_mask_t` table (one row per mask and cell inside it), written
# by `052`.
#
# What one intervention does, for the transitions *into* its target classes (optionally only
# *from* its `from_classes`), on the raw potentials of one period:
#
# - `absolute`: potentials of cells in the zone are set to `absolute_value` (capped at 1), as
#   `absolute_prob_adjust()` does. A value of 0 excludes the zone.
# - `relative`: the original's percentile rule (`relative_prob_adjust()`). Pool the positive
#   potentials of the target class inside and outside the zone, take the mean of each above its
#   percentile, and express their gap as a percentage of their average (`perc_diff`, at least
#   `threshold_pct` in magnitude). The cells above the percentile on the side named by the
#   valency are then multiplied by (1 +/- perc_diff / 100).
#
# Both are per-cell multiplicative edits of the raw potentials. `adjusted_trans_pot_v()` then
# rescales each transition to its demanded rate, so an intervention relocates change; it does
# not alter how much land changes (that belongs in the demand, `070`).

intrv_mask_table <- "intervention_mask_t"

as_intrv_mask_t <- function(x) {
  x <- data.table::as.data.table(x)[, .(
    mask = as.character(mask),
    id_coord = as.integer(id_coord),
    value = as.numeric(value)
  )]
  evoland::as_ducklake_db_t(
    x,
    class_name = intrv_mask_table,
    key_cols = c("mask", "id_coord"),
    partition_cols = "mask"
  )
}

read_intervention_spec <- function(path) {
  spec <- data.table::fread(path, na.strings = "", colClasses = list(character = "ranking"))
  split_list <- function(x) lapply(x, function(v) if (is.na(v)) character(0) else strsplit(v, ";")[[1]])
  spec[, `:=`(
    ranking = suppressWarnings(as.integer(ranking)),
    target_classes = split_list(target_classes),
    from_classes = split_list(from_classes)
  )]
  stopifnot(
    "adjust_type must be absolute or relative" = all(spec$adjust_type %in% c("absolute", "relative")),
    "zone must be inside or outside" = all(spec$zone %in% c("inside", "outside")),
    "unknown valency" = all(
      spec[adjust_type == "relative", valency] %in%
        c("increase", "decrease", "increase_inside_decrease_outside")
    ),
    "increase_inside_decrease_outside needs zone = inside" =
      nrow(spec[valency == "increase_inside_decrease_outside" & zone != "inside"]) == 0L
  )
  spec[]
}

# Interventions of one SSP that are active in a period: those with any implementation year
# inside the period's span, in ranking order (unranked last, as the original).
interventions_for_period <- function(spec, scenario, period) {
  period_start <- as.integer(format(as.Date(period$start_date), "%Y"))
  period_end <- as.integer(format(as.Date(period$end_date), "%Y"))
  active <- spec$ssp == scenario & spec$year_start <= period_end & spec$year_end >= period_start
  spec[active][order(is.na(ranking), ranking)]
}

read_mask <- function(db, mask_name) {
  db$get_query(
    "select id_coord, value from {mask_read_expr} where mask = {mask_name}",
    mask_read_expr = db$get_read_expr(intrv_mask_table)
  )
}

# The (positive) potentials of one zone above its percentile, and their mean
upper_tail <- function(values, percentile) {
  values <- values[values > 0]
  if (length(values) == 0L) {
    return(list(cut = NA_real_, mean = NA_real_))
  }
  cut <- stats::quantile(values, probs = percentile / 100, names = FALSE)
  list(cut = cut, mean = mean(values[values >= cut]))
}

# Per-cell multipliers of the relative rule for one pooled target class. `pots` has columns
# value and in_zone; returns list(factor = numeric(nrow(pots)), perc_diff = numeric(1)).
relative_factors <- function(pots, valency, pct_in, pct_out, threshold) {
  inside <- upper_tail(pots[in_zone == TRUE, value], pct_in)
  outside <- upper_tail(pots[in_zone == FALSE, value], pct_out)
  factor <- rep(1, nrow(pots))
  perc_diff <- (inside$mean - outside$mean) / ((inside$mean + outside$mean) / 2) * 100
  if (!is.finite(perc_diff) || perc_diff == 0) {
    return(list(factor = factor, perc_diff = perc_diff))
  }
  magnitude <- max(abs(perc_diff), threshold)
  upper_in <- pots$in_zone & pots$value > inside$cut
  upper_out <- !pots$in_zone & pots$value > outside$cut
  if (valency == "increase") {
    # raise the zone if it already leads; otherwise lower its competition
    if (perc_diff > 0) factor[upper_in] <- 1 + magnitude / 100 else factor[upper_out] <- 1 - magnitude / 100
  } else if (valency == "decrease") {
    # lower the zone if it leads; otherwise raise its competition
    if (perc_diff > 0) factor[upper_in] <- 1 - magnitude / 100 else factor[upper_out] <- 1 + magnitude / 100
  } else {
    factor[upper_in] <- 1 + magnitude / 100
    factor[upper_out] <- 1 - magnitude / 100
  }
  list(factor = factor, perc_diff = sign(perc_diff) * magnitude)
}

# Edit the potentials of `id_period_post`, as read through the active run's lineage, by the
# given interventions (rows of the spec, already filtered to the scenario and period), and
# write the edited transitions back onto the active run -- each transition's whole slice, since
# a run inherits its ancestors' potentials slice-wise. Returns a log, one row per intervention
# and target class.
apply_allocation_interventions <- function(db, id_period_post, interventions) {
  if (nrow(interventions) == 0L) {
    return(data.table::data.table())
  }
  lulc_names <- db$lulc_meta_t[, .(id_lulc, name)]
  viable <- db$trans_meta_t[is_viable == TRUE, .(id_trans, id_lulc_anterior, id_lulc_posterior)][
    lulc_names, on = c(id_lulc_anterior = "id_lulc"), name_anterior := i.name
  ][
    lulc_names, on = c(id_lulc_posterior = "id_lulc"), name_posterior := i.name
  ]
  unknown <- setdiff(
    unlist(c(interventions$target_classes, interventions$from_classes)),
    lulc_names$name
  )
  stopifnot("intervention names a class missing from lulc_meta_t" = length(unknown) == 0L)

  pots <- db$get_query(
    "select id_trans, id_coord, value from {pot_read_expr} where id_period_post = {id_period_post}",
    pot_read_expr = db$get_read_expr("trans_pot_t")
  )[viable, on = "id_trans", nomatch = NULL]
  stopifnot("no transition potentials for this period - predict them first" = nrow(pots) > 0L)

  anterior <- NULL # the anterior map, read only if an intervention filters by current class
  masks <- list()
  edited <- integer(0)
  log <- list()

  for (i in seq_len(nrow(interventions))) {
    intrv <- interventions[i]
    targets <- intrv$target_classes[[1L]]
    from <- intrv$from_classes[[1L]]

    if (is.null(masks[[intrv$mask]])) {
      masks[[intrv$mask]] <- read_mask(db, intrv$mask)
    }
    mask <- masks[[intrv$mask]]
    if (nrow(mask) == 0L) {
      # a mask whose source is not ingested yet (see 052); skipped loudly, logged as NA
      warning("mask '", intrv$mask, "' is missing; skipping intervention ", intrv$intervention)
      log[[length(log) + 1L]] <- data.table::data.table(
        intervention = intrv$intervention,
        target_class = paste(targets, collapse = ";"),
        n_trans = NA_integer_,
        perc_diff = NA_real_,
        n_cells_edited = NA_integer_
      )
      next
    }

    if (length(from) > 0L) {
      if (is.null(anterior)) {
        anterior <- db$get_query(
          "select id_coord, id_lulc from {lulc_read_expr} where id_period = {id_period_post - 1L}",
          lulc_read_expr = db$get_read_expr("lulc_data_t")
        )[lulc_names, on = "id_lulc", name := i.name]
      }
      mask <- mask[anterior[name %in% from, .(id_coord)], on = "id_coord", nomatch = NULL]
    }
    in_mask <- if (isTRUE(intrv$mask_upper_quartile)) {
      # continuous marginality: the zone is its upper quartile among the eligible cells
      mask[value >= stats::quantile(value, 0.75, names = FALSE, na.rm = TRUE), id_coord]
    } else {
      mask[value == 1, id_coord]
    }

    for (target in targets) {
      rows <- pots[
        name_posterior == target & (length(from) == 0L | name_anterior %in% from),
        which = TRUE
      ]
      log_row <- data.table::data.table(
        intervention = intrv$intervention,
        target_class = target,
        n_trans = data.table::uniqueN(pots$id_trans[rows]),
        perc_diff = NA_real_,
        n_cells_edited = 0L
      )
      if (length(rows) == 0L) {
        log[[length(log) + 1L]] <- log_row
        next
      }
      in_zone <- pots$id_coord[rows] %in% in_mask
      if (intrv$zone == "outside") {
        in_zone <- !in_zone
      }

      if (intrv$adjust_type == "absolute") {
        new_value <- min(1, intrv$absolute_value)
        hit <- rows[in_zone & pots$value[rows] > 0]
        data.table::set(pots, i = hit, j = "value", value = new_value)
        log_row[, n_cells_edited := length(hit)]
      } else {
        rel <- relative_factors(
          data.table::data.table(value = pots$value[rows], in_zone = in_zone),
          valency = intrv$valency,
          pct_in = intrv$percentile_intervention,
          pct_out = intrv$percentile_non_intervention,
          threshold = intrv$threshold_pct
        )
        hit <- rows[rel$factor != 1]
        data.table::set(
          pots,
          i = hit,
          j = "value",
          value = pmin(1, pmax(0, pots$value[hit] * rel$factor[rel$factor != 1]))
        )
        log_row[, `:=`(perc_diff = rel$perc_diff, n_cells_edited = length(hit))]
      }
      edited <- union(edited, unique(pots$id_trans[hit]))
      log[[length(log) + 1L]] <- log_row
    }
  }

  if (length(edited) > 0L) {
    db$trans_pot_t <- evoland::as_trans_pot_t(pots[
      id_trans %in% edited,
      .(id_run = db$id_run, id_trans, id_period_post = id_period_post, id_coord, value)
    ])
  }
  data.table::rbindlist(log)[, `:=`(id_run = db$id_run, id_period_post = id_period_post)][]
}
