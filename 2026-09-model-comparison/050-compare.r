#' ---
#' title: "PIE benchmark: comparison across tools"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' Imports every simulated 1999 map made outside the evoland database (lulcc, CLUinPy, the
#' standalone Dinamica run) as runs, and scores all of them against the held-out observation
#' with the same code. The ensembles are the estimator × allocator crossing (`common.r`) plus
#' each tool as shipped, for reference:
#'
#' - **Figure of merit** (Pontius et al. 2008) and its components, against the value expected
#'   from random allocation of the same quantities (`figure_of_merit_null`); persistence scores 0.
#' - **Quantity and allocation disagreement** (Pontius & Millones 2011) of the 1999 maps.
#' - **Gross change**: cells changed between 1991 and the simulated 1999, against the 4 702
#'   observed. Tools that allocate class totals (CLUE family) need not reproduce it.
#' - **Fuzzy similarity of differences** for the built-land gains (Hagen 2003, as in Dinamica).
#' - **Variance decomposition** of the figure of merit over the estimator × allocator crossing
#'   (stochastic allocators): estimator, allocator, interaction, and replicate variance.
#' - **Wall time** per stage, from the `timings-*.csv` every step writes.

#| label: setup
#| output: false
library(evoland)
library(data.table)
library(terra)
library(ggplot2)
source("2026-09-model-comparison/common.r")
db <- evoland_db$new(path = db_path)
maps_dir <- file.path(outputs_dir, "maps")
figures_dir <- file.path(pie_dir, "figures")
dir.create(figures_dir, showWarnings = FALSE)

#' # Ensembles

#| label: ensembles
crossing <-
  crossing_members()[,
    .(
      estimator,
      allocator,
      estimator_label,
      allocator_label,
      member,
      id_run,
      id_run_parent,
      as_shipped = FALSE,
      # maps made outside evoland's database, imported below
      maps = fifelse(
        allocator %in% c("clue-s", "ordered", "clumondo"),
        external_maps_dir(allocator, estimator),
        NA_character_
      )
    )
  ]
as_shipped <-
  native_pipelines[,
    .(member = seq_len(n_members)),
    by = .(native, id_run_parent = id_run, offset, estimator_label, allocator_label)
  ][,
    .(
      estimator = native,
      allocator = native,
      estimator_label,
      allocator_label,
      member,
      id_run = id_run_parent + offset + member,
      id_run_parent,
      as_shipped = TRUE,
      maps = file.path(outputs_dir, "maps", native)
    )
  ]
members <- rbind(crossing, as_shipped)
members[, ensemble := paste(estimator_label, "×", allocator_label)]
members[as_shipped == TRUE, ensemble := paste(ensemble, "(as shipped)")]
ensembles <-
  members[,
    .(
      estimator_label = estimator_label[1],
      allocator_label = allocator_label[1],
      as_shipped = as_shipped[1],
      n_members = .N
    ),
    by = ensemble
  ]
ensembles

#' # Import the maps made outside the database
#'
#' Runs `parent + offset + i`, children of their estimator's run (the as-shipped lulcc and
#' CLUinPy parents are registered here), so every map is read through the same lineage machinery.

#| label: import
db$id_run <- NULL
runs <- db$runs_t
imported <- members[!is.na(maps)]
parents_missing <- setdiff(imported$id_run_parent, runs$id_run)
runs <- rbind(
  runs[!id_run %in% imported$id_run],
  data.table(
    id_run = parents_missing,
    parent_id_run = 0L,
    description = paste("parent of the as-shipped runs", parents_missing)
  ),
  imported[, .(
    id_run,
    parent_id_run = id_run_parent,
    description = paste(ensemble, "realisation", member)
  )],
  fill = TRUE
)
db$commit(as_runs_t(runs), "runs_t", method = "overwrite")

# The maps share the grid of the input rasters, so their values are read by cell index rather
# than through extract_using_coords_t()'s point geometries (ethzplus/evoland-plus#86), and each
# ensemble is committed at once: one snapshot per ensemble instead of one per map.
coords <- db$coords_t
cells <- terra::cellFromXY(read_lu(1991), as.matrix(coords[, .(lon, lat)]))
for (dir_maps in unique(imported$maps)) {
  ensemble_maps <- imported[maps == dir_maps]
  db$commit(
    as_lulc_data_t(rbindlist(lapply(seq_len(nrow(ensemble_maps)), function(i) {
      map <- terra::rast(file.path(dir_maps, sprintf("r%02d.tif", ensemble_maps$member[i])))
      data.table(
        id_run = ensemble_maps$id_run[i],
        id_coord = coords$id_coord,
        id_period = 3L,
        id_lulc = as.integer(map[cells][[1]])
      )[!is.na(id_lulc)]
    }))),
    "lulc_data_t",
    method = "upsert"
  )
}

#' # Figure of merit

#| label: fom
db$id_run <- NULL
fom <-
  db$figure_of_merit_v(
    id_period_anterior = 2L,
    id_period_post = 3L,
    id_run_reference = id_run_observed,
    id_run_simulated = members$id_run
  )[
    members,
    on = "id_run"
  ]

# ensemble expectation: ratio of the mean counts, not the mean of the ratios
fom_ensemble <-
  fom[,
    c(
      lapply(.SD, mean),
      .(
        fom_sd = sd(figure_of_merit),
        fom_min = min(figure_of_merit),
        fom_max = max(figure_of_merit),
        figure_of_merit_null = mean(figure_of_merit_null)
      )
    ),
    by = .(ensemble, estimator_label, allocator_label, as_shipped),
    .SDcols = c("hits", "wrong_hits", "misses", "false_alarms")
  ][,
    `:=`(
      figure_of_merit = hits / (hits + wrong_hits + misses + false_alarms),
      producers_accuracy = hits / (hits + wrong_hits + misses),
      users_accuracy = hits / (hits + wrong_hits + false_alarms)
    )
  ][,
    skill_over_null := figure_of_merit / figure_of_merit_null
  ][
    order(-figure_of_merit)
  ]
fom_ensemble[,
  .(
    ensemble,
    figure_of_merit = round(figure_of_merit, 4),
    fom_sd = round(fom_sd, 4),
    null = round(figure_of_merit_null, 4),
    skill_over_null = round(skill_over_null, 2),
    producers_accuracy = round(producers_accuracy, 3),
    users_accuracy = round(users_accuracy, 3)
  )
]

fom_by_transition <-
  db$figure_of_merit_v(
    id_period_anterior = 2L,
    id_period_post = 3L,
    id_run_reference = id_run_observed,
    id_run_simulated = members$id_run,
    by_transition = TRUE
  )[
    members[, .(id_run, ensemble)],
    on = "id_run"
  ][,
    .(
      observed = mean(observed),
      simulated = mean(simulated),
      hits = mean(hits),
      figure_of_merit = mean(hits) / (mean(observed) + mean(simulated) - mean(hits)),
      figure_of_merit_null = mean(figure_of_merit_null)
    ),
    by = .(ensemble, id_lulc_anterior, id_lulc_posterior)
  ]
dcast(
  fom_by_transition,
  id_lulc_anterior + id_lulc_posterior ~ ensemble,
  value.var = "simulated"
)

#' # Quantity and allocation disagreement, gross change
#'
#' Each simulated 1999 map against the observed one (Pontius & Millones 2011), and against the
#' 1991 map for the gross change and the simulated transition counts. One DuckDB query counts
#' the cells of every run by their 1991, observed and simulated class; `lulc_crosstab_v()`
#' does the same for one pair of runs at a time, but calling it twice per run costs two
#' catalog round trips each (ethzplus/evoland-plus#85), five minutes over the ~500 runs here.
#' Every run stores its complete 1999 map, so the query reads the runs' own rows.

#| label: disagreement
db$id_run <- NULL
crosstab <- function(id_run_anterior, id_run_post, id_period_anterior) {
  db$lulc_crosstab_v(
    id_period_anterior = id_period_anterior,
    id_period_post = 3L,
    id_run_anterior = id_run_anterior,
    id_run_post = id_run_post
  )
}
pontius <- function(ct) {
  # rows: observed class, columns: simulated class, as shares of all cells
  share <- ct[, .(id_lulc_anterior, id_lulc_posterior, p = n_cells / sum(n_cells))]
  quantity <- sum(abs(
    share[, .(p = sum(p)), keyby = .(k = id_lulc_anterior)]$p -
      share[, .(p = sum(p)), keyby = .(k = id_lulc_posterior)][J(lulc_classes), on = "k"][
        is.na(p),
        p := 0
      ]$p
  )) /
    2
  total <- share[id_lulc_anterior != id_lulc_posterior, sum(p)]
  c(quantity = quantity, allocation = total - quantity, total = total)
}
run_cells <-
  db$get_query(glue::glue(
    "
    select
      s.id_run,
      a.id_lulc as id_lulc_1991,
      o.id_lulc as id_lulc_observed,
      s.id_lulc as id_lulc_simulated,
      count(*)::integer as n_cells
    from
      dl_db.lulc_data_t s
      inner join dl_db.lulc_data_t a
        on a.id_coord = s.id_coord and a.id_run = 0 and a.id_period = 2
      inner join dl_db.lulc_data_t o
        on o.id_coord = s.id_coord and o.id_run = {id_run_observed} and o.id_period = 3
    where
      s.id_period = 3
      and s.id_run in ({paste(members$id_run, collapse = ', ')})
    group by all
    "
  ))
stopifnot(setequal(run_cells$id_run, members$id_run))
simulated_transitions <-
  run_cells[,
    .(n_cells = sum(n_cells)),
    by = .(id_run, id_lulc_anterior = id_lulc_1991, id_lulc_posterior = id_lulc_simulated)
  ]
disagreement <-
  run_cells[,
    as.list(c(
      pontius(.SD[,
        .(n_cells = sum(n_cells)),
        by = .(id_lulc_anterior = id_lulc_observed, id_lulc_posterior = id_lulc_simulated)
      ]),
      gross_change = sum(n_cells[id_lulc_1991 != id_lulc_simulated])
    )),
    by = id_run
  ][
    members[, .(id_run, ensemble)],
    on = "id_run"
  ]
disagreement_ensemble <-
  disagreement[,
    .(
      quantity = mean(quantity),
      allocation = mean(allocation),
      total = mean(total),
      gross_change = mean(gross_change)
    ),
    by = ensemble
  ]
observed_transitions <- crosstab(id_run_observed, id_run_observed, 2L)
observed_gross_change <- observed_transitions[id_lulc_anterior != id_lulc_posterior, sum(n_cells)]
observed_gross_change
disagreement_ensemble[order(total)]

#' Simulated transitions, ensemble means, against the observed counts; the class-total
#' allocators need not reproduce them.

#| label: transitions
transitions_ensemble <-
  simulated_transitions[
    members[, .(id_run, ensemble)],
    on = "id_run"
  ][
    id_lulc_anterior != id_lulc_posterior,
    .(n_cells = sum(n_cells) / uniqueN(id_run)),
    by = .(ensemble, transition = paste0(id_lulc_anterior, "->", id_lulc_posterior))
  ]
dcast(
  rbind(
    transitions_ensemble,
    observed_transitions[
      id_lulc_anterior != id_lulc_posterior,
      .(
        ensemble = "observed",
        transition = paste0(id_lulc_anterior, "->", id_lulc_posterior),
        n_cells
      )
    ]
  ),
  ensemble ~ transition,
  value.var = "n_cells",
  fill = 0
)

# maps for the fuzzy similarity and the outcome figure
lulc_wide <- dcast(
  db$fetch(
    "lulc_data_t",
    where = glue::glue(
      "id_period = 3 and id_run in ({paste(c(id_run_observed, members$id_run), collapse = ',')})"
    )
  ),
  id_coord ~ id_run,
  value.var = "id_lulc"
)
anterior_1991 <-
  db$fetch("lulc_data_t", where = "id_run = 0 and id_period = 2")[
    lulc_wide[, .(id_coord)],
    on = "id_coord"
  ]$id_lulc
observed_1999 <- lulc_wide[[as.character(id_run_observed)]]

#' # Fuzzy similarity of the built-land gains
#'
#' Hagen's fuzzy similarity of differences, window 11 cells with exponential decay (divisor 2),
#' for forest → built and other → built, the two largest observed transitions. The overall
#' similarity is the minimum of the two directions.

#| label: fuzzy
# runs in parallel in forked workers (PIE_WORKERS, default: all cores), which must not touch the
# database connection: the coordinates are read once, here
coords_minimal <- db$coords_minimal
lulc_map <- function(id_run) {
  tabular_to_raster(
    lulc_wide[, .(id_coord, id_lulc = get(as.character(id_run)))],
    coords_minimal
  )
}
map_1991 <- tabular_to_raster(
  data.table(id_coord = lulc_wide$id_coord, id_lulc = anterior_1991),
  coords_minimal
)
map_1999 <- lulc_map(id_run_observed)
n_workers <- as.integer(Sys.getenv("PIE_WORKERS", parallel::detectCores()))
fuzzy <-
  rbindlist(parallel::mclapply(mc.cores = n_workers, members$id_run, function(id) {
    simulated <- lulc_map(id)
    rbindlist(lapply(list(c(1L, 2L), c(3L, 2L)), function(tr) {
      s <- calc_transition_similarity(
        map_1991,
        map_1999,
        simulated,
        from_class = tr[1],
        to_class = tr[2],
        window_size = 11L,
        use_exp_decay = TRUE,
        decay_divisor = 2
      )
      data.table(id_run = id, transition = paste0(tr[1], "->", tr[2]), similarity = s$similarity)
    }))
  }))[
    members[, .(id_run, ensemble)],
    on = "id_run"
  ]
fuzzy_ensemble <- dcast(
  fuzzy[, .(similarity = mean(similarity)), by = .(ensemble, transition)],
  ensemble ~ transition,
  value.var = "similarity"
)
fuzzy_ensemble

#' # Estimator × allocator variance decomposition
#'
#' The crossing of every estimator with every stochastic allocator, 20 replicates each (the
#' deterministic greedy allocator has no replicate variance and is left out). Two-way ANOVA sums
#' of squares, as shares of the total.

#| label: anova
crossed <- fom[as_shipped == FALSE & allocator_label != "greedy"]
fit_anova <- stats::aov(figure_of_merit ~ estimator_label * allocator_label, data = crossed)
ss <- summary(fit_anova)[[1]]
variance_shares <- data.table(
  component = c("estimator", "allocator", "interaction", "replicate (stochastic)"),
  sum_sq = ss[["Sum Sq"]],
  share = ss[["Sum Sq"]] / sum(ss[["Sum Sq"]]),
  p_value = c(ss[["Pr(>F)"]][1:3], NA)
)
variance_shares
dcast(
  fom[
    as_shipped == FALSE,
    .(fom = mean(figure_of_merit)),
    by = .(estimator_label, allocator_label)
  ],
  estimator_label ~ allocator_label,
  value.var = "fom"
)

#' # Wall time

#| label: timings
timings <- rbindlist(lapply(
  list.files(outputs_dir, pattern = "^timings-.*\\.csv$", full.names = TRUE),
  fread
))
timings_summary <-
  timings[,
    .(n = .N, total_s = sum(seconds), median_s = stats::median(seconds)),
    by = .(tool, stage)
  ][
    order(tool, stage)
  ]
timings_summary

#' # Figures

#| label: fig-fom
#| fig-width: 7
#| fig-height: 5
allocator_levels <- allocators$label
allocator_colours <- setNames(
  c("#2a78d6", "#eb6834", "#4a3aa7", "#1baf7a", "#eda100", "#e87ba4"),
  allocator_levels
)
allocator_pch <- setNames(c(16, 17, 8, 15, 18, 6), allocator_levels)
# rows: the crossing's estimators, then each tool as shipped
fom[, row := fifelse(as_shipped, paste0(estimator_label, ", as shipped"), estimator_label)]
fom[, allocator_key := sub(" (simulate.ego)", "", allocator_label, fixed = TRUE)]
row_levels <- c(estimators$label, unique(fom[as_shipped == TRUE, row]))

# rows top to bottom, the allocators side by side within a row
fom[,
  `:=`(
    y = match(row, row_levels) + (match(allocator_key, allocator_levels) - 3.5) * 0.12,
    allocator_f = factor(allocator_key, levels = allocator_levels)
  )
]
fig_fom <- ggplot(fom, aes(figure_of_merit, y, colour = allocator_f, shape = allocator_f)) +
  geom_hline(yintercept = seq_along(row_levels)[-1] - 0.5, colour = "grey90") +
  geom_hline(yintercept = nrow(estimators) + 0.5, colour = "grey40") +
  geom_vline(xintercept = mean(fom$figure_of_merit_null), linetype = 2, colour = "grey40") +
  annotate(
    "text",
    x = mean(fom$figure_of_merit_null),
    y = 0.45,
    label = "random allocation",
    hjust = 1.05,
    size = 2.8,
    colour = "grey30"
  ) +
  geom_point(size = 1.3) +
  scale_colour_manual(values = allocator_colours, drop = FALSE) +
  scale_shape_manual(values = allocator_pch, drop = FALSE) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  scale_y_reverse(breaks = seq_along(row_levels), labels = row_levels) +
  labs(
    x = "Figure of merit, 1991 to 1999 (20 realisations each)",
    y = NULL,
    colour = "allocator",
    shape = "allocator"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom"
  )
fig_fom
ggsave(file.path(figures_dir, "pie-figure-of-merit.pdf"), fig_fom, width = 7, height = 5.5)

#| label: fig-maps
#| fig-width: 9
#| fig-height: 6
# one estimator (random forest) through every allocator: the median-FoM realisation of each,
# coloured by outcome
map_panels <- paste("random forest", "×", allocators$label)
outcome_map <- function(id) {
  simulated <- lulc_wide[[as.character(id)]]
  obs_change <- observed_1999 != anterior_1991
  sim_change <- simulated != anterior_1991
  outcome <- fcase(
    obs_change & sim_change & simulated == observed_1999 , 1L ,
    obs_change & !sim_change                             , 2L ,
    !obs_change & sim_change                             , 3L ,
    obs_change & sim_change                              , 4L ,
    default = 0L
  )
  tabular_to_raster(data.table(id_coord = lulc_wide$id_coord, id_lulc = outcome), db$coords_minimal)
}
outcome_levels <- c("persistence (both)", "hit", "miss", "false alarm", "wrong hit")
outcome_colours <- setNames(
  c("#ecebe6", "#1baf7a", "#2a78d6", "#e34948", "#eda100"),
  outcome_levels
)
outcome_cells <- rbindlist(lapply(map_panels, function(e) {
  runs_e <- fom[ensemble == e][order(figure_of_merit)]
  id <- runs_e$id_run[ceiling(nrow(runs_e) / 2)]
  cells <- as.data.table(terra::as.data.frame(outcome_map(id), xy = TRUE))
  setnames(cells, 3, "outcome")
  cells[,
    panel := sprintf(
      "%s\nFoM %.3f",
      sub(" × ", " / ", e, fixed = TRUE),
      runs_e[id_run == id, figure_of_merit]
    )
  ]
}))
outcome_cells[,
  `:=`(
    outcome = factor(outcome_levels[outcome + 1L], levels = outcome_levels),
    panel = factor(panel, levels = unique(panel))
  )
]
fig_maps <- ggplot(outcome_cells, aes(x, y, fill = outcome)) +
  geom_raster() +
  scale_fill_manual(values = outcome_colours, drop = FALSE) +
  facet_wrap(~panel, ncol = 3) +
  coord_equal() +
  labs(fill = NULL) +
  theme_void(base_size = 9) +
  theme(legend.position = "bottom")
fig_maps
ggsave(file.path(figures_dir, "pie-outcome-maps.pdf"), fig_maps, width = 9, height = 6.4)

#' # Tables written

#| label: write
fwrite(fom, file.path(figures_dir, "pie-fom-per-run.csv"))
fwrite(
  fom_ensemble[disagreement_ensemble, on = "ensemble"][fuzzy_ensemble, on = "ensemble"],
  file.path(figures_dir, "pie-ensemble-summary.csv")
)
fwrite(fom_by_transition, file.path(figures_dir, "pie-fom-by-transition.csv"))
fwrite(transitions_ensemble, file.path(figures_dir, "pie-simulated-transitions.csv"))
fwrite(variance_shares, file.path(figures_dir, "pie-variance-shares.csv"))
fwrite(timings_summary, file.path(figures_dir, "pie-timings.csv"))
