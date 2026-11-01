#' ---
#' title: "PIE benchmark: figure of merit and outcome maps (plot)"
#' date: last-modified
#' ---
#'
#' The two overview figures of `050-compare.r`, drawn from its outputs (`figures/pie-fom-per-run.csv`,
#' `results/`) so that they can be iterated on without the database: needs data.table, arrow and
#' ggplot2 only. Run from the repository root.
#'
#' - `figures/pie-figure-of-merit.pdf`: FoM of every realisation, per estimator and allocator;
#' - `figures/pie-outcome-maps.pdf`: the random forest through every allocator, the median-FoM
#'   realisation of each, coloured by outcome.

#| label: setup
#| output: false
library(data.table)
library(ggplot2)
source("2026-09-model-comparison/common.r")
figures_dir <- file.path(pie_dir, "figures")
res_dir <- file.path(pie_dir, paste0("results", scale_suffix))
fom <- fread(file.path(figures_dir, "pie-fom-per-run.csv"))
cells <- as.data.table(arrow::read_parquet(file.path(res_dir, "cells.parquet")))
changes <- as.data.table(arrow::read_parquet(file.path(res_dir, "changes.parquet")))

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
outcome_of <- function(id) {
  simulated <- cells$anterior
  ch <- changes[id_run == id]
  simulated[match(ch$id_coord, cells$id_coord)] <- ch$simulated
  obs_change <- cells$observed != cells$anterior
  sim_change <- simulated != cells$anterior
  fcase(
    obs_change & sim_change & simulated == cells$observed , 1L ,
    obs_change & !sim_change                              , 2L ,
    !obs_change & sim_change                              , 3L ,
    obs_change & sim_change                               , 4L ,
    default = 0L
  )
}
outcome_levels <- c("persistence (both)", "hit", "miss", "false alarm", "wrong hit")
outcome_colours <- setNames(
  c("#ecebe6", "#1baf7a", "#2a78d6", "#e34948", "#eda100"),
  outcome_levels
)
outcome_cells <- rbindlist(lapply(map_panels, function(e) {
  runs_e <- fom[ensemble == e][order(figure_of_merit)]
  id <- runs_e$id_run[ceiling(nrow(runs_e) / 2)]
  data.table(
    x = cells$x,
    y = cells$y,
    outcome = outcome_of(id),
    panel = sprintf(
      "%s\nFoM %.3f",
      sub(" × ", " / ", e, fixed = TRUE),
      runs_e[id_run == id, figure_of_merit]
    )
  )
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
