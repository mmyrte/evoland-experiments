#' ---
#' title: "Figure 2: ensembles vs. single maps (plot)"
#' date: last-modified
#' ---
#'
#' Draws Fig. 2 from the results of `010-fig2-ensembles.r` (`results/fig2/`), so the figure can
#' be iterated on without rerunning the allocators. Needs only data.table, arrow, ggplot2 and
#' patchwork. Run from the repository root.
#'
#' Six cells of a 3 x 2 grid: three maps on top, the FoM strip across two cells and the bias
#' panel in the last one below. Map panels show one n_window x n_window block of the domain, the
#' block with the most observed change; every score, panel (d) and panel (e) use the whole
#' domain. Each panel keeps its own legend.

#| label: setup
#| output: false
library(data.table)
library(ggplot2)
library(patchwork)

res_dir <- "2026-09-paper-figures/results/fig2"
out_dir <- "2026-09-paper-figures/figures"

cells <- as.data.table(arrow::read_parquet(file.path(res_dir, "cells.parquet")))
runs <- fread(file.path(res_dir, "runs.csv"))
changes <- as.data.table(arrow::read_parquet(file.path(res_dir, "changes.parquet")))
fom <- fread(file.path(res_dir, "fom.csv"))
fom_ensemble <- fread(file.path(res_dir, "fom-ensemble.csv"))
fom_deterministic <- fread(file.path(res_dir, "fom-deterministic.csv"))
cdf <- fread(file.path(res_dir, "cdf.csv"))
scalars <- fread(file.path(res_dir, "scalars.csv"))
n_realisations <- scalars$n_realisations
n_window <- scalars$n_window
fom_null <- scalars$fom_null

allocator_levels <- c("CLUMPY", "Dinamica", "CLUE-S", "CLUMondo")

#| label: figure-data
coords_xy <- cells[block == TRUE, .(id_coord, x, y)]
maps <- cells[, .(id_coord, anterior, observed)]

#' A simulated map: the period-2 class, except where `changes.parquet` records a change
simulated_map <- function(run) {
  m <- maps[, .(id_coord, simulated = anterior)]
  m[changes[id_run == run], on = "id_coord", simulated := i.simulated]
  m[]
}

outcome <- function(anterior, observed, simulated) {
  obs_change <- anterior != observed
  sim_change <- anterior != simulated
  fcase(
    obs_change & sim_change & simulated == observed , "hit"         ,
    obs_change & sim_change                         , "wrong hit"   ,
    obs_change                                      , "miss"        ,
    sim_change                                      , "false alarm" ,
    default = "persistence"
  )
}

# the realisation shown: the CLUMPY draw closest to its ensemble median
fom_clumpy <- fom[allocator == "CLUMPY"]
shown_run <- fom_clumpy[order(abs(figure_of_merit - median(figure_of_merit)))][1, id_run]

lulc_names <- c("1" = "forest", "2" = "arable", "3" = "urban", "4" = "lake (fixed)")
panel_lulc <- maps[, .(id_coord, status = lulc_names[as.character(observed)])]
panel_draw <- maps[simulated_map(shown_run), on = "id_coord"][,
  .(id_coord, status = outcome(anterior, observed, simulated))
]
clumpy_runs <- runs[allocator == "CLUMPY", id_run]
panel_freq <- changes[id_run %in% clumpy_runs, .(n = .N), by = id_coord][
  maps[, .(id_coord)],
  on = "id_coord"
][, .(id_coord, p_change = fcoalesce(as.numeric(n), 0) / length(clumpy_runs))]
observed_change_cells <- maps[anterior != observed, .(id_coord)]

#| label: figure
#| fig-width: 7.2
#| fig-height: 7.4
col_surface <- "#f1f0ed"
# land cover as area fills, in conventional hues but stepped in lightness (urban darkest) so the
# classes separate under colour-vision deficiencies (validated: CVD and normal-vision distances
# >= 15 on all pairs); kept apart from the allocator and outcome hues used in the other panels
lulc_cols <- c(
  "forest" = "#5AAE61",
  "arable" = "#E8D48B",
  "urban" = "#45434F",
  "lake (fixed)" = "#8EC3E6"
)
# outcomes in two hue families (validated, CVD-safe on all pairs): change in the right place is
# blue (dark: right class, light: wrong class), errors are warm (vermilion: miss, orange: false
# alarm)
outcome_cols <- c(
  "persistence" = col_surface,
  "hit" = "#0072B2",
  "wrong hit" = "#56B4E9",
  "miss" = "#D55E00",
  "false alarm" = "#E69F00"
)
seq_blue <- c("#fcfcfb", "#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b")

theme_panel_text <- theme(
  plot.title = element_text(size = 9, face = "bold", hjust = 0, margin = margin(b = 3)),
  plot.subtitle = element_text(size = 7.5, colour = "#555555", margin = margin(b = 4)),
  legend.position = "bottom",
  legend.title = element_blank(),
  legend.key.size = unit(7, "pt"),
  legend.text = element_text(size = 7)
)
theme_map <- theme_void(base_size = 9) +
  theme_panel_text +
  theme(plot.margin = margin(2, 6, 2, 6))
# a white halo under the black outline keeps it visible on dark fills (urban) and light ones
outline <- function() {
  changed <- coords_xy[observed_change_cells, on = "id_coord", nomatch = NULL]
  list(
    geom_tile(data = changed, aes(x, y), fill = NA, colour = "white", linewidth = 1.5, width = 70, height = 70, inherit.aes = FALSE),
    geom_tile(data = changed, aes(x, y), fill = NA, colour = "#1a1a1a", linewidth = 0.7, width = 70, height = 70, inherit.aes = FALSE)
  )
}

p_a <- ggplot(coords_xy[panel_lulc, on = "id_coord", nomatch = NULL], aes(x, y, fill = status)) +
  geom_tile(colour = "white", linewidth = 0.15) +
  outline() +
  scale_fill_manual(values = lulc_cols, breaks = names(lulc_cols)) +
  coord_equal(expand = FALSE) +
  labs(
    title = "(a) Observed land use, period 3",
    subtitle = sprintf(
      "all cells; outline: changed since period 2\n%d \u00d7 %d window of the domain",
      n_window,
      n_window
    )
  ) +
  theme_map +
  guides(fill = guide_legend(nrow = 2))

p_b <- ggplot(coords_xy[panel_draw, on = "id_coord", nomatch = NULL], aes(x, y, fill = status)) +
  geom_tile(colour = "white", linewidth = 0.15) +
  scale_fill_manual(
    values = outcome_cols,
    breaks = intersect(names(outcome_cols)[-1], unique(panel_draw$status))
  ) +
  coord_equal(expand = FALSE) +
  labs(
    title = "(b) One realisation",
    subtitle = sprintf(
      "random forest, CLUMPY, median draw\nFoM %.2f (whole domain)",
      fom[id_run == shown_run, figure_of_merit]
    )
  ) +
  theme_map +
  guides(fill = guide_legend(nrow = 2))

p_c <- ggplot(coords_xy[panel_freq, on = "id_coord", nomatch = NULL], aes(x, y)) +
  geom_tile(aes(fill = p_change), colour = "white", linewidth = 0.15) +
  outline() +
  scale_fill_gradientn(
    colours = seq_blue,
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    name = "share of realisations",
    guide = guide_colourbar(title.position = "top", title.hjust = 0.5)
  ) +
  coord_equal(expand = FALSE) +
  labs(
    title = "(c) Change frequency",
    subtitle = "random forest, CLUMPY ensemble\noutline: observed change"
  ) +
  theme_map +
  theme(
    legend.title = element_text(size = 7),
    legend.key.width = unit(20, "pt"),
    legend.key.height = unit(5, "pt")
  )

# (d): one row per allocator, realisations jittered around it; the deterministic allocation below
# Okabe-Ito, in the same order as the metric figures (030-metric-relations-plot.r), so each
# allocator keeps its colour across the paper. CLUE-S and CLUMondo are only 7.6 apart under
# deuteranopia (validator floor 6-8): (d) names its rows, (e) adds linetypes.
allocator_cols <- c(
  "CLUMPY" = "#0072B2",
  "Dinamica" = "#E69F00",
  "CLUE-S" = "#009E73",
  "CLUMondo" = "#CC79A7",
  "rank-and-fill" = "#D55E00"
)
allocator_linetypes <- c(
  "observed change" = "solid",
  "CLUMPY" = "solid",
  "Dinamica" = "42",
  "CLUE-S" = "11",
  "CLUMondo" = "2212",
  "rank-and-fill" = "73"
)
row_levels <- c(allocator_levels, "rank-and-fill")
fom[, row := factor(as.character(allocator), levels = rev(row_levels))]
fom_ensemble[, row := factor(as.character(allocator), levels = rev(row_levels))]
fom_deterministic[, row := factor("rank-and-fill", levels = rev(row_levels))]
set.seed(7)
p_d <- ggplot(fom, aes(x = figure_of_merit, y = row)) +
  geom_vline(xintercept = 0, colour = "#9a9a9a", linewidth = 0.4) +
  geom_vline(xintercept = fom_null, colour = "#9a9a9a", linewidth = 0.4, linetype = "22") +
  # the nulls, labelled along their lines
  annotate(
    "text",
    x = 0,
    y = length(row_levels) + 0.4,
    label = "null: no change",
    angle = 90,
    hjust = 1,
    vjust = 1.4,
    size = 2.4,
    colour = "#555555"
  ) +
  annotate(
    "text",
    x = fom_null,
    y = length(row_levels) + 0.4,
    label = "null: random placement",
    angle = 90,
    hjust = 1,
    vjust = 1.4,
    size = 2.4,
    colour = "#555555"
  ) +
  geom_jitter(aes(colour = allocator), height = 0.18, width = 0, size = 0.8, alpha = 0.5) +
  geom_point(
    data = fom_ensemble,
    aes(shape = "ensemble expectation"),
    size = 2.6,
    fill = "#1a1a1a",
    colour = "white",
    stroke = 0.6
  ) +
  geom_point(
    data = fom_deterministic,
    aes(shape = "deterministic allocation"),
    size = 3,
    fill = allocator_cols[["rank-and-fill"]],
    colour = "white",
    stroke = 0.7
  ) +
  scale_colour_manual(values = allocator_cols, guide = "none") +
  scale_shape_manual(
    values = c("ensemble expectation" = 21, "deterministic allocation" = 23),
    name = NULL
  ) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1))) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0.02, 0.06))) +
  labs(
    title = "(d) Figure of merit against the held-out observation",
    subtitle = sprintf(
      "random-forest potentials; one dot per realisation (n = %d per allocator)",
      n_realisations
    ),
    x = "figure of merit",
    y = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme_panel_text +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "#e6e6e6", linewidth = 0.3)
  )

# (e): colour = allocator; black = observed change; band = CLUMPY's 5-95 % range
cdf_lines <- cdf[, .(x, cdf = med, what)]
what_cols <- c("observed change" = "#1a1a1a", allocator_cols)
cdf_lines[, what := factor(what, levels = names(what_cols))]
p_e <- ggplot() +
  geom_abline(slope = 1, intercept = 0, colour = "#9a9a9a", linewidth = 0.4, linetype = "22") +
  annotate(
    "text",
    x = 0.6,
    y = 0.66,
    label = "random placement",
    angle = 45,
    size = 2.3,
    colour = "#555555"
  ) +
  geom_ribbon(
    data = cdf[what == "CLUMPY"],
    aes(x = x, ymin = lo, ymax = hi),
    fill = allocator_cols[["CLUMPY"]],
    alpha = 0.2
  ) +
  geom_line(data = cdf_lines, aes(x, cdf, colour = what, linetype = what), linewidth = 0.55) +
  scale_colour_manual(values = what_cols, breaks = names(what_cols)) +
  scale_linetype_manual(values = allocator_linetypes, breaks = names(what_cols)) +
  scale_x_continuous(labels = function(v) paste0(v * 100, " %"), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "(e) Where change is placed",
    subtitle = "changed cells by their true probability;\nmedian realisation per allocator",
    x = "percentile among candidate cells",
    y = "cumulative share of change"
  ) +
  theme_minimal(base_size = 9) +
  theme_panel_text +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#e6e6e6", linewidth = 0.3)
  ) +
  guides(
    colour = guide_legend(ncol = 2, override.aes = list(linewidth = 0.7)),
    linetype = guide_legend(ncol = 2)
  )

fig2 <- p_a +
  p_b +
  p_c +
  p_d +
  p_e +
  plot_layout(design = "ABC\nDDE", heights = c(1, 1.25))
ggsave(
  file.path(out_dir, "fig2-ensembles.pdf"),
  fig2,
  width = 7.2,
  height = 7.4
  # device = cairo_pdf
)
fig2
