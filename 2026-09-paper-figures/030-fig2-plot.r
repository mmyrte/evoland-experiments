#' ---
#' title: "Figure 2: ensembles vs. single maps (plot)"
#' date: last-modified
#' ---
#'
#' Draws Fig. 2 from the results of `010-fig2-ensembles.r` (`results/fig2/`), so the figure can
#' be iterated on without rerunning the allocators. Needs only data.table, arrow, ggplot2 and
#' patchwork. Run from the repository root.
#'
#' Two figures (split after the Overleaf comment of 2026-10-23, "it's overload like this"):
#'
#' - `fig2a-fom.pdf`, conventional validation: the observed map, one realisation scored cell by
#'   cell, and how those cells make up its figure of merit;
#' - `fig2b-ensembles.pdf`, why ensembles: the change frequency of the CLUMPY ensemble, the FoM of
#'   every realisation with each allocator's Brier skill, and where change is placed.
#'
#' Map panels show one n_window x n_window block of the domain, the block with the most observed
#' change; every score uses the whole domain. Each panel keeps its own legend.

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

# the figures name the allocator, not the package: CLUMPY's single-cell allocator is uSAM
for (d in list(runs, fom, fom_ensemble)) d[allocator == "CLUMPY", allocator := "uSAM"]
cdf[what == "CLUMPY", what := "uSAM"]
allocator_levels <- c("uSAM", "Dinamica", "CLUE-S", "CLUMondo")

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

# the realisation shown: a typical CLUMPY draw (FoM within 0.005 of the ensemble median) that
# also shows every outcome in the map window, wrong hits included, so the FoM panel can be read
# off the map; among those, the one with the most wrong hits in the window
fom_clumpy <- fom[allocator == "uSAM"]
window_ids <- cells[block == TRUE, id_coord]

lulc_names <- c("1" = "forest", "2" = "arable", "3" = "urban", "4" = "lake (fixed)")
panel_lulc <- maps[, .(id_coord, status = lulc_names[as.character(observed)])]
window_wrong_hits <- function(run) {
  d <- maps[simulated_map(run), on = "id_coord"][id_coord %in% window_ids]
  d[, sum(outcome(anterior, observed, simulated) == "wrong hit")]
}
candidates <- fom_clumpy[abs(figure_of_merit - median(figure_of_merit)) <= 0.005]
candidates[, wrong_in_window := vapply(id_run, window_wrong_hits, integer(1))]
shown_run <- candidates[order(-wrong_in_window, abs(figure_of_merit - median(fom_clumpy$figure_of_merit)))][1, id_run]
stopifnot(candidates[id_run == shown_run, wrong_in_window] > 0)
panel_draw <- maps[simulated_map(shown_run), on = "id_coord"][,
  .(id_coord, status = outcome(anterior, observed, simulated))
]
clumpy_runs <- runs[allocator == "uSAM", id_run]
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
    geom_tile(data = changed, aes(x, y), fill = NA, colour = "white", linewidth = 0.55, width = 84, height = 84, inherit.aes = FALSE),
    geom_tile(data = changed, aes(x, y), fill = NA, colour = "#1a1a1a", linewidth = 0.28, width = 84, height = 84, inherit.aes = FALSE)
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
    subtitle = "random forest, uSAM, a typical draw,\nscored cell by cell"
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
    title = "(a) Change frequency",
    subtitle = "random forest, uSAM ensemble\noutline: observed change"
  ) +
  theme_map +
  theme(
    legend.title = element_text(size = 7),
    legend.key.width = unit(20, "pt"),
    legend.key.height = unit(5, "pt")
  )

# Fig. 2a (c): the realisation's cells by outcome, and the figure of merit computed from them
fom_shown <- fom[id_run == shown_run]
decomp <- data.table(
  status = factor(c("hit", "wrong hit", "miss", "false alarm"), levels = c("hit", "wrong hit", "miss", "false alarm")),
  n = c(fom_shown$hits, fom_shown$wrong_hits, fom_shown$misses, fom_shown$false_alarms)
)
decomp[, `:=`(xmax = cumsum(n), xmin = cumsum(n) - n)]
total <- decomp[, sum(n)]
p_fom <- ggplot(decomp) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = status), colour = "white", linewidth = 0.4) +
  geom_text(aes(x = (xmin + xmax) / 2, y = 1.18, label = n), size = 2.5, colour = "#333333") +
  annotate("segment", x = 0, xend = decomp[status == "hit", xmax], y = -0.18, yend = -0.18, colour = "#1a1a1a", linewidth = 0.5) +
  annotate("segment", x = 0, xend = total, y = -0.42, yend = -0.42, colour = "#1a1a1a", linewidth = 0.5) +
  annotate("text", x = decomp[status == "hit", xmax] + 8, y = -0.18, label = "hits", hjust = 0, size = 2.5) +
  annotate("text", x = total + 8, y = -0.42, label = "all changed cells, observed or simulated", hjust = 1, vjust = -0.6, size = 2.5) +
  annotate(
    "text", x = 0, y = -1.0, hjust = 0, size = 2.9,
    label = sprintf(
      "FoM = hits / all changed cells\n        = %d / %d = %.2f",
      fom_shown$hits, total, fom_shown$figure_of_merit
    )
  ) +
  annotate(
    "text", x = 0, y = -1.75, hjust = 0, size = 2.5, colour = "#555555",
    label = sprintf("random placement of the same quantities: %.2f", fom_null)
  ) +
  scale_fill_manual(values = outcome_cols[-1]) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(limits = c(-2.05, 1.35)) +
  labs(
    title = "(c) Figure of merit",
    subtitle = "cells of the whole domain by outcome;\npersistent cells do not count"
  ) +
  theme_void(base_size = 9) +
  theme_panel_text +
  theme(plot.margin = margin(2, 6, 2, 6)) +
  guides(fill = "none")  # same colours as (b), whose legend sits next to it

# Fig. 2b: (a) every realisation's figure of merit, and beside it each ensemble's Brier skill, on
# the same allocator rows; (b) where change is placed. The change-frequency map of an earlier
# version is gone: the trade-off between the two scores does not need it.
#
# Okabe-Ito, in the same order as the metric figures (030-metric-relations-plot.r), so each
# allocator keeps its colour across the paper; uPAM in the light blue of uSAM's family. CLUE-S and
# CLUMondo are only 7.6 apart under deuteranopia (validator floor 6-8): (a) names its rows, (b)
# adds linetypes.
allocator_cols <- c(
  "uSAM" = "#0072B2",
  "uPAM" = "#56B4E9",
  "Dinamica" = "#E69F00",
  "CLUE-S" = "#009E73",
  "CLUMondo" = "#CC79A7",
  "rank-and-fill" = "#D55E00"
)
allocator_linetypes <- c(
  "observed change" = "22",
  "uSAM" = "solid",
  "uPAM" = "solid",
  "Dinamica" = "42",
  "CLUE-S" = "11",
  "CLUMondo" = "2212",
  "rank-and-fill" = "73"
)
# rows: the stochastic allocators a little apart, the (near-)deterministic ones close together
row_y <- c(
  "uSAM" = 5.0, "uPAM" = 4.3, "Dinamica" = 3.6,
  "CLUE-S" = 2.5, "CLUMondo" = 2.0, "rank-and-fill" = 1.5
)
y_lim <- c(1.1, 5.9)
fom <- fom[allocator %in% names(row_y)]
fom_ensemble <- fom_ensemble[allocator %in% names(row_y)]
fom[, y := row_y[as.character(allocator)]]
fom_ensemble[, y := row_y[as.character(allocator)]]
fom_deterministic[, y := row_y[["rank-and-fill"]]]

# the Brier skill of each ensemble frequency (of the single map for rank-and-fill), against the
# uniform forecast; the references as vertical lines, like the FoM nulls: the null model, the
# uniform forecast itself (skill 0 by definition), and the potentials the allocators sample from
brier <- fread(file.path(res_dir, "brier.csv"))
brier[learner == "CLUMPY", learner := "uSAM"]
skill <- rbind(
  brier[forecast == "ensemble frequency (fair)", .(allocator = learner, skill)],
  brier[learner == "random forest" & forecast == "deterministic", .(allocator = "rank-and-fill", skill)]
)[allocator %in% names(row_y)]
skill[, y := row_y[allocator]]
skill_potentials <- brier[learner == "random forest" & forecast == "adjusted potentials", skill]

# a reference line, labelled along it from the top (or, where points sit near the top, the bottom)
null_label <- function(x, label, linetype, from = "top") {
  list(
    geom_vline(xintercept = x, colour = "#9a9a9a", linewidth = 0.4, linetype = linetype),
    annotate(
      "text", x = x, y = if (from == "top") y_lim[2] else y_lim[1], label = label, angle = 90,
      hjust = if (from == "top") 1 else 0, vjust = 1.4, size = 2.3, colour = "#555555"
    )
  )
}
theme_strip <- theme_minimal(base_size = 9) +
  theme_panel_text +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "#e6e6e6", linewidth = 0.3)
  )

set.seed(7)
p_d <- ggplot(fom, aes(x = figure_of_merit, y = y)) +
  null_label(0, "null: no change", "solid") +
  null_label(fom_null, "null: random placement", "22") +
  geom_jitter(aes(colour = allocator), height = 0.12, width = 0, size = 0.8, alpha = 0.5) +
  geom_point(data = fom_ensemble, aes(shape = "ensemble expectation"), size = 2.6, fill = "#1a1a1a", colour = "white", stroke = 0.6) +
  geom_point(
    data = fom_deterministic, aes(shape = "deterministic allocation"),
    size = 3, fill = allocator_cols[["rank-and-fill"]], colour = "white", stroke = 0.7
  ) +
  scale_colour_manual(values = allocator_cols, guide = "none") +
  scale_shape_manual(values = c("ensemble expectation" = 21, "deterministic allocation" = 23), name = NULL) +
  scale_y_continuous(breaks = row_y, labels = names(row_y), limits = y_lim, expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, NA), breaks = seq(0, 0.25, 0.05), expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    title = "(a) Single maps vs. ensembles",
    subtitle = sprintf(
      "left: figure of merit, one dot per realisation (n = %d each);\nright: Brier skill of each ensemble frequency",
      n_realisations
    ),
    x = "figure of merit",
    y = NULL
  ) +
  theme_strip

p_bs <- ggplot(skill, aes(x = skill, y = y)) +
  null_label(0, "null: uniform forecast", "solid") +
  null_label(skill_potentials, "potentials (no allocation)", "22", from = "bottom") +
  geom_point(aes(fill = allocator), shape = 21, size = 2.6, colour = "white", stroke = 0.6) +
  scale_fill_manual(values = allocator_cols, guide = "none") +
  scale_y_continuous(breaks = row_y, labels = NULL, limits = y_lim, expand = c(0, 0)) +
  scale_x_continuous(breaks = c(-0.3, 0, 0.2), labels = c("-0.3", "0", "0.2"), expand = expansion(mult = 0.12)) +
  labs(x = "Brier skill", y = NULL) +
  theme_strip +
  theme(plot.margin = margin(2, 4, 2, 14))

# (b): colour = allocator; the observed change drawn last as a thin dashed black line, so the
# allocator curves under it stay visible; bands: 5-95 % over realisations
cdf[what == "CLUMPY", what := "uSAM"]
cdf <- cdf[what %in% c("observed change", names(allocator_cols))]
cdf_lines <- cdf[what != "observed change", .(x, cdf = med, what)]
cdf_observed <- cdf[what == "observed change", .(x, cdf = med)]
what_cols <- c("observed change" = "#1a1a1a", allocator_cols)
cdf_lines[, what := factor(what, levels = names(what_cols))]
p_e <- ggplot() +
  geom_abline(slope = 1, intercept = 0, colour = "#9a9a9a", linewidth = 0.4, linetype = "22") +
  annotate("text", x = 0.6, y = 0.66, label = "random placement", angle = 45, size = 2.3, colour = "#555555") +
  geom_ribbon(data = cdf[!is.na(lo)], aes(x = x, ymin = lo, ymax = hi, fill = what), alpha = 0.18, show.legend = FALSE) +
  geom_line(data = cdf_lines, aes(x, cdf, colour = what, linetype = what), linewidth = 0.6) +
  geom_line(data = cdf_observed, aes(x, cdf, colour = "observed change", linetype = "observed change"), linewidth = 0.55) +
  scale_fill_manual(values = allocator_cols, guide = "none") +
  scale_colour_manual(values = what_cols, breaks = names(what_cols)) +
  scale_linetype_manual(values = allocator_linetypes, breaks = names(what_cols)) +
  scale_x_continuous(labels = function(v) paste0(v * 100, " %"), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "(b) Where change is placed",
    subtitle = "changed cells by true probability;\nline: median, band: 5 to 95 % of realisations",
    x = "percentile among candidate cells",
    y = "cumulative share of change"
  ) +
  theme_minimal(base_size = 9) +
  theme_panel_text +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "#e6e6e6", linewidth = 0.3)) +
  guides(
    colour = guide_legend(ncol = 2, override.aes = list(linewidth = 0.7)),
    linetype = guide_legend(ncol = 2)
  )

fig2a <- p_a + p_b + p_fom + plot_layout(widths = c(1, 1, 1.15))
ggsave(file.path(out_dir, "fig2a-fom.pdf"), fig2a, width = 7.2, height = 3.2)

fig2b <- (p_d + p_bs + plot_layout(widths = c(2.2, 1), guides = "collect") & theme(legend.position = "bottom")) |
  p_e
fig2b <- fig2b + plot_layout(widths = c(3.2, 2))
ggsave(file.path(out_dir, "fig2b-ensembles.pdf"), fig2b, width = 7.2, height = 3.4)
fig2a
fig2b
