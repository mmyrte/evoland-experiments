#' ---
#' title: "PIE benchmark: paper Fig. 3, the estimator × allocator matrix"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' Draws the paper's Fig. 3 from the tables `050-compare.r` and `051-validation-metrics.r`
#' write, for PIE or, with `PIE_DATASET=synthetic`, for the synthetic landscape
#' (`000-synthetic-data.r`). The figure argues for probabilistic evaluation, so it shows only the
#' ensembles' fair Brier skill (Overleaf comments: the version coloured by FoM made the single
#' deterministic maps look best, and FoM in brackets made it too busy).
#'
#' - **(a)** fair Brier skill of each ensemble (relative to the uniform forecast, i.e. knowing only
#'   the quantities), for every estimator crossed with every allocator, with the potentials
#'   themselves as a first column (the forecast before any allocation) and each tool as shipped
#'   (outlined) below. Allocators are named as allocators: CLUMPY's patch allocator is uPAM.
#' - **(b)** what the variance of the Brier score is due to, over the crossing of the stochastic
#'   allocators.
#'
#' Output: `figures/fig3-pie-matrix.pdf` (`ggsave()`, with the platform's default PDF device), copied
#' by hand to `evoland-plus-paper/figures/`.

#| label: setup
#| output: false
library(data.table)
library(ggplot2)
library(patchwork)
source("2026-09-model-comparison/common.r")
results_dir <- file.path(pie_dir, paste0("results", scale_suffix))
summary_dt <- fread(file.path(figures_dir, "pie-ensemble-summary.csv"))
forecast <- fread(file.path(results_dir, "metrics-forecast.csv"), encoding = "UTF-8")
shares <- fread(file.path(results_dir, "metrics-variance-shares.csv"))

# allocators in the order of the argument: stochastic first, then the (near-)deterministic ones
alloc_order <- c("potentials", "CLUMPY", "Dinamica", "CLUE-S", "CLUMondo", "Ordered", "greedy")
alloc_labels <- c(
  "potentials" = "potentials\n(no allocation)",
  "CLUMPY" = "uPAM\n(CLUMPY)",
  "Dinamica" = "Dinamica\nEGO",
  "CLUE-S" = "CLUE-S\n(lulcc)",
  "CLUMondo" = "CLUMondo\n(CLUinPy)",
  "Ordered" = "Ordered\n(lulcc)",
  "greedy" = "rank-and-fill\n(evoland)"
)

ens <- forecast[kind == "ensemble"]
ens[, allocator := sub(" (simulate.ego)", "", allocator_label, fixed = TRUE)]
fom <- summary_dt[, .(estimator_label, allocator = sub(" (simulate.ego)", "", allocator_label, fixed = TRUE), as_shipped, figure_of_merit)]
ens <- fom[ens, on = .(estimator_label, allocator, as_shipped)]
pot <- forecast[kind == "potentials", .(
  estimator_label = sub(" potentials$", "", forecast), allocator = "potentials", as_shipped = FALSE,
  brier_skill, figure_of_merit = NA_real_
)]
cells_dt <- rbind(ens[, .(estimator_label, allocator, as_shipped, brier_skill, figure_of_merit)], pot)
cells_dt[, row := fifelse(as_shipped, paste0(estimator_label, " (as shipped)"), estimator_label)]
row_levels <- c(estimators$label, unique(cells_dt[as_shipped == TRUE, row]))
cells_dt[, `:=`(
  row = factor(row, levels = rev(row_levels)),
  allocator = factor(allocator, levels = alloc_order),
  cell_label = sprintf("%+.2f", round(brier_skill, 2) + 0),
  # the colour scale is clamped to [-0.6, 0.2]; cells beyond keep their numbers
  skill_fill = pmin(pmax(brier_skill, -0.6), 0.2),
  dark = brier_skill < -0.4
)]

#' # Figure

#| label: fig3
#| fig-width: 8.6
#| fig-height: 4.6
n_cross <- nrow(estimators)
matrix_panel <- ggplot(cells_dt, aes(allocator, row)) +
  geom_tile(aes(fill = skill_fill), width = 0.96, height = 0.92) +
  geom_tile(
    data = cells_dt[as_shipped == TRUE],
    fill = NA, colour = "#0b0b0b", linewidth = 0.6, width = 0.96, height = 0.92
  ) +
  geom_text(aes(label = cell_label, colour = dark), size = 2.5, lineheight = 0.9, show.legend = FALSE) +
  geom_hline(yintercept = length(row_levels) - n_cross + 0.5, linewidth = 0.3) +
  annotate("segment", x = c(1.5, 3.5), xend = c(1.5, 3.5), y = length(row_levels) - n_cross + 0.5,
    yend = length(row_levels) + 1.3, linewidth = 0.3, colour = "#555555") +
  annotate("text", x = 2.5, y = length(row_levels) + 1.05, label = "stochastic", size = 2.6, colour = "#555555") +
  annotate("text", x = 5.5, y = length(row_levels) + 1.05, label = "deterministic or near-deterministic", size = 2.6, colour = "#555555") +
  # diverging around zero, skill of knowing only the quantities: blue better, red worse
  scale_fill_gradient2(
    low = "#b2182b", mid = "#f7f7f7", high = "#2166ac", midpoint = 0,
    limits = c(-0.6, 0.2), breaks = c(-0.6, -0.3, 0, 0.2), labels = c("-0.6 or less", "-0.3", "0", "0.2 or more"),
    name = "fair Brier skill",
    guide = guide_colourbar(title.position = "top", title.hjust = 0.5)
  ) +
  scale_colour_manual(values = c(`FALSE` = "#0b0b0b", `TRUE` = "white")) +
  scale_x_discrete(position = "top", labels = alloc_labels, drop = FALSE) +
  scale_y_discrete(expand = expansion(add = c(0.5, 1.4))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "(a) Fair Brier skill of each ensemble",
    subtitle = "skill relative to knowing only the quantities of change: above 0 better, below 0 worse; rows: estimators, columns: allocators",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid = element_blank(), plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 9),
    plot.subtitle = element_text(size = 7.5, colour = "#555555"),
    legend.position = "bottom", legend.title = element_text(size = 7.5), legend.text = element_text(size = 7),
    legend.key.height = unit(5, "pt"), legend.key.width = unit(28, "pt")
  )

share_metrics <- c("brier" = "Brier\nscore")
sh <- shares[metric %in% names(share_metrics)]
sh[, `:=`(
  metric = factor(share_metrics[metric], levels = share_metrics),
  component = factor(component, levels = rev(c("estimator", "allocator", "interaction", "replicate (stochastic)")))
)]
shares_panel <- ggplot(sh, aes(x = metric, y = share, fill = component)) +
  geom_col(width = 0.62, colour = "white") +
  geom_text(
    data = sh[share >= 0.06],
    aes(label = sprintf("%.0f %%", 100 * share)),
    position = position_stack(vjust = 0.5), size = 2.5, colour = "white"
  ) +
  scale_fill_manual(
    values = c("estimator" = "#7b5ea7", "allocator" = "#1a1a1a", "interaction" = "#8c8c8c", "replicate (stochastic)" = "#c9c7bf"),
    breaks = c("estimator", "allocator", "interaction", "replicate (stochastic)"),
    name = NULL
  ) +
  scale_y_continuous(labels = scales::percent, expand = expansion(0)) +
  labs(title = "(b) Variance of the\nBrier score due to", x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid = element_blank(), plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 9),
    legend.position = "bottom", legend.direction = "vertical", legend.text = element_text(size = 7),
    legend.key.size = unit(7, "pt")
  )

fig3 <- matrix_panel + shares_panel + plot_layout(widths = c(5, 0.8))
fig3
ggsave(file.path(figures_dir, "fig3-pie-matrix.pdf"), fig3, width = 8.6, height = 4.6)
