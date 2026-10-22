#' ---
#' title: "PIE benchmark: paper Fig. 3, the estimator × allocator matrix"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' Draws the paper's Fig. 3 from the tables `050-compare.r` writes:
#'
#' - **(a)** ensemble figure of merit in each cell of the estimator × allocator crossing, as a
#'   multiple of the random-allocation null, and below it each tool as shipped with its own
#'   estimator (outlined).
#' - **(b)** shares of the per-run FoM variance over the crossing of the stochastic allocators.
#'
#' Output: `figures/fig3-pie-matrix.pdf` (`ggsave()`, with the platform's default PDF device), copied
#' by hand to `evoland-plus-paper/figures/`.

#| label: setup
#| output: false
library(data.table)
library(ggplot2)
library(patchwork)
source("2026-09-model-comparison/common.r")
figures_dir <- file.path(pie_dir, "figures")
summary_dt <- fread(file.path(figures_dir, "pie-ensemble-summary.csv"))
shares <- fread(file.path(figures_dir, "pie-variance-shares.csv"))

# rows: the crossing's estimators, then each tool as shipped; columns: the allocators
summary_dt[, allocator := sub(" (simulate.ego)", "", allocator_label, fixed = TRUE)]
summary_dt[, row := fifelse(as_shipped, paste0(estimator_label, "\n(as shipped)"), estimator_label)]
row_levels <- c(estimators$label, unique(summary_dt[as_shipped == TRUE, row]))
allocator_labels <- setNames(
  paste0(
    allocators$label,
    "\n(",
    sub("Dinamica EGO via evoland", "Dinamica", allocators$source),
    ")"
  ),
  allocators$label
)
summary_dt[,
  `:=`(
    row = factor(row, levels = rev(row_levels)),
    allocator = factor(allocator, levels = allocators$label),
    cell_label = sprintf("%.2f\n(%.3f)", skill_over_null, figure_of_merit),
    # white text on the darker half of the ramp
    dark = (skill_over_null - min(skill_over_null)) / diff(range(skill_over_null)) > 0.55
  )
]
# crossing cells without an ensemble stay grey
empty <-
  CJ(
    row = factor(estimators$label, levels = rev(row_levels)),
    allocator = factor(allocators$label, levels = allocators$label)
  )[
    !summary_dt,
    on = .(row, allocator)
  ]

#' # Figure

#| label: fig3
#| fig-width: 8.6
#| fig-height: 5
matrix_panel <- ggplot(summary_dt, aes(allocator, row)) +
  geom_tile(data = empty, fill = "#f4f3ef", width = 0.96, height = 0.92) +
  geom_tile(aes(fill = skill_over_null), width = 0.96, height = 0.92) +
  geom_tile(
    data = summary_dt[as_shipped == TRUE],
    fill = NA,
    colour = "#0b0b0b",
    linewidth = 0.6,
    width = 0.96,
    height = 0.92
  ) +
  geom_text(
    aes(label = cell_label, colour = dark),
    size = 2.6,
    lineheight = 0.9,
    show.legend = FALSE
  ) +
  geom_hline(yintercept = length(row_levels) - nrow(estimators) + 0.5, linewidth = 0.3) +
  scale_fill_gradient(low = "#e3eefb", high = "#0d3a73", guide = "none") +
  scale_colour_manual(values = c(`FALSE` = "#0b0b0b", `TRUE` = "white")) +
  scale_x_discrete(position = "top", labels = allocator_labels) +
  labs(
    title = "(a) figure of merit / random-allocation null (figure of merit)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(), plot.title.position = "plot")

shares[, component := factor(component, levels = rev(component))]
shares_panel <- ggplot(shares, aes(x = 1, y = share, fill = component)) +
  geom_col(width = 0.6, colour = "white") +
  geom_text(
    aes(x = 1.35, label = sprintf("%s\n%.0f%%", component, 100 * share)),
    position = position_stack(vjust = 0.5),
    hjust = 0,
    size = 2.6,
    lineheight = 0.9
  ) +
  scale_fill_manual(
    values = c("#2a78d6", "#eb6834", "#1baf7a", "#c9c7bf")[seq_len(nrow(shares))],
    breaks = levels(shares$component)[nrow(shares):1],
    guide = "none"
  ) +
  scale_y_continuous(labels = scales::percent, expand = expansion(0)) +
  scale_x_continuous(limits = c(0.6, 2.4), breaks = NULL) +
  labs(title = "(b) variance of FoM", x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(), plot.title.position = "plot")

fig3 <- matrix_panel + shares_panel + plot_layout(widths = c(3.2, 1))
fig3
ggsave(file.path(figures_dir, "fig3-pie-matrix.pdf"), fig3, width = 8.6, height = 5)
