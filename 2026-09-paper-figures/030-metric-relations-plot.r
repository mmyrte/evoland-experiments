#' ---
#' title: "Fig. 2 case: how the validation metrics relate (plot)"
#' date: last-modified
#' ---
#'
#' Plots the metrics of `020-validation-metrics.r` from `results/fig2/`; needs only data.table,
#' arrow, ggplot2 and patchwork. Run from the repository root. Two figures:
#'
#' - `figures/metric-relations.pdf`: every single-map metric against how concentrated the map's
#'   change is (share in the top 5 % of the true probabilities), one dot per map. The observed
#'   change sets the reference: an unbiased allocator of the true probabilities sits on that line.
#'   A metric that keeps rising to the right of it rewards allocation bias.
#' - `figures/metric-agreement.pdf`: (a) Spearman correlations between the metrics over all
#'   maps; (b) the ensembles and surfaces as probabilistic forecasts.

#| label: setup
#| output: false
library(data.table)
library(ggplot2)
library(patchwork)

res_dir <- "2026-09-paper-figures/results/fig2"
out_dir <- "2026-09-paper-figures/figures"
per_run <- fread(file.path(res_dir, "metrics-per-run.csv"))
forecast <- fread(file.path(res_dir, "metrics-forecast.csv"))

# Okabe-Ito, fixed order; shapes as a second encoding (validated: CVD separation needs it)
allocator_cols <- c(
  "CLUMPY" = "#0072B2",
  "Dinamica" = "#E69F00",
  "CLUE-S" = "#009E73",
  "CLUMondo" = "#CC79A7",
  "rank-and-fill" = "#D55E00"
)
allocator_shapes <- c("CLUMPY" = 16, "Dinamica" = 17, "CLUE-S" = 15, "CLUMondo" = 18, "rank-and-fill" = 23)
ink <- "#1a1a1a"
muted <- "#555555"

metrics <- data.table(
  metric = c(
    "figure_of_merit",
    "ksim",
    "fks_r4",
    "similarity_of_differences",
    "auc_transition",
    "allocation_disagreement",
    "brier"
  ),
  label = c(
    "Figure of merit",
    "Kappa Simulation",
    "Fuzzy Kappa Simulation (r = 4)",
    "Similarity of differences",
    "TOC AUC, per transition",
    "Allocation disagreement (lower is better)",
    "Brier score (lower is better)"
  )
)

theme_fig <- theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(size = 9, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 7.5, colour = muted),
    strip.text = element_text(size = 7.5, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#e6e6e6", linewidth = 0.3),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 7)
  )

#| label: relations
#| fig-width: 7.2
#| fig-height: 6
maps <- per_run[allocator != "observed"]
obs_top5 <- per_run[allocator == "observed", top5_truth]
long <- melt(
  maps,
  id.vars = c("id_run", "allocator", "top5_truth"),
  measure.vars = metrics$metric,
  variable.name = "metric"
)[metrics, on = "metric"]
long[, label := factor(label, levels = metrics$label)]
long[, allocator := factor(allocator, levels = names(allocator_cols))]

p_rel <- ggplot(long, aes(top5_truth, value, colour = allocator, shape = allocator)) +
  geom_vline(xintercept = obs_top5, colour = ink, linewidth = 0.4, linetype = "22") +
  geom_point(size = 1.1, alpha = 0.6) +
  facet_wrap(~label, scales = "free_y", ncol = 3) +
  scale_colour_manual(values = allocator_cols) +
  scale_shape_manual(values = allocator_shapes) +
  scale_x_continuous(labels = function(v) paste0(round(v * 100), " %")) +
  labs(
    title = "Single-map metrics against how concentrated the simulated change is",
    subtitle = sprintf(
      "one point per map; dashed: the observed change (%.0f %% in the top 5 %%), where an unbiased allocator of the truth sits",
      100 * obs_top5
    ),
    x = "share of simulated change in the top 5 % of the true probabilities",
    y = NULL
  ) +
  theme_fig +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 2, alpha = 1)))
ggsave(file.path(out_dir, "metric-relations.pdf"), p_rel, width = 7.2, height = 6)
p_rel

#| label: agreement
#| fig-width: 7.2
#| fig-height: 3.8
cor_cols <- c(metrics$metric, "top5_truth")
cor_labels <- c(
  "FoM",
  "KSim",
  "FKS",
  "SoD",
  "AUC",
  "alloc. disagr.",
  "Brier",
  "concentration"
)
cm <- cor(as.matrix(maps[, ..cor_cols]), method = "spearman")
dimnames(cm) <- list(cor_labels, cor_labels)
cm_long <- as.data.table(as.table(cm))[, setNames(.SD, c("a", "b", "rho"))]
cm_long[, `:=`(a = factor(a, levels = cor_labels), b = factor(b, levels = rev(cor_labels)))]
p_cor <- ggplot(cm_long, aes(a, b, fill = rho)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", rho)), size = 2.2, colour = ink) +
  scale_fill_gradient2(low = "#b2182b", mid = "#f1f0ed", high = "#2166ac", limits = c(-1, 1), name = "Spearman") +
  labs(title = "(a) Rank correlation between metrics", subtitle = "over all maps of the four ensembles and rank-and-fill", x = NULL, y = NULL) +
  coord_equal() +
  theme_fig +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.title = element_text(size = 7)
  )

fc <- melt(
  forecast,
  id.vars = "forecast",
  measure.vars = c("auc_transition", "brier_skill"),
  variable.name = "metric"
)
fc[, metric := fcase(
  metric == "auc_transition", "TOC AUC per transition",
  metric == "brier_skill", "Brier skill"
)]
fc[, forecast := factor(forecast, levels = rev(unique(forecast)))]
# no-skill references: AUC 0.5 (random ranking), Brier skill 0 (the uniform forecast)
baseline <- data.table(metric = c("TOC AUC per transition", "Brier skill"), x = c(0.5, 0))
p_fc <- ggplot(fc, aes(value, forecast)) +
  geom_vline(data = baseline, aes(xintercept = x), colour = muted, linewidth = 0.4, linetype = "22") +
  geom_segment(
    data = fc[baseline, on = "metric"],
    aes(x = x, xend = value, yend = forecast),
    colour = "#c8c8c8",
    linewidth = 0.5
  ) +
  geom_point(size = 2.2, colour = ink) +
  facet_wrap(~metric, scales = "free_x") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  labs(
    title = "(b) Ensembles and surfaces as forecasts",
    subtitle = "dashed: no skill; fair Brier score for the ensembles",
    x = NULL,
    y = NULL
  ) +
  theme_fig
fig_agree <- p_cor + p_fc + plot_layout(widths = c(1, 1.25))
ggsave(file.path(out_dir, "metric-agreement.pdf"), fig_agree, width = 7.2, height = 3.8)
fig_agree
