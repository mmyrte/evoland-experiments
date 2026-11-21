#' ---
#' title: "PIE benchmark: how the validation metrics relate (plot)"
#' date: last-modified
#' ---
#'
#' Appendix figures from `051-validation-metrics.r` (`results/`); needs data.table, arrow,
#' ggplot2 and patchwork only. Run from the repository root.
#'
#' - `figures/pie-metric-relations.pdf`: every single-map metric of the crossing against how
#'   much the map concentrates change beyond its potentials: the share of its change in the top
#'   5 % of its estimator's potentials, minus the share an unbiased sampler of those potentials
#'   puts there. 0 is unbiased; a metric that rises to the right rewards allocation bias.
#' - `figures/pie-metric-agreement.pdf`: (a) the share of each metric's variance due to the
#'   estimator, the allocator, their interaction and the replicates; (b) Spearman correlations
#'   between the metrics; (c) the ensembles and potentials as probabilistic forecasts.

#| label: setup
#| output: false
library(data.table)
library(ggplot2)
library(patchwork)
source("2026-09-model-comparison/common.r")
res_dir <- file.path(pie_dir, paste0("results", scale_suffix))
per_run <- fread(file.path(res_dir, "metrics-per-run.csv"))
forecast <- fread(file.path(res_dir, "metrics-forecast.csv"))
shares <- fread(file.path(res_dir, "metrics-variance-shares.csv"))

# Okabe-Ito in a fixed order, shapes as a second encoding
allocator_cols <- setNames(
  c("#0072B2", "#E69F00", "#D55E00", "#009E73", "#56B4E9", "#CC79A7"),
  allocators$label
)
allocator_shapes <- setNames(c(16, 17, 8, 15, 5, 18), allocators$label)
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
  ),
  short = c("FoM", "KSim", "FKS", "SoD", "AUC", "alloc. disagr.", "Brier")
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
#| fig-height: 6.2
crossing <- per_run[as_shipped == FALSE]
crossing[, excess := top5_potential - top5_expected]
long <- melt(
  crossing,
  id.vars = c("id_run", "estimator_label", "allocator_label", "excess"),
  measure.vars = metrics$metric,
  variable.name = "metric"
)[metrics, on = "metric"]
long[, `:=`(
  label = factor(label, levels = metrics$label),
  allocator_label = factor(allocator_label, levels = allocators$label)
)]
p_rel <- ggplot(long, aes(excess, value, colour = allocator_label, shape = allocator_label)) +
  geom_vline(xintercept = 0, colour = ink, linewidth = 0.4, linetype = "22") +
  geom_point(size = 1, alpha = 0.6) +
  facet_wrap(~label, scales = "free_y", ncol = 3) +
  scale_colour_manual(values = allocator_cols) +
  scale_shape_manual(values = allocator_shapes) +
  scale_x_continuous(labels = function(v) paste0(round(v * 100), " pp")) +
  labs(
    title = "Single-map metrics against allocation bias, PIE 1991 to 1999",
    subtitle = "one point per map, five estimators x six allocators; dashed: an unbiased sampler of the potentials",
    x = "share of change in the top 5 % of the potentials, minus the share an unbiased sampler puts there",
    y = NULL
  ) +
  theme_fig +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 2, alpha = 1)))
ggsave(file.path(figures_dir, "pie-metric-relations.pdf"), p_rel, width = 7.2, height = 6.2)
p_rel

#| label: agreement
#| fig-width: 7.2
#| fig-height: 7
shares <- shares[metrics[, .(metric, short)], on = "metric", nomatch = NULL]
shares[, `:=`(
  short = factor(short, levels = rev(metrics$short)),
  component = factor(component, levels = c("estimator", "allocator", "interaction", "replicate (stochastic)"))
)]
p_var <- ggplot(shares, aes(share, short, fill = component)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.4) +
  scale_fill_manual(values = c("#0072B2", "#E69F00", "#009E73", "#c8c8c8")) +
  scale_x_continuous(labels = function(v) paste0(v * 100, " %"), expand = c(0, 0)) +
  labs(
    title = "(a) Variance decomposition by metric",
    subtitle = "estimators x stochastic allocators",
    x = NULL,
    y = NULL
  ) +
  theme_fig +
  guides(fill = guide_legend(nrow = 2))

cor_cols <- c(metrics$metric, "excess")
cm <- cor(as.matrix(crossing[, ..cor_cols]), method = "spearman", use = "pairwise.complete.obs")
lab <- c(metrics$short, "bias")
dimnames(cm) <- list(lab, lab)
cm_long <- as.data.table(as.table(cm))[, setNames(.SD, c("a", "b", "rho"))]
cm_long[, `:=`(a = factor(a, levels = lab), b = factor(b, levels = rev(lab)))]
p_cor <- ggplot(cm_long, aes(a, b, fill = rho)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", rho)), size = 2.1, colour = ink) +
  scale_fill_gradient2(low = "#b2182b", mid = "#f1f0ed", high = "#2166ac", limits = c(-1, 1), name = "Spearman") +
  coord_equal() +
  labs(title = "(b) Rank correlation between metrics", subtitle = "over all maps of the crossing", x = NULL, y = NULL) +
  theme_fig +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")

fc <- forecast[kind == "ensemble" & as_shipped == FALSE]
fc <- melt(fc, id.vars = c("estimator_label", "allocator_label"), measure.vars = c("auc_transition", "brier_skill"), variable.name = "metric")
ref <- melt(
  forecast[kind == "potentials", .(estimator_label = sub(" potentials$", "", forecast), auc_transition, brier_skill)],
  id.vars = "estimator_label",
  variable.name = "metric"
)
lbl <- c(auc_transition = "TOC AUC, per transition", brier_skill = "Brier skill vs. uniform forecast (fair)")
fc[, metric := lbl[as.character(metric)]]
ref[, metric := lbl[as.character(metric)]]
fc[, allocator_label := factor(allocator_label, levels = allocators$label)]
p_fc <- ggplot(fc, aes(value, estimator_label)) +
  geom_point(data = ref, aes(shape = "the potentials"), colour = ink, size = 2.4, fill = "white") +
  geom_point(aes(colour = allocator_label, group = allocator_label), size = 1.8, position = position_dodge(width = 0.6)) +
  facet_wrap(~metric, scales = "free_x") +
  scale_colour_manual(values = allocator_cols) +
  scale_shape_manual(values = c("the potentials" = 21)) +
  labs(title = "(c) Ensembles as probabilistic forecasts", subtitle = "class frequencies of each ensemble (one map for greedy and Ordered)", x = NULL, y = NULL) +
  theme_fig +
  guides(colour = guide_legend(nrow = 1))

fig_agree <- (p_var | p_cor) / p_fc + plot_layout(heights = c(1, 0.9))
ggsave(file.path(figures_dir, "pie-metric-agreement.pdf"), fig_agree, width = 7.2, height = 7)
fig_agree
