#' ---
#' title: "PIE benchmark: paper Fig. 3, the estimator × allocator matrix"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' Draws the paper's Fig. 3 from the tables `050-compare.r` writes:
#'
#' - **(a)** ensemble figure of merit in each cell of the estimator × allocator matrix, as a
#'   multiple of the random-allocation null; empty where the tools do not allow the pairing.
#'   Each tool's own pairing is outlined.
#' - **(b)** shares of the per-run FoM variance in the fully crossed block (three estimators ×
#'   CLUMPY, Dinamica).
#'
#' Output: `figures/fig3-pie-matrix.pdf`, copied by hand to `evoland-plus-paper/figures/`.

#| label: setup
#| output: false
library(data.table)
source("2026-09-model-comparison/common.r")
figures_dir <- file.path(pie_dir, "figures")
summary_dt <- fread(file.path(figures_dir, "pie-ensemble-summary.csv"))
shares <- fread(file.path(figures_dir, "pie-variance-shares.csv"))

estimators <- c(
  "random forest",
  "logistic regression",
  "Weights of Evidence",
  "GLM suitability (lulcc)",
  "logistic suitability (CLUinPy)"
)
estimator_labels <- c(
  "random forest\n(evoland)",
  "logistic regression\n(evoland)",
  "Weights of Evidence\n(Dinamica)",
  "GLM suitability\n(lulcc)",
  "logistic suitability\n(CLUinPy)"
)
allocators <- c("CLUMPY", "Dinamica", "CLUE-S", "Ordered", "CLUMondo")
allocator_labels <- c("CLUMPY\n(evoland)", "Expander/\nPatcher\n(Dinamica)", "CLUE-S\n(lulcc)", "Ordered\n(lulcc)", "CLUMondo\n(CLUinPy)")
native <- data.table(
  estimator = c("random forest", "logistic regression", "Weights of Evidence", "GLM suitability (lulcc)", "GLM suitability (lulcc)", "logistic suitability (CLUinPy)"),
  allocator = c("CLUMPY", "CLUMPY", "Dinamica", "CLUE-S", "Ordered", "CLUMondo")
)

#' # Figure

#| label: fig3
#| fig-width: 8
#| fig-height: 4.2
# sequential, one hue, light -> dark (blue ramp)
ramp <- grDevices::colorRampPalette(c("#e3eefb", "#2a78d6", "#0d3a73"))(100)
skill_range <- range(summary_dt$skill_over_null)

draw_fig3 <- function() {
  layout(matrix(c(1, 2), nrow = 1), widths = c(3.2, 1.4))
  op <- par(mar = c(1, 9.5, 6, 1), cex = 0.75)
  on.exit(par(op))
  plot(
    NA,
    xlim = c(0.5, length(allocators) + 0.5),
    ylim = c(length(estimators) + 0.5, 0.5),
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  axis(3, at = seq_along(allocators), labels = allocator_labels, tick = FALSE, padj = 0, line = -0.5)
  axis(2, at = seq_along(estimators), labels = estimator_labels, tick = FALSE, las = 1)
  mtext("(a) figure of merit / random-allocation null", side = 3, line = 4.5, adj = 0, cex = 0.8)
  for (i in seq_along(estimators)) {
    for (j in seq_along(allocators)) {
      cell <- summary_dt[estimator == estimators[i] & allocator == allocators[j]]
      if (nrow(cell) == 0L) {
        rect(j - 0.48, i - 0.46, j + 0.48, i + 0.46, col = "#f4f3ef", border = NA)
        next
      }
      k <- 1 + round(99 * (cell$skill_over_null - skill_range[1]) / diff(skill_range))
      rect(j - 0.48, i - 0.46, j + 0.48, i + 0.46, col = ramp[k], border = NA)
      is_native <- nrow(native[estimator == estimators[i] & allocator == allocators[j]]) > 0
      if (is_native) {
        rect(j - 0.48, i - 0.46, j + 0.48, i + 0.46, col = NA, border = "#0b0b0b", lwd = 1.5)
      }
      text(
        j,
        i,
        sprintf("%.2f\n(%.3f)", cell$skill_over_null, cell$figure_of_merit),
        col = if (k > 55) "white" else "#0b0b0b",
        cex = 0.9
      )
    }
  }

  par(mar = c(4, 3.5, 6, 0.5))
  stack <- matrix(shares$share, ncol = 1)
  share_colours <- c("#2a78d6", "#eb6834", "#1baf7a", "#c9c7bf")
  barplot(
    stack,
    col = share_colours,
    border = "white",
    width = 0.6,
    space = 0.3,
    axes = FALSE,
    ylim = c(0, 1),
    xlim = c(0, 2)
  )
  axis(2, at = seq(0, 1, 0.25), labels = paste0(seq(0, 100, 25), "%"), las = 1, cex.axis = 0.8)
  mids <- cumsum(shares$share) - shares$share / 2
  text(0.8, mids, sprintf("%s\n%.0f%%", shares$component, 100 * shares$share), pos = 4, xpd = TRUE, cex = 0.75)
  mtext("(b) variance of FoM", side = 3, line = 4.5, adj = 0, cex = 0.8)
}
draw_fig3()
cairo_pdf(file.path(figures_dir, "fig3-pie-matrix.pdf"), width = 8, height = 4.2)
draw_fig3()
invisible(dev.off())
