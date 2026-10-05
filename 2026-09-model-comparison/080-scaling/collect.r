#' ---
#' title: "PIE benchmark: scaling"
#' date: last-modified
#' number-sections: true
#' ---
#'
#' Collects the scaling runs of `080-scaling/run.sh`: PIE tiled k × k (k = 1, 2, 3, 4, 6, i.e.
#' 0.11 to 4.1 million cells; k = 6 is about the size of the Swiss 100 m grid), one realisation per
#' tool, each pipeline step its own process. Per step: wall time and peak resident memory of the
#' largest process (`/usr/bin/time -v`); per stage within a step: the wall times the steps record
#' themselves (`timings-*.csv`). Single machine: 4 cores, 16 GiB, R's vector heap capped at 12 GiB
#' (`R_MAX_VSIZE`) so that a step runs out of memory cleanly instead of taking the machine down.

#| label: setup
#| output: false
library(data.table)
pie_dir <- "2026-09-model-comparison"
figures_dir <- file.path(pie_dir, "figures")
scales <- c(1L, 2L, 3L, 4L, 6L)

#| label: steps
read_steps <- function(k) {
  marks <- list.files(
    file.path(pie_dir, paste0("outputs-k", k), "scaling"),
    pattern = "\\.done$",
    full.names = TRUE
  )
  rbindlist(lapply(marks, function(f) {
    x <- strsplit(readLines(f, warn = FALSE)[1], ",")[[1]]
    length(x) <- 5
    data.table(k = as.integer(x[1]), step = x[2], status = x[3], wall = x[4], peak_rss_kb = as.numeric(x[5]))
  }))
}
wall_seconds <- function(w) {
  vapply(strsplit(ifelse(is.na(w) | w == "", "NA", w), ":"), function(p) {
    p <- suppressWarnings(as.numeric(p))
    if (anyNA(p)) NA_real_ else sum(p * 60^(rev(seq_along(p)) - 1))
  }, numeric(1))
}
steps <- rbindlist(lapply(scales, read_steps))
steps[, `:=`(
  cells = 113563 * k^2,
  seconds = wall_seconds(wall),
  peak_rss_gib = peak_rss_kb / 2^20,
  outcome = fcase(
    status == "0", "ok",
    status == "124", "timeout",
    status == "skipped", "skipped (calibration failed)",
    default = "failed"
  )
)]
steps[order(step, k), .(step, k, cells, outcome, seconds, peak_rss_gib = round(peak_rss_gib, 2))]

#| label: stages
stages <- rbindlist(lapply(scales, function(k) {
  files <- list.files(file.path(pie_dir, paste0("outputs-k", k)), "^timings-.*\\.csv$", full.names = TRUE)
  rbindlist(lapply(files, fread))[, k := k]
}), fill = TRUE)
stage_table <- dcast(
  stages[, .(seconds = sum(seconds)), by = .(tool, stage, k)],
  tool + stage ~ k,
  value.var = "seconds"
)
stage_table

#| label: write
fwrite(steps, file.path(figures_dir, "scaling-steps.csv"))
fwrite(stages, file.path(figures_dir, "scaling-stages.csv"))

#| label: fig
#| fig-width: 9
#| fig-height: 4
step_labels <- c(
  `010-evoland-calibrate.r` = "evoland: set-up, neighbours, fit, predict",
  `020-alloc-evoland-clumpy.r` = "evoland: CLUMPY allocation",
  `020-alloc-evoland-dinamica.r` = "evoland: Dinamica allocation",
  `030-dinamica-native.r` = "Dinamica: WoE calibration + allocation",
  `030-lulcc.r` = "lulcc: GLM, CLUE-S, Ordered",
  `030-cluinpy.r` = "CLUinPy: suitability + CLUMondo"
)
colours <- setNames(c("#2a78d6", "#1baf7a", "#eb6834", "#eda100", "#e87ba4", "#4a3aa7"), names(step_labels))
plot_scaling <- function() {
  op <- par(mfrow = c(1, 2), mar = c(4, 4.5, 1, 1), cex = 0.8, las = 1)
  on.exit(par(op))
  ok <- steps[step %in% names(step_labels)]
  for (what in c("seconds", "peak_rss_gib")) {
    plot(
      NA,
      log = "xy",
      xlim = range(ok$cells),
      ylim = range(ok[[what]][ok$outcome == "ok"], na.rm = TRUE),
      xlab = "cells",
      ylab = if (what == "seconds") "wall time (s)" else "peak resident memory (GiB)"
    )
    if (what == "peak_rss_gib") abline(h = 12, lty = 2, col = "grey50")
    for (s in names(step_labels)) {
      d <- ok[step == s][order(cells)]
      d_ok <- d[outcome == "ok"]
      lines(d_ok$cells, d_ok[[what]], col = colours[s], lwd = 2)
      points(d_ok$cells, d_ok[[what]], col = colours[s], pch = 16)
      d_fail <- d[outcome %in% c("failed", "timeout")]
      if (nrow(d_fail) > 0 && what == "peak_rss_gib") {
        points(d_fail$cells, pmin(d_fail[[what]], 16), col = colours[s], pch = 4, cex = 1.5, lwd = 2)
      }
    }
  }
  legend("topleft", legend = step_labels, col = colours, lwd = 2, bty = "n", cex = 0.75)
}
plot_scaling()
cairo_pdf(file.path(figures_dir, "scaling.pdf"), width = 9, height = 4)
plot_scaling()
invisible(dev.off())
