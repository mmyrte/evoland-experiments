# TODO — 2026-09-paper-figures

## Figure 2

- [ ] (superseded by the item above) **Decide what (d) argues.** FoM alone favours the deterministic map, and the multiclass
      Brier score gives the ensemble no meaningful edge over the adjusted potentials or
      climatology (see README). Candidate argument: single-map scores are unreliable (FoM spread),
      and map-level quantities need an ensemble. A panel showing the distribution of a
      configuration metric or a non-linear downstream response would make the second point;
      the per-cell Brier score cannot.
- [x] **Estimation, not allocation, limits skill** (`030`), and the learner is the main factor
      (`031`, README): log_reg reaches 95 % of the attainable skill at 90 × 90, ranger as in
      Fig. 2 58 %, ranger with larger leaves 75–84 %.
- [x] **Rebuild Fig. 2** on 90 × 90 with ranger (`min.node.size = 50`, all predictors), map
      panels on a 30 × 30 window (README: potentials reach ~90 % of the attainable skill).
- [ ] **Panel (d) argument after the rebuild:** the FoM spread across draws is narrow at 90 × 90,
      so "single-map scores are unreliable" no longer carries the figure. Candidate: show the
      bias of the deterministic map. Plot the distribution of potential (or accessibility) at
      changed cells for observed vs. realisations vs. greedy, next to or instead of the FoM
      strip.
- [ ] evoland: `commit_upsert()` builds an empty `update set` when all columns are keys
      (e.g. `trans_preds_t`), which DuckDB rejects. Skip the `when matched` clause in that case.
      `031` works around it with `method = "append"`.
- [ ] `020-fig2-ensembles.r` still carries its own copy of the synthetic process; switch it to
      `source("2026-09-paper-figures/000-synthetic-process.r")`.
- [ ] Replicate `030` over several landscape seeds per domain size; size and landscape are
      confounded with one seed each (`031` already uses three seeds).
- [ ] **Strengthen the signal** so that skill scores mean something: a larger grid, more change
      per period, or more calibration periods. Currently the potentials reach a skill of 0.04
      over climatology.
- [ ] Panel (d): show FoM as a discrete distribution (dot histogram per H) instead of a violin,
      since the banding is intrinsic.
- [ ] **Replace the deterministic stand-in** with the real single-map tools (Dinamica EGO,
      lulcc, TerrSet LCM) once `2026-09-model-comparison/` produces them. Add the
      neighbourhood-only null from that TODO.
- [ ] **Reproducibility across environments:** a knitr-purled Rscript run and `quarto render`
      of the same file gave slightly different FoM values (median 0.093 vs. 0.102). Find the
      source: ranger threading, tie-breaking in the predictor ranking, or RNG state consumed by
      set-up code.
- [ ] Only two of the four process transitions become viable after calibrating on a single
      period (forest → arable, arable → urban); 7 of 56 cells of observed change are
      unmodelled. Either tune the process rates or lower `min_cardinality_abs` further.
- [ ] Decide whether the mock-up stays synthetic or moves to a real Arealstatistik extent once
      the comparison pipeline exists.
- [ ] LULC palette: the vignette colours fail colour-vision checks (forest vs. arable for
      deuteranopes). The panels use outcome colours instead, but any figure that shows classes
      needs a new palette.
