# TODO — 2026-09-paper-figures

## Figure 2

- [ ] **Decide what (d) argues.** FoM alone favours the deterministic map, and the multiclass
      Brier score gives the ensemble no meaningful edge over the adjusted potentials or
      climatology (see README). Candidate argument: single-map scores are unreliable (FoM spread),
      and map-level quantities need an ensemble. A panel showing the distribution of a
      configuration metric or a non-linear downstream response would make the second point;
      the per-cell Brier score cannot.
- [ ] **Estimation, not allocation, limits skill** (`030-skill-attribution.r`, README). Next:
      swap the learner (e.g. a logistic regression on the true feature set, which matches the
      generating process) and add calibration periods, to see which closes the estimation gap.
      Only then scale the domain for Fig. 2 (with a 30 × 30 window for the map panels).
- [ ] Replicate the attribution over several landscape seeds per domain size; size and landscape
      are confounded with one seed each.
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
