# TODO — 2026-09-paper-figures

## Figure 2

- [ ] **Decide what (d) argues.** FoM alone favours the deterministic map (see README). Options:
      add a Brier / reliability panel for the change-frequency surface; or plot FoM against the
      Brier score, one point per realisation plus the ensemble and deterministic maps; or keep
      (d) as is and make the spread argument in the text.
- [ ] **Replace the deterministic stand-in** with the real single-map tools (Dinamica EGO,
      lulcc, TerrSet LCM) once `2026-09-model-comparison/` produces them. Add the
      neighbourhood-only null from that TODO.
- [ ] **Reproducibility across environments:** a knitr-purled Rscript run and `quarto render`
      of the same file gave slightly different FoM values (median 0.093 vs. 0.102). Find the
      source: ranger threading, tie-breaking in the predictor ranking, or RNG state consumed by
      set-up code.
- [ ] Only two of the four process transitions become viable after calibrating on a single
      period (forest → arable, arable → urban); about 10 cells of observed change are
      unmodelled. Either tune the process rates or lower `min_cardinality_abs` further.
- [ ] Decide whether the mock-up stays synthetic or moves to a real Arealstatistik extent once
      the comparison pipeline exists.
- [ ] LULC palette: the vignette colours fail colour-vision checks (forest vs. arable for
      deuteranopes). The panels use outcome colours instead, but any figure that shows classes
      needs a new palette.
