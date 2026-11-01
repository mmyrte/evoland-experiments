# TODO — 2026-09-paper-figures

## Figure 2

Done (see README for the numbers):

- [x] Synthetic drivers fixed (the reformat had kept only the last term of each sum).
- [x] Multiclass Brier score against probabilistic baselines; the binary "change or not" score
      dropped.
- [x] Estimation vs. allocation separated (`010-skill-attribution`), and the learner identified
      as the main factor (`010-learner-comparison`).
- [x] 90 × 90 domain with 30 × 30 map windows; calibrated random forest
      (`min.node.size = 50`, all predictors); shared synthetic process
      (`000-synthetic-process.r`).
- [x] Panel (e): where change is placed, by true probability; KS distances computed but
      commented out.
- [x] Logistic regression tried as a second learner in (d) and (e), then dropped: it overlapped
      the random forest too much to read. Fig. 2 shows ranger only.
- [x] Panel (a) shows observed land use in period 3 (Okabe-Ito palette) with the observed change
      outlined; panels on a 3 × 2 grid with a legend per panel.
- [x] Single-cell allocation (uSAM): the synthetic process changes cells independently, and
      the estimated patches moved change onto less probable neighbours.

Open:

- [ ] **Replace the deterministic stand-in** with the real single-map tools (Dinamica EGO,
      lulcc, TerrSet LCM) once `2026-09-model-comparison/` produces them. Add the
      neighbourhood-only null from that TODO.
- [ ] **Run-to-run nondeterminism:** two runs of the same script on the same machine gave FoM
      medians of 0.105 and 0.104 (earlier: Rscript vs. `quarto render`, 0.093 vs. 0.102).
      Suspects: ranger threading, tie-breaking in the ranking, RNG state consumed by set-up code.
      On the current pin (`3bc7856`), two runs of `010-learner-comparison` are bit-identical, and
      Fig. 2's CLUMPY, CLUE-S and CLUMondo numbers reproduce the last render; only Dinamica
      (fresh seed per run) moves. Not yet checked: two runs of `010-fig2-ensembles` against each
      other.
- [ ] Decide whether the mock-up stays synthetic or moves to a real Arealstatistik extent once
      the comparison pipeline exists.
- [ ] Decide how to present the patch finding: estimated patch sizes from a neighbourhood-driven
      process are spurious clusters, and uPAM on them biases where change goes. Discussion
      point for the paper, or a panel in the appendix?

## Validation metrics

- [x] Fig. 2 split into compute (`010`, writes `results/fig2/`) and plot (`030-fig2-plot.r`).
- [x] Further metrics on every map (`020-validation-metrics.r`): KSim, Fuzzy Kappa Simulation,
      similarity of differences, TOC AUC, disagreement, Brier; relations in
      `figures/metric-{relations,agreement}.pdf`.
- [ ] Decide which of the metric figures goes into the paper (main text or appendix), and whether
      Fig. 2 (d) should show a kappa next to the FoM.
- [ ] Fig. 2 palette: CLUMondo (`#e87ba4`) and rank-and-fill (`#eb6834`) are hard to tell apart
      even with normal colour vision (ΔE 13 < 15); the metric figures use Okabe-Ito instead.

## Experiments

- [ ] Replicate `010-skill-attribution` over several landscape seeds per domain size; size and
      landscape are confounded with one seed each (`010-learner-comparison` already uses three).
- [x] evoland: upserts into all-key tables (`trans_preds_t`) work on the current pin (`develop`
      at `3bc7856`, ethzplus/evoland-plus#84); `010-learner-comparison` upserts again.
- [ ] evoland `runs_t`: proposal for standard columns (`seed`, `kind`, `member`,
      `created_by`/`created_at`, an attributes map) pending a decision; the scripts here add
      `seed` ad hoc.
