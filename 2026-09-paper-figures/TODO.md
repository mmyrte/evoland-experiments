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
- [ ] Decide whether the mock-up stays synthetic or moves to a real Arealstatistik extent once
      the comparison pipeline exists.
- [ ] Decide how to present the patch finding: estimated patch sizes from a neighbourhood-driven
      process are spurious clusters, and uPAM on them biases where change goes. Discussion
      point for the paper, or a panel in the appendix?

## Experiments

- [ ] Replicate `010-skill-attribution` over several landscape seeds per domain size; size and
      landscape are confounded with one seed each (`010-learner-comparison` already uses three).
- [ ] evoland: the fix for upserts into all-key tables (`trans_preds_t`) is on the evoland-plus
      branch `claude/elegant-cerf-w44s45` (`aa148e2`). Once it is merged and the pin moves,
      `010-learner-comparison` can return from `method = "append"` to upsert.
- [ ] evoland `runs_t`: proposal for standard columns (`seed`, `kind`, `member`,
      `created_by`/`created_at`, an attributes map) pending a decision; the scripts here add
      `seed` ad hoc.
