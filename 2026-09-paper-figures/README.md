# Paper figures (2026-09) — mock-ups for the evoland-plus model description paper

**Status:** active. Figures for the paper in
[mmyrte/evoland-plus-paper](https://github.com/mmyrte/evoland-plus-paper), built on small
synthetic cases so they run in minutes. [`TODO.md`](TODO.md) holds the open work.

Outputs are written to `figures/` and copied by hand to `evoland-plus-paper/figures/`. A second
Overleaf project on this repo would remove the copy step.

| Step | Figure | What it shows |
| --- | --- | --- |
| [`000-synthetic-process.r`](000-synthetic-process.r) | — | Synthetic process and scores, sourced by the skill-attribution and learner-comparison steps |
| [`010-fig2-ensembles.r`](010-fig2-ensembles.r) | Fig. 2, `figures/fig2-ensembles.pdf` | Ensembles vs. single maps on a backcast |
| [`010-skill-attribution.r`](010-skill-attribution.r) | `figures/skill-attribution.csv` | Where the skill is lost: potentials or allocation |
| [`010-learner-comparison.r`](010-learner-comparison.r) | `figures/learner-comparison.{csv,pdf}` | Which learner, feature set and number of calibration periods close the estimation gap |

## Figure 2: ensembles vs. single maps

The domain is a synthetic 90 × 90 grid (map panels: the 30 × 30 block with the most observed
change), set up like the evoland-plus vignette
`stochastic-allocation-sensitivity.qmd`. Two differences from the vignette:

- **Structured initial map:** urban land clusters where accessibility is high, there is a
  small immutable lake, forest sits on the better sites and arable land takes the rest.
- **Known change process:** land use changes through a stated logistic process
  (`000-synthetic-process.r`), so the model has a signal to learn. The vignette's noise
  increments have none.
- **Estimator:** ranger with 500 trees and `min.node.size = 50` on all available predictors,
  the best setting in `010-learner-comparison` without logistic regression's home advantage.

The backcast works as follows:

- **Calibration** uses the transition from period 1 to 2 only. Period 3 is flagged as
  extrapolated, so model fitting and allocation parameters never see it.
- **Held-out observation:** the observed period 3 is stored in its own run, which is a sibling
  of the ensemble runs, so the ensemble cannot read it.
- **Demand** is the observed 2 → 3 quantity of each viable transition. The figure is about
  *where* change is placed, not how much.
- **Ensemble:** 100 CLUMPY realisations, each run registered in `runs_t` with its own seed.
- **Deterministic stand-in:** the same demand is also allocated greedily on the same adjusted
  potentials. This stands in for tools without stochastic allocation until
  `2026-09-model-comparison/` provides the real Dinamica, lulcc and LCM runs.

Panels:
- **(a)** observed change
- **(b)** one realisation, the draw with the median figure of merit, coloured by outcome
  (hit / miss / false alarm / wrong hit)
- **(c)** change frequency across the ensemble, with the observed change outlined
- **(d)** FoM of every realisation, the ensemble expectation (from mean counts), the
  deterministic map, and the persistence and random-allocation nulls

### What the numbers say (last render)

All four transitions are viable; none of the 573 observed changes on the 6687 forest and arable
cells is unmodelled.

| quantity | value |
| --- | --- |
| FoM, realisations: median (5–95 %) | 0.105 (0.093–0.118) |
| FoM, ensemble expectation | 0.105 |
| FoM, deterministic greedy | 0.240 |
| FoM, random-allocation null | 0.033 |

Multiclass Brier score over the cells that can change, and skill against climatology:

| forecast | Brier | skill vs. climatology |
| --- | --- | --- |
| adjusted potentials | 0.133 | 0.160 |
| ensemble frequency (fair: 0.136) | 0.137 | 0.134 |
| climatology | 0.159 | 0 |
| persistence | 0.171 | −0.08 |
| deterministic greedy | 0.209 | −0.31 |
| single realisation, median | 0.275 | −0.74 |

What changed with the larger domain and the calibrated learner:

1. **Estimation is no longer the bottleneck.** The potentials reach a skill of 0.16. The truth
   reaches about 0.18 on this landscape (`010-skill-attribution`), so the estimates capture almost 90 % of it.
2. **The ensemble frequency is slightly worse than its potentials** (Brier +0.004). That is the
   uPAM patch loss measured in `010-skill-attribution`: the synthetic process changes single cells, while the
   estimated patches average 1.1–1.2 cells.
3. **The "unreliable single-map score" argument largely disappears.** The FoM spread across draws
   is 0.093–0.118 at 90 × 90, against 0.029–0.105 at 30 × 30. It was a small-domain effect.
4. **The deterministic map still wins FoM by a factor of 2.3** (0.240 vs. 0.105), and it is the
   worst probabilistic forecast after single draws (skill −0.31). Greedy allocation puts all
   change on the highest potentials. The realisations spread it in proportion to the
   probabilities, as the observed change does.

So the figure's case for stochastic allocation has to be the bias of the deterministic map,
not a score: point 4, made visible in **panel (e)**. For every changed cell it takes the
percentile of the cell's adjusted potential among all cells that could make the same transition,
which lets the four transitions be pooled. It then compares the distribution of those percentiles
for the observed change, the realisations and the greedy map:

| changed cells | share in top 5 % of potential | median percentile | KS distance to observed |
| --- | --- | --- | --- |
| observed | 0.34 | 0.90 | — |
| realisations (median) | 0.26 | 0.87 | 0.10 |
| deterministic greedy | 0.74 | 0.97 | 0.57 |

Observed change happens across the upper half of the potential range, not only at its top. The
realisations reproduce that, the greedy map does not: three quarters of its change sits in the
top 5 %. This is Mazy's allocation-bias argument, shown empirically, and it is the same mechanism
that wins the greedy map its FoM. The realisations' residual distance (0.10: slightly too little
change at both the bottom and the very top) reflects the estimated potentials, not the
allocator; `010-skill-attribution` showed that uSAM reproduces the potentials it is given.

## Skill attribution: potentials or allocation?

[`010-skill-attribution.r`](010-skill-attribution.r) uses the known process to separate the
two. It crosses **potentials** (estimated by ranger, as in Fig. 2, vs. the oracle true
probabilities `q`, restricted to the viable transitions) with **allocation** (none, CLUMPY uSAM
on single cells, CLUMPY uPAM with the estimated patches), at 30 × 30 and 90 × 90 cells, with 100
members per ensemble. Because `q` is known, the expected multiclass Brier score of a forecast `p`
splits into a distance to the truth, mean of sum_k (p_k − q_k)^2, and an irreducible term,
mean of sum_k q_k (1 − q_k). The distance no longer depends on the single observed period 3.

| quantity (distance to truth, expected skill vs. climatology) | 30 × 30 | 90 × 90 |
| --- | --- | --- |
| climatology | 0.021, 0 | 0.027, 0 |
| estimated potentials (adjusted) | 0.0125, 0.050 | 0.0137, 0.087 |
| ensemble on estimated potentials, uSAM | 0.0124, 0.051 | 0.0138, 0.087 |
| ensemble on estimated potentials, uPAM | 0.0148, 0.036 | 0.0140, 0.085 |
| oracle potentials (adjusted) | 0.0040, 0.100 | 0.0004, 0.177 |
| ensemble on oracle potentials, uSAM | 0.0040, 0.100 | 0.0003, 0.177 |
| ensemble on oracle potentials, uPAM | 0.0063, 0.087 | 0.0030, 0.159 |
| truth, all four transitions (ceiling) | 0, 0.124 | 0, 0.179 |

Viable transitions: 2 at 30 × 30 (7 of 56 observed changes unmodelled), all 4 at 90 × 90 (573
changes, none unmodelled).

What it shows:

1. **Allocation loses nothing with uSAM.** The ensemble frequency reproduces the potentials it
   samples from, at both sizes and for both potential sources. This verifies that the C++ uSAM is
   unbiased. uPAM adds a small, consistent loss (distance +0.002–0.003) because it places change
   in patches while the synthetic process changes single cells.
2. **The loss is in estimation.** The estimated potentials capture only 40 % (30 × 30) and 49 %
   (90 × 90) of the attainable skill. Their distance to the truth does not shrink with nine times
   the data (0.0125 → 0.0137), so a larger domain alone does not fix it. Suspects: a single
   calibration period, ranger's poorly calibrated probabilities for rare events, and the
   predictor set (top 4 by rpart importance; distance-band neighbourhood shares that only
   approximate the process's 5 × 5 window).
3. **The ceiling is low.** Even the true probabilities only reach a skill of 0.12–0.18 over
   climatology, because the events are rare (per-cell probabilities of a few percent). A larger
   domain does not raise the ceiling, but it makes realised scores track expected ones (90 × 90:
   realised and expected skill agree to about 0.02; 30 × 30: up to 0.03 apart on 0.05).
   Caveat: each size uses one landscape, so size and landscape are confounded; the higher
   ceiling at 90 × 90 reflects that landscape, not the size.
4. **FoM tells another story.** The FoM spread narrows with size (estimated, uSAM: 0.05–0.13 at
   30 × 30, 0.10–0.13 at 90 × 90), and uSAM scores higher than uPAM throughout.

## Learner comparison: the learner closes the gap

[`010-learner-comparison.r`](010-learner-comparison.r) crosses seven learners with three feature
sets, one vs. two calibration periods (paired: same target transition), 30 × 30 vs. 90 × 90 and
three landscape seeds. It scores the adjusted potentials only, since `010-skill-attribution` showed that uSAM
passes them through unchanged. Run: 11 min on 3 workers, no failed configurations. Output:
`figures/learner-comparison.{csv,pdf}`.

Share of attainable skill (1 = as good as the true probabilities, 0 = climatology), mean over
three seeds, one calibration period:

| learner | 30 × 30, rpart top 4 | 30 × 30, all available | 90 × 90, rpart top 4 | 90 × 90, all available | 90 × 90, process features |
| --- | --- | --- | --- | --- | --- |
| log_reg | 0.76 | 0.48 | 0.95 | 0.95 | 0.97 |
| ranger_large_leaves | 0.61 | 0.69 | 0.75 | 0.83 | 0.84 |
| xgboost | −0.02 | −0.07 | 0.76 | 0.77 | 0.79 |
| cv_glmnet | 0.33 | 0.19 | 0.73 | 0.77 | 0.72 |
| ranger (Fig. 2 setting) | 0.27 | 0.49 | 0.58 | 0.72 | 0.65 |
| naive_bayes | 0.24 | 0.14 | 0.41 | 0.36 | 0.48 |
| featureless | −0.01 | −0.01 | 0.00 | 0.00 | 0.00 |
| oracle, viable transitions | 0.92 | | 0.99 | | |

What it shows:

1. **The learner is the main factor.** Logistic regression reaches 95–97 % of the attainable skill
   at 90 × 90, even with only the predictors a modeller has. Ranger as configured for Fig. 2
   reaches 58 %; the same forest with larger leaves (`min.node.size = 50`) reaches 75–84 %. The
   gap is in calibration: small leaves give overconfident probabilities, and a proper score
   punishes that.
2. **More data helps the flexible learners, not the fixed one.** From 30 × 30 to 90 × 90, xgboost
   goes from no skill to 0.76–0.79, and cv_glmnet and ranger roughly double. A second calibration
   period helps at 30 × 30 (more data again) and little at 90 × 90. The seeds agree at 90 × 90
   (ranger: 0.57–0.58; log_reg: 0.92–0.97). At 30 × 30 they do not (ranger: 0.07–0.46).
3. **The feature set matters little** once the learner is right. Process features gain at most a
   few points; rpart's top 4 hurt ranger at 90 × 90.
4. **Naive Bayes, the closest to Dinamica's weights of evidence, stays poor** (0.36–0.49 at
   90 × 90). Its raw probabilities are badly calibrated (distance to truth 0.10–0.35 before
   adjustment, 0.016 after). The adjustment to demand rescues it only partly.
5. **Caveat: log_reg has home advantage.** The synthetic process is logistic, so a logistic
   regression is close to correctly specified. On real data, the flexible learners may win. The
   general lesson is calibration and enough data, not "use a GLM".

For Fig. 2, a 90 × 90 domain with log_reg (or ranger with larger leaves, to avoid the home
advantage) puts the estimates within a few percent of the truth. Then the figure shows what
allocation and ensembles do, rather than estimation error. The ceiling stays low (skill ~0.18
over climatology), as the events are rare.
