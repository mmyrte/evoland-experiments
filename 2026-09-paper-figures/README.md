# Paper figures (2026-09) — mock-ups for the evoland-plus model description paper

**Status:** active. Figures for the paper in
[mmyrte/evoland-plus-paper](https://github.com/mmyrte/evoland-plus-paper), built on small
synthetic cases so they run in minutes. [`TODO.md`](TODO.md) holds the open work.

Outputs are written to `figures/` and copied by hand to `evoland-plus-paper/figures/`. A second
Overleaf project on this repo would remove the copy step.

**Running:** `000-synthetic-process.r` defines the synthetic process and the scores, and the
`010-*` steps source it. The steps are independent (own databases, own outputs), so they share
one stage: `./execute-all.sh --workers 3 '2026-09-paper-figures/0*'` renders `000`, then the
three `010` steps in parallel. `010-learner-comparison` additionally runs its cases on its own
PSOCK cluster.

| Step | Figure | What it shows |
| --- | --- | --- |
| [`000-synthetic-process.r`](000-synthetic-process.r) | — | Synthetic process and scores, sourced by the skill-attribution and learner-comparison steps |
| [`010-fig2-ensembles.r`](010-fig2-ensembles.r) | Fig. 2, `figures/fig2-ensembles.pdf` | Ensembles vs. single maps on a backcast |
| [`010-skill-attribution.r`](010-skill-attribution.r) | `figures/skill-attribution.csv` | Where the skill is lost: potentials or allocation |
| [`010-learner-comparison.r`](010-learner-comparison.r) | `figures/learner-comparison.{csv,pdf}` | Which learner, feature set and number of calibration periods close the estimation gap |

## Figure 2: ensembles vs. single maps

The domain is a synthetic 90 × 90 grid (map panels: the 30 × 30 block with the most observed
change), set up like the evoland-plus vignette
`stochastic-allocation-sensitivity.qmd`, with these differences:

- **Structured initial map:** urban land clusters where accessibility is high, there is a
  small immutable lake, forest sits on the better sites and arable land takes the rest.
- **Known change process:** land use changes through a stated logistic process
  (`000-synthetic-process.r`), so the model has a signal to learn. The vignette's noise
  increments have none.
- **Estimators:** a random forest (ranger, 500 trees, `min.node.size = 50`, all available
  predictors), the best setting in `010-learner-comparison` without logistic regression's home
  advantage; and a logistic regression, which is correctly specified for this logistic process
  and so shows what allocation does with potentials close to the truth. Each has its own
  ensemble of 100 realisations under its own parent run.
- **Allocation:** single-cell (uSAM). The process changes cells independently; the estimated
  1.1–1.2-cell "patches" are clusters induced by its neighbourhood terms, and allocating with
  them (uPAM) moves change onto less probable neighbours (see below).

The backcast works as follows:

- **Calibration** uses the transition from period 1 to 2 only. Period 3 is flagged as
  extrapolated, so model fitting and allocation parameters never see it.
- **Held-out observation:** the observed period 3 is stored in its own run, which is a sibling
  of the ensemble runs, so the ensemble cannot read it.
- **Demand** is the observed 2 → 3 quantity of each viable transition. The figure is about
  *where* change is placed, not how much.
- **Ensembles:** 100 CLUMPY realisations per learner, each run registered in `runs_t` with its
  own seed.
- **Deterministic stand-in:** the same demand is also allocated greedily on each learner's
  adjusted potentials. This stands in for tools without stochastic allocation until
  `2026-09-model-comparison/` provides the real Dinamica, lulcc and LCM runs.

Panels, on a 3 × 2 grid with a legend each:
- **(a)** observed land use in period 3, with the change since period 2 outlined
- **(b)** one random-forest realisation, the draw with the median figure of merit, coloured by
  outcome (hit / miss / false alarm / wrong hit)
- **(c)** change frequency across the random-forest ensemble, with the observed change outlined
- **(d)** FoM of every realisation, one row per learner, with each ensemble's expectation (from
  mean counts), each deterministic map, and the persistence and random-allocation nulls
- **(e)** where change is placed: the cumulative distribution of changed cells over the
  percentile of their true probability, for the observed change, the realisations (median and
  5–95 % band) and the deterministic maps; the diagonal is random allocation

### What the numbers say (last render)

All four transitions are viable; none of the 573 observed changes on the 6687 forest and arable
cells is unmodelled.

| | random forest | logistic regression |
| --- | --- | --- |
| FoM, realisations: median (5–95 %) | 0.136 (0.121–0.150) | 0.151 (0.139–0.162) |
| FoM, ensemble expectation | 0.136 | 0.150 |
| FoM, deterministic greedy | 0.244 | 0.257 |
| Brier skill, adjusted potentials | 0.160 | 0.183 |
| Brier skill, ensemble frequency (fair) | 0.164 | 0.184 |
| Brier skill, deterministic | −0.30 | −0.27 |
| Brier skill, single realisation (median) | −0.62 | −0.58 |

References: FoM of random allocation 0.033; Brier skill of the true probabilities 0.191
(multiclass Brier score over the cells that can change).

**Skill reference.** Skill is measured against the *random-allocation forecast*, which knows the
quantity of change but not its location. Every cell of a land-use class gets the same probability
for each transition: the transition's demand divided by the class's area. It is the cell-wise
expectation of the random-allocation null used for the FoM. The Brier skill score
BSS = 1 − BS / BS_random is 0 for a forecast no better than placing the right quantity at random,
1 for a perfect one, and negative for one worse than random placement. (Forecast verification
calls this kind of reference "climatology".)

Panel (e) ranks every changed cell by its true transition probability, as a percentile among
the cells that could make the same transition:

| changed cells | share in top 5 % | median percentile |
| --- | --- | --- |
| expected under the true probabilities | 0.361 | — |
| observed | 0.361 | 0.909 |
| realisations, random forest (median) | 0.334 | 0.900 |
| realisations, logistic regression (median) | 0.381 | 0.918 |
| deterministic, random forest | 0.649 | 0.964 |
| deterministic, logistic regression | 0.731 | 0.970 |

What the figure argues:

1. **FoM is the wrong yardstick for a stochastic process.** The deterministic maps score nearly
   twice the FoM of any realisation, because they put all change on the most probable cells.
   Panel (e) shows that observed change does not do that: only a third of it falls in the top
   5 %, exactly what the true probabilities predict. The realisations reproduce this; the
   deterministic maps put two thirds to three quarters of their change there.
2. **The ensemble loses nothing.** Its frequency is as good a probabilistic forecast as the
   potentials it samples from (skill 0.164 vs. 0.160, 0.184 vs. 0.183), and the hard maps are
   worse than random allocation.
3. **Why logistic regression fits so neatly.** The synthetic process is logistic in the drivers
   and neighbourhood shares, so a logistic regression is correctly specified: its potentials
   reach 96 % of the skill of the truth, and an unbiased sampler of near-true probabilities
   reproduces the observed placement in expectation. It slightly overshoots the top 5 % (0.38
   vs. 0.36), within what one observed draw can show. The random forest is close but less
   sharp. On real data, no learner has this advantage.
4. **Patches matter.** With uPAM on the estimated patches, the logistic-regression ensemble put
   only 28 % of its change in the top 5 %, and its ensemble lost skill against its potentials
   (0.166 vs. 0.183). Patch parameters estimated from a neighbourhood-driven process are not
   neutral: they describe clustering that the neighbourhood terms already produce.

## Skill attribution: potentials or allocation?

[`010-skill-attribution.r`](010-skill-attribution.r) uses the known process to separate the
two. It crosses **potentials** (estimated by ranger, as in Fig. 2, vs. the oracle true
probabilities `q`, restricted to the viable transitions) with **allocation** (none, CLUMPY uSAM
on single cells, CLUMPY uPAM with the estimated patches), at 30 × 30 and 90 × 90 cells, with 100
members per ensemble. Because `q` is known, the expected multiclass Brier score of a forecast `p`
splits into a distance to the truth, mean of sum_k (p_k − q_k)^2, and an irreducible term,
mean of sum_k q_k (1 − q_k). The distance no longer depends on the single observed period 3.

| quantity (distance to truth, expected skill vs. random allocation) | 30 × 30 | 90 × 90 |
| --- | --- | --- |
| random allocation | 0.021, 0 | 0.027, 0 |
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
   random allocation, because the events are rare (per-cell probabilities of a few percent). A larger
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

Share of attainable skill (1 = as good as the true probabilities, 0 = random allocation), mean over
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
over random allocation), as the events are rare.
