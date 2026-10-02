# Paper figures (2026-09) — mock-ups for the evoland-plus model description paper

**Status:** active. Figures for the paper in
[mmyrte/evoland-plus-paper](https://github.com/mmyrte/evoland-plus-paper), built on small
synthetic cases so they run in minutes. [`TODO.md`](TODO.md) holds the open work.

Outputs are written to `figures/` and copied by hand to `evoland-plus-paper/figures/`. A second
Overleaf project on this repo would remove the copy step.

| Step | Figure | What it shows |
| --- | --- | --- |
| [`000-synthetic-process.r`](000-synthetic-process.r) | — | Synthetic process and scores, sourced by 030 and 031 |
| [`020-fig2-ensembles.r`](020-fig2-ensembles.r) | Fig. 2, `figures/fig2-ensembles.pdf` | Ensembles vs. single maps on a backcast |
| [`030-skill-attribution.r`](030-skill-attribution.r) | `figures/skill-attribution.csv` | Where the skill is lost: potentials or allocation |
| [`031-learner-comparison.r`](031-learner-comparison.r) | `figures/learner-comparison.{csv,pdf}` | Which learner, feature set and number of calibration periods close the estimation gap |

## Figure 2: ensembles vs. single maps

The domain is a synthetic 30 × 30 grid, set up like the evoland-plus vignette
`stochastic-allocation-sensitivity.qmd`. Two differences from the vignette:

- **Structured initial map:** urban land clusters where accessibility is high, there is a
  small immutable lake, forest sits on the better sites and arable land takes the rest.
- **Known change process:** land use changes through a stated logistic process (table in the
  `.qmd`), so the model has a signal to learn. The vignette's noise increments have none.

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

Calibration leaves two viable transitions, forest → arable and arable → urban. Of the 56 observed
changes on the 761 cells that can change, 7 come from transitions the model cannot produce.

| quantity | value |
| --- | --- |
| FoM, realisations: median (5–95 %) | 0.062 (0.029–0.105) |
| FoM, ensemble expectation | 0.068 |
| FoM, deterministic greedy | 0.129 |
| FoM, random-allocation null | 0.032 |

Multiclass Brier score over the cells that can change (0–2, lower is better), and the skill
score against climatology (each class's demand rates applied to all of its cells):

| forecast | Brier | skill vs. climatology |
| --- | --- | --- |
| adjusted potentials | 0.132 | 0.04 |
| ensemble frequency (fair: 0.135) | 0.136 | 0.01 |
| climatology | 0.138 | 0 |
| persistence | 0.147 | −0.07 |
| deterministic greedy | 0.213 | −0.55 |
| single realisation, median (5–95 %) | 0.243 (0.223–0.260) | −0.77 |

**Which Brier score.** The first version scored only *whether* a cell changes (binary), so a
forest cell simulated as arable where it actually became urban counted as correct. The
multiclass score sums the squared error over all posterior classes. Here each anterior class has
only one viable transition, so the two scores order the forecasts identically (multiclass ≈ 2 ×
binary). On real data with competing transitions they will not.

**What the scores support.** On FoM, the deterministic map beats the median draw, as expected:
FoM rewards putting change where the potential is highest, which is what a hard allocation does.
On the Brier score, the ensemble beats every hard map by a wide margin, but that is close to
guaranteed. The Brier score is a proper scoring rule, and a hard map pays the maximum penalty for
every error. Against the forecasts that are also probabilities, the ensemble has almost no skill:
it barely beats climatology and is slightly worse than the adjusted potentials it is sampled
from.

So the case for ensembles cannot be "the ensemble is the better per-cell forecast": the potential
surface gives that more cheaply. What remains, and what the figure should argue:

1. **Single-map scores are unreliable.** A single map's FoM depends heavily on the draw: the
   5–95 % range spans a factor of 3.6.
2. **Quantities that depend on the whole map.** Patch structure, configuration metrics, or a
   non-linear downstream response (habitat quality, connectivity) are not functions of per-cell
   marginals, so only an ensemble of maps gives their distribution.

Overall skill is low (potentials: 0.04 over climatology): 56 changes on 761 cells is a weak
signal. **Banding** in (d) is intrinsic: with demand fixed, FoM = H / (Q_obs + Q_sim − H) for an
integer number of hits H, so the dots stack on a few discrete values (H = 3, 4, 5, … gives 0.029,
0.040, 0.050, …). New seeds only change which values are occupied.

See [`TODO.md`](TODO.md).

## Skill attribution: potentials or allocation?

[`030-skill-attribution.r`](030-skill-attribution.r) uses the known process to separate the
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

## Learner comparison (to run)

[`031-learner-comparison.r`](031-learner-comparison.r) follows up on the estimation gap. It
crosses seven learners (featureless, log_reg, cv_glmnet, naive_bayes, ranger as in Fig. 2,
ranger with larger leaves, xgboost) with three feature sets: the rpart top 4 of what a modeller
has, all of it, or the terms of the generating logits. With the last set, log_reg is
correctly specified. It also crosses one vs. two calibration periods (paired: same target
transition), 30 × 30 vs. 90 × 90, and three landscape seeds. It scores the adjusted potentials
only, since `030` showed that uSAM passes them through unchanged. Learners whose package is
missing are skipped; `rv add glmnet xgboost e1071` installs them.

Run with `./execute-all.sh '2026-09-paper-figures/031-*'` (after `000`, which it sources).
