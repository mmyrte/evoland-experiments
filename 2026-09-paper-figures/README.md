# Paper figures (2026-09) — mock-ups for the evoland-plus model description paper

**Status:** active. Figures for the paper in
[mmyrte/evoland-plus-paper](https://github.com/mmyrte/evoland-plus-paper), built on small
synthetic cases so they run in minutes. [`TODO.md`](TODO.md) holds the open work.

Outputs are written to `figures/` and copied by hand to `evoland-plus-paper/figures/`. A second
Overleaf project on this repo would remove the copy step.

| Step | Figure | What it shows |
| --- | --- | --- |
| [`020-fig2-ensembles.qmd`](020-fig2-ensembles.qmd) | Fig. 2, `figures/fig2-ensembles.{pdf,png}` | Ensembles vs. single maps on a backcast |

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
