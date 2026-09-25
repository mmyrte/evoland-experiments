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

| quantity | value |
| --- | --- |
| FoM, realisations: median (5–95 %) | 0.102 (0.064–0.138) |
| FoM, ensemble expectation | 0.098 |
| FoM, deterministic greedy | 0.172 |
| FoM, random-allocation null | 0.036 |
| Brier score of change, ensemble frequency | 0.059 |
| Brier score of change, single realisation (median) | 0.106 |
| Brier score of change, deterministic | 0.089 |

On FoM, the deterministic map beats the median draw. That is expected: FoM rewards putting
change where the potential is highest, which is exactly what a hard allocation does. The case
for ensembles is not a higher FoM per map. It rests on two other results:

1. **Single-map scores are unreliable.** Any single map's FoM depends heavily on the draw: the
   5–95 % range spans a factor of two.
2. **The ensemble is the better forecast.** As a probabilistic forecast of change, the
   change-frequency surface has the best Brier score, well below both the deterministic map
   and a single draw.

Panel (d) currently only shows the first point. See [`TODO.md`](TODO.md).
