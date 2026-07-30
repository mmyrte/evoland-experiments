# Notes — transition-rate solver as step `07-transition-rates.qmd`

Working notes for the design decision tracked in
[ethzplus/evoland-plus#32](https://github.com/ethzplus/evoland-plus/issues/32).
Analysis + recommendation, **not** production code.

Everything numeric below was **executed** — `lpSolve` is available, so the shipped solver was
ported to base R and run on the real `NCCS_simulation_LULC_areas.xlsx` inputs for all five
SSPs, alongside the `lm()` extrapolation it would replace. Scratch scripts are throwaway; the
numbers are the deliverable.

---

## 0. TL;DR

1. **The premise "replace rates with absolutes" is already true of the shipped solver.**
   `lulcc.simulationtransitionratesolver()` is *entirely* an absolute-area LP. Every decision
   variable is an area; every constraint is an area balance. Rates appear in exactly two
   places: as *coefficients* on `area[i,t]` in the soft min/max bound rows, and in the
   post-hoc division `optimized_rate = x_value / source_area`. So the thing the user wants to
   build is the thing that already exists. There is no "rate-based extrapolation logic" inside
   the solver to replace.

2. **The rate-based thing worth replacing is `extrapolate_trans_rates()`**, which is a
   completely different object: 21-ish independent `lm(rate ~ id_period)` fits with no coupling
   at all. Absolutes do not beat rates there because of a units argument — they beat it because
   a *solver with mass-balance constraints* beats *independent univariate regressions*. Those
   are two orthogonal changes and the user is conflating them.

3. **Where units genuinely matter is at the three interfaces**, not inside the optimisation:
   - *elicitation* (experts state absolute areas → absolutes win),
   - *portability* between the original 4,129,078-cell grid and this pipeline's coord set
     (→ **shares** win, this is the strongest argument in the whole note),
   - *consumption* by `adjusted_trans_pot_v()` / `allocate_clumpy_cpp()` (→ **rates** win,
     and are effectively mandatory unless you change C++).
   The right answer is therefore "all three, at different layers" (§5.3), and `trans_rates_t`
   already carries both `count` and `rate`, so nothing is blocked. Absolutes and shares are
   *provably the same LP* up to a scalar (§5.1) — that part of the choice is presentational.

4. **The mass-conservation argument for absolutes does not survive measurement.** I ran the
   `lm()` path end to end (§3.6): total area drift `−0.0000 %`, no outflow row above 1.
   Conservation is structural in the simulator, not something the rate table provides. The
   real deficiency of `extrapolate_trans_rates()` is that **it has no input for a scenario
   target at all** — it lands 10–19 % off every SSP.

5. **Recommendation: reproduce the shipped LP** (it is the replication target and it runs),
   with the §4 bugs fixed behind a flag, plus four docx ideas that are all LP-representable:
   the `compute_final_bounds` precheck, hard-forbidden edges, an L1 terminal-fit term and a
   minimax fairness bound. **No QP dependency is needed.** §6.

6. **Three defects in the shipped solver, found by running it** (§4). The worst:
   `build_diff_row()` assigns over its own coefficient, so the entire `chosen_shape` mechanism
   is dead code and the objective is dominated ~3,700× by a term the LP cannot influence.
   Fixing one line cuts out-of-bounds transition churn by up to **35 %**.

7. **The most useful number this step can produce is not the rates.** It is that **24 of 50
   SSP × class targets are unreachable** under observed transition bounds, several by 4–5×,
   glacier by 1.81× in all five scenarios (§3.4) — and that 42–63 % of the solved flow ends up
   outside the historic envelope (§3.5). Build the precheck first; report it as a result.

---

## 1. What `lulcc.simulationtransitionratesolver.R` actually optimises

Read in full: `/home/user/NCCS-SSP-scenarios/Scripts/Functions/lulcc.simulationtransitionratesolver.R`
(1018 lines; the solver proper is lines 34–578, then two wrappers).

### 1.1 Decision variables — all absolute

| block | count | meaning | units |
| --- | --- | --- | --- |
| `x[i,j,t]` | `L²·Num_steps` | area of class *i* becoming class *j* during interval *t* | **cells** |
| `devLower[i,j,t]` | `L²·Num_steps` | slack below the min-rate bound | **cells** |
| `devUpper[i,j,t]` | `L²·Num_steps` | slack above the max-rate bound | **cells** |
| `area[l,t]` | `L·(Num_steps+1)` | area of class *l* at time point *t* | **cells** |
| `shapeSlack[s]` | variable | per-class curvature violation | **cells / step** |
| `smoothSlack[l,t]` | `L·(Num_steps−1)` | 2nd-difference violation | **cells** |

`L = 10` classes, `Num_steps = 8` for the 2020→2060 leg at 5-yearly steps: 800 flow vars +
1600 slacks + 90 area vars + 720 shape slacks + 70 smoothing slacks ≈ **3280 variables**,
~2000 constraint rows. Trivially small for `lpSolve`.

### 1.2 Constraints — all absolute

Verbatim from the source, in order:

```
(C1)  area[l,0]                = init_area[l]                       for all l
(C2)  Σ_l area[l,t]            = region_area                        for t = 0..T      <- mass conservation
(C3)  Σ_j x[i,j,t]             = area[i,t]                          for t = 0..T-1    <- row (outflow) closure
(C4)  area[j,t+1]              = Σ_i x[i,j,t]                       for t = 0..T-1    <- column (inflow) closure
(C5a) x[i,j,t] - (rmax+m)·area[i,t] - devUpper[i,j,t]   <= 0
(C5b) -x[i,j,t] + (rmin-m)·area[i,t] - devLower[i,j,t]  <= 0
(C6)  0.99·target[l] <= area[l,T] <= 1.01·target[l]                 <- HARD terminal band
(C7)  sign(area[l,t] - area[l,t-1]) = sign(target[l] - init[l])     <- HARD monotonicity
(C8)  shape: ±( Δarea[l,t]/len_t - Δarea[l,t+1]/len_{t+1} ) <= shapeSlack   (soft)
(C9)  | area[l,t+1] - 2·area[l,t] + area[l,t-1] |  <=  smoothSlack  (soft)
```

`x >= 0` is implicit — `lpSolve::lp()` assumes non-negative variables.

Note that (C2) is *redundant* given (C1)+(C3)+(C4): row closure plus column closure already
conserve total area exactly. It is a numerical belt-and-braces row, not a modelling choice.

### 1.3 Objective — pure penalty minimisation, no fit term

```
min  Σ  λ_bounds · w_i · (devLower + devUpper)          (λ = 0.1)
   + Σ  μ_shape  · w_l · shapeSlack                      (μ = 15)
   + Σ  μ_smooth · w_l · smoothSlack                     (μ = 100)

with  w_l = 1 / max(init_area[l], 1e-6)
```

Two consequences worth flagging:

- **There is no objective term rewarding proximity to the target.** Hitting the demand is
  purely the hard band (C6). So the solver either hits ±1% or is *infeasible* — there is no
  graceful degradation. That is the single biggest robustness problem with the shipped model
  and is exactly what the docx's quadratic terminal-fit term + `compute_final_bounds` precheck
  are trying to fix.
- **The shape term does not work at all.** The docstring blames the temporal-smoothing
  constraint (*"tends to override the effect of the shape-based constraint"*), and the
  weights do look lopsided (`μ_smooth = 100` at the call site vs `μ_shape = 15`; the roxygen
  even documents `mu_temporal_smoothness`'s default as **1**, so the call site is 100× the
  documented default). But that is a misdiagnosis — see §3.3 and §4.1. The real cause is a
  coefficient-clobbering bug in `build_diff_row()`, plus the fact that a straight line
  satisfies every one of the five shapes by construction. `chosen_shape` is not "nearly
  decorative"; it is inert.

### 1.4 Verdict on the user's premise

**Confirmed, and it is stronger than stated.** The rates in (C5) are not "the model working in
rate space" — they are a *linear* bound whose right-hand side happens to be proportional to a
decision variable. Writing `x[i,j,t] <= rmax·area[i,t]` versus `x[i,j,t] <= cap_ij_t` (a
constant) is the *only* real difference between "rate bounds" and "absolute bounds", and the
rate version is strictly better here: it makes the cap adapt as the source class shrinks, which
is what keeps a class from being drained faster than history supports once it is small. Making
those bounds absolute would be a regression, not a simplification.

The final `optimized_rate = x_value / source_area` is a pure unit conversion for the consumer.

---

## 2. What actually goes wrong in `extrapolate_trans_rates()`

`/home/user/evoland-plus/R/trans_rates_t.R`, lines 119–165. The whole model is:

```r
obs_rates |>
  split(by = c("id_run", "id_trans")) |>
  lapply(\(subtable) {
    mod <- lm(rate ~ id_period, data = subtable)     # 3-4 points, 2 parameters
    predictions <- predict(mod, newdata = ...)
    predictions[predictions < 0] <- 0                # clamp
    ...
  })
```

Failure modes, in decreasing order of severity:

1. **No mass balance in either direction** — *as stated in issue #32*. Nothing forces
   `Σ_j rate[i→j] <= 1`, and nothing links class *i*'s outflow to any other class's inflow.
   **But see §3.6: I measured this and it does not bite in this pipeline.** Total area is
   conserved *structurally* by the simulator (one class per cell, row closure in
   `adjusted_trans_pot_v`, one destination drawn per cell in `allocate_clumpy_cpp`), and the
   fitted outflow sums stay far below 1 (max 0.237). This is the weakest of the arguments for
   changing formulation, and I would not lead with it.
2. **No class-trajectory consistency.** Every `id_trans` gets its own slope. Since observed
   rates are noisy across only 3 usable transitions, extrapolating 4 periods out compounds it.
   The clamp at 0 is a one-sided, non-conservative repair: it removes negative flow but adds
   nothing back to persistence, so the row sum silently changes.
3. **`id_period` treated as unit-spaced.** The observed intervals are **12 y (1985→1997),
   12 y (1997→2009), 9 y (2009→2018)**, and the future ones are 10 y. Fitting `rate ~ id_period`
   regresses a *per-interval* quantity on an *index*, so the 9-year interval's rate is
   systematically low relative to its neighbours and drags the slope down; and the projection
   onto 10-year steps is on yet another scale. The fix in rate space is to fit an
   *annualised* rate (`1 - (1 - rate)^(1/Δt)`) against calendar year, and re-inflate to the
   target step length — a change entirely orthogonal to absolutes.
4. **No scenario input at all.** This is the real gap for SSP-CH: `extrapolate_trans_rates()`
   is a *persistence-of-trend* extrapolator. It cannot express "SSP1 wants closed forest at
   1.186 Mha by 2100" because there is nowhere to put that number. Any solver, in any unit
   system, fixes this simply by having a target argument.
5. The `TODO` on line 138 — *"maybe fit on count instead of rate? easier to reason about"* — is
   the seed of the user's question. Note that fitting counts instead of rates would make (1)
   *worse*, not better: counts are unbounded above by the source class size, so a positive
   trend can demand a flow larger than the class.

`trans_rates_t` already stores `count` **and** `rate`, and `validate.trans_rates_t()` only
checks `rate >= 0`. So the storage layer is unit-agnostic and imposes no constraint on this
decision.

---

## 3. Numerical experiment — the solver actually run on the real numbers

`lpSolve` **is** installed on this machine, so everything below was executed, not reasoned
about. Scripts in
`/tmp/claude-0/-home-user/b32661b2-21ce-54d3-9864-e47ef7aa7fab/scratchpad/`
(`lp_common.R` = base-R port of the shipped LP, `precheck.R` = `compute_final_bounds`,
`t_run*.R` = experiments). They are throwaway; the point is the numbers.

### 3.0 Inputs used

- **Demand**: `Tools/NCCS_simulation_LULC_areas.xlsx` sheet `shhet1`, all 5 SSPs × 10 classes.
  `init_area` equals the 2018 Arealstatistik areas exactly (cross-checked against
  `Transition_Tables.xlsx` sheet `01_Historic_Areal_coverage`, column "2018 (2020)"), so
  `init_area` is the **2018 survey state labelled 2020**.
- **Historic rates**: `Transition_Tables.xlsx` sheet `01_extrapolated_trans_rates`, columns
  `1997`/`2009`/`2018` = the three calibration-period rates
  (`1985→1997`, `1997→2009`, `2009→2018`). 21 off-diagonal edges. This is the same content as
  the `trans_rates_table_calibration_periods_SS.csv` the prep script reads (that CSV is not in
  the repo). Persistence (`i→i`) reconstructed as `1 − Σ_{j≠i} rate`, exact by construction.
- **Grid**: `t = 2018, 2028, 2038, 2048, 2058`, i.e. 4 × 10 y — the evoland decadal grid
  anchored on the last observation. Targets at 2058 obtained by piecewise-linear
  interpolation of the (2020, 2060, 2100) demand anchors.
- Rate bounds annualised (`1 − (1−r)^(Δt'/Δt)`) from their native 12/12/9-year intervals to
  the 10-year step before taking min/max.

### 3.1 First useful fact: the demand is *already* mass-balanced in absolute area

```
             init_area   final_2060   final_2100
SSP0..SSP5   4,129,078    4,129,078    4,129,078      (identical to the last decimal)
```

All five scenarios and both horizons sum to exactly `region_area = 4,129,078` cells.
So the LP's `Σ_l area[l,t] = region_area` constraint is *compatible with the elicited data as
given*, and linear interpolation between two mass-balanced target vectors is itself
mass-balanced — meaning the resampling onto the decadal grid (§7.5) cannot break conservation.
That is a genuine, checkable win for stating demand in absolute areas: **the conservation error
is visible in the input file**, before any solver runs.

### 3.2 All five SSPs solve. And the ±1% band is *binding everywhere*

Problem size on the decadal grid: ~1,330 variables, ~1,060 constraints; `lp()` returns in well
under a second. Total area drift `0.000000 %` in every scenario (as it must be — (C3)+(C4)).

But look at `pct_err` of the solved terminal areas against the targets:

```
SSP0: -1.00 -1.00 +1.00 -1.00 -0.19 +1.00 +1.00 -1.00 -1.00 -1.00
SSP1: -1.00 +1.00 -0.97 -1.00 +1.00 +1.00 -1.00 -1.00 -1.00 -1.00
SSP4: -1.00 +1.00 -1.00 +1.00 +1.00 +0.50 -1.00 -1.00 -1.00 -1.00
```

Nearly every class parks exactly on a band edge. That is the direct consequence of §1.3:
**there is no objective term that rewards being close to the target**, so the LP treats the
±1% band as free real estate and spends it to reduce penalties elsewhere. On a 1.1 Mcell class
that is ±11,000 cells of unremarked slop. Anyone reading "the solver hits the 2060 targets" is
being misled — it hits *a 2%-wide box around* them, biased to one side.

*Implication for the port*: either shrink `terminal_band` to something like 1e-3, or add an
explicit terminal-fit term to the objective (which is what the docx formulation does, in
quadratic form; an L1 version `min Σ w_l·|area[l,T] − target_l|` stays an LP and is the cheap
fix).

### 3.3 `chosen_shape` has *no* effect — and the reason is a coefficient-clobbering bug

(The first thing I found here was that the trajectories come out linear regardless of shape.
Chasing why led to §4.1, which is the most consequential finding in this note. Read the two
together.)

With the shipped defaults (`mu_temporal_smoothness = 100`, `mu_shape = 15`), the solved
trajectories are **exactly linear** for every class in every scenario:

```
SSP4, per-step deltas (cells), smoothing ON:
  Perm_crops  -6162  -6162  -6162  -6162     (chosen_shape = "Instant decline")
  Shrubland   21996  21996  21996  21996     (chosen_shape = "Delayed growth")
  Alp_Past   -36921 -36921 -36921 -36921     (chosen_shape = "Constant change")
smoothing slack = 0, shape slack = 9.0e6 cells
```

Sweeping `mu_temporal_smoothness` over `100, 10, 1, 0.1` changes the trajectories but leaves
the total shape slack essentially pinned at ~9.0e6 in every case — the shape penalty never
falls, no matter how much room it is given. That is not a weight-tuning problem; it is the
signature of a constraint that *cannot* be satisfied. §4.1 shows why: the curvature row is
malformed, so each shape slack is forced to ≈ the class's own area regardless of the
trajectory. The docstring's "smoothing tends to override shape" is a misdiagnosis of a bug.

With the bug fixed, shape slack drops to exactly **0** and the objective falls from 305.6 to
0.082 — but the trajectories are *still* straight lines. This is the second, independent
reason `chosen_shape` is inert, and it survives the bug fix:

> **A straight line satisfies all five shapes simultaneously.** The constraints are weak
> one-sided inequalities on curvature — "instant" asks `R(t) ≥ R(t+1)`, "delayed" asks
> `R(t) ≤ R(t+1)`, "constant" asks `R(t) = R(t+1)`. Zero curvature satisfies every one of
> them with equality. So zero-curvature is always in the feasible set of every shape, and it
> is always the cheapest place to be.

To make an elicited shape actually bind you need either a *strict* curvature requirement
(minimum |second difference| scaled to the class's total change) or a fitted target curve.
This matters for §6: the docx's convex/concave/constant slacks *weighted toward small classes*
plus a minimax fairness bound is a direct attempt to fix exactly this, and it is the part of
the quadratic formulation I would port even if the rest is skipped.

### 3.4 The headline: **24 of 50 SSP class targets are physically unreachable**

Implemented `compute_final_bounds` (the docx precheck, see §6) as a pure LP: maximise and minimise
`area[l,T]` subject to mass balance + row/column closure + **hard** historic max off-diagonal
rates, persistence free, no min-rate bounds, no monotonicity. That is the *loosest honest*
reachability question: "can class *l* get there at all, given that no edge has ever moved
faster than it historically did?"

```
TRUE reachable band at 2058 (4 x 10y steps), as % of 2018 area:
  Int_AG        -15.8 .. +9.4      Static         0.0 ..  +3.4
  Perm_crops    -31.6 .. +25.9     Closed_Forest  -4.3 ..  +9.2
  Grassland     -13.1 .. +12.3     Open_Forest   -43.1 .. +48.1
  Shrubland     -15.2 ..  +6.7     Urban          -1.5 .. +15.8
  Alp_Past       -6.7 ..  +2.1     Glacier       -21.5 ..   0.0
```

Against the interpolated 2058 SSP targets:

| Scenario | infeasible classes | worst class | miss (cells) | asked / achievable |
| --- | --- | --- | --- | --- |
| SSP0 | 3 | `Int_AG` | +114,568 | **4.15×** |
| SSP1 | 5 | `Static` | +80,951 | **4.15×** |
| SSP3 | 4 | `Closed_Forest` | +22,819 | 1.22× |
| SSP4 | 5 | `Alp_Past` | −112,520 | **4.53×** |
| SSP5 | 7 | `Closed_Forest` | −128,555 | **3.69×** |

`Glacier` is unreachable in **all five** scenarios by 1.81× (target −40,066 cells, historic
maximum −22,159). Note the original authors patched glacier areas with Zekollari-modelled
values *without* touching the rate bounds — so this gap was structural in the original study
too, absorbed silently by the soft slack.

Two honest readings of this, and I think both are true:

- **The elicited SSP targets are normative, not extrapolative.** They deliberately break with
  the observed 1985–2018 envelope; that is the point of a scenario. So a precheck that *fails*
  on an unreachable target would fail on almost everything. Issue #32's phrasing — *"an
  infeasible expert target should fail loudly"* — is too strong. It should **report**, in the
  form "SSP4 asks Alp_Past to decline 4.5× faster than any observed decade", and let the
  analyst decide.
- **Therefore the soft rate bounds in the shipped LP are load-bearing, not sloppy.** Making
  them hard makes the whole exercise infeasible. What is wrong is not that they are soft, but
  that nothing *reports* how badly they were violated.

### 3.5 …and 42–63% of the solved flow lies outside the historic envelope

Because the bounds are soft and cheap (`λ = 0.1` × `1/init_area`), the LP buys its way out
wholesale:

| Scenario | total off-diagonal flow | flow outside historic bounds | on **zero-history** edges |
| --- | --- | --- | --- |
| SSP0 | 393,885 | 187,953 (47.7 %) | 116,025 |
| SSP1 | 426,710 | 178,888 (41.9 %) | 61,776 |
| SSP3 | 407,834 | 177,647 (43.6 %) | 107,763 |
| SSP4 | 461,328 | 253,281 (54.9 %) | 97,319 |
| SSP5 | 586,013 | 367,836 (62.8 %) | 288,666 |

Largest invented edges (no historic support at all, summed over scenarios and steps):

```
Closed_Forest -> Int_AG    129,539 cells      Static -> Int_AG        27,570
Closed_Forest -> Static    124,415            Static -> Open_Forest   26,693
Closed_Forest -> Grassland  63,519            Alp_Past -> Closed_For. 26,080
Grassland -> Shrubland      40,355            Glacier -> Shrubland    24,845
Closed_Forest -> Urban      33,144            Static -> Shrubland     21,741
```

`Static → anything` is the giveaway: `static` is the *immutable* catch-all class (lakes,
rock, motorways). The LP is deforesting into arable land and turning glaciers into shrubland
via edges that have never been observed and, in evoland, would not even be modelled.

**Root cause is a one-line bug** in the shipped LP, lines 231–236:

```r
# if r_max == 0 i.e. transition is not allowed then set the upper bound to 0
if (r_max == 0) {
  row_up[idx_area(i_i, t_i)] <- 0      # -> row is:  x[i,j,t] - devUpper[i,j,t] <= 0
} else { ... }
```

The comment says "not allowed"; the code says "allowed at the cheapest available penalty",
because `devUpper` is still in the row. A forbidden edge should be `x[i,j,t] = 0` with no
slack (or the variable dropped). This is exactly the docx's "hard-forbidden edges" item, and
it is the single most important thing to fix in a port.

*Downstream consequence in evoland specifically*: `trans_pot_t` is only populated for
`trans_meta_t[is_viable == TRUE]` (`trans_pot_t.R:107`), and `adjusted_trans_pot_v()` joins
potentials to rates. A rate on a non-viable edge therefore has **no potentials to scale** and
is silently dropped at allocation time — so the LP's promised class trajectory quietly does
not materialise. Not a crash, not a warning: just a scenario that does not do what it says.

### 3.6 The other side: what the `lm()` extrapolation actually does

Reproduced `extrapolate_trans_rates()` exactly (per-edge `lm(rate ~ id_period)` on the three
observed rates at `id_period` 2/3/4, predict 5–8, clamp negatives), then forward-simulated the
Markov chain from the 2018 areas.

```
             2018    lm 2058   SSP0    SSP1    SSP3    SSP4    SSP5
Int_AG      388383   344622   539313  292214  387364  371316  412601
Closed_For 1112032  1191278  1071294 1157777 1237286 1221141  935772
Grassland   539423   549884   480506  595623  478578  492256  677454
Alp_Past    476677   454420   472820  434591  444936  332317  409606
Glacier     102992    77402    62926   62926   62926   62926   62926

mean |lm − SSP target| as % of class init area:
  SSP0 14.4 %   SSP1 10.3 %   SSP3 10.6 %   SSP4 19.5 %   SSP5 13.9 %
```

**Here is the finding that cuts against the user's premise.** Total area drift under the
`lm()` arm was `−0.0000 %`, no row's outflow rates summed above 1 (max 0.237), and zero rows
needed renormalising. **The mass-balance argument in issue #32 does not hold in this
pipeline.** Conservation is structural: every cell has exactly one class, `adjusted_trans_pot_v`
closes each cell's row to ≤ 1, and `allocate_clumpy_cpp` draws one destination per cell. The
simulator conserves area no matter what the rate table says. What the rate table *can* do is
demand more outflow from a class than exists — and that is bounded by `Σ_j rate[i→j] ≤ 1`,
which held comfortably here (worst class `Open_Forest`, 0.237 and rising ~0.031/decade — it
would take ~25 more decades to break).

So the real deficiencies of `extrapolate_trans_rates()` are, honestly ranked:

1. **It has no input for a scenario target.** It is a trend-persistence extrapolator. It
   produces *one* future, ~10–19 % off every SSP target, and there is nowhere to state the
   SSP. This is the entire reason to build a solver, and it has nothing to do with units.
2. **No control of the class trajectory** — you get whatever the 21 independent slopes imply,
   with no way to say "closed forest declines, and it declines early".
3. **`id_period` unit-spacing** (12/12/9 y observed → 10 y future) biases every slope. Real,
   but fixable in rate space in ~5 lines and orthogonal to this whole design decision.
4. **A dimensional bug in the `count` column** (`trans_rates_t.R:159`):
   ```r
   count = as.integer(round(coord_count * predictions))
   ```
   `rate` is defined by `get_obs_trans_rates()` as `n / total` where `total` is the count of
   the **anterior class** in that period (the `totals` CTE groups by `id_lulc_anterior`). So
   the count implied by a rate is `n_anterior × rate`, not `n_all_cells × rate`. As written
   the stored `count` is inflated by `total_cells / n_anterior` — a factor of ~3.7 for
   `closed_forest` and ~40 for `glacier`. Nothing downstream reads `count` today, which is why
   it has gone unnoticed; anything that starts reasoning in absolutes will read it first.


---

## 4. Defects found in the shipped solver (all verified by running it)

Anything ported must fix these. Ordered by impact.

### 4.1 🔴 `build_diff_row()` clobbers its own coefficient — the shape term is dead code

```r
build_diff_row <- function(l_i, t1, t2, ratio) {
  row_ <- numeric(...)
  row_[idx_area(l_i, t1)]     <-  1      # <- (a)
  row_[idx_area(l_i, t1 - 1)] <- -1
  row_[idx_area(l_i, t2)]     <- -ratio
  row_[idx_area(l_i, t2 - 1)] <-  ratio  # <- (b) OVERWRITES (a)
  row_
}
```

It is only ever called as `build_diff_row(l_i, t_i, t_i + 1, ratio)`, so `t2 - 1 == t1` and
line (b) **assigns over** line (a). These are `<-`, not `+=`. The intended row is

```
Δarea[l,t1] − ratio·Δarea[l,t2]  =  (1+ratio)·A[t1] − A[t1−1] − ratio·A[t1+1]
```

what is actually built is

```
ratio·A[t1] − A[t1−1] − ratio·A[t1+1]
```

which for `ratio = 1` evaluates to `−A[t1]` on any smooth trajectory: it is an *area*, not a
curvature. So every shape slack is driven to ≈ the class's current area, and the shape term
contributes a near-constant ~`mu_shape` per row to the objective that the LP can do nothing
about.

Consequences, measured on the decadal grid:

| Scenario | objective (as shipped) | objective (fixed) | rate-bound violation, cells (shipped → fixed) | change |
| --- | --- | --- | --- | --- |
| SSP0 | 314.494 | **0.032** | 125,267 → 123,367 | −1.5 % |
| SSP1 | 393.581 | **0.030** | 103,926 → 78,616 | **−24.4 %** |
| SSP3 | 307.038 | **0.033** | 146,315 → 94,438 | **−35.5 %** |
| SSP4 | 305.589 | **0.082** | 263,541 → 228,998 | −13.1 % |
| SSP5 | 395.159 | **0.055** | 397,333 → 366,817 | −7.7 % |

The objective drops by ~4 orders of magnitude. That is the real damage: with the bug, the
bogus shape term is ~3,700× larger than the rate-bound term, so **the solver was effectively
not minimising rate-bound violations at all** — those were rounding error in the objective.
Fixing one line cuts unexplained transition churn by up to a third.

Fix: use `+=` semantics (`row_[i] <- row_[i] + c`), or build the row from the general
expression directly.

### 4.2 🔴 "Forbidden" edges are merely cheap

Lines 231–236, already covered in §3.5. `r_max == 0` produces `x[i,j,t] − devUpper ≤ 0`
instead of `x[i,j,t] = 0`. Result: 62,000–289,000 cells per scenario flow along edges with no
historic precedent, including `static → arable`, `static → alp_past` and
`closed_forest → static`. In evoland these edges have no `trans_pot_t` rows, so the flow is
silently dropped at allocation and the promised trajectory quietly under-delivers.

Fix: for `r_max == 0`, emit a hard `x[i,j,t] = 0` row (or, better, never create the variable).
Drive the allowed edge set from `trans_meta_t[is_viable == TRUE]`, not from
`maxRate > 0`.

### 4.3 🟠 `Step_length` is documented as a vector but passed as a scalar

`lulcc.solvemultiplefutureperiods()` sets `Step_length <- 5` (a scalar, because it is also fed
to `seq()`), then passes it down to the solver, which indexes `Step_length[t_i]` for
`t_i = 1 … Num_steps−1`. For `Num_steps = 8` that yields `Step_length[2..7] = NA`, so
`ratio = 5/NA = NA` and NAs land in the constraint matrix.

`lpSolve::lp()` **swallows them silently** — verified: it returns `status = 0` and a *different*
answer (objective 399.31 vs 393.58 with a proper vector) with no warning, no error, no
`NA` in the output. There is no way to notice this from the outside.

Fix: `stopifnot(length(Step_length) == Num_steps, !anyNA(Step_length))` at entry, and treat
`Step_length`/`Time_steps` as first-class vectors derived from `periods_t`.

### 4.4 🟠 The ±1 % terminal band is a *bias*, not a tolerance

§3.2: with no objective term rewarding target proximity, essentially every class lands exactly
on a band edge. Either shrink the band to ~1e-3, or add an L1 fit term
`min Σ_l w_l·(u_l⁺ + u_l⁻)` with `area[l,T] − target_l = u_l⁺ − u_l⁻`, which keeps the model an
LP. I would do both.

### 4.5 🟡 Rate bounds are used at the wrong time scale

`Simulation_trans_tables_prep.R` takes min/max over the columns `1985_1997`, `1997_2009`,
`2009_2018` — three intervals of **12, 12 and 9 years** — and applies them unchanged as
bounds on **5-year** steps. That inflates the permitted per-step churn by roughly 2×.
It also means the 9-year interval is systematically the "low" one, biasing `minRate` down.

Fix: annualise before comparing, `r_ann = 1 − (1 − r)^(1/Δt)`, take min/max on the annualised
scale, then re-inflate to the actual step length. All the numbers in §3 use this. The same
correction applies verbatim to point 3 in §2 about `lm(rate ~ id_period)`.

### 4.6 🟡 `init_area[l_i]` is positional

`subset_lulc_exp_areas$init_area[l_i]` assumes the data frame's row order matches
`unique(subset_lulc_exp_areas$LULC)`. It happens to, in this spreadsheet. Join on the class
key instead.

### 4.7 🟡 Row-drop filters in the wrapper are index-coupled

`lulcc.solvemultiplefutureperiods()` computes `NA_rows` and `Neg_zero_rows` from
`Trans_area_extraps` and then applies the *same* logical vector to `Trans_rate_extraps`. That
only works while the two frames stay row-aligned — they are built by separate
`pivot_wider()` + `arrange()` calls. Don't reproduce this; filter each frame on its own key.


---

## 5. Absolutes vs rates vs shares — the answer to the actual question

### 5.1 The three are related by two divisions, and nothing else

```
count[i→j,t]                              absolute flow, cells
rate[i→j,t]  = count[i→j,t] / area[i,t]   share of the SOURCE CLASS
share[l,t]   = area[l,t]    / region_area share of the LANDSCAPE
```

Inside an optimisation with a fixed `region_area`, **absolutes and shares are the same model up
to a scalar**: divide every variable, every RHS and every bound by `region_area` and you have
the share formulation; the constraint matrix is unchanged, the solution is identical, only the
condition number moves. There is no modelling content in that choice. Anyone claiming the docx
is "better because it uses shares" is claiming something about the *quadratic terms*, not about
the units.

Rates are the one that is genuinely different, because `area[i,t]` is itself a variable:
`x ≤ r·area[i,t]` is a *ratio* constraint, and it is what makes the permitted flow shrink with
the source class. Expressed as an absolute cap it would be a constant, which is a strictly
worse model (a class with 5,000 cells left would still be allowed to shed 20,000).

So the honest scorecard for "does the absolutes framing buy what I hope":

| claim in issue #32 | verdict |
| --- | --- |
| "Area is conserved by construction" | **True in the LP, but not a *gain* over rates** — §3.6 shows the rate path conserves area too, structurally, downstream. |
| "Targets are directly interpretable" | **True and important.** This is the real win, and it is about *elicitation*, not arithmetic. |
| "Period length enters explicitly" | **True but orthogonal.** You can annualise rates in five lines (§4.5) without changing formulation. |
| "Rates don't compose into a mass balance" | **Misleading.** The reason `extrapolate_trans_rates()` produces incoherent trajectories is that it fits 21 *independent univariate regressions* — not that it works in rates. A coupled solver in rate space would be just as coherent. |

**Blunt version: the units are mostly presentational; the solver is not.** If the user swapped
`extrapolate_trans_rates()` for a coupled solver *that still worked in rates*, they would get
~95 % of the benefit. If they switched to absolutes and kept 21 independent `lm()` fits, they
would get ~0 %. The user's instinct points at the right file for the wrong reason — which is
fine, because the fix is the same either way.

### 5.2 Where the unit choice *does* bite

Four places, in decreasing order of how much I would weight them.

**(a) Portability of the elicited demand — shares win, decisively.**
This is the one argument I would actually act on. The demand spreadsheet is stated on a grid
of **4,129,078 cells**. This pipeline's `coords_t` is restricted (in `01-ingest-lulc-data.qmd`)
to coordinates that carry Arealstatistik data on a 100 m grid over the CH bounding box; the
class definitions are a re-derivation from NOAS04, not a copy. The totals *will not match*,
and `init_area` from the spreadsheet is not the same number as
`db$lulc_data_t[id_period == 4, .N, by = id_lulc]`.

An absolute target of "1,157,777 cells of closed forest" is therefore **meaningless outside the
grid it was elicited on**, whereas "28.04 % of Switzerland is closed forest" transfers
unchanged. The step-07 code should:

```
share_target[l] <- demand_area[l] / sum(demand_area)          # normalise on the SOURCE grid
target[l]       <- share_target[l] * observed_total_this_grid  # rehydrate on OUR grid
```

Because all five scenarios and both horizons already sum to exactly 4,129,078 (§3.1), this
round-trip is exact and mass-preserving. Store the *shares* in the DB as the scenario input;
derive cells at solve time.

**(b) Consumption by the allocator — rates win, and are effectively mandatory.**
`adjusted_trans_pot_v()` column-scales each transition's potentials by `rate / mean_value`, so
the mean adjusted potential over the source class equals the target `rate`. Then
`allocate_clumpy_cpp` computes the uPAM quota as

```cpp
remaining[q] = rt * m0;      // N_{u->v} = P(v|u) * #J    (alloc_clumpy.cpp:642)
```

where `m0` is the size of the **actually available source pool at simulation time**, and uSAM
has no quota at all — quantity of change is enforced purely in expectation through the scaled
potentials. Both paths take a rate and multiply by realised state. Handing them an absolute
count would require changing the view *and* the C++.

That is not just an implementation accident, it is the right contract: allocation is
stochastic, patches quantise, `avoid_aggregation` fails some attempts, and row closure clips.
The simulated `area[i,t]` drifts away from the LP's predicted `area[i,t]`. A **rate
re-anchors to reality every step**; an absolute count would over-shoot when the class turned
out smaller than the LP assumed and under-shoot when larger, and the error would compound over
four steps. Keep `rate` as the interface.

**(c) Numerical conditioning — mild, and it favours shares.**
The LP as built spans ~10⁶ (areas) down to ~10⁻³ (rate coefficients) — nine orders of
magnitude in the constraint matrix. In practice `lpSolve` handled it fine here (all 25
solves clean, `status = 0`). But the objective weights `1/init_area` are already an implicit
normalisation, and the docx's ridge term is a symptom of someone hitting conditioning
problems. Working in shares removes ~6 orders of magnitude for free. Low stakes, no downside.

**(d) Interpretability of diagnostics — absolutes win.**
"SSP4 asks alpine pasture to decline 4.5× faster than any observed decade, a shortfall of
112,520 cells" is a sentence an analyst can act on. Its rate-space equivalent is not. Every
number in §3.4/§3.5 was computed in cells for exactly this reason.

### 5.3 Recommended layering

Do not pick one. Assign each unit to the layer where it is correct:

| layer | unit | why |
| --- | --- | --- |
| scenario input, stored in the DB | **share of landscape** | grid-independent, portable, exact round-trip (§3.1) |
| solver internals | **absolute cells** (= shares × total) | matches the existing model; readable diagnostics; conditioning is fine at this size |
| `trans_rates_t.rate` | **rate** | what `adjusted_trans_pot_v` and the allocator consume |
| `trans_rates_t.count` | **cells** | already in the schema; fix the §3.6-(4) dimensional bug while you are here |

This costs two multiplications and settles the question.

---

## 6. Shipped LP vs the docx formulation

I could **not** obtain the Word document — no `.docx` exists anywhere on this machine, and
issue #32 is the only description available. Everything in the "docx" column below is from the
issue text, so treat it as second-hand. Flagged again in §9.

| aspect | shipped LP (`lulcc.simulationtransitionratesolver.R`) | docx formulation (per issue #32) |
| --- | --- | --- |
| state variable | absolute area, cells | share of landscape, Σ = 1 |
| solver class | **LP** — `lpSolve::lp()` | **QP** — quadratic terms; `lpSolve` insufficient |
| terminal fit | hard 99–101 % band, **no objective term** | quadratic terminal-fit term **plus** relative-width guard band as constraints |
| historic preference | none — only min/max rate bound slacks | quadratic pull of per-class outflow proportions toward the row-normalised historic pattern |
| zero-history edges | soft, mispriced (§4.2) | linear penalty **and** a hard-forbidden set |
| monotonicity | hard, from `sign(final − init)`, no tolerance | up/down/flat increments with a tolerance |
| shape | one-sided curvature inequality + slack; broken (§4.1) and inert (§3.3) | convex/concave/constant slacks, **weighted toward small classes** |
| fairness across classes | implicit only, via `w_l = 1/init_area` | explicit **minimax** variable bounding the worst per-class shape distortion |
| temporal smoothing | L1 on second differences, `μ = 100` | **absent** |
| regularisation | none | ridge |
| infeasibility handling | none — soft slack absorbs it silently | **`compute_final_bounds` precheck LP** |
| variable step lengths | explicit `Time_steps` / `Step_length` (but see §4.3) | not described |

**They are not the same model.** The docx is not "the shipped LP with better notation"; it
replaces the shape mechanism, adds two entirely new objective terms, and drops smoothing.

### 6.1 Which to reproduce: **the shipped LP.** Reasons, ordered

1. **This experiment's stated purpose is replication.** `00-setup-db.qmd` names the run
   *"Replication of SSP-CH in new evoland"*. The published SSP-CH results came out of the
   shipped LP. Reproducing a different model — even a better one — forfeits the ability to say
   "we reproduced SSP-CH", and there would be nothing to diff against when numbers disagree.
2. **It runs.** I executed it on the real inputs, all 5 scenarios, in under a second each. The
   docx model has, as far as anyone here knows, never been run.
3. **No new dependency.** `lpSolve` is a small, ancient, dependency-free package.
   `quadprog`/`osqp`/`ROI` are not currently in `rproject.toml` and none of them is free.
4. **Most of the docx's value is LP-representable anyway** (see 6.2), so framing this as
   "LP vs QP" is a false choice.

### 6.2 …but port five docx ideas, four of which stay inside an LP

| docx idea | port? | how, in LP |
| --- | --- | --- |
| **`compute_final_bounds` precheck** | ✅ **yes, first** | already implemented and run (§3.4); it is a plain LP — max/min `area[l,T]` under mass balance + hard rate bounds. Highest value-per-line in this whole note. |
| **hard-forbidden edges** | ✅ yes | fixes §4.2. Equality row `x[i,j,t] = 0`, or omit the variable. Drive from `trans_meta_t[is_viable]`. |
| **terminal-fit term** | ✅ yes, L1 | `area[l,T] − target_l = u⁺ − u⁻`, minimise `Σ w_l (u⁺ + u⁻)`. Fixes §4.4 without a QP. |
| **minimax fairness** | ✅ yes | a minimax of linear expressions **is** an LP: add `z`, constrain `z ≥ w_l · slack_l` for all `l`, put `z` in the objective. No quadratic needed. |
| **historic-preference term** | 🟡 optional, L1 | `|x[i,j,t] − p̂_ij · area[i,t]|` with `p̂` the row-normalised historic outflow pattern, via ± slacks. Linear. This is the term that would most improve plausibility — it is what stops the LP inventing `closed_forest → arable`. |
| shares as state variable | ➖ skip | provably equivalent (§5.1); do the normalisation at the boundary instead. |
| quadratic weighting / ridge | ➖ defer | only worth a new solver dependency if the L1 versions visibly misbehave. |
| dropping temporal smoothing | ✅ yes | set `mu_temporal_smoothness` to the *documented* default (1) or 0. Once §4.1 is fixed it stops mattering much, but it currently forces straight lines. |

Net: an LP with hard-forbidden edges, an L1 terminal fit, a minimax fairness bound and
(optionally) an L1 historic-preference term is ~90 % of the docx model, runs on `lpSolve`, and
stays diffable against the original.

### 6.3 One thing the shipped LP has that the docx does not

The **temporal-smoothing constraint**. It is the only mechanism in either model that says
anything about the *interior* of the trajectory once shape is inert. Do not delete it outright;
keep it available at low weight (1, its documented default) as a tie-breaker among the many
trajectories that satisfy everything else. With the §4.1 fix in place, `μ_smooth = 1` and
`μ_smooth = 100` give identical solutions on the decadal grid, so the setting is currently free.


---

## 7. Proposed integration as `07-transition-rates.qmd`

### 7.1 File layout

```
2026-05-ssp-ch/
  07-transition-rates.qmd          # literate step: reads demand, calls the solver, commits
  R/
    trans-rate-solver.R            # the ported LP + precheck; sourced, not packaged
```

`_quarto.yml` sets `execute-dir: project`, so the chunk does
`source("2026-05-ssp-ch/R/trans-rate-solver.R")` — same working directory as
`evoland_db$new(path = "ssp-ch.evolanddb")`. Putting it under `R/` keeps it out of
`execute-all.sh`'s `0*.qmd` glob. `air.toml` (line width 100) and `.lintr` apply as usual;
`object_name_linter` is disabled, so `snake_case` free functions are fine.

Experiment-local, per the decision already taken. Upstreaming stays on #32.

### 7.2 Callable signature

Three functions, deliberately separable so the precheck can run without the solver:

```r
#' Derive per-edge min/max transition rate bounds from observed history.
#'
#' Rates are annualised before min/max is taken, then re-inflated to `step_years`,
#' because the observed intervals are 12/12/9 y and the future steps are 10 y.
#' Edges absent from `obs_rates` in a given period count as rate 0 for that period,
#' NOT as missing -- otherwise `min_rate` is biased upward.
trans_rate_bounds <- function(
  obs_rates,        # trans_rates_t from db$get_obs_trans_rates()
  periods,          # periods_t, for the true interval lengths
  trans_meta,       # trans_meta_t, for the full edge set + is_viable
  step_years,       # numeric(1) or numeric(n_steps): target step length(s)
  include_persistence = TRUE
)
# -> data.table(id_lulc_anterior, id_lulc_posterior, min_rate, max_rate, is_viable)

#' Reachability precheck. Max/min achievable area per class at the horizon under
#' mass balance + hard rate bounds. Pure LP; no target needed.
trans_rate_reachability <- function(
  init_area,        # data.table(id_lulc, area)
  bounds,
  n_steps,
  monotone_sign = NULL
)
# -> data.table(id_lulc, area_init, area_min, area_max)

#' Solve for per-transition flows reaching class-area targets.
solve_trans_rates <- function(
  init_area,        # data.table(id_lulc, area)          -- cells at the anchor period
  targets,          # data.table(id_lulc, area)          -- cells at the horizon
  shapes,           # data.table(id_lulc, shape)         -- the 5 chosen_shape values
  bounds,           # from trans_rate_bounds()
  step_years,       # numeric(n_steps) -- ALWAYS a vector; validated, see S4.3
  lambda_bounds  = 0.1,     # rate-bound violation
  mu_shape       = 15,      # curvature preference
  mu_smooth      = 1,       # 2nd-difference; the DOCUMENTED default, not the call site's 100
  mu_target      = 1e3,     # NEW: L1 terminal fit (S4.4)
  margin         = 0.01,
  terminal_band  = 0.01,
  fairness       = TRUE,    # NEW: minimax bound on worst per-class shape distortion
  forbid_non_viable = TRUE  # NEW: hard x = 0 (S4.2)
)
# -> list(status, objective, areas [L x (n_steps+1)], flows [L x L x n_steps],
#         rates [L x L x n_steps], diagnostics = list(bound_violation_by_edge,
#         target_error_by_class, forbidden_leakage, reachability))
```

Notes on the signature:

- Everything is keyed by `id_lulc` / `id_trans`, never by class *name* — the crosswalk to the
  spreadsheet's names happens once, in the `.qmd`, where it is visible.
- `step_years` is a vector and is `stopifnot`-checked. §4.3.
- Returns *diagnostics*, not just the solution. Given §3.4/§3.5, a solver that does not tell
  you how far outside history it went is actively misleading.

### 7.3 Reading the demand and the crosswalk

The demand is 50 rows and never changes. I would **embed it in the `.qmd` as a
`data.table::rowwiseDT`** rather than reading the sibling repo's `.xlsx`:

- `NCCS-SSP-scenarios/` is a read-only reference checkout, not a declared dependency of
  `evoland-experiments`; a relative `../NCCS-SSP-scenarios/Tools/...` path makes the pipeline
  unrunnable for anyone who cloned only this repo.
- It matches the repo's existing idiom — `00-setup-db.qmd` already builds `trajectory_specs`
  and `ssp_specs` as `rowwiseDT`.
- `air.toml` already has `skip = ["tribble", "rowwiseDT"]`, so the aligned literal survives
  formatting.
- Record the source path + md5 in `reporting_t`, as `01-ingest-lulc-data.qmd` does for the
  Arealstatistik download.

Two data traps to handle explicitly, in the open:

```r
# 1. one row reads "delayed decline" (lowercase) -- SSP0 / Open_Forest
shape := factor(tolower(trimws(shape)), levels = ..., labels = ...)

# 2. LULC crosswalk is 1:1 by lowercased name EXCEPT Int_AG
lulc_crosswalk <- data.table::rowwiseDT(
  ssp_ch_name =,  id_lulc_name =,
  "Int_AG",       "arable",          # <- the only non-obvious one
  "Perm_crops",   "perm_crops",
  "Grassland",    "grassland",
  "Shrubland",    "shrubland",
  "Static",       "static",
  "Closed_Forest","closed_forest",
  "Open_Forest",  "open_forest",
  "Urban",        "urban",
  "Alp_Past",     "alp_past",
  "Glacier",      "glacier"
)
stopifnot(setequal(lulc_crosswalk$id_lulc_name, db$lulc_meta_t$name))
```

**Normalise to shares at ingest** (§5.2a). The spreadsheet's grid has 4,129,078 cells; ours
will not. Store `share = area / sum(area)` per (scenario, horizon) and rehydrate against our
own observed total:

```r
init_area <- db$get_query(glue::glue(
  "select id_lulc, count(*)::int as area
   from {db$get_read_expr('lulc_data_t')}
   where id_period = {anchor_period}
   group by id_lulc"
))
target_cells <- share_target * sum(init_area$area)
```

The demand's own `init_area` column is then used only as a **check** — plot/report our
observed 2018 shares against the spreadsheet's, and flag classes where the re-derivation from
NOAS04 disagrees with the original aggregation. That comparison is worth having in the report
regardless; it is the cheapest possible validation of `01-ingest-lulc-data.qmd`.

### 7.4 Deriving the historic bounds from `db$get_obs_trans_rates()`

```r
db$id_run <- 0L
obs_rates <- db$get_obs_trans_rates()      # id_run, id_period, id_trans, count, rate
db$trans_rates_t <- obs_rates              # commit the observed rows on the BASE run
bounds <- trans_rate_bounds(obs_rates, db$periods_t, db$trans_meta_t, step_years = 10)
```

Four things `trans_rate_bounds()` must get right, all of which the original gets wrong or
sidesteps:

1. **Absent rows are zeros.** `get_obs_trans_rates()` only emits `(id_period, id_trans)`
   combinations that actually occurred. Taking `min()` over the present rows would give a
   spuriously high floor for any edge that was absent in one period. Complete the grid against
   `trans_meta_t × observed periods` and fill 0.
2. **Persistence is not in `trans_meta_t`.** `create_trans_meta_t()` filters
   `id_lulc_anterior != id_lulc_posterior` (`trans_meta_t.R:82`), so there is no `id_trans`
   for `i→i`. The LP needs the diagonal, so reconstruct it as
   `persist[i,p] = 1 − Σ_j rate[i→j,p]` over **all** transitions (viable *and* non-viable —
   they all really happened), and drop it again on output.
3. **Annualise before min/max.** Interval lengths come from
   `periods_t[, as.numeric(end_date - start_date)/365.25]`, and the observed spacing is
   12/12/9 y. `r_ann = 1 − (1 − r)^(1/Δt)`; re-inflate with `1 − (1 − r_ann)^step_years`.
   §4.5.
4. **`is_viable` defines the edge set.** Carry it through so `solve_trans_rates()` can hard-zero
   the rest (§4.2). Do *not* infer forbiddenness from `max_rate == 0`.

### 7.5 The decadal resampling

This is the fiddliest part and deserves explicit prose in the `.qmd`.

**The problem.** Rates in `trans_rates_t` are indexed by `id_period`, and
`get_obs_trans_rates()` defines the rate at period *p* as the transition from the state at
*p−1* to the state at *p*. So the LP's time grid is a sequence of **class-area states**, one
per period, and what the solver needs is the calendar year each state sits at.

- Observed states are pinned to survey years: **1985, 1997, 2009, 2018** (periods 1–4).
  Note these are *not* the period midpoints or boundaries, and the spacing is **12, 12, 9**.
- Extrapolated periods 5–8 have no survey, so the anchor is a convention.

**Recommendation.** Anchor future states by continuing whole periods from the last
observation:

```
id_period      4      5      6      7      8
anchor year  2018   2028   2038   2048   2058
step_years          10     10     10     10
```

Then interpolate the demand curve at 2058. The demand has anchors at (init, 2060, 2100); since
`init_area` equals the 2018 survey exactly (verified in §3.0), read those anchors as
**(2018, 2060, 2100)** and take

```
target(2058) = init + (target_2060 − init) · (2058 − 2018) / (2060 − 2018)
             = init + 0.952 · Δ_2060
```

Linear interpolation is safe here because both endpoints are exactly mass-balanced (§3.1), so
the interpolated vector is too — no renormalisation, no drift.

**The alternative** — anchoring on period *end* years (2024, 2034, 2044, 2054, 2064), with
`step_years = c(6, 10, 10, 10, 10)` from the 2018 observation — is equally defensible and is
what the "2060 falls inside period 8" framing suggests. It requires extrapolating 4 years into
the 2060→2100 segment for the terminal target. The solver supports either; **pick one, write
down why, and put the choice in a single named variable** at the top of the chunk. The two
differ by roughly 10 % of the 2018→2060 change, which is well inside the ±1 % terminal band's
own slop (§4.4) — so this is a documentation problem, not an accuracy problem.

**What is emphatically *not* needed** is a two-stage "solve at 5-yearly, then aggregate to
decadal". Aggregating flows across sub-steps is not a matter of summing them (the composition
of two transition matrices is not their sum), and the solver takes arbitrary `Time_steps` /
`Step_length` anyway. Solve directly on the grid you will simulate on.

**Horizon note.** The pipeline currently ends at period 8 (`end_extrapolated = "2060-01-01"`),
so only the 2060 target is in play; `final_area_2100` is unused until the horizon is extended.
Keep it in the ingested table anyway — it costs nothing and the 2100 anchor is what makes the
interpolation to 2058 well-posed.

### 7.6 Interaction with `runs_t`

`runs_t` is ordered `base → climate → SSP`, so the SSP leaves are
`id_run = climate_id_run * 10 + ssp_idx` and there are **5 climate runs × 5 SSPs = 25 leaves**,
but only **5 distinct demand solutions** (the LP inputs are the observed initial state and the
per-SSP targets; nothing in it is climate-dependent).

Because SSP sits *below* climate in the tree, there is no shared "SSP node" to hang the rates
on. So: solve 5 times, write the result to all 5 leaves of each SSP.

```r
ssp_runs <- db$runs_t[!is.na(ssp)]

solutions <- lapply(unique(ssp_runs$ssp), \(this_ssp) {
  solve_trans_rates(
    init_area = init_area,
    targets   = targets[ssp == this_ssp],
    shapes    = shapes[ssp == this_ssp],
    bounds    = bounds,
    step_years = rep(10, 4)
  )
}) |> setNames(unique(ssp_runs$ssp))

for (i in seq_len(nrow(ssp_runs))) {
  db$id_run <- ssp_runs$id_run[i]
  db$trans_rates_t <- as_trans_rates_t(
    solutions[[ssp_runs$ssp[i]]] |>
      trans_rates_long(id_run = ssp_runs$id_run[i], id_periods = 5:8, trans_meta = db$trans_meta_t)
  )
}
```

Volume: 21 viable transitions × 4 periods × 25 leaves ≈ **2,100 rows**. That is exactly the
trade the ordering rationale in `00-setup-db.qmd` anticipates ("the SSP-specific tables …
are negligible by comparison"); this step is the first concrete instance of it, and it is
worth one sentence in the prose confirming the cost is real and tiny.

Inheritance works out cleanly: `get_evoland_db_read_expr()` resolves per `id_period` slice, so
the observed rates committed at `id_run = 0` for periods 2–4 remain visible from an SSP leaf
that only stores periods 5–8. Do **not** write the observed rows onto the leaves.

### 7.7 What lands in `trans_rates_t`

For each `(id_run, id_period ∈ 5:8, id_trans)` on viable, off-diagonal edges:

```
rate  = flows[i,j,t] / areas[i,t]      # the LP's own predicted source area
count = round(flows[i,j,t])            # cells; the ACTUAL flow, not coord_count * rate
```

Populate **both** columns and make them mutually consistent (`count ≈ rate × areas[i,t]`).
That closes the §3.6-(4) dimensional bug for anything produced by this step, and gives
downstream code a choice without a conversion.

Drop the diagonal, drop non-viable edges (they should be exactly 0 once §4.2 is fixed — assert
it rather than filtering silently), and assert `rate ∈ [0, 1]` and `Σ_j rate[i→j] ≤ 1` before
committing.


---

## 8. Risks, and how to handle infeasibility

### 8.1 The targets are outside the historic envelope, and that is not a bug

§3.4: 24 of 50 (SSP × class) targets are unreachable under hard historic max rates, several by
4–5×; glacier by 1.81× in **all five** scenarios. This is not a data-entry problem — the SSPs
are normative visions, and "Switzerland deglaciates faster than 1985–2018 suggests" is a claim
about climate, not a violation of physics.

So issue #32's framing — *"an infeasible expert target should fail loudly with 'class X cannot
exceed Y under observed transition bounds'"* — **would abort every scenario**. The precheck's
job is not to gate; it is to **quantify and report**. Concretely, `07` should emit a table like

```
Scenario  class          asked      achievable   ratio   verdict
SSP4      alp_past      -144,360     -31,840      4.53x   far outside history
SSP0      arable        +150,930     +36,362      4.15x   far outside history
SSP5      static        +121,926     +25,734      4.74x   far outside history
*         glacier        -40,066     -22,159      1.81x   outside history (all SSPs)
SSP3      closed_forest +125,254    +102,435      1.22x   near the edge
```

and fail only on a configurable threshold (say `ratio > 10`, or negative target, or
`Σ target ≠ region_area`). Everything below that is a **finding to report in the write-up**,
because it is genuinely the most interesting quantitative statement this step produces: *how
far each SSP departs from observed Swiss land-use dynamics.*

### 8.2 The 42–63 % out-of-envelope flow will distort allocation

§3.5. Downstream, `adjusted_trans_pot_v()` scales potentials by `rate / mean_value` — an
**unbounded** multiplier. When the LP demands a rate several times the historically observed
one, the scaled probabilities exceed 1 for many cells, the row-closure branch

```sql
when sum(scaled_value) over (partition by id_coord) > 1.0
then scaled_value / sum(scaled_value) over (partition by id_coord)
```

kicks in, and the *relative* ordering within a cell is preserved but the *absolute* quantity is
clipped. uPAM's quota (`rt * m0`) then cannot be met, the pool empties, and the loop exits
early. The result is a scenario that under-delivers its own demand, silently.

Mitigations, in order of preference:
1. Fix §4.2 (hard-forbid non-viable edges) — removes the worst offenders outright.
2. Add the L1 historic-preference term (§6.2) — keeps flows near the observed pattern where
   the target does not force otherwise.
3. **Report the realised areas after allocation and compare against the LP's `areas` matrix.**
   This is the only way to notice the failure. It belongs in `09d-report.qmd`, but the LP's
   predicted trajectory has to be *persisted* for it to be possible — consider writing the
   solved `area[l,t]` somewhere, not just the rates.
4. Consider clipping `rate` at `min(rate, some multiple of historic max)` before commit, and
   reporting the clip. Refusing to promise what the allocator cannot deliver is better than
   promising it.

### 8.3 Small-class conditioning

`weight_i = 1/max(init_area, 1e-6)` means `glacier` (102,992 cells) gets ~11× the penalty
weight of `closed_forest` (1,112,032). That is deliberate and good. But it also means a class
that becomes small *during* the horizon is progressively over-weighted relative to its
remaining area, since the weight is fixed at `init_area`. With glacier declining ~40 % over
the horizon this is a real, if second-order, effect. The docx's minimax fairness variable is
a cleaner answer; it is LP-representable (§6.2).

### 8.4 Degeneracy and solution non-uniqueness

The LP has many optimal solutions (§3.3: with the shape term fixed, `μ_smooth = 1` and
`μ_smooth = 100` give *identical* answers, which means the smoothing term is not binding at
all). `lpSolve` returns one vertex with no warning about alternates. Two consequences:
- Do not read fine structure in the flow pattern as meaningful. Only the class trajectories
  and the aggregate churn are well-determined.
- A tiny regularisation term (the docx's ridge, or an L1 surrogate pulling flows toward the
  historic pattern) would make the answer *reproducible under solver changes*. That is a
  better argument for the historic-preference term than plausibility is.

### 8.5 Version and dependency risks

- `lpSolve` is **not** in `rproject.toml`. It has to be added. It is tiny, ancient, C-only, no
  transitive dependencies.
- `lpSolve::lp()` silently swallows `NA` in `const.mat` (§4.3, verified). Validate inputs
  before calling; there is no error to catch.
- Do not add `quadprog`/`osqp`/`ROI` for this. §6.2 shows the LP surrogates cover it.

### 8.6 Ordering risk within the pipeline

`07` needs `trans_meta_t` (from `04`) and `lulc_data_t` (from `01`) but **not** `06`'s fitted
models — the rates are demand-side only. It can therefore be developed and tested before `06`
is finished, which is worth knowing given `06` is still open in `TODO.md`. The dependency in
the other direction is real though: `08`/`09` cannot run without `07`.

---

## 9. What I could not determine

1. **The Word document itself.** No `.docx` exists on this machine and I could not locate the
   file referenced at the end of #20. Everything in §6's right-hand column is from the issue
   text — second-hand. Specifically unverified: the exact form of the quadratic terminal-fit
   and historic-preference terms; whether the docx keeps min/max rate bounds in the *main*
   model or only in `compute_final_bounds`; whether it supports irregular step lengths at all;
   and the precise definition of "relative-width guard band". Before committing to §6's
   recommendation, someone should read the docx and check the difference table.
2. **The real observed rates for *this* pipeline.** No `ssp-ch.evolanddb` exists yet, so
   `db$get_obs_trans_rates()` could not be run. §3 uses the original study's calibration rates
   from `Transition_Tables.xlsx` (21 edges) as a stand-in. Evoland's own `is_viable` set will
   differ — `04-viable-transition-identification.qmd` uses `min_cardinality_abs = 1000` and
   excludes `static` as an *anterior* class, which is a different rule from the original's
   `Model_lookup.xlsx` list. **A broader viable set would widen the reachable band in §3.4 and
   could change the infeasibility counts substantially.** Re-run the precheck against the real
   `trans_meta_t` before quoting those numbers anywhere.
   (Note that excluding `static` as anterior is *also* what makes `static → …` flows in §3.5
   pure leakage: they can never be allocated.)
3. **The original's `trans_rates_table_calibration_periods_SS.csv`** is not in the repo, so I
   could not verify that sheet `01_extrapolated_trans_rates` reproduces it exactly (the
   persistence rows in particular were reconstructed, not read).
4. **Whether the original study ran with the §4.1 bug.** The code in
   `NCCS-SSP-scenarios/Scripts/Functions/` has it. Whether the published Zenodo outputs were
   produced by this exact revision, I cannot tell from here. If they were, then "reproduce the
   shipped LP" has an awkward corollary: reproducing it *faithfully* means reproducing the bug.
   My recommendation is to **implement both, behind a flag**, run them side by side once, and
   report the difference (§4.1 table) — then use the fixed version for everything downstream.
   The comparison is ~10 lines and settles the replication question honestly.
5. **Whether `chosen_shape` should be rescued at all.** Given it has never had any effect
   (§3.3), the published SSP-CH trajectories are straight lines between 2020 and 2060
   regardless of the shape column. Making shapes bite would therefore *change* the replication
   target. That is a scientific call for the author of the original elicitation, not a
   technical one.
6. **`alloc_dinamica`'s consumption path** I only skimmed; it writes `trans_rates_dinamica_v`
   to CSV and shells out. The `is_viable` filter is present there (`evoland_db_views.R:142`),
   so the leakage in §3.5 is dropped rather than allocated — but I did not verify what Dinamica
   does when a transition in the potential maps has no row in the rate table.

---

## 10. Suggested order of work

1. Add `lpSolve` to `rproject.toml`.
2. Port the solver into `2026-05-ssp-ch/R/trans-rate-solver.R` **with §4.1–§4.3 fixed** and a
   `shipped_bugs = FALSE` flag that restores them for the replication diff.
3. Implement `trans_rate_reachability()` first — it is ~60 lines, needs no targets, and
   answers the most interesting question in the step.
4. Write `07-transition-rates.qmd` around the embedded demand table, the crosswalk, and the
   share round-trip.
5. Report the reachability table (§8.1) and the out-of-envelope flow table (§3.5) in the step's
   own output. These are results, not diagnostics.
6. Only then consider the L1 terminal-fit / minimax / historic-preference additions (§6.2),
   with the plain port as the baseline to diff against.
