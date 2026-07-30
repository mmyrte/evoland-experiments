# Notes — transition-rate solver as step `07-transition-rates.qmd`

Working notes for the design decision tracked in
[ethzplus/evoland-plus#32](https://github.com/ethzplus/evoland-plus/issues/32).
Analysis + recommendation, **not** production code.

Status: living document, written incrementally.

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
   The right answer is therefore "all three, at different layers", and `trans_rates_t` already
   carries both `count` and `rate`, so nothing is blocked.

4. **Recommendation: reproduce the shipped LP**, port it faithfully with the bugs fixed (see
   §4), add the docx's `compute_final_bounds` precheck LP (which *is* an LP and needs no new
   dependency), and defer the rest of the QP formulation. Rationale in §5.

_(Sections below get filled in as the analysis proceeds; §1–2 are settled, §3+ in progress.)_

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
- **`μ_smooth = 100` swamps `μ_shape = 15`.** With the ~6.7× weight ratio the second-difference
  penalty (which pulls trajectories toward straight lines) dominates the curvature preference
  (which is the entire point of `chosen_shape`). The docstring admits this: *"Note the
  behaviour of this constraint tends to override the effect of the shape-based constraint."*
  So in the shipped configuration `chosen_shape` is nearly decorative. Confirmed numerically in
  §3.

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

1. **No mass balance in either direction.** Nothing forces `Σ_j rate[i→j] <= 1` (so a class can
   be asked to export more area than it has), and nothing links class *i*'s outflow to any other
   class's inflow. Total landscape area is not conserved except by accident, because the
   allocator caps outflow per cell — i.e. the conservation is enforced by *truncation
   downstream*, silently.
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
