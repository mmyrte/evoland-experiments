# The transition-rate solver, re-implemented in evoland-plus

Notes for the authors of
`NCCS-SSP-scenarios/Scripts/Functions/lulcc.simulationtransitionratesolver.R`.

Your solver has been re-implemented in the evoland-plus package as the R6 class
[`trans_rate_lp`](https://github.com/ethzplus/evoland-plus/blob/main/R/trans_rates_lp.R),
tracked in [ethzplus/evoland-plus#32](https://github.com/ethzplus/evoland-plus/issues/32).
This document says what the re-implementation does, where it departs from your code and
from the quadratic formulation in `Future_transitions_doc.docx`, and why.

Everything numeric below was executed, either on the real SSP-CH inputs
(`NCCS_simulation_LULC_areas.xlsx`, `Transition_Tables.xlsx`) or on the ported solver.

---

## 0. TL;DR

1. **The model is yours.** Same decision variables, same mass balance, same soft rate
   bounds, same shape and smoothing mechanisms, same monotonicity rule. Nothing was
   re-derived; the structure was transcribed and then checked against the docx.

2. **Three defects in the shipped code are not reproduced** (§3). The worst,
   `build_diff_row()` assigning over its own coefficient, makes the entire `chosen_shape`
   mechanism inert and inflates the objective by ~3,700×, so the solver was effectively not
   minimising rate-bound violations at all. Fixing that one line cuts out-of-envelope
   transition churn by up to 35 %.

3. **The docx is a quadratic program; evoland solves an LP.** Every quadratic penalty was
   replaced by its absolute-value equivalent, which is linear and needs no new solver
   dependency (§4.2, and §2.4 for what "L1" means here). Only the ridge term has no linear
   counterpart and was dropped (§4.3).

4. **Units are now explicit at every layer** (§1): targets may be stated as shares, the
   program is solved in shares, everything reported is in cells, and what reaches the
   allocator is a rate. Historic rates are annualised before being compared, because the
   observed periods are 12/12/9 years long and the future ones are 10.

5. **Two constraints are new** (§5): a hard `rate_limits` block that makes the reachability
   precheck a subset of the same program rather than a second implementation of it, and
   `shape_strictness`, which is what makes an elicited shape actually bind.

6. **The most useful output is still the reachability precheck** (§6). 24 of 50
   (scenario × class) targets are unreachable under observed transition rates, glacier by
   1.81× in all five scenarios, `Static` in SSP3 by an infinite margin because it has no
   outflow transition at all.

7. **Three deviations need your ruling** (§8): what the fairness variable should bound,
   whether flat classes should be pinned, and whether `chosen_shape` should be rescued at
   all.

---

## 1. Units

The single most common source of confusion in the original code is that four different
quantities are all called "rate" or "area" somewhere. The re-implementation fixes each to
one layer.

| layer | unit | why |
| --- | --- | --- |
| scenario target, as supplied | **share of landscape** or cells | a share is grid-independent; cells are only meaningful on the grid they were elicited on |
| program internals | **share of landscape** | as in the docx; identical to the absolute program up to a scalar, but spans six fewer orders of magnitude |
| everything reported | **cells** | "a shortfall of 112,880 cells" is actionable, its share-space equivalent is not |
| `trans_rates_t.rate` | **rate of the anterior class** | what `adjusted_trans_pot_v()` and the allocators consume |
| `trans_rates_t.count` | **cells** | the actual flow, not `total_cells × rate` |

Two consequences worth stating plainly.

**Targets stated in cells do not transfer between grids.** The SSP-CH demand is stated on a
4,129,078-cell grid. evoland's own `coords_t` is a re-derivation from NOAS04 and will not
have the same total. `trans_rate_lp$new()` therefore accepts `targets` with either an `area`
column (checked against the grid it was given) or a `share` column (rehydrated against it).
Because all five scenarios and both horizons already sum to exactly 4,129,078, the
share round trip is exact.

**Rates only compose within one period length.** `Simulation_trans_tables_prep.R` takes
min/max over the columns `1985_1997`, `1997_2009`, `2009_2018` — intervals of 12, 12 and 9
years — and applies them unchanged as bounds on 5-year steps. That inflates the permitted
per-step churn by roughly 2× and makes the 9-year interval systematically the "low" one, so
`minRate` is biased down. `trans_rate_bounds()` annualises first,
`r_ann = 1 − (1 − r)^(1/Δt)`, takes min/max/mean on the annual scale, then re-inflates to
the length of an extrapolated period. Persistence is the complement of the total outflow and
compounds directly, `p_ann = p^(1/Δt)`. All numbers in §6 use this.

---

## 2. The program

Built by `trans_rate_lp`, one constraint block at a time. Every block has an `add_` method
of the same name, and the rows stay tagged with it, so a program can be assembled once and
solved over a subset of its blocks.

### 2.1 Variables

All non-negative, all in shares. `lpSolve::lp()` has no notion of variable bounds, which is
why any quantity that may take either sign is split into two variables.

| block | count | your name | meaning |
| --- | --- | --- | --- |
| `flow` | `L²·T` | `x[i,j,t]` | share moving from class `i` to `j` during a period |
| `area` | `L·(T+1)` | `area[l,t]` | share of class `l` at a state |
| `rate_lower`, `rate_upper` | `L²·T` each | `devLower`, `devUpper` | rate-bound violation |
| `shape` | `L·(T−1)` | `shapeSlack` | curvature violation |
| `smoothness` | `L·(T−1)` | `smoothSlack` | second-difference violation |
| `target_over`, `target_under` | `L` each | — | terminal fit, either side |
| `historic` | `L²·T` | — | distance from the historic outflow pattern |
| `fairness` | 1 | — | worst per-class violation |

Time is indexed by `id_period`, not by a step counter: the flows of period `p` take the
landscape from its state at `p−1` to its state at `p`, which is already the convention in
`trans_rates_t`. Period lengths come from `periods_t`, one per period, never a scalar.

### 2.2 Constraint blocks

| block | statement | source |
| --- | --- | --- |
| `initial` | `area[l, anchor] = observed share` | C1 / docx §1.1 |
| `conservation` | `Σ_l area[l,p] = 1` | C2 / docx §1.2 |
| `closure` | `Σ_j x[i,j,p] = area[i,p−1]`, `area[j,p] = Σ_i x[i,j,p]` | C3, C4 / docx §2 |
| `forbidden` | `x[i,j,p] = 0` on non-viable transitions | intent of C5a / docx §3.2 |
| `rate_limits` | `x[i,j,p] ≤ r_max·area[i,p−1]`, hard | docx §12, new as a block |
| `rate_bounds` | `(r_min−m)·area ≤ x ≤ (r_max+m)·area`, with slack | C5a, C5b / docx §3.1 |
| `historic` | `\|x − p̂·area\| ≤ slack` | docx §4 |
| `target` | `area[l,T] − over + under = target`, plus optional hard band | docx §6 |
| `monotonicity` | `area[l,p] ≥ area[l,p−1]` or `≤`, from `sign(target − init)` | C7 / docx §7 |
| `shape` | one-sided curvature per `chosen_shape` | C8 / docx §8 |
| `smoothness` | `\|area[p+1] − 2·area[p] + area[p−1]\| ≤ slack` | C9, absent from the docx |
| `fairness` | `z ≥ w_l · Σ violation of class l` | docx §9, see §8 |

The curvature row is the one worth checking line by line, because it is where the shipped
code fails. For a class `l` and a pair of consecutive periods, with
`ratio = Δt_p / Δt_next`:

```
D = −area[l, p−1] + (1 + ratio)·area[l, p] − ratio·area[l, p+1]
```

which is exactly `Δt_p · (d1 − d2)` in the notation of docx §8.1. Concave shapes (*instant
growth*, *delayed decline*) ask for `D ≥ 0`, convex ones (*delayed growth*, *instant
decline*) for `D ≤ 0`, and *constant change* for both, i.e. `|D| ≤ slack`. That reproduces
docx §8.2–§8.4 and the intent of `build_diff_row()`. Note the slack is in units of
`Δt_p · slope` rather than of slope, exactly as in your code — it rescales `mu_shape`, and
nothing else.

### 2.3 Objective

```
min  λ_bounds  Σ w_i (rate_lower + rate_upper)      λ_bounds = 0.1
   + μ_shape   Σ w_l  shape                          μ_shape  = 15
   + μ_smooth  Σ w_l  smoothness                     μ_smooth = 1
   + μ_target  Σ w_l (target_over + target_under)    μ_target = 1e3   (new)
   + μ_historic Σ w_i historic                       μ_historic = 0   (new, off)
   + fair_weight · fairness                                           (new)

with  w_l = 1 / max(init_share_l, 1e-9)
```

The weights follow your code, not the docx: `w_l = 1/init_share` per source class, where the
docx has `dev_weight(i,j)` per transition for the rate slacks and `1/sqrt(init_frac)` for the
shape slacks. `μ_smooth` is the value your roxygen documents as the default (1), not the 100
at the call site in `Simulation_trans_tables_prep.R`.

### 2.4 What "L1" means in this document

The docx and your code do not use the term, so: an **L1 penalty** is one that charges the
*absolute value* of a deviation, `Σ |e|`, where the docx charges its *square*, `Σ e²`.

An absolute value is not itself linear, but minimising one is, and that is the whole trick.
Write the deviation as the difference of two non-negative variables, `e = u⁺ − u⁻`, and
charge `u⁺ + u⁻`. At the optimum only one of the two is non-zero, so what is charged is
`|e|`. The result is an ordinary LP row and an ordinary LP objective — no quadratic solver.

The practical difference from a square: a squared penalty spreads a given total error over
many small deviations, whereas an absolute one is indifferent between spreading and
concentrating, and so tends to leave most deviations at exactly zero and a few large. For
the terminal fit this is a feature — classes that can hit their target do so exactly, and
the ones that cannot carry the whole shortfall visibly. For the historic-preference term it
means flows sit exactly on the historic pattern wherever the target does not force
otherwise.

The same reasoning covers the fairness variable: a **minimax** — minimise the largest of
several linear expressions — is an LP, because "the largest" can be written as one extra
variable `z` with one `z ≥ ...` row per expression.

---

## 3. Defects in the shipped solver, not reproduced

All four were found by running your code on the real inputs.

### 3.1 🔴 `build_diff_row()` assigns over its own coefficient

```r
row_[idx_area(l_i, t1)]     <-  1      # (a)
row_[idx_area(l_i, t1 - 1)] <- -1
row_[idx_area(l_i, t2)]     <- -ratio
row_[idx_area(l_i, t2 - 1)] <-  ratio  # (b) t2 - 1 == t1, so this overwrites (a)
```

The function is only ever called as `build_diff_row(l_i, t_i, t_i + 1, ratio)`, so `t2 − 1`
and `t1` are the same index and line (b) replaces line (a). These are `<-`, not `+=`. The
row that is built is

```
ratio·A[t1] − A[t1−1] − ratio·A[t1+1]
```

which for `ratio = 1` evaluates to `−A[t1]` on any smooth trajectory: an *area*, not a
curvature. Every shape slack is therefore driven to roughly the class's own area, and the
shape term contributes a near-constant `mu_shape` per row that the LP cannot influence.

| scenario | objective as shipped | fixed | rate-bound violation, cells | change |
| --- | --- | --- | --- | --- |
| SSP0 | 314.494 | **0.032** | 125,267 → 123,367 | −1.5 % |
| SSP1 | 393.581 | **0.030** | 103,926 → 78,616 | **−24.4 %** |
| SSP3 | 307.038 | **0.033** | 146,315 → 94,438 | **−35.5 %** |
| SSP4 | 305.589 | **0.082** | 263,541 → 228,998 | −13.1 % |
| SSP5 | 395.159 | **0.055** | 397,333 → 366,817 | −7.7 % |

The objective drops by four orders of magnitude. That is the real damage: the bogus shape
term is ~3,700× larger than the rate-bound term, so violations of the historic bounds were
rounding error in the objective and the solver was not meaningfully minimising them.

The docstring blames the temporal-smoothing constraint for the shape term having no effect
(*"tends to override the effect of the shape-based constraint"*). That is a misdiagnosis.
Sweeping `mu_temporal_smoothness` over 100, 10, 1, 0.1 leaves the total shape slack pinned
at ~9.0e6 in every case — the signature of a constraint that cannot be satisfied, not of one
that is outcompeted.

### 3.2 🔴 "Forbidden" transitions are merely cheap

```r
# if r_max == 0 i.e. transition is not allowed then set the upper bound to 0
if (r_max == 0) {
  row_up[idx_area(i_i, t_i)] <- 0      # -> row is:  x[i,j,t] - devUpper[i,j,t] <= 0
}
```

The comment says "not allowed"; the code says "allowed at the cheapest available penalty",
because `devUpper` is still in the row. docx §3.2 asks for `x[i,j,t] = 0`. Measured
consequence: 62,000–289,000 cells per scenario flow along transitions with no historic
support, including `static → arable`, `static → alp_past` and `closed_forest → static`.

This matters more in evoland than in your pipeline. `trans_pot_t` is only populated for
`trans_meta_t[is_viable == TRUE]`, and `adjusted_trans_pot_v()` joins potentials to rates, so
a rate on a non-viable transition has no potentials to scale and is silently dropped at
allocation time. Not a crash, not a warning: a scenario that quietly does not do what it
says. The re-implementation emits a hard `x = 0` row per non-viable transition and period.

Related, at your call site: `Simulation_trans_tables_prep.R` builds `viable_trans_rates`
(lines 151–161) and then passes the *unfiltered* `trans_rates` to
`lulcc.solvemultiplefutureperiods()` on line 174. The viability filter is computed and
discarded.

### 3.3 🟠 `Step_length` is documented as a vector and passed as a scalar

`lulcc.solvemultiplefutureperiods()` sets `Step_length <- 5` — a scalar, because it is also
fed to `seq()` — and passes it down to the solver, which indexes `Step_length[t_i]` for
`t_i = 1 … Num_steps−1`. For `Num_steps = 8` that yields `NA` for `t_i = 2..7`, so
`ratio = 5/NA = NA` and `NA`s land in the constraint matrix.

`lpSolve::lp()` swallows them silently: it returns `status = 0` and a *different* answer
(objective 399.31 against 393.58 with a proper vector), with no warning and no `NA` in the
output. There is no way to notice this from the outside. In evoland the period lengths come
from `periods_t`, one per period, and a missing one is an error.

### 3.4 🟠 The ±1 % terminal band is a bias, not a tolerance

There is no objective term rewarding proximity to the target, so the band is the only thing
pulling the solution towards it. The LP treats it as free real estate:

```
pct_err of solved terminal areas against targets
SSP0: -1.00 -1.00 +1.00 -1.00 -0.19 +1.00 +1.00 -1.00 -1.00 -1.00
SSP1: -1.00 +1.00 -0.97 -1.00 +1.00 +1.00 -1.00 -1.00 -1.00 -1.00
SSP4: -1.00 +1.00 -1.00 +1.00 +1.00 +0.50 -1.00 -1.00 -1.00 -1.00
```

Nearly every class parks exactly on a band edge. On a 1.1 Mcell class that is ±11,000 cells
of unremarked slop, biased to one side. Anyone reading "the solver hits the 2060 targets" is
being told something weaker than they think: it hits a 2 %-wide box around them.

The docx already fixes this with its quadratic terminal fit (§6.2). evoland uses the L1
version, `μ_target = 1e3`, and turns the hard band **off** by default — see §4.4.

### 3.5 🟡 Smaller things

- `subset_lulc_exp_areas$init_area[l_i]` assumes the data frame's row order matches
  `unique(subset_lulc_exp_areas$LULC)`. It happens to, in this spreadsheet. evoland joins on
  `id_lulc`.
- `lulcc.solvemultiplefutureperiods()` computes `NA_rows` and `Neg_zero_rows` from
  `Trans_area_extraps` and applies the same logical vector to `Trans_rate_extraps`, which
  only works while the two frames stay row-aligned; they are built by separate
  `pivot_wider()` + `arrange()` calls.

---

## 4. The quadratic formulation

`Future_transitions_doc.docx` specifies a QP. evoland solves an LP with `lpSolve`, a small
dependency-free package, and it is now a *suggested* dependency — most of evoland never
solves a program. Here is the accounting, section by section.

### 4.1 Ported unchanged

| docx | evoland |
| --- | --- |
| §1.1 initial shares, §1.2 conservation | `initial`, `conservation` |
| §2.1–2.3 flow conservation, non-negativity | `closure`; non-negativity is implicit in `lpSolve` |
| §3.1 rate bounds with slacks and margin | `rate_bounds` |
| §3.2 hard-forbidden transitions | `forbidden` |
| §6.1 guard band | `terminal_band`, off by default |
| §7.2–7.3 monotonicity | `monotonicity`, without the `inc`/`dec` variables — the inequality says the same thing with fewer columns; §7.4 differs, see §4.4 |
| §8.1–8.4 slopes and shape inequalities | `shape` |
| §12 precheck LP | `rate_limits` block + `$reachability`; bounds differ, see §4.4 |

### 4.2 Ported as L1 or minimax

| docx | quadratic form | evoland |
| --- | --- | --- |
| §6.2 terminal fit | `ρ Σ (area[i,T] − target)²` | `area − over + under = target`, charge `μ_target·w·(over + under)` |
| §4.1 historic preference | `η Σ (x − p̂·area)²` | `\|x − p̂·area\| ≤ historic`, charge `μ_historic·w·historic` |
| §9 fairness | `τ ≥ class_shape_sq`, quadratic inside | `z ≥ w_l · Σ violation`, linear inside — but over the rate-bound violation, not the shape distortion; see §8 |
| §3.3 rate-slack penalty | `λ Σ dev_weight (devU² + devL²)` | `λ Σ w_i (rate_upper + rate_lower)` |
| §8.6 shape penalty | `μ Σ w_i s²`, `w = 1/√init_frac` | `μ Σ w_l s`, `w = 1/init_share` |

`p̂` in the historic term is your row-normalised historic outflow pattern. Because the
persistence diagonal is reconstructed as `1 − Σ_j rate`, the row of observed rates already
sums to 1, so the historic mean rate *is* the row-normalised preference — no separate
normalisation step is needed.

### 4.3 Not implementable in an LP

- **§10 ridge regularisation.** `ε(Σ area² + Σ x²)` has no linear counterpart. Its purpose —
  making the objective strictly convex so the solution is unique — cannot be had from an LP,
  which will return one vertex of an optimal face without saying that others exist. The
  practical consequence is measured and real: solved flows move by a few percent between
  runs of equivalent objective while class trajectories and terminal areas do not. **Do not
  read fine structure in the flow pattern as meaningful.** The L1 historic-preference term
  is the closest available substitute, since it pins most flows to a definite value.
- **Quadratic weighting as such.** Whether a deviation is charged linearly or quadratically
  changes *which* optimum is chosen, not whether one exists (§2.4). Where that choice
  matters to you, it is called out above.

### 4.4 Deliberate deviations

- **§5 zero-history penalty is not implemented.** `κ_zero · Σ 1[zero_mask] · x` is linear and
  would port directly. It is absent because evoland takes the allowed transition set from
  `trans_meta_t$is_viable` and hard-forbids the rest, which is a stronger statement than a
  price. If you want zero-history transitions discouraged rather than banned, this is the
  cheapest thing on the list to add.
- **§3.2's "or if both bounds are zero" is not a forbidding rule.** evoland forbids on
  `is_viable`, not on `r_max == 0`. A viable transition that happened not to occur in any
  observed period keeps its soft bound and can carry up to `margin · area`.
- **§12's precheck uses the min bound and the margin; evoland's does not.** The precheck
  imposes hard maxima only, leaves persistence free and applies no margin. It answers the
  loosest honest question — "can this class get there at all, given that no transition has
  ever moved faster than it historically did?" — so a target it calls unreachable is
  unreachable under any tightening. Adding `r_min` would narrow the band and raise the
  unreachable count in §6.
- **§12's precheck applies monotonicity to the target class; evoland's applies none.** The
  extremes are monotone in practice, so this rarely binds.
- **§7's flat classes are not pinned.** The docx forces `area[i,t+1] = area[i,t]` for a class
  whose target equals its initial share within `mono_zero_tol`; your code, and evoland,
  leave such a class free to move and return. See §8.
- **The hard band is off by default.** With `forbid_non_viable = TRUE`, a hard ±1 % band on
  an out-of-reach target makes the program *infeasible* rather than close: on the SSP-CH
  demand, for SSP0, SSP3 and SSP4 (§6). The L1 fit lands as close as the viable transition
  set allows and reports the shortfall, which is the more useful failure mode. Set
  `terminal_band = 0.01` to get your behaviour back.

---

## 5. What is new

**`rate_limits`, and the precheck as a subset of the program.** The reachability LP of docx
§12 is not a separate model in evoland: it is the same object solved over the blocks
`initial`, `conservation`, `closure`, `forbidden`, `rate_limits`. `$blocks` records which
blocks enter a solve and which enter the precheck. This is worth the extra rows because it
makes it structurally impossible for the two programs to drift apart.

**Reachability at every period, not only the horizon.** `$reachability` returns
`area_min`/`area_max` per class *and* period, which shows where a trajectory runs out of
room rather than only whether it arrives.

**`shape_strictness`, which makes `chosen_shape` bind.** With the §3.1 bug fixed, shape slack
drops to exactly 0 and the trajectories are still straight lines. This is a second,
independent reason `chosen_shape` has no effect, and it survives the bug fix:

> A straight line satisfies all five shapes simultaneously. The constraints are weak
> one-sided inequalities on curvature — *instant* asks `d1 ≥ d2`, *delayed* asks `d1 ≤ d2`,
> *constant* asks `d1 = d2`. Zero curvature satisfies every one of them with equality, so it
> is always in the feasible set of every shape, and always the cheapest place to be.

This is a property of the formulation, not of the implementation: docx §8.2–§8.4 has it too.
`shape_strictness` requires a minimum curvature — a fraction of the class's mean per-step
change — which is what turns a shape from a preference the solver can ignore into one it
pays for ignoring. It defaults to 0, i.e. to your behaviour, because turning it on changes
the published trajectories (§8).

**Diagnostics as results.** `$diagnostics` returns the reachability verdict, the per-class
target error, the per-transition bound violations, a flow summary (how much flow sits above
the historic maximum, by how much it overshoots, how much lands on non-viable transitions),
and the realised curvature slack. A solver that does not report how far outside history it
went is actively misleading.

**The trajectory is recoverable from the rate table.** `trans_rate_areas()` replays a
`trans_rates_t` forward from an observed state. Round-tripping the solved rates through it
reproduces the program's own area trajectory to within a thousandth of a cell on the
4.1-Mcell SSP-CH grid, which is what makes realised-versus-predicted comparison possible
after allocation.

---

## 6. Reachability

Implemented as docx §12, run against the real inputs on the evoland decadal grid: states at
2018, 2028, 2038, 2048, 2058, targets interpolated onto 2058 from the (2018, 2060, 2100)
demand anchors.

Reachable band at 2058, as a percentage of 2018 area:

```
  Int_AG        -15.8 .. +9.4      Static         0.0 ..  +3.4
  Perm_crops    -31.6 .. +25.9     Closed_Forest  -4.3 ..  +9.2
  Grassland     -13.1 .. +12.3     Open_Forest   -43.1 .. +48.1
  Shrubland     -15.2 ..  +6.7     Urban          -1.5 .. +15.8
  Alp_Past       -6.7 ..  +2.1     Glacier       -21.5 ..   0.0
```

Against the interpolated targets, **24 of 50 (scenario × class) targets are unreachable**:

| scenario | unreachable | worst class | asked | achievable | ratio |
| --- | --- | --- | --- | --- | --- |
| SSP0 | 3 | `Int_AG` | +151,308 | +36,365 | **4.16×** |
| SSP1 | 5 | `Static` | +106,952 | +25,735 | **4.16×** |
| SSP3 | 4 | `Static` | −22,541 | 0 | **∞** |
| SSP4 | 5 | `Alp_Past` | −144,722 | −31,842 | **4.54×** |
| SSP5 | 7 | `Static` | +122,232 | +25,735 | **4.75×** |

`Glacier` is unreachable in all five scenarios by 1.81× (asked −40,167, achievable −22,160).
Note that the glacier areas were patched with Zekollari-modelled values without the rate
bounds being touched, so this gap was structural in the original study too — absorbed
silently by the soft slack.

`Static` in SSP3 is the interesting one. It is asked to *shrink*, and it has no outflow
transition at all: in the 21-transition set it appears only as a destination. No amount of
slack can drain it, which is why the ratio is infinite rather than merely large. The
re-implementation refuses to solve above `max_reachability_ratio` (default 10) and reports
everything below it — the SSPs are normative visions, and a precheck that failed on every
departure from observed dynamics would abort every scenario.

What that costs at solve time, per scenario, on the same inputs:

Total absolute terminal shortfall, in cells:

| configuration | SSP0 | SSP1 | SSP3 | SSP4 | SSP5 |
| --- | --- | --- | --- | --- | --- |
| soft bounds, hard ±1 % band | 0 | 0 | 0 | 0 | 0 |
| hard-forbidden transitions, hard ±1 % band | **infeasible** | 0 | **infeasible** | **infeasible** | 0 |
| hard-forbidden transitions, L1 fit (default) | 80,475 | 0 | 129,863 | 66,583 | 0 |

The first row reaches every target because the soft bounds let it invent the transitions it
needs — 149,000 to 346,000 cells of flow on transitions with no historic support. The last
row is what the viable transition set can actually deliver. Across all three configurations,
63 % to 98 % of the off-diagonal flow sits above the historic maximum for its transition;
that is the price of the targets, and it is now reported rather than absorbed.

### Downstream consequence worth designing for

`adjusted_trans_pot_v()` scales potentials by an **unbounded** `rate / mean_value`
multiplier. When the program demands several times the observed rate, scaled probabilities
exceed 1, the row-closure branch clips them, uPAM's quota `rt * m0` cannot be met, the pool
empties and the loop exits early. The scenario then under-delivers its own demand, silently.
Hard-forbidding non-viable transitions removes the worst of it; the L1 historic-preference
term (`mu_historic`) is the next lever; and comparing `$areas` against the realised areas
after allocation is the only way to notice what is left.

---

## 7. Running it

```r
bounds <- trans_rate_bounds(db$get_obs_trans_rates(), db$periods_t, db$trans_meta_t)

solver <- trans_rate_lp$new(
  lulc_data = db$lulc_data_t,      # the last observed period is the initial state
  bounds    = bounds,
  periods   = db$periods_t,        # extrapolated periods are the steps to solve for
  targets   = data.table(id_lulc = ..., share = ...),
  shapes    = data.table(id_lulc = ..., shape = ...)
)
solver$reachability                # answers without targets, too
solver$solve()
solver$diagnostics
solver$id_run <- 11L
db$trans_rates_t <- solver$trans_rates_t
```

`trans_rate_reachability()` and `solve_trans_rates()` are one-call wrappers over the same
class. `lpSolve` must be installed; it is a suggested dependency.

---

## 8. Three questions for you

1. **What should the fairness variable bound?** docx §9 bounds the worst per-class *shape*
   distortion. evoland bounds the worst per-class *rate-bound* violation instead, because
   shape slack is identically zero unless `shape_strictness > 0` (§5), which would make the
   docx's version vacuous by construction. Both are one row per class; say which you want
   and it is a one-line change.

2. **Should a flat class be pinned?** docx §7.4 forces `area[i,t+1] = area[i,t]` for a class
   whose target equals its initial area. Your code does not, and neither does evoland — such
   a class may move and come back. Pinning is stricter than monotonicity and would forbid a
   class from acting as a temporary reservoir, which is occasionally how mass balance is
   satisfied.

3. **Should `chosen_shape` be rescued at all?** It has never had any effect, in your code or
   in the docx formulation (§5). The published SSP-CH trajectories are therefore straight
   lines between 2020 and 2060 regardless of the shape column. Making shapes bind changes
   the replication target, so `shape_strictness` defaults to 0. Whether the elicited shapes
   were meant to be binding is a question about the elicitation, not about the code.
