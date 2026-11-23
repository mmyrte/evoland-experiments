# mlr3-custom-learners

mlr3 learners that experiments use but that are out of scope for `evoland.plus`.
`evoland.plus` takes any mlr3 learner, and any mlr3 filter for covariate selection, so a learner kept here works with it like a stock one.

These are plain R files, not a package.
An experiment sources the file it needs from the repository root (the working directory of every step, see the top-level README) and then uses the learner by its key.

| File           | Contents                                                                 |
| -------------- | ------------------------------------------------------------------------ |
| `grrf.r`       | Guided regularized random forest, `LearnerClassifGrrf`, key `classif.grrf` |
| `grrf-tests.r` | Checks of `grrf.r`; run `Rscript mlr3-custom-learners/grrf-tests.r` from the root |
| `ekde.r`, `ekde.cpp` | Bayes-eKDE transition probabilities, `LearnerClassifEkde`, key `classif.ekde`; the density estimator in C++ |
| `ekde-tests.r` | Checks of `ekde.r`, against a brute-force estimator and the Python reference; run `Rscript mlr3-custom-learners/ekde-tests.r` from the root |
| `ekde-reference/reference.py` | The Python reference (clumpy, ekde, hyperclip) on files, called by `ekde-tests.r` |

## Guided regularized random forest (`grrf.r`)

### Use with `evoland.plus`

```r
source("mlr3-custom-learners/grrf.r") # defines LearnerClassifGrrf, registers "classif.grrf"

learner <- mlr3::lrn("classif.grrf", gamma = 0.5, num.trees = 100L, max.depth = 20L)
scores <- db$get_pred_filter_score(
  filter = mlr3filters::flt("importance", learner = learner)
)
```

`$importance()` returns the impurity importance of the guided regularized forest, so the learner serves as the learner of `mlr3filters::FilterImportance`.
It also predicts (`predict_type = "response"` or `"prob"`), for two or more classes, so it can be a transition model.
The learner works on mirai daemons, which `get_pred_filter_score()` maps over when they are set: its methods refer only to package namespaces, never to objects of the session that sourced the file.
The daemons need `ranger` installed.

### Method

A *regularized random forest* (RRF) penalizes a split on a feature that the forest has not split on before.
With $F$ the set of features used so far in the forest, the gain of a split on feature $X_i$ is

$$
\mathrm{Gain}_R(X_i) =
\begin{cases}
\lambda_i \, \mathrm{Gain}(X_i) & X_i \notin F \\
\mathrm{Gain}(X_i) & X_i \in F
\end{cases}
$$

with a coefficient $\lambda_i \in [0, 1]$; a smaller $\lambda_i$ is a larger penalty.
A feature that only carries information already carried by a feature in $F$ rarely wins against it, so the regularized forest concentrates its splits, and its importance, on a small set of non-redundant features.

The *guided* regularized random forest (Deng & Runger 2013, <https://doi.org/10.1016/j.patcog.2013.05.018>) sets the coefficient of each feature from the importance $\mathrm{Imp}_i$ of an ordinary random forest grown first:

$$
\lambda_i = (1 - \gamma)\,\lambda + \gamma\,\frac{\mathrm{Imp}_i}{\max_j \mathrm{Imp}_j}
$$

The base coefficient $\lambda \in (0, 1]$ penalizes all new features alike; the guidance $\gamma \in [0, 1]$ shifts the coefficient towards the normalized importance, so features unimportant in the first forest are penalized more.

- $\gamma = 0$, $\lambda = 1$: no penalty, an ordinary random forest. The learner then grows only one forest, so under the same seed it reproduces `ranger::ranger()`.
- $\gamma = 0$, $\lambda < 1$: an unguided RRF, the same penalty for every feature.
- $\gamma = 1$: the coefficient is the normalized importance alone; a feature that the first forest never split on is never chosen.

Both forests are grown with `ranger` and its `regularization.factor` ($\lambda_i$), with impurity importance (Gini for classification).
`ranger` shares $F$ across the trees, so it grows a regularized forest on one thread.
If the first forest makes no split at all, all features count as equally important ($\mathrm{Imp}_i / \max_j \mathrm{Imp}_j = 1$).
`regularization.usedepth = TRUE` additionally penalizes deep splits more, following Wundervald et al. (2020, <https://doi.org/10.1109/ACCESS.2020.3032095>).

### Parameters

Parameters left unset run at the default declared in the parameter set, also after `$param_set$values <- list(...)` has replaced all values.

| Parameter                   | Range                                        | Default             | Meaning                                                      |
| --------------------------- | -------------------------------------------- | ------------------- | ------------------------------------------------------------ |
| `gamma`                     | [0, 1]                                       | 0.5                 | Guidance $\gamma$                                            |
| `lambda`                    | (0, 1]                                       | 1                   | Base coefficient $\lambda$                                   |
| `regularization.usedepth`   | logical                                      | `FALSE`             | Depth-dependent penalty (Wundervald et al. 2020)             |
| `balance_classes`           | logical                                      | `FALSE`             | Case weights $n / (n_\text{classes}\,n_\text{class})$, so every class weighs the same |
| `num.trees`                 | ≥ 1                                          | 500                 | Trees in each forest                                         |
| `max.depth`                 | ≥ 0                                          | 0                   | Maximum tree depth; 0 is unlimited                           |
| `num.threads`               | ≥ 1                                          | 1                   | Threads for the first forest and for prediction              |
| `mtry`, `min.node.size`, `sample.fraction`, `replace`, `splitrule`, `num.random.splits`, `respect.unordered.factors`, `seed` | as in `ranger` | `ranger`'s | Passed on to both forests |

The fitted model is a list: `forest`, the guided regularized `ranger` forest; `importance_initial`, the importance of the first forest (`NULL` for $\gamma = 0$); `coef_reg`, the coefficients $\lambda_i$ by feature.

### Differences from `LearnerClassifGrrf` of `evoland.plus`

`evoland.plus` shipped a learner of the same name and key until it was removed from the package as out of scope.
This one is a reimplementation, not a copy, and fixes what an audit found:

- **Coefficient.** The package computed $(1 - \gamma) + \gamma\,\frac{\mathrm{Imp}_i - \min \mathrm{Imp}}{\max \mathrm{Imp} - \min \mathrm{Imp}}$: no base coefficient, and min-max instead of max normalization, so the least important feature always got $1 - \gamma$. Here the coefficient is that of Deng & Runger (2013); for the same $\gamma$, features with little but nonzero importance are penalized somewhat less. With $\gamma = 0$ the package applied no penalty, although it documented an "equal penalty"; that needs $\lambda < 1$ here.
- **`max.depth`.** The package documented a default of 100 but never passed it, so trees grew unlimited. Here the documented default is 0 (unlimited), which is what runs.
- **Class weights.** The package always weighted cases by inverse class frequency. That is not part of the method; here it is `balance_classes`, off by default.
- **Threads.** The package left `num.threads` to `ranger`, which grew the first forest on all cores. Here the default is 1, as for the second forest, which `ranger` grows on one thread anyway.
- **Seeds.** With $\gamma = 0$ there is no first forest, so the random number stream differs from the package's.
- **Scope.** Probability prediction, more than two classes, and more `ranger` parameters (`lambda`, `mtry`, `min.node.size`, `splitrule`, `seed`, …) are new.

`2026-05-ssp-ch/050-covariate-selection.qmd` uses $\gamma = 0.5$, `max.depth = 20` and the class weights, so its coefficients change through the normalization and the first forest through the threads.
Its importance scores, and possibly the selected predictors, are therefore not reproduced exactly by a rerun: the committed report, `050-covariate-importances-grrf.csv` and the two importance plots were computed with the package's learner.

## Bayes-eKDE (`ekde.r`)

### Use with `evoland.plus`

```r
source("mlr3-custom-learners/ekde.r") # compiles ekde.cpp, registers "classif.ekde"

learner <- mlr3::lrn("classif.ekde", predict_type = "prob", bounds = "left")
db$trans_models_t <- db$fit_full_models(learner = learner)
db$predict_trans_pot(id_period_post = 3L, select_score = "no.crossval", select_maximize = TRUE)
```

The learner estimates transition probabilities as clumpy does (Mazy & Longaretti 2022; Mazy 2022, Ch. 2), with the estimator of clumpy ported to C++: the result agrees with clumpy's to rounding when the same rows are given in the same order (see Differences).
It takes numeric and integer features, two or more classes, `predict_type = "response"` or `"prob"`.
In evoland.plus a transition model is a binary classifier on the cells of the anterior class $u$, target `did_transition`; the learner then returns $P(v \mid u, z)$ for `TRUE`.

Factors and logicals are refused (mlr3 stops on the unsupported feature types before training): the estimator is a density on a continuous space, and clumpy treats discrete variables separately, which is not ported.
Select the numeric predictors for its runs (`trans_preds_t`), as `ekde-tests.r` does.
A constant feature, or a class with no more rows than features, cannot be whitened: training stops with an error naming the columns or the class (evoland.plus records it as a failed model).

### Method

For an anterior class $u$ and a posterior class $v$, Bayes' rule gives

$$
P(v \mid u, z) = P(v \mid u)\,\frac{\rho(z \mid u, v)}{\rho(z \mid u)},
$$

with $P(v \mid u)$ the share of the cells of $u$ that went to $v$, and the two densities estimated by kernel density estimation (Mazy 2022, Sec. 2.4).
In mlr3 terms, the classes of the task are the $v$, the class frequencies of the training rows the $P(v \mid u)$, and $\rho(z \mid v)$ is estimated from the rows of each class.

**eKDE** (Mazy 2022, Sec. 2.4.3 to 2.5.1), for the rows $y_1, \dots, y_n$ of one class:

1. *Whitening.* $x \mapsto W^\top (x - \bar y)$ with $W = V \Sigma^{-1} \sqrt{n - 1}$ from the singular value decomposition of the centred data, so the whitened data have unit covariance ($W W^\top = C^{-1}$, Eq. 2.10). The density in the original space is the whitened one divided by $|\det W^{-1}|$.
2. *Bandwidth.* One bandwidth $h$ in whitened space, by default Terrell's maximal smoothing rule (Terrell 1990; Mazy 2022, Eq. 2.24) with the roughness of the box kernel, $\int K^2 = 2^{-d}$:
   $$h = \left(\frac{(d+8)^{(d+6)/2}\,\pi^{d/2}\,2^{-d}}{16\,n\,(d+2)\,\Gamma(d/2+4)}\right)^{1/(d+4)}.$$
   Alternatively Scott's (Silverman's) rule $n^{-1/(d+4)}$, widened by 1.288 for the box kernel as the reference does, or a fixed $h$.
3. *Binning* (after Wells & Ting 2019). Cells of width $\delta = 2h/q$ ($q$ odd, default 51) from the minimum of the whitened data: a row falls in cell $\operatorname{trunc}((x - x_{\min}) / \delta)$ (truncated toward zero, per coordinate). Each occupied cell $c$ gets the weight $\nu_c$, its number of rows.
4. *Box kernel on the grid.* The density at $z$ is the sum of the weights of the occupied cells within $(q-1)/2$ cells of the cell of $z$ in every coordinate:
   $$\tilde\rho(z) = \frac{1}{n\,(2h)^d\,|\det W^{-1}|} \sum_{c\,:\,\|c - c(z)\|_\infty \le (q-1)/2} \nu_c,$$
   the box kernel $K(x) = 2^{-d}\,\mathbf 1\{\|x\|_\infty \le 1\}$ with its centre moved to the centre of the cell (Eq. 2.9, Sec. 2.5.1.2). Summed over all cells it integrates to 1. Only the cells near $z$ are visited, by a search through the lexicographically sorted cells, one coordinate at a time (ekdefunc.pyx `merge()`); each distinct cell of the query rows is evaluated once.
5. *Boundary correction* (Sec. 2.4.4, Eq. 2.25). A bounded feature is a half-space $x_k \ge l_k$ (or $\le u_k$), a half-space in whitened space too. The weight of a cell near a bound is divided by the share of a hypercube centred on the cell that lies within the bounds, $\nu_c / \mathrm{vol}(H_c \cap D)$, so that the kernel truncated by the bound still has mass 1; the share is the volume of a hypercube clipped by hyperplanes (Cho & Kim 2020; the `hyperclip` package). Rows outside the bounds get density 0.

**Bayes adjustment.** The ratio above is noisy where the densities are small, and its mean over the rows need not be $P(v \mid u)$.
clumpy (`_bayes.py`, `bayes_adjustment()`) rescales each column to the mean $P(v \mid u)$, then repeats, until no row sums to more than 1 or `n_corrections_max` rounds have passed: divide the rows that sum to more than 1 by their sum, and rescale the columns again.

The learner offers that procedure (`adjustment = "clumpy"`), and by default its fixed point over all rows (`adjustment = "ipf"`): the same two steps, but every row divided by its sum, iterated over the training rows until the column means are the class priors (iterative proportional fitting). The result is a prior shift,

$$
P(v \mid z) = \frac{c_v\,P(v)\,\tilde\rho(z \mid v)}{\sum_w c_w\,P(w)\,\tilde\rho(z \mid w)},
$$

with factors $c_v$ fitted at training and stored in the model, so that the probabilities sum to 1 for every row, lie in $[0, 1]$, and their mean over the training rows is the class prior.
$\rho(z \mid u)$ is then the mixture of the class densities, $\sum_w c_w P(w) \tilde\rho(z \mid w)$, rather than an eKDE of its own: a separate estimate of $\rho(z \mid u)$ would cancel in the normalization.
The prediction of a row depends on that row only.
`adjustment = "none"` is the same with $c_v = 1$, plain Bayes' rule with the mixture.

`adjustment = "clumpy"` reproduces the reference: $\rho(z \mid u)$ is a separate eKDE, fitted, as clumpy does, on the rows being predicted, and the adjustment is computed over them, so a prediction depends on the set of rows predicted together (predict all cells of the anterior class at once, as `015-clumpy-kde` does; mlr3's `parallel_predict`, which evoland.plus switches on, would split them when future workers are set).
Its rows sum to at most 1, not to 1; mlr3 requires 1, so the class `remainder_class` (default: the negative class of a two-class task; required with more classes) gets one minus the others, which is how `015-clumpy-kde` uses the result (it writes only the $v \neq u$).
On the data of the model comparison clumpy's loop does not converge: after its 1000 rounds most rows still sum to slightly more than 1 (up to 1.02 to 1.1 in `ekde-tests.r`), and a final division by the row sums ends it; the learner warns.

### Parameters

Parameters left unset run at the default declared in the parameter set, also after `$param_set$values <- list(...)` has replaced all values.

| Parameter             | Range                                | Default       | Meaning |
| --------------------- | ------------------------------------ | ------------- | ------- |
| `kernel`              | `"box"`                              | `"box"`       | Kernel; only the box kernel is ported (see Ported) |
| `bandwidth`           | `"terrel"`, `"scott"`, `"silverman"` | `"terrel"`    | Bandwidth rule (`scott` = `silverman`) |
| `h`                   | > 0 or `NULL`                        | `NULL`        | Fixed bandwidth in whitened units; overrides `bandwidth` |
| `q`                   | odd ≥ 1                              | 51            | Cells per kernel width, $\delta = 2h/q$ |
| `whiten`              | logical                              | `TRUE`        | Whitening; without it $h$ is in the units of the features |
| `bounds`              | `"none"`, `"left"`, `"right"`, `"both"`: one value, or one per feature (named, or in feature order) | `"none"` | Bounded features; named vectors leave the others unbounded |
| `lower`, `upper`      | numeric, named by feature, or `NULL` | `NULL`        | Bound positions; features not given are bounded at the minimum / maximum of each sample fitted, as the reference does |
| `boundary_correction` | `"reference"`, `"kernel"`, `"none"`  | `"reference"` | Side of the clipped hypercube: $h$ as the reference, $2h$ (the kernel's support) as Eq. 2.25 states, or no correction |
| `zero`                | ≥ 0                                  | 1e-14         | Tolerance of the clipped volume (the reference's `zero`) |
| `n_fit_max`           | ≥ 1                                  | 100000        | Each density is fitted on at most this many rows, a random subsample of a larger class (R's random number stream) |
| `adjustment`          | `"ipf"`, `"clumpy"`, `"none"`        | `"ipf"`       | Bayes adjustment, see Method |
| `n_corrections_max`   | ≥ 1                                  | 1000          | Rounds of the adjustment (`ipf` or `clumpy`) |
| `tol`                 | ≥ 0                                  | 1e-10         | `ipf`: stop when every class mean is within this relative tolerance of its prior |
| `zero_density`        | `"prior"`, `"uniform"`               | `"prior"`     | `ipf`, `none`: probabilities of a row where every class density is 0 (outside the data or the bounds) |
| `remainder_class`     | a class, or `NULL`                   | `NULL`        | `clumpy`: the class that gets one minus the others (default: the negative class of a two-class task) |
| `num_threads`         | ≥ 1                                  | 1             | Threads of the density evaluation (the grid search); the result does not depend on them |

The fitted model is a list: `features`, `classes`, `prior` (class frequencies of the training rows), `kde` (one fitted eKDE per class, `NULL` for a class without rows: mean, whitening matrix and scale, bandwidth, grid origin and width, occupied cells and their corrected weights, bound half-spaces), `adjustment` and `factors` ($c_v$).
`$densities(newdata)` returns $\tilde\rho(z \mid v)$ for each class.
`ekde_fit()` and `ekde_predict()`, defined by `ekde.r`, are the density estimator on its own.

### Ported from the reference, and licence

The reference is mmyrte/clumpy at `e9f73b9`, the revision `pyproject.toml` pins (the code of upstream CLUMPY, `ekde` and `hyperclip` by F.-R. Mazy, build fixes only).

| Here | From | Licence of the source |
| --- | --- | --- |
| `ekde.cpp`: binning, the grid search of the box kernel | `ekde/ekde/base.py` (`fit()`, `predict()`, `_discretize()`), `ekde/cython/ekdefunc.pyx` (`count_diff_*`, `sparse()`, `merge()`, `explore()`) | BSD-3-Clause |
| `ekde.cpp`: clipped volume | `hyperclip/cython/hyperfunc.pyx` (`volumes()`, `clipping_condition_B_and_volume()`, `compute_vertex()`, `test_vertex()`, `gauss_AB()`, `determinant()`): the general formula of Cho & Kim in any dimension, not a special case | GPL-3.0 |
| `ekde.r`: whitening, bandwidth rules, bound half-spaces, normalization | `ekde/ekde/whitening_transformer.py`, `ekde/ekde/base.py`, `hyperclip/hyperclip/hyperplane.py` | BSD-3-Clause, GPL-3.0 |
| `ekde.r`: Bayes rule and adjustment | `clumpy/clumpy/transition_probability_estimation/_bayes.py` (`fit()`, `compute()`, `bayes_adjustment()` without log computations) | GPL-3.0 |

`ekde.r` and `ekde.cpp` are a derived work of GPL-3.0 code and are therefore under the GPL-3.0 (or later), with ekde's BSD notice kept; `ekde-reference/reference.py` only calls the reference.
The arithmetic follows the reference operation by operation, in its order of summation, which is why the results agree to rounding.

Not ported:

- the Gaussian and Epanechnikov kernels of `ekde`: in `explore()` the squared distance is not reset between the cells of a row, accumulates across them and is multiplied by $\delta^2$ once per cell, and the Epanechnikov kernel ignores $h$; neither integrates to 1, the comparison does not use them, and porting them would reproduce the defect. The Gaussian boundary correction (a Monte Carlo integral with unseeded `np.random`) goes with them;
- `log_computations` of `bayes_adjustment()` (default off, unused);
- the parallel prediction of `ekde` (`n_jobs`: processes that share a `./.temp_joblib_memmap` directory in the working directory, so that two predictions running at once in one directory can read each other's arrays): the port has threads instead (`num_threads`);
- clumpy's handling of discrete variables.

### Differences from the reference

- **Signs of the whitening.** The whitening is unique only up to the signs of the singular vectors, and the estimator is not invariant to them: reflecting a whitened axis moves the grid, whose origin is the minimum of the data, so the cells, the kernel windows and the densities change: by a factor of up to about 4 for five features, and the probabilities by up to 1 (`ekde-tests.r`, (c)). The reference keeps the signs that LAPACK returns, and these depend on the order of the rows (and on the LAPACK build). The port makes the largest component of each singular vector positive, and sorts the rows before whitening, so its result does not depend on the order of the rows at all, not even by rounding.
  With the reference's singular vectors given these signs (`canonical_signs` in `reference.py`, the only change made to it), the port agrees with the reference to rounding.
- **Hyperplanes of the bounds.** The reference finds each bound's hyperplane by solving for the hyperplane through $d$ transformed points, perturbing the system with unseeded random noise when it is singular (a bound through the origin without whitening), and orients it with a randomly drawn data point that lies on no bound; if no row is strictly inside every bound (a binary feature, say), that draw never ends. The port writes the hyperplane down directly, the same one scaled the same way ($|r| = 1$).
- **Clipped volume for many bounds.** `hyperfunc.pyx volumes()` sizes a buffer by the dimension instead of the number of hyperplanes, so it fails when more hyperplanes than dimensions reach one cell (two-sided bounds in one dimension); the port does not.
- **Rows.** The port sorts the training rows (and subsamples above `n_fit_max` from the sorted rows with R's random numbers, where the reference uses `np.random`), so a subsample is the reference's only when no class exceeds `n_fit_max`.
- **Adjustment.** By default `ipf`, see Method; `clumpy` is the reference's.

Reference quirks that are ported as they are (`boundary_correction = "reference"`):

- the boundary correction clips a hypercube of side $h$, half the kernel's support $2h$, and centred on the cell rather than the row; Eq. 2.25 calls for the kernel's own support. `boundary_correction = "kernel"` uses side $2h$;
- a hyperplane enters the correction of a cell only when $|a \cdot x + r|\,\|a\|_\infty / \|a\|_2^2 \le h/2$, which is stricter than the hypercube touching the hyperplane, so cells near a bound that is oblique in whitened space can go uncorrected;
- neither correction removes the bias at a bound, as it renormalizes the kernels of the data near the bound, not the estimate at the bound: for uniform data on $[0, 1]$ (`ekde-tests.r`, (b)) the mean bias within $h/4$ of a bound is −0.42 without correction, −0.33 with the reference's, −0.22 with the kernel-support one; the latter keeps the mass within the bounds (1.0000 against 0.985 and 0.974);
- the truncation toward zero puts the slab of one cell width below the data minimum into the first cell, so the estimate there is that of the first cell; the integral over the grid cells is 1, the integral over space exceeds 1 by the mass of that slab;
- the clipped volume of Cho & Kim sums terms of alternating sign; with several bounds near one cell and integer features it loses digits (the port and the reference then agree to about $10^{-11}$ instead of $10^{-15}$, (c)).

### The model comparison: `015-clumpy-kde` does not replay on the synthetic landscape

Two runs of `015-clumpy-kde` on the same data can differ because the reference's whitening depends on the order of the rows (above).
In `015`, $\rho(z \mid u)$ is fitted on the prediction rows, `pred_<u>.csv`, which `pred_wide_predict_v()` returns in whatever order the database produces them (the training rows happen to be ordered by `id_coord`).
On the synthetic landscape's inputs for forest (`outputs-synthetic/clumpy-kde-work`, 48 041 training and 44 165 prediction rows, nine features), the reference run twice on identical files gives identical potentials; with the prediction rows shuffled and the training rows as they are, the forest → built potentials change at 843 cells by more than $10^{-6}$, by up to 0.24, the size of the difference seen between the two runs of the comparison (forest → other happens to keep its signs here and agrees to $4 \times 10^{-15}$); with both shuffled, up to 1.
The singular vectors of the shuffled rows differ from the others in sign only (by $10^{-14}$ otherwise).
On PIE the potentials agreed to $2 \times 10^{-15}$, consistent with the rows arriving in the same order there, or with signs that do not flip for those data.
The port is independent of the order of the rows; `015` could also be made to replay by sorting its CSV files by `id_coord`, which does not make its result less arbitrary.

### Replacing `015-clumpy-kde`

`015` fits, per anterior class $u$, the densities of the final classes (staying included) with left bounds at each class's minimum, Terrell's bandwidth, the box kernel, $q = 51$, whitening, `n_fit_max = 1e5`, and clumpy's adjustment with $P(v \mid u)$ from the training rows.
The same with the learner, per anterior class, from the `train`/`pred` tables that `015` builds:

```r
task <- mlr3::as_task_classif(train[, c(cols, "final"), with = FALSE][, final := factor(final)], target = "final")
learner <- mlr3::lrn(
  "classif.ekde",
  predict_type = "prob",
  bounds = "left",
  adjustment = "clumpy",
  remainder_class = as.character(u)
)
learner$train(task)
P <- learner$predict_newdata(pred[, ..cols])$prob # columns v != u: the potentials of 015
```

This agrees with the reference up to the signs of the whitening.
On the forest inputs of the synthetic landscape (nine features, left bounds; reference with canonical signs) the class densities agree to $3 \times 10^{-6}$ (relative) and the potentials to $6 \times 10^{-7}$; without bounds the densities agree to $10^{-14}$, so the difference is the clipped volume's loss of digits with nine bounded, partly integer-valued features, amplifying differences of rounding in the whitening.
The reference takes 34 s to fit and 48 s to compute these potentials; the port 14 s to fit (13 s of it the clipped volumes) and 6 s for the densities.
In evoland.plus's own setting (a binary model per transition) the default `ipf` adjustment is the one to use: its predictions do not depend on which rows are predicted together, and its potentials have the observed transition rate as their mean over the calibration cells.

### Compilation, workers, serialization

Sourcing `ekde.r` compiles `ekde.cpp` with `Rcpp::sourceCpp()` into `.cache/rcpp-ekde/R-<version>-<platform>/` (git-ignored) under the repository root: about 6 s the first time, after which every session, and every mirai daemon, finds the build there and loads it in well under a second.
A lock directory makes processes that source the file at the same time wait for one build.
The fitted model contains only vectors, matrices and lists, so it survives `qs2::qs_serialize()`, as evoland.plus stores it.
The learner's methods find their R functions in an environment that is serialized with the learner, and the compiled functions through `getOption("ekde.cpp")`; in a session that has not sourced `ekde.r` (a mirai daemon, or a fresh session reading a stored model), the first fit or prediction loads the build from the absolute path recorded when `ekde.r` was sourced (compiling it if needed). So daemons need neither `source()` nor anything but Rcpp, mlr3 and paradox, but they need the repository's directory on their file system.

### Timings

`ekde-tests.r` (g), on one core of the 4-core cloud container used for the tests (wall seconds; two classes, left bounds, about 5 % positives; each density fitted on at most $10^5$ rows; the reference with `n_jobs = 1`):

| rows | features | port `ipf`: fit / predict | port `ipf`, 4 threads | port `clumpy`: predict | reference: fit / predict |
| ---: | ---: | ---: | ---: | ---: | ---: |
| $10^5$ | 3 | 3.7 / 3.5 | 1.2 / 1.2 | 23 | 0.07 / 19 |
| $10^6$ | 3 | 48 / 48 | 15 / 14 | 183 | 0.2 / 164 |
| $10^5$ | 5 | 9.0 / 8.7 | 2.6 / 2.6 | 27 | 0.35 / 45 |
| $10^6$ | 5 | 131 / 130 | 39 / 39 | 312 | 0.7 / 467 |

Fitting the eKDE itself takes well under a second here (few cells near the bounds); `ipf`'s fit includes the densities at every training row, for the prior factors.
Per density evaluation the port is about 2 to 4 times as fast as the reference on one thread.
With `adjustment = "clumpy"` both spend most of the prediction in the fit of $\rho(z)$ on the predicted rows and in the 1000 rounds of the adjustment over all rows (R matrix operations here, NumPy there).
With nine bounded features the clipped volumes dominate the fit instead (see Limits).

### Limits

- **Dimension.** The grid search visits the occupied cells within the kernel window, so its cost grows with the number of occupied cells there, which grows quickly with $d$ for a fixed $n$. The clipped volume enumerates the vertices of the hypercube against each set of nearby bounds: of the order of $2^d \binom{m}{k}$ systems per cell near $m$ bounds; with nine left-bounded features (the model comparison) it takes most of the fit (13 s for 43 000 rows, against 0.1 s for the rest). The cofactor determinants are factorial in the number of bounds active at one cell. More than 30 features are refused by the clipped volume.
- **Memory.** The model holds the occupied cells ($\le n$ per class, integer, $d$ columns) and their weights: for $10^5$ rows and five features at most 2.8 MB per class. Prediction sorts the cells of the query rows ($n \times d$ integers).
- **Rows.** A class with fewer rows than features plus one, or with collinear features, cannot be whitened (an error). Each class density is fitted on at most `n_fit_max` rows.
- **Threads.** Only the grid search runs on `num_threads` threads (C++ standard threads; each distinct query cell is summed by one thread, so the result is the same for any number); the clipped volumes and the rest run on one.

### References

- Mazy, F.-R. and Longaretti, P.-Y. (2022). An accurate and powerful calibration-estimation method based on kernel density estimation. arXiv:2203.17078.
- Mazy, F.-R. (2022). *Towards a coherent algorithmic theory and implementation of pattern-based land use and land cover change modeling.* PhD thesis, Université Grenoble Alpes, Ch. 2. <https://theses.hal.science/tel-04382012v1>
- Terrell, G. R. (1990). The maximal smoothing principle in density estimation. *Journal of the American Statistical Association* 85(410), 470–477. <https://doi.org/10.1080/01621459.1990.10476223>
- Cho, Y. and Kim, S. (2020). Volume of hypercubes clipped by hyperplanes and combinatorial identities. *Electronic Journal of Linear Algebra* 36, 228–255. <https://doi.org/10.13001/ela.2020.5085>
- Wells, J. R. and Ting, K. M. (2019). A new simple and efficient density estimator that enables fast systematic search. *Pattern Recognition Letters* 122, 92–98. <https://doi.org/10.1016/j.patrec.2018.12.020>
