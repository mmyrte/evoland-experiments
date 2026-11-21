# mlr3-custom-learners

mlr3 learners that experiments use but that are out of scope for `evoland.plus`.
`evoland.plus` takes any mlr3 learner, and any mlr3 filter for covariate selection, so a learner kept here works with it like a stock one.

These are plain R files, not a package.
An experiment sources the file it needs from the repository root (the working directory of every step, see the top-level README) and then uses the learner by its key.

| File           | Contents                                                                 |
| -------------- | ------------------------------------------------------------------------ |
| `grrf.r`       | Guided regularized random forest, `LearnerClassifGrrf`, key `classif.grrf` |
| `grrf-tests.r` | Checks of `grrf.r`; run `Rscript mlr3-custom-learners/grrf-tests.r` from the root |

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
