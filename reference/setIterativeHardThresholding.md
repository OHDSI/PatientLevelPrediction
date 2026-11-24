# Create setting for Iterative Hard Thresholding model

Logistic regression with `K` non-zero coefficients, fitted with the
IterativeHardThresholding package (version 2.0.0 or later). It maximizes
the log-likelihood minus a small ridge penalty (`variance`) over models
with `K` covariates. When `K` or `variance` has several values, the
hyperparameter search chooses between them on the training folds, as for
other models, by the held-out log-likelihood unless the hyperparameter
settings name another `tuningMetric`. Each fold's training rows are
converted to Cyclops data once, and each fold starts a candidate from
its own fit at the nearest earlier one (the next smaller `K`). The
`foldWorkers` of
[`createHyperparameterSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createHyperparameterSettings.md)
fit the folds in parallel.

## Usage

``` r
setIterativeHardThresholding(
  K = 25,
  variance = 1,
  exclude = c(),
  forceIntercept = FALSE,
  seed = NULL,
  algorithm = "default",
  control = list(),
  oneStandardError = FALSE,
  threads = -1,
  ...
)
```

## Arguments

- K:

  Number of covariates in the model (default 25, or 10 with
  `algorithm = "legacy"`, as in earlier versions); the intercept and
  `exclude` do not count. With fewer covariates than `K`, the model has
  all of them. One whole number, or several to choose from by
  cross-validation, for example `c(5, 10, 25, 50, 100)`, which takes
  several times longer to fit.

- variance:

  Ridge variance (default 1): a positive number, `Inf` for plain L0 (the
  best subset of size `K`), several numbers to choose from by
  cross-validation, or `"tune"` for the grid 0.01, 0.03, 0.1, 0.3, 1, 3
  and `Inf`. In earlier versions the second argument was `penalty`, so
  give `variance` by name; given by position, it gives a warning.

- exclude:

  Covariate ids (numbers) that are not penalized and do not count
  towards `K`.

- forceIntercept:

  Logical: penalize the intercept and count it towards `K`.

- seed:

  An option to add a seed when training the model.

- algorithm:

  `"default"`, `"chu2020"` (the algorithm of Chu et al., 2020) or
  `"legacy"` (IterativeHardThresholding 1.0.3: Broken Adaptive Ridge
  with at most `K` covariates, counting the intercept; deprecated, to
  reproduce earlier models).

- control:

  A named list of algorithm switches; see
  [`IterativeHardThresholding::createIhtControl()`](https://rdrr.io/pkg/IterativeHardThresholding/man/createIhtControl.html).
  Empty entries take the defaults of `algorithm`.

- oneStandardError:

  With several `K`: choose the smallest `K` whose held-out
  log-likelihood is within one standard error of the best. This gives
  smaller models, which can discriminate less well than the best `K`.

- threads:

  Kept for earlier code; not used. The refits run on `control$threads`
  threads (default 1); to fit the folds in parallel, use
  `createHyperparameterSettings(foldWorkers = )`.

- ...:

  Only with `algorithm = "legacy"`: the arguments of
  IterativeHardThresholding 1.0.3 (`penalty`, `fitBestSubset`,
  `initialRidgeVariance`, `tolerance`, `maxIterations`, `threshold`,
  `delta`).

## Value

`modelSettings` object

## References

Chu BB, Keys KL, German CA, Zhou H, Zhou JJ, Sobel EM, Sinsheimer JS,
Lange K (2020). Iterative hard thresholding in genome-wide association
studies: Generalized linear models, prior weights, and double sparsity.
GigaScience 9(6), giaa044.

## Examples

``` r
modelIht <- setIterativeHardThresholding(K = 5, seed = 42)
# choose K by cross-validation
modelIht <- setIterativeHardThresholding(K = c(10, 25, 50), seed = 42)
```
