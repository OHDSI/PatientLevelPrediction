# Create setting for Broken Adaptive Ridge logistic regression

Creates model settings for Broken Adaptive Ridge logistic regression
using Cyclops and the BrokenAdaptiveRidge prior.
`initialRidgeVariance = "auto"` first fits a ridge model with Cyclops
cross-validation and uses the selected ridge variance to initialize BAR.
`penalty = "auto"` cross-validates over a BAR penalty grid and refits
using the penalty with the highest mean out-of-fold AUC. Automatic
penalty tuning fits one model per fold and grid value, in addition to
the final model and cross-validation refits, so it can be substantially
slower than using a fixed penalty.

## Usage

``` r
setBrokenAdaptiveRidge(
  initialRidgeVariance = "auto",
  seed = NULL,
  includeCovariateIds = c(),
  noShrinkage = c("(Intercept)"),
  penalty = "auto",
  penaltyRatio = 0.1,
  penaltyGridSize = 10,
  threads = -1,
  forceIntercept = FALSE,
  upperLimit = 20,
  lowerLimit = 0.01,
  tolerance = 2e-06,
  maxIterations = 3000,
  threshold = 1e-06
)
```

## Arguments

- initialRidgeVariance:

  Numeric prior starting variance, or `"auto"` to estimate this using
  ridge cross-validation.

- seed:

  An option to add a seed when training the model.

- includeCovariateIds:

  A set of covariateIds to limit the analysis to.

- noShrinkage:

  A set of covariates which are forced into the model. The default is
  the intercept. This takes precedence over `forceIntercept`: a
  covariate listed here is excluded from the prior even when
  `forceIntercept = TRUE`.

- penalty:

  Numeric BAR penalty, `"bic"` to use `log(n) / 2`, or `"auto"` to
  cross-validate over a penalty grid.

- penaltyRatio:

  Minimum penalty in the automatic grid as a ratio of the `log(n) / 2`
  starting penalty.

- penaltyGridSize:

  Number of penalties to evaluate when `penalty = "auto"`.

- threads:

  An option to set number of threads when training model.

- forceIntercept:

  Logical: Include the intercept coefficient in the prior, unless it is
  listed in `noShrinkage`. To penalize the intercept, set this to `TRUE`
  and remove the intercept from `noShrinkage`.

- upperLimit:

  Numeric: Upper prior variance limit for grid-search.

- lowerLimit:

  Numeric: Lower prior variance limit for grid-search.

- tolerance:

  Numeric convergence tolerance passed to both Cyclops and BAR. In
  Cyclops, this controls the maximum relative change in its convergence
  criterion; in BAR, it controls the maximum absolute coefficient change
  between outer iterations.

- maxIterations:

  Integer maximum iteration count passed to both the Cyclops optimizer
  and the BAR outer loop.

- threshold:

  Numeric BAR threshold.

## Value

`modelSettings` object

## References

Fridgeirsson EA, Williams R, Rijnbeek P, Suchard MA, Reps JM. Comparing
penalization methods for linear models on large observational health
data. Journal of the American Medical Informatics Association.
2024;31(7):1514-1521.
[doi:10.1093/jamia/ocae109](https://doi.org/10.1093/jamia/ocae109)

Li N, Peng X, Kawaguchi E, Suchard MA, Li G. A scalable surrogate L0
sparse regression method for generalized linear models with applications
to large scale data. Journal of Statistical Planning and Inference.
2021;213:262-281.
[doi:10.1016/j.jspi.2020.12.001](https://doi.org/10.1016/j.jspi.2020.12.001)

## Examples

``` r
modelBar <- setBrokenAdaptiveRidge(seed = 42)
```
