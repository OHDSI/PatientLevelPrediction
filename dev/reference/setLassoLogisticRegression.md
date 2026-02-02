# Create modelSettings for lasso logistic regression

Create modelSettings for lasso logistic regression

## Usage

``` r
setLassoLogisticRegression(
  variance = 0.01,
  seed = NULL,
  includeCovariateIds = c(),
  noShrinkage = c(0),
  threads = -1,
  forceIntercept = FALSE,
  upperLimit = 20,
  lowerLimit = 0.01,
  tolerance = 2e-06,
  maxIterations = 3000,
  priorCoefs = NULL
)
```

## Arguments

- variance:

  Numeric: prior distribution starting variance

- seed:

  An option to add a seed when training the model

- includeCovariateIds:

  a set of covariateIds to limit the analysis to

- noShrinkage:

  a set of covariates whcih are to be forced to be included in in the
  final model. Default is the intercept

- threads:

  An option to set number of threads when training model.

- forceIntercept:

  Logical: Force intercept coefficient into prior

- upperLimit:

  Numeric: Upper prior variance limit for grid-search

- lowerLimit:

  Numeric: Lower prior variance limit for grid-search

- tolerance:

  Numeric: maximum relative change in convergence criterion from from
  successive iterations to achieve convergence

- maxIterations:

  Integer: maximum iterations of Cyclops to attempt before returning a
  failed-to-converge error

- priorCoefs:

  Use coefficients from a previous model as starting points for model
  fit (transfer learning)

## Value

`modelSettings` object

## Examples

``` r
modelLasso <- setLassoLogisticRegression(seed=42)
```
