# Create setting for lasso Cox model

Create setting for lasso Cox model

## Usage

``` r
setCoxModel(
  variance = 0.01,
  seed = NULL,
  includeCovariateIds = c(),
  noShrinkage = c(),
  threads = -1,
  upperLimit = 20,
  lowerLimit = 0.01,
  tolerance = 2e-07,
  maxIterations = 3000
)
```

## Arguments

- variance:

  Numeric: prior distribution starting variance

- seed:

  An option to add a seed when training the model

- includeCovariateIds:

  a set of covariate IDS to limit the analysis to

- noShrinkage:

  a set of covariates whcih are to be forced to be included in the final
  model. default is the intercept

- threads:

  An option to set number of threads when training model

- upperLimit:

  Numeric: Upper prior variance limit for grid-search

- lowerLimit:

  Numeric: Lower prior variance limit for grid-search

- tolerance:

  Numeric: maximum relative change in convergence criterion from
  successive iterations to achieve convergence

- maxIterations:

  Integer: maximum iterations of Cyclops to attempt before returning a
  failed-to-converge error

## Value

`modelSettings` object

## Examples

``` r
coxL1 <- setCoxModel()
```
