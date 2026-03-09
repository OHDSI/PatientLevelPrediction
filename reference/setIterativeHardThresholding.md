# Create setting for Iterative Hard Thresholding model

Create setting for Iterative Hard Thresholding model

## Usage

``` r
setIterativeHardThresholding(
  K = 10,
  penalty = "bic",
  seed = sample(1e+05, 1),
  exclude = c(),
  forceIntercept = FALSE,
  fitBestSubset = FALSE,
  initialRidgeVariance = 0.1,
  tolerance = 1e-08,
  maxIterations = 10000,
  threshold = 1e-06,
  delta = 0
)
```

## Arguments

- K:

  The maximum number of non-zero predictors

- penalty:

  Specifies the IHT penalty; possible values are `BIC` or `AIC` or a
  numeric value

- seed:

  An option to add a seed when training the model

- exclude:

  A vector of numbers or covariateId names to exclude from prior

- forceIntercept:

  Logical: Force intercept coefficient into regularization

- fitBestSubset:

  Logical: Fit final subset with no regularization

- initialRidgeVariance:

  integer

- tolerance:

  numeric

- maxIterations:

  integer

- threshold:

  numeric

- delta:

  numeric

## Value

`modelSettings` object

## Examples

``` r
modelIht <- setIterativeHardThresholding(K = 5, seed = 42)
```
