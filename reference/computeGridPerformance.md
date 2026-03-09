# Computes grid performance for a hyperparameter combination (backwards compatible)

Computes grid performance for a hyperparameter combination (backwards
compatible)

## Usage

``` r
computeGridPerformance(prediction, param, performanceFunct = "computeAuc")
```

## Arguments

- prediction:

  A data.frame with predictions and an `index` column for folds.

- param:

  A list of hyperparameters (values may include `NULL`).

- performanceFunct:

  String or function to compute performance on a prediction data.frame.
  Default is
  [`PatientLevelPrediction::computeAuc`](https://ohdsi.github.io/PatientLevelPrediction/reference/computeAuc.md).

## Value

A list with overall and per-fold performance plus the parameter summary.

## Examples

``` r
prediction <- data.frame(
  rowId = c(1, 2, 3, 4, 5),
  outcomeCount = c(0, 1, 0, 1, 0),
  value = c(0.1, 0.9, 0.2, 0.8, 0.3),
  index = c(1, 1, 1, 1, 1)
)
param <- list(hyperParam1 = 5, hyperParam2 = 100)
computeGridPerformance(prediction, param, performanceFunct = PatientLevelPrediction::computeAuc)
#> $metric
#> [1] "PatientLevelPrediction::computeAuc"
#> 
#> $cvPerformance
#> [1] 1
#> 
#> $cvPerformancePerFold
#> [1] 1
#> 
#> $param
#> $param$hyperParam1
#> [1] 5
#> 
#> $param$hyperParam2
#> [1] 100
#> 
#> 
#> $hyperSummary
#>                               metric fold value hyperParam1 hyperParam2
#> 1 PatientLevelPrediction::computeAuc   CV     1           5         100
#> 2 PatientLevelPrediction::computeAuc    1     1           5         100
#> 
```
