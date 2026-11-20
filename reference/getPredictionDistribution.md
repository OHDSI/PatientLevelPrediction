# Calculates the prediction distribution

Calculates the prediction distribution

## Usage

``` r
getPredictionDistribution(
  prediction,
  predictionType = "binary",
  typeColumn = "evaluation"
)
```

## Arguments

- prediction:

  A prediction object

- predictionType:

  The type of prediction (binary or survival)

- typeColumn:

  A column that is used to stratify the results

## Value

The 0.00, 0.1, 0.25, 0.5, 0.75, 0.9, 1.00 quantile pf the prediction,
the mean and standard deviation per class

## Details

Calculates the quantiles from a predition object

## Examples

``` r
prediction <- data.frame(rowId = 1:100, 
                         outcomeCount = stats::rbinom(1:100, 1, prob=0.5), 
                         value = runif(100), 
                         evaluation = rep("Train", 100))
getPredictionDistribution(prediction)
#>   class PersonCount averagePredictedProbability StDevPredictedProbability
#> 1     0          60                   0.4776914                 0.2791246
#> 2     1          40                   0.5485504                 0.3067507
#>   MinPredictedProbability P05PredictedProbability P25PredictedProbability
#> 1             0.006595086              0.06174722               0.2302247
#> 2             0.008196403              0.06297827               0.2549444
#>   MedianPredictedProbability P75PredictedProbability P95PredictedProbability
#> 1                  0.4651258               0.6925676               0.9215393
#> 2                  0.6367611               0.8065994               0.9493524
#>   MaxPredictedProbability evaluation
#> 1               0.9783956      Train
#> 2               0.9595244      Train
```
