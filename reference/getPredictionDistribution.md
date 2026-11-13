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
#> 1     0          44                   0.5023062                 0.3114205
#> 2     1          56                   0.5565364                 0.2983835
#>   MinPredictedProbability P05PredictedProbability P25PredictedProbability
#> 1              0.03845911              0.07905232               0.2178688
#> 2              0.02863622              0.08382923               0.3024435
#>   MedianPredictedProbability P75PredictedProbability P95PredictedProbability
#> 1                  0.4778457               0.8128481               0.9669017
#> 2                  0.6276499               0.7854039               0.9506434
#>   MaxPredictedProbability evaluation
#> 1               0.9936175      Train
#> 2               0.9990015      Train
```
