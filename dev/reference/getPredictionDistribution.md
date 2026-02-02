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
#> 1     0          55                   0.4121088                 0.3074000
#> 2     1          45                   0.5370691                 0.3030384
#>   MinPredictedProbability P05PredictedProbability P25PredictedProbability
#> 1              0.01699869              0.03523575               0.1617706
#> 2              0.03819018              0.10522076               0.3164229
#>   MedianPredictedProbability P75PredictedProbability P95PredictedProbability
#> 1                  0.2999095               0.6587862               0.9383479
#> 2                  0.5173045               0.8868419               0.9549556
#>   MaxPredictedProbability evaluation
#> 1               0.9887700      Train
#> 2               0.9868817      Train
```
