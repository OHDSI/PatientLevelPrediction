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
#> 1     0          54                   0.4002458                 0.3123997
#> 2     1          46                   0.5153511                 0.2969872
#>   MinPredictedProbability P05PredictedProbability P25PredictedProbability
#> 1              0.01699869              0.03513347               0.1453738
#> 2              0.03819018              0.10698287               0.2387856
#>   MedianPredictedProbability P75PredictedProbability P95PredictedProbability
#> 1                  0.2899180               0.6630212               0.9406494
#> 2                  0.4543858               0.8312385               0.9198616
#>   MaxPredictedProbability evaluation
#> 1               0.9887700      Train
#> 2               0.9868817      Train
```
