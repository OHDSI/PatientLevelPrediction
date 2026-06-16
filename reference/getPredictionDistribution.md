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
#> 1     0          51                   0.5128484                 0.2711195
#> 2     1          49                   0.5425417                 0.2844193
#>   MinPredictedProbability P05PredictedProbability P25PredictedProbability
#> 1             0.035629275              0.09752239               0.3078378
#> 2             0.008196403              0.06438216               0.3116165
#>   MedianPredictedProbability P75PredictedProbability P95PredictedProbability
#> 1                  0.4624087               0.7325787               0.9203973
#> 2                  0.5785027               0.7492125               0.9456908
#>   MaxPredictedProbability evaluation
#> 1               0.9747146      Train
#> 2               0.9991571      Train
```
