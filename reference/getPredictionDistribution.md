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
#> 1     0          60                   0.4887031                 0.2945912
#> 2     1          40                   0.4652769                 0.3049600
#>   MinPredictedProbability P05PredictedProbability P25PredictedProbability
#> 1            0.0034677039              0.05135562               0.2115317
#> 2            0.0004050434              0.02626329               0.2207261
#>   MedianPredictedProbability P75PredictedProbability P95PredictedProbability
#> 1                  0.5178518               0.7429045               0.9211282
#> 2                  0.4291551               0.7275602               0.9626023
#>   MaxPredictedProbability evaluation
#> 1               0.9959923      Train
#> 2               0.9930453      Train
```
