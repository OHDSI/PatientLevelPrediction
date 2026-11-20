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
#> 1     0          49                   0.5134984                 0.2947246
#> 2     1          51                   0.6333748                 0.2681453
#>   MinPredictedProbability P05PredictedProbability P25PredictedProbability
#> 1              0.01282715              0.05044932               0.2656927
#> 2              0.02208828              0.11347898               0.4752186
#>   MedianPredictedProbability P75PredictedProbability P95PredictedProbability
#> 1                  0.5274747               0.8072410               0.9280165
#> 2                  0.6639279               0.8553189               0.9547284
#>   MaxPredictedProbability evaluation
#> 1               0.9735457      Train
#> 2               0.9801129      Train
```
