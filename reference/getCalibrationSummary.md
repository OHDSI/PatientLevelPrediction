# Get a sparse summary of the calibration

Get a sparse summary of the calibration

## Usage

``` r
getCalibrationSummary(
  prediction,
  predictionType,
  typeColumn = "evaluation",
  numberOfStrata = 10,
  truncateFraction = 0.05
)
```

## Arguments

- prediction:

  A prediction object as generated using the
  [`predict`](https://rdrr.io/r/stats/predict.html) functions.

- predictionType:

  The type of prediction (binary or survival)

- typeColumn:

  A column that is used to stratify the results

- numberOfStrata:

  The number of strata in the plot.

- truncateFraction:

  This fraction of probability values will be ignored when plotting, to
  avoid the x-axis scale being dominated by a few outliers.

## Value

A dataframe with the calibration summary

## Details

Generates a sparse summary showing the predicted probabilities and the
observed fractions. Predictions are stratified into equally sized bins
of predicted probabilities.

## Examples

``` r
# mock prediction dataframe
set.seed(42)
predictedRisk <- stats::runif(200)
outcomeCount <- stats::rbinom(200, 1, predictedRisk)
prediction <- data.frame(
  rowId = seq_len(200),
  value = predictedRisk,
  outcomeCount = outcomeCount,
  evaluationType = "Test"
)
calibrationSummary <- getCalibrationSummary(
  prediction,
  "binary",
  numberOfStrata = 10,
  typeColumn = "evaluationType"
)
calibrationSummary
#>    predictionThreshold PersonCountAtRisk PersonCountWithOutcome
#> 1            0.0000000                20                      2
#> 2            0.1173561                20                      5
#> 3            0.1973272                20                      7
#> 4            0.3303762                20                      5
#> 5            0.4424904                20                      7
#> 6            0.5554134                20                     16
#> 7            0.6433001                20                     14
#> 8            0.7348425                20                     14
#> 9            0.7977286                20                     17
#> 10           0.9179193                20                     19
#>    averagePredictedProbability StDevPredictedProbability
#> 1                   0.04894757                0.04204267
#> 2                   0.15631803                0.02570906
#> 3                   0.25747157                0.04101106
#> 4                   0.38632217                0.03116537
#> 5                   0.49870612                0.03497119
#> 6                   0.60232644                0.02534790
#> 7                   0.70145232                0.02850449
#> 8                   0.76252240                0.01966478
#> 9                   0.85854559                0.03773604
#> 10                  0.94954756                0.02125922
#>    MinPredictedProbability P25PredictedProbability MedianPredictedProbability
#> 1             0.0002388966             0.006487695                 0.04096264
#> 2             0.1174873617             0.135451997                 0.14875933
#> 3             0.1974103423             0.217010101                 0.25734470
#> 4             0.3330719834             0.358451729                 0.38957425
#> 5             0.4469696281             0.463625369                 0.51430975
#> 6             0.5603327462             0.581158227                 0.60620577
#> 7             0.6456318784             0.678263858                 0.71208852
#> 8             0.7365883146             0.743777915                 0.76202601
#> 9             0.8110551413             0.828746219                 0.84324564
#> 10            0.9180564173             0.934785277                 0.94358801
#>    P75PredictedProbability MaxPredictedProbability observedIncidence evaluation
#> 1               0.08323118               0.1161747              0.10       Test
#> 2               0.18119234               0.1969945              0.25       Test
#> 3               0.28921827               0.3240860              0.35       Test
#> 4               0.41094436               0.4357716              0.25       Test
#> 5               0.53271498               0.5504941              0.35       Test
#> 6               0.62027279               0.6417455              0.80       Test
#> 7               0.72580572               0.7340943              0.70       Test
#> 8               0.77894916               0.7943970              0.70       Test
#> 9               0.90373374               0.9179040              0.85       Test
#> 10              0.96469766               0.9888917              0.95       Test
```
