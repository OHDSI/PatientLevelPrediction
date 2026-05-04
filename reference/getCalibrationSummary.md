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
# simulate data
data("simulationProfile")
plpData <- simulatePlpData(simulationProfile, n = 500, seed = 42)
#> Generating covariates
#> Generating cohorts
#> Generating outcomes
# create study population, split into train/test and preprocess with default settings
population <- createStudyPopulation(plpData, outcomeId = 3)
#> outcomeId: 3
#> binary: TRUE
#> includeAllOutcomes: TRUE
#> firstExposureOnly: FALSE
#> washoutPeriod: 0
#> removeSubjectsWithPriorOutcome: TRUE
#> priorOutcomeLookback: 99999
#> requireTimeAtRisk: TRUE
#> minTimeAtRisk: 364
#> restrictTarToCohortEnd: FALSE
#> riskWindowStart: 1
#> startAnchor: cohort start
#> riskWindowEnd: 365
#> endAnchor: cohort start
#> restrictTarToCohortEnd: FALSE
#> Removing subjects with prior outcomes (if any)
#> Removing non outcome subjects with insufficient time at risk (if any)
#> Outcome is 0 or 1
#> Population created with: 481 observations, 481 unique subjects and 249 outcomes
#> Population created in 0.046 secs
data <- splitData(plpData, population, createDefaultSplitSetting())
#> test: 0.25
#> train: 0.75
#> nfold: 3
#> seed: 71823
#> Creating a 25% test and 75% train (into 3 folds) random stratified split by class
#> Data split into 120 test cases and 361 train cases (121, 120, 120)
#> Starting to limit covariate data to population...
#> Finished limiting covariate data to population...
#> Starting to limit covariate data to population...
#> Finished limiting covariate data to population...
#> Data split in 1.42 secs
data$Train$covariateData <- preprocessData(data$Train$covariateData)
#> minFraction: 0.001
#> normalize: TRUE
#> removeRedundancy: TRUE
#> Removing 1 redundant covariates
#> Removing 0 infrequent covariates
#> Normalizing covariates
#> Tidying covariates took 1.66 secs
saveLoc <- file.path(tempdir(), "calibrationSummary")
# fit a lasso logistic regression model using the training data
plpModel <- fitPlp(data$Train, modelSettings=setLassoLogisticRegression(seed=42),
                   analysisId=1, analysisPath=saveLoc)
#> Running Cyclops
#> Done.
#> GLM fit status:  OK
#> Warning: Model had no non-zero coefficients so predicted same for all population...
#> Warning: Model had no non-zero coefficients so predicted same for all population...
#> Warning: Model had no non-zero coefficients so predicted same for all population...
#> Returned from fitting Cyclops model
#> Getting variable importance
#> Getting predictions on train set
#> predictProbabilities - predictAndromeda start
#> Warning: Model had no non-zero coefficients so predicted same for all population...
#> Prediction took 0.000654 secs
#> Returned from classifier function
#> Time to fit model: 0.406 secs
calibrationSummary <- getCalibrationSummary(plpModel$prediction, 
                                            "binary", 
                                            numberOfStrata = 10,
                                            typeColumn = "evaluationType")
calibrationSummary
#>   predictionThreshold PersonCountAtRisk PersonCountWithOutcome
#> 1           0.0000000               361                    187
#> 2           0.0000000               121                     63
#> 3           0.5166667               240                    124
#>   averagePredictedProbability StDevPredictedProbability MinPredictedProbability
#> 1                   0.5180055                         0               0.5180055
#> 2                   0.5166667                         0               0.5166667
#> 3                   0.5186722                         0               0.5186722
#>   P25PredictedProbability MedianPredictedProbability P75PredictedProbability
#> 1               0.5180055                  0.5180055               0.5180055
#> 2               0.5166667                  0.5166667               0.5166667
#> 3               0.5186722                  0.5186722               0.5186722
#>   MaxPredictedProbability observedIncidence evaluation
#> 1               0.5180055         0.5180055      Train
#> 2               0.5166667         0.5206612         CV
#> 3               0.5186722         0.5166667         CV
# clean up
unlink(saveLoc, recursive = TRUE)
```
