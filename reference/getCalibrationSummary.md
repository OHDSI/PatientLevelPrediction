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
#> Population created with: 477 observations, 477 unique subjects and 235 outcomes
#> Population created in 0.0557 secs
data <- splitData(plpData, population, createDefaultSplitSetting())
#> test: 0.25
#> train: 0.75
#> nfold: 3
#> seed: 71823
#> Creating a 25% test and 75% train (into 3 folds) random stratified split by class
#> Data split into 118 test cases and 359 train cases (120, 120, 119)
#> Starting to limit covariate data to population...
#> Finished limiting covariate data to population...
#> Starting to limit covariate data to population...
#> Finished limiting covariate data to population...
#> Data split in 1.64 secs
data$Train$covariateData <- preprocessData(data$Train$covariateData)
#> minFraction: 0.001
#> normalize: TRUE
#> removeRedundancy: TRUE
#> Removing 1 redundant covariates
#> Removing 0 infrequent covariates
#> Normalizing covariates
#> Tidying covariates took 1.93 secs
saveLoc <- file.path(tempdir(), "calibrationSummary")
# fit a lasso logistic regression model using the training data
plpModel <- fitPlp(data$Train, modelSettings=setLassoLogisticRegression(seed=42),
                   analysisId=1, analysisPath=saveLoc)
#> Running Cyclops
#> Done.
#> GLM fit status:  OK
#> Returned from fitting Cyclops model
#> Getting variable importance
#> Creating variable importance data frame
#> Getting predictions on train set
#> predictProbabilities - predictAndromeda start
#> Prediction took 0.171 secs
#> Returned from classifier function
#> Time to fit model: 1.28 secs
calibrationSummary <- getCalibrationSummary(plpModel$prediction, 
                                            "binary", 
                                            numberOfStrata = 10,
                                            typeColumn = "evaluationType")
calibrationSummary
#>    predictionThreshold PersonCountAtRisk PersonCountWithOutcome
#> 1            0.0000000                66                     24
#> 2            0.3981933                43                     13
#> 3            0.4066764                36                     12
#> 4            0.4587164                49                     24
#> 5            0.4695045                35                     20
#> 6            0.4782993                22                     11
#> 7            0.5518748                36                     23
#> 8            0.5847190                37                     26
#> 9            0.6325440                35                     24
#> 10           0.0000000                36                     17
#> 11           0.4027178                37                     13
#> 12           0.4097031                57                     22
#> 13           0.4368572                42                     21
#> 14           0.4452362                15                      6
#> 15           0.4662635                31                     16
#> 16           0.4698311                33                     13
#> 17           0.5412586                36                     24
#> 18           0.5653457                39                     25
#> 19           0.6276576                33                     20
#>    averagePredictedProbability StDevPredictedProbability
#> 1                    0.3817281              0.0343801913
#> 2                    0.4065834              0.0006101161
#> 3                    0.4316532              0.0141562152
#> 4                    0.4691854              0.0016092770
#> 5                    0.4782993              0.0000000000
#> 6                    0.5093763              0.0199722588
#> 7                    0.5659203              0.0080275909
#> 8                    0.6213220              0.0156677432
#> 9                    0.6995556              0.0531940247
#> 10                   0.3461583              0.0455473704
#> 11                   0.4038822              0.0020546261
#> 12                   0.4281336              0.0095068764
#> 13                   0.4452362              0.0000000000
#> 14                   0.4642332              0.0039956637
#> 15                   0.4697746              0.0003144037
#> 16                   0.5180588              0.0262012831
#> 17                   0.5566002              0.0065681593
#> 18                   0.6025167              0.0223929956
#> 19                   0.6985194              0.0471177028
#>    MinPredictedProbability P25PredictedProbability MedianPredictedProbability
#> 1                0.2375306               0.3750528                  0.3981933
#> 2                0.4026756               0.4066764                  0.4066764
#> 3                0.4120927               0.4170064                  0.4256068
#> 4                0.4598014               0.4695045                  0.4695045
#> 5                0.4782993               0.4782993                  0.4782993
#> 6                0.4792075               0.4889493                  0.5113114
#> 7                0.5540393               0.5614325                  0.5627379
#> 8                0.5890836               0.6243069                  0.6313105
#> 9                0.6394826               0.6394826                  0.7061963
#> 10               0.1934243               0.3494836                  0.3494836
#> 11               0.4031680               0.4031680                  0.4031680
#> 12               0.4168477               0.4168477                  0.4368572
#> 13               0.4452362               0.4452362                  0.4452362
#> 14               0.4552583               0.4655432                  0.4662635
#> 15               0.4680805               0.4698311                  0.4698311
#> 16               0.4698897               0.4988611                  0.5351908
#> 17               0.5437398               0.5548147                  0.5583874
#> 18               0.5677383               0.5815392                  0.6050564
#> 19               0.6277126               0.6634596                  0.6912178
#>    P75PredictedProbability MaxPredictedProbability observedIncidence evaluation
#> 1                0.3981933               0.3981933         0.3636364      Train
#> 2                0.4066764               0.4066764         0.3023256      Train
#> 3                0.4416497               0.4587164         0.3333333      Train
#> 4                0.4695045               0.4695045         0.4897959      Train
#> 5                0.4782993               0.4782993         0.5714286      Train
#> 6                0.5225041               0.5486281         0.5000000      Train
#> 7                0.5700988               0.5818093         0.6388889      Train
#> 8                0.6313105               0.6325440         0.7027027      Train
#> 9                0.7363453               0.8046058         0.6857143      Train
#> 10               0.3617496               0.4009167         0.4722222         CV
#> 11               0.4031680               0.4097031         0.3513514         CV
#> 12               0.4368572               0.4368572         0.3859649         CV
#> 13               0.4452362               0.4452362         0.5000000         CV
#> 14               0.4662635               0.4662635         0.4000000         CV
#> 15               0.4698311               0.4698311         0.5161290         CV
#> 16               0.5375369               0.5375369         0.3939394         CV
#> 17               0.5623540               0.5637506         0.6666667         CV
#> 18               0.6204841               0.6276576         0.6410256         CV
#> 19               0.7220991               0.8018202         0.6060606         CV
# clean up
unlink(saveLoc, recursive = TRUE)
```
