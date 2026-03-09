# Plot the Observed vs. expected incidence, by age and gender

Plot the Observed vs. expected incidence, by age and gender

## Usage

``` r
plotDemographicSummary(
  plpResult,
  typeColumn = "evaluation",
  saveLocation = NULL,
  fileName = "roc.png"
)
```

## Arguments

- plpResult:

  A plp result object as generated using the
  [`runPlp`](https://ohdsi.github.io/PatientLevelPrediction/reference/runPlp.md)
  function.

- typeColumn:

  The name of the column specifying the evaluation type

- saveLocation:

  Directory to save plot (if NULL plot is not saved)

- fileName:

  Name of the file to save to plot, for example 'plot.png'. See the
  function `ggsave` in the ggplot2 package for supported file formats.

## Value

A ggplot object. Use the
[`ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html) function
to save to file in a different format.

## Details

Create a plot showing the Observed vs. expected incidence, by age and
gender \#'

## Examples

``` r
# \donttest{
data("simulationProfile")
plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#> Generating covariates
#> Generating cohorts
#> Generating outcomes
saveLoc <- file.path(tempdir(), "plotDemographicSummary")
plpResult <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#> Use timeStamp: TRUE
#> Creating save directory at: /tmp/RtmpAqRl34/plotDemographicSummary/2026-03-09-3
#> Currently in a tryCatch or withCallingHandlers block, so unable to add global calling handlers. ParallelLogger will not capture R messages, errors, and warnings, only explicit calls to ParallelLogger. (This message will not be shown again this R session)
#> Patient-Level Prediction Package version 6.6.0
#> Study started at: 2026-03-09 14:03:34.720721
#> AnalysisID:         2026-03-09-3
#> AnalysisName:       Study details
#> TargetID:           1
#> OutcomeID:          3
#> Cohort size:        1000
#> Covariates:         98
#> Creating population
#> Outcome is 0 or 1
#> Population created with: 954 observations, 954 unique subjects and 434 outcomes
#> Population created in 0.0515 secs
#> seed: 123
#> Creating a 25% test and 75% train (into 3 folds) random stratified split by class
#> Data split into 238 test cases and 716 train cases (239, 239, 238)
#> Data split in 1.3 secs
#> Train Set:
#> Fold 1 239 patients with 109 outcomes - Fold 2 239 patients with 109 outcomes - Fold 3 238 patients with 108 outcomes
#> 66 covariates in train data
#> Test Set:
#> 238 patients with 108 outcomes
#> Removing 2 redundant covariates
#> Removing 0 infrequent covariates
#> Normalizing covariates
#> Tidying covariates took 1.51 secs
#> Train Set:
#> Fold 1 239 patients with 109 outcomes - Fold 2 239 patients with 109 outcomes - Fold 3 238 patients with 108 outcomes
#> 64 covariates in train data
#> Test Set:
#> 238 patients with 108 outcomes
#> 
#> Running Cyclops
#> Done.
#> GLM fit status:  OK
#> Creating variable importance data frame
#> Prediction took 0.182 secs
#> Time to fit model: 0.982 secs
#> Removing infrequent and redundant covariates and normalizing
#> Removing infrequent and redundant covariates covariates and normalizing took 0.442 secs
#> Prediction took 0.166 secs
#> Prediction done in: 1.02 secs
#> Calculating Performance for Test
#> =============
#> AUC                 61.40
#> 95% lower AUC:      54.59
#> 95% upper AUC:      68.21
#> AUPRC:              57.58
#> Brier:              0.23
#> Eavg:               0.06
#> Calibration in large- Mean predicted risk 0.4489 : observed risk 0.4538
#> Calibration in large- Intercept 0.245
#> Weak calibration intercept: 0.245 - gradient:2.0312
#> Hosmer-Lemeshow calibration gradient: 2.10 intercept:         -0.56
#> Average Precision:  0.58
#> Calculating Performance for Train
#> =============
#> AUC                 59.21
#> 95% lower AUC:      55.21
#> 95% upper AUC:      63.20
#> AUPRC:              55.33
#> Brier:              0.24
#> Eavg:               0.02
#> Calibration in large- Mean predicted risk 0.4553 : observed risk 0.4553
#> Calibration in large- Intercept 0.0374
#> Weak calibration intercept: 0.0374 - gradient:1.2041
#> Hosmer-Lemeshow calibration gradient: 1.17 intercept:         -0.08
#> Average Precision:  0.55
#> Calculating Performance for CV
#> =============
#> AUC                 56.43
#> 95% lower AUC:      52.21
#> 95% upper AUC:      60.66
#> AUPRC:              52.00
#> Brier:              0.24
#> Eavg:               0.01
#> Calibration in large- Mean predicted risk 0.4555 : observed risk 0.4553
#> Calibration in large- Intercept 0.0072
#> Weak calibration intercept: 0.0072 - gradient:1.0442
#> Hosmer-Lemeshow calibration gradient: 0.97 intercept:         0.01
#> Average Precision:  0.52
#> Time to calculate evaluation metrics: 0.228 secs
#> Calculating covariate summary @ 2026-03-09 14:03:40.175684
#> This can take a while...
#> Creating binary labels
#> Joining with strata
#> calculating subset of strata 1
#> calculating subset of strata 2
#> calculating subset of strata 3
#> calculating subset of strata 4
#> Restricting to subgroup
#> Calculating summary for subgroup TrainWithOutcome
#> Restricting to subgroup
#> Calculating summary for subgroup TrainWithNoOutcome
#> Restricting to subgroup
#> Calculating summary for subgroup TestWithOutcome
#> Restricting to subgroup
#> Calculating summary for subgroup TestWithNoOutcome
#> Aggregating with labels and strata
#> Finished covariate summary @ 2026-03-09 14:03:42.162293
#> Time to calculate covariate summary: 1.99 secs
#> Run finished successfully.
#> Saving PlpResult
#> Creating directory to save model
#> plpResult saved to ..\/tmp/RtmpAqRl34/plotDemographicSummary/2026-03-09-3\plpResult
#> runPlp time taken: 7.48 secs
plotDemographicSummary(plpResult)

# clean up 
unlink(saveLoc, recursive = TRUE)
# }
```
