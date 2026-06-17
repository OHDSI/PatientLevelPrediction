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
#> Creating save directory at: /tmp/RtmphVamTI/plotDemographicSummary/2026-06-17-3
#> Currently in a tryCatch or withCallingHandlers block, so unable to add global calling handlers. ParallelLogger will not capture R messages, errors, and warnings, only explicit calls to ParallelLogger. (This message will not be shown again this R session)
#> Patient-Level Prediction Package version 6.6.0
#> Study started at: 2026-06-17 14:02:46.390289
#> AnalysisID:         2026-06-17-3
#> AnalysisName:       Study details
#> TargetID:           1
#> OutcomeID:          3
#> Cohort size:        1000
#> Covariates:         98
#> Creating population
#> Outcome is 0 or 1
#> Population created with: 961 observations, 961 unique subjects and 499 outcomes
#> Population created in 0.051 secs
#> seed: 123
#> Creating a 25% test and 75% train (into 3 folds) random stratified split by class
#> Data split into 239 test cases and 722 train cases (241, 241, 240)
#> Data split in 1.3 secs
#> Train Set:
#> Fold 1 241 patients with 125 outcomes - Fold 2 241 patients with 125 outcomes - Fold 3 240 patients with 125 outcomes
#> 67 covariates in train data
#> Test Set:
#> 239 patients with 124 outcomes
#> Removing 2 redundant covariates
#> Removing 0 infrequent covariates
#> Normalizing covariates
#> Tidying covariates took 1.49 secs
#> Train Set:
#> Fold 1 241 patients with 125 outcomes - Fold 2 241 patients with 125 outcomes - Fold 3 240 patients with 125 outcomes
#> 65 covariates in train data
#> Test Set:
#> 239 patients with 124 outcomes
#> 
#> Running Cyclops
#> Done.
#> GLM fit status:  OK
#> Creating variable importance data frame
#> Prediction took 0.178 secs
#> Time to fit model: 0.906 secs
#> Removing infrequent and redundant covariates and normalizing
#> Removing infrequent and redundant covariates covariates and normalizing took 0.416 secs
#> Prediction took 0.175 secs
#> Prediction done in: 0.985 secs
#> Calculating Performance for Test
#> =============
#> AUC                 61.69
#> 95% lower AUC:      54.93
#> 95% upper AUC:      68.46
#> AUPRC:              61.18
#> Brier:              0.24
#> Eavg:               0.02
#> Calibration in large- Mean predicted risk 0.5272 : observed risk 0.5188
#> Calibration in large- Intercept -0.028
#> Weak calibration intercept: -0.028 - gradient:0.9273
#> Hosmer-Lemeshow calibration gradient: 1.17 intercept:         -0.13
#> Average Precision:  0.64
#> Calculating Performance for Train
#> =============
#> AUC                 63.34
#> 95% lower AUC:      59.54
#> 95% upper AUC:      67.14
#> AUPRC:              65.93
#> Brier:              0.23
#> Eavg:               0.01
#> Calibration in large- Mean predicted risk 0.5194 : observed risk 0.5194
#> Calibration in large- Intercept -0.0099
#> Weak calibration intercept: -0.0099 - gradient:1.1658
#> Hosmer-Lemeshow calibration gradient: 1.17 intercept:         -0.08
#> Average Precision:  0.67
#> Calculating Performance for CV
#> =============
#> AUC                 61.94
#> 95% lower AUC:      57.89
#> 95% upper AUC:      65.99
#> AUPRC:              64.20
#> Brier:              0.24
#> Eavg:               0.02
#> Calibration in large- Mean predicted risk 0.519 : observed risk 0.5194
#> Calibration in large- Intercept -0.0092
#> Weak calibration intercept: -0.0092 - gradient:1.1791
#> Hosmer-Lemeshow calibration gradient: 1.20 intercept:         -0.11
#> Average Precision:  0.64
#> Time to calculate evaluation metrics: 0.212 secs
#> Calculating covariate summary @ 2026-06-17 14:02:51.683329
#> This can take a while...
#> Creating binary labels
#> Joining with strata
#> calculating subset of strata 1
#> calculating subset of strata 2
#> calculating subset of strata 3
#> calculating subset of strata 4
#> Restricting to subgroup
#> Calculating summary for subgroup TrainWithNoOutcome
#> Restricting to subgroup
#> Calculating summary for subgroup TrainWithOutcome
#> Restricting to subgroup
#> Calculating summary for subgroup TestWithOutcome
#> Restricting to subgroup
#> Calculating summary for subgroup TestWithNoOutcome
#> Aggregating with labels and strata
#> Finished covariate summary @ 2026-06-17 14:02:53.613975
#> Time to calculate covariate summary: 1.93 secs
#> Run finished successfully.
#> Saving PlpResult
#> Creating directory to save model
#> plpResult saved to ..\/tmp/RtmphVamTI/plotDemographicSummary/2026-06-17-3\plpResult
#> runPlp time taken: 7.26 secs
plotDemographicSummary(plpResult)

# clean up 
unlink(saveLoc, recursive = TRUE)
# }
```
