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
gender

The shaded band around the expected line is the mean predicted risk plus
or minus 1.96 standard deviations of the individual predicted risks in
each group. It shows how much predicted risks vary within the group, and
is not a confidence interval for the mean or for the observed incidence.
\#'

## Examples

``` r
# \donttest{
data("simulationProfile")
plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#> Generating covariates
#> Generating cohorts
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpdnvUil/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> Generating outcomes
saveLoc <- file.path(tempdir(), "plotDemographicSummary")
plpResult <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#> Use timeStamp: TRUE
#> Creating save directory at: /tmp/RtmpdnvUil/plotDemographicSummary/2026-11-17-3
#> Currently in a tryCatch or withCallingHandlers block, so unable to add global calling handlers. ParallelLogger will not capture R messages, errors, and warnings, only explicit calls to ParallelLogger. (This message will not be shown again this R session)
#> Patient-Level Prediction Package version 6.7.0
#> Study started at: 2026-11-17 15:36:05.178081
#> AnalysisID:         2026-11-17-3
#> AnalysisName:       Study details
#> TargetID:           1
#> OutcomeID:          3
#> Cohort size:        1000
#> Covariates:         98
#> Creating population
#> Outcome is 0 or 1
#> Population created with: 965 observations, 965 unique subjects and 493 outcomes
#> Population created in 0.0453 secs
#> seed: 123
#> Creating a 25% test and 75% train (into 3 folds) random stratified split by class
#> Data split into 241 test cases and 724 train cases (242, 241, 241)
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpdnvUil/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpdnvUil/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpdnvUil/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpdnvUil/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> ℹ This message has been shown 60 times and will not be shown again this session.
#> Data split in 1.24 secs
#> Train Set:
#> Fold 1 242 patients with 124 outcomes - Fold 2 241 patients with 123 outcomes - Fold 3 241 patients with 123 outcomes
#> 62 covariates in train data
#> Test Set:
#> 241 patients with 123 outcomes
#> Removing 3 redundant covariates
#> Removing 0 infrequent covariates
#> Normalizing covariates
#> Tidying covariates took 1.23 secs
#> Train Set:
#> Fold 1 242 patients with 124 outcomes - Fold 2 241 patients with 123 outcomes - Fold 3 241 patients with 123 outcomes
#> 59 covariates in train data
#> Test Set:
#> 241 patients with 123 outcomes
#> 
#> Running Cyclops
#> Done.
#> GLM fit status:  OK
#> Creating variable importance data frame
#> Prediction took 0.133 secs
#> Time to fit model: 0.315 secs
#> Removing infrequent and redundant covariates and normalizing
#> Removing infrequent and redundant covariates covariates and normalizing took 0.207 secs
#> Prediction took 0.143 secs
#> Prediction done in: 0.465 secs
#> Calculating Performance for Test
#> =============
#> AUC                 60.79
#> 95% lower AUC:      53.69
#> 95% upper AUC:      67.90
#> AUPRC:              64.91
#> Brier:              0.24
#> Eavg:               0.02
#> Calibration in large- Mean predicted risk 0.5114 : observed risk 0.5104
#> Calibration in large- Intercept -0.0135
#> Weak calibration intercept: -0.0135 - gradient:1.263
#> Hosmer-Lemeshow calibration gradient: 1.29 intercept:         -0.15
#> Average Precision:  0.65
#> Calculating Performance for Train
#> =============
#> AUC                 63.62
#> 95% lower AUC:      59.59
#> 95% upper AUC:      67.65
#> AUPRC:              65.33
#> Brier:              0.24
#> Eavg:               0.03
#> Calibration in large- Mean predicted risk 0.511 : observed risk 0.511
#> Calibration in large- Intercept -0.0126
#> Weak calibration intercept: -0.0126 - gradient:1.3798
#> Hosmer-Lemeshow calibration gradient: 1.37 intercept:         -0.19
#> Average Precision:  0.65
#> Calculating Performance for CV
#> =============
#> AUC                 57.52
#> 95% lower AUC:      53.37
#> 95% upper AUC:      61.67
#> AUPRC:              59.28
#> Brier:              0.24
#> Eavg:               0.01
#> Calibration in large- Mean predicted risk 0.5109 : observed risk 0.511
#> Calibration in large- Intercept 0.0034
#> Weak calibration intercept: 0.0034 - gradient:0.9317
#> Hosmer-Lemeshow calibration gradient: 0.99 intercept:         0.00
#> Average Precision:  0.59
#> Time to calculate evaluation metrics: 0.205 secs
#> Calculating covariate summary @ 2026-11-17 15:36:09.016168
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
#> Finished covariate summary @ 2026-11-17 15:36:09.58451
#> Time to calculate covariate summary: 0.569 secs
#> Run finished successfully.
#> Saving PlpResult
#> Creating directory to save model
#> plpResult saved to ..\/tmp/RtmpdnvUil/plotDemographicSummary/2026-11-17-3\plpResult
#> runPlp time taken: 4.44 secs
plotDemographicSummary(plpResult)

# clean up 
unlink(saveLoc, recursive = TRUE)
# }
```
