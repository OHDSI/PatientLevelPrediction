# Extracts covariates based on cohorts

Extracts covariates based on cohorts

## Usage

``` r
createCohortCovariateSettings(
  cohortName,
  settingId,
  cohortDatabaseSchema = NULL,
  cohortTable = NULL,
  cohortId,
  startDay = -30,
  endDay = 0,
  count = FALSE,
  ageInteraction = FALSE,
  lnAgeInteraction = FALSE,
  analysisId = 456
)
```

## Arguments

- cohortName:

  Name for the cohort

- settingId:

  An id between 1 and 100 for this combination of time window (startDay,
  endDay) and value (count, ageInteraction, lnAgeInteraction). It is
  part of the covariateId (cohortId x 100000 + settingId x 1000 +
  analysisId), so settings for the same cohort need different
  settingIds. Using the same settingId for the same window and value
  across cohorts keeps covariateIds readable.

- cohortDatabaseSchema:

  The schema of the database with the cohort. If nothing is specified
  then the cohortDatabaseSchema from databaseDetails at runtime is used.

- cohortTable:

  the table name that contains the covariate cohort. If nothing is
  specified then the cohortTable from databaseDetails at runtime is
  used.

- cohortId:

  cohort id for the covariate cohort

- startDay:

  The number of days prior to index to start observing the cohort

- endDay:

  The number of days prior to index to stop observing the cohort

- count:

  If FALSE the covariate value is binary (1 means cohort occurred
  between index+startDay and index+endDay, 0 means it did not) If TRUE
  then the covariate value is the number of unique cohort_start_dates
  between index+startDay and index+endDay

- ageInteraction:

  If TRUE multiple covariate value by the patient's age in years

- lnAgeInteraction:

  If TRUE multiple covariate value by the log of the patient's age in
  years

- analysisId:

  The analysisId for the covariate. Count or age-interaction covariates
  are reported with analysisId + 1000 in the covariateRef and
  analysisRef

## Value

An object of class `covariateSettings` specifying how to create the
cohort covariate with the covariateId cohortId x 100000 + settingId x
1000 + analysisId. Its `reportedAnalysisId` element is the analysisId
the covariate gets in the covariateRef and analysisRef.

## Details

The user specifies a cohort and time period and then a covariate is
constructed whether they are in the cohort during the time periods
relative to target population cohort index

## Examples

``` r
createCohortCovariateSettings(cohortName="testCohort",
                              settingId=1,
                              cohortId=1,
                              cohortDatabaseSchema="cohorts",
                              cohortTable="cohort_table")
#> $covariateName
#> [1] "testCohort"
#> 
#> $covariateId
#> [1] 101456
#> 
#> $cohortDatabaseSchema
#> [1] "cohorts"
#> 
#> $cohortTable
#> [1] "cohort_table"
#> 
#> $cohortIds
#> [1] 1
#> 
#> $startDay
#> [1] -30
#> 
#> $endDays
#> [1] 0
#> 
#> $count
#> [1] FALSE
#> 
#> $ageInteraction
#> [1] FALSE
#> 
#> $lnAgeInteraction
#> [1] FALSE
#> 
#> $analysisId
#> [1] 456
#> 
#> $reportedAnalysisId
#> [1] 456
#> 
#> attr(,"fun")
#> [1] "PatientLevelPrediction::getCohortCovariateData"
#> attr(,"class")
#> [1] "covariateSettings"
```
