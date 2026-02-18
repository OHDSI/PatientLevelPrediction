# covariateSummary

Summarises the covariateData to calculate the mean and standard
deviation per covariate if the labels are given it also stratifies this
by class label and if the trainRowIds and testRowIds specifying the
patients in the train/test sets respectively are input, these values are
also stratified by train and test set

## Usage

``` r
covariateSummary(
  covariateData,
  cohort,
  labels = NULL,
  strata = NULL,
  variableImportance = NULL,
  featureEngineering = NULL
)
```

## Arguments

- covariateData:

  The covariateData part of the plpData that is extracted using
  `getPlpData`

- cohort:

  The patient cohort to calculate the summary

- labels:

  A data.frame with the columns rowId and outcomeCount

- strata:

  A data.frame containing the columns rowId, strataName

- variableImportance:

  A data.frame with the columns covariateId and value (the variable
  importance value)

- featureEngineering:

  (currently not used ) A function or list of functions specifying any
  feature engineering to create covariates before summarising

## Value

A data.frame containing: CovariateCount, CovariateMean and
CovariateStDev for any specified stratification

## Details

The function calculates various metrics to measure the performance of
the model

## Examples

``` r
data("simulationProfile")
plpData <- simulatePlpData(simulationProfile, n = 100, seed = 42)
#> Generating covariates
#> Loading required namespace: FeatureExtraction
#> Generating cohorts
#> Generating outcomes
covariateSummary <- covariateSummary(plpData$covariateData, plpData$cohorts)
#> Calculating covariate summary @ 2026-02-18 11:55:20.227813
#> This can take a while...
#> calculating subset of strata 1
#> Restricting to subgroup
#> Calculating summary for subgroup 
#> Aggregating with no labels or strata
#> Finished covariate summary @ 2026-02-18 11:55:20.744504
#> Time to calculate covariate summary: 0.517 secs
head(covariateSummary)
#> # A tibble: 6 × 9
#>   covariateId covariateName     analysisId conceptId valueAsConceptId collisions
#>         <dbl> <chr>                  <dbl>     <dbl>            <dbl>      <dbl>
#> 1    80180102 condition_occurr…        102     80180               NA         NA
#> 2    81893102 condition_occurr…        102     81893               NA         NA
#> 3    30753102 condition_occurr…        102     30753               NA         NA
#> 4  4285898102 condition_occurr…        102   4285898               NA         NA
#> 5  4266809102 condition_occurr…        102   4266809               NA         NA
#> 6  4310024102 condition_occurr…        102   4310024               NA         NA
#> # ℹ 3 more variables: CovariateCount <dbl>, CovariateMean <dbl>,
#> #   CovariateStDev <dbl>
```
