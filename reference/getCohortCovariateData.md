# Extracts covariates based on cohorts

Extracts covariates based on cohorts

## Usage

``` r
getCohortCovariateData(
  connection,
  tempEmulationSchema = NULL,
  oracleTempSchema = NULL,
  cdmDatabaseSchema,
  cdmVersion = "5",
  cohortTable = "#cohort_person",
  rowIdField = "row_id",
  aggregated,
  cohortIds,
  covariateSettings,
  ...
)
```

## Arguments

- connection:

  The database connection

- tempEmulationSchema:

  The schema to use for temp tables

- oracleTempSchema:

  DEPRECATED The temp schema if using oracle

- cdmDatabaseSchema:

  The schema of the OMOP CDM data

- cdmVersion:

  version of the OMOP CDM data

- cohortTable:

  the table name that contains the target population cohort

- rowIdField:

  string representing the unique identifier in the target population
  cohort

- aggregated:

  whether the covariate should be aggregated

- cohortIds:

  cohort id for the target cohort

- covariateSettings:

  settings for the covariate cohorts and time periods

- ...:

  additional arguments from FeatureExtraction

## Value

CovariateData object with covariates, covariateRef, and analysisRef
tables

## Details

The user specifies a cohort and time period and then a covariate is
constructed whether they are in the cohort during the time periods
relative to target population cohort index

## Examples
