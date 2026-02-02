# Extract the patient level prediction data from the server

This function executes a large set of SQL statements against the
database in OMOP CDM format to extract the data needed to perform the
analysis.

## Usage

``` r
getPlpData(databaseDetails, covariateSettings, restrictPlpDataSettings = NULL)
```

## Arguments

- databaseDetails:

  The cdm database details created using
  [`createDatabaseDetails()`](https://ohdsi.github.io/PatientLevelPrediction/dev/reference/createDatabaseDetails.md)

- covariateSettings:

  An object of type `covariateSettings` or a list of such objects as
  created using the `createCovariateSettings` function in the
  `FeatureExtraction` package.

- restrictPlpDataSettings:

  Extra settings to apply to the target population while extracting
  data. Created using
  [`createRestrictPlpDataSettings()`](https://ohdsi.github.io/PatientLevelPrediction/dev/reference/createRestrictPlpDataSettings.md).
  This is optional.

## Value

'r plpDataObjectDoc()\`

## Details

Based on the arguments, the at risk cohort data is retrieved, as well as
outcomes occurring in these subjects. The at risk cohort is identified
through user-defined cohorts in a cohort table either inside the CDM
instance or in a separate schema. Similarly, outcomes are identified
through user-defined cohorts in a cohort table either inside the CDM
instance or in a separate schema. Covariates are automatically extracted
from the appropriate tables within the CDM. If you wish to exclude
concepts from covariates you will need to manually add the concept_ids
and descendants to the `excludedCovariateConceptIds` of the
`covariateSettings` argument.

## Examples
