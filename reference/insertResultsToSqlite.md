# Create sqlite database with the results

This function create an sqlite database with the PLP result schema and
inserts all results

## Usage

``` r
insertResultsToSqlite(
  resultLocation,
  cohortDefinitions = NULL,
  databaseList = NULL,
  sqliteLocation = file.path(resultLocation, "sqlite"),
  skipDiagnostics = FALSE
)
```

## Arguments

- resultLocation:

  (string) location of directory where the main package results were
  saved

- cohortDefinitions:

  A set of one or more cohorts extracted using
  ROhdsiWebApi::exportCohortDefinitionSet()

- databaseList:

  A list created by `createDatabaseList` to specify the databases

- sqliteLocation:

  (string) location of directory where the sqlite database will be saved

- skipDiagnostics:

  Whether to skip uploading the diagnostics

## Value

Returns the location of the sqlite database file

## Details

This function can be used upload PatientLevelPrediction results into an
sqlite database

## Examples
