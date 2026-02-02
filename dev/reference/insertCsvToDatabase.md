# Function to insert results into a database from csvs

This function converts a folder with csv results into plp objects and
loads them into a plp result database

## Usage

``` r
insertCsvToDatabase(
  csvFolder,
  connectionDetails,
  databaseSchemaSettings,
  modelSaveLocation,
  csvTableAppend = ""
)
```

## Arguments

- csvFolder:

  The location to the csv folder with the plp results

- connectionDetails:

  A connection details for the plp results database that the csv results
  will be inserted into

- databaseSchemaSettings:

  A object created by `createDatabaseSchemaSettings` with all the
  settings specifying the result tables to insert the csv results into

- modelSaveLocation:

  The location to save any models from the csv folder - this should be
  the same location you picked when inserting other models into the
  database

- csvTableAppend:

  A string that appends the csv file names

## Value

Returns a data.frame indicating whether the results were inported into
the database

## Details

The user needs to have plp csv results in a single folder and an
existing plp result database

## Examples
