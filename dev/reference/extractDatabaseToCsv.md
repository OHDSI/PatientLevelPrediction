# Exports all the results from a database into csv files

Exports all the results from a database into csv files

## Usage

``` r
extractDatabaseToCsv(
  conn = NULL,
  connectionDetails,
  databaseSchemaSettings = createDatabaseSchemaSettings(resultSchema = "main"),
  csvFolder,
  minCellCount = 5,
  sensitiveColumns = getPlpSensitiveColumns(),
  fileAppend = NULL
)
```

## Arguments

- conn:

  The connection to the database with the results

- connectionDetails:

  The connectionDetails for the result database

- databaseSchemaSettings:

  The result database schema settings

- csvFolder:

  Location to save the csv files

- minCellCount:

  The min value to show in cells that are sensitive (values less than
  this value will be replaced with -1)

- sensitiveColumns:

  A named list (name of table columns belong to) with a list of columns
  to apply the minCellCount to.

- fileAppend:

  If set to a string this will be appended to the start of the csv file
  names

## Value

The directory path where the results were saved

## Details

Extracts the results from a database into a set of csv files

## Examples
