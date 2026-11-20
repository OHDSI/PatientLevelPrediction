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

  The min value to show in cells that are sensitive. Counts below this
  value are replaced with -1, together with the values that give them
  back, such as covariate means, observed incidences, the rates of a
  threshold summary row and the predicted risks of a small group.

- sensitiveColumns:

  A named list, one element per result table (lower case, such as
  `"demographic_summary"`), each a list of column groups. A group is a
  character vector of count columns censored together, or a list with
  `counts` (the count columns), `alsoCensor` (columns censored with
  them) and, for long tables, `rows` (such as
  `list(metric = "outcomeCount")`, the rows that hold counts). The
  default covers the counts in the prediction distribution, covariate,
  calibration, demographic and threshold summaries, the attrition, the
  population size and outcome count of the evaluation statistics and the
  counts of the diagnostics. The attrition saved with each model in the
  `models` folder is censored too.

- fileAppend:

  If set to a string this will be appended to the start of the csv file
  names

## Value

The directory path where the results were saved

## Details

Extracts the results from a database into a set of csv files

## Examples
