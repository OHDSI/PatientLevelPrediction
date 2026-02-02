# Create the PatientLevelPrediction database result schema settings

This function specifies where the results schema is and lets you pick a
different schema for the cohorts and databases

## Usage

``` r
createDatabaseSchemaSettings(
  resultSchema = "main",
  tablePrefix = "",
  targetDialect = "sqlite",
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
  cohortDefinitionSchema = resultSchema,
  tablePrefixCohortDefinitionTables = tablePrefix,
  databaseDefinitionSchema = resultSchema,
  tablePrefixDatabaseDefinitionTables = tablePrefix
)
```

## Arguments

- resultSchema:

  (string) The name of the database schema with the result tables.

- tablePrefix:

  (string) A string that appends to the PatientLevelPrediction result
  tables

- targetDialect:

  (string) The database management system being used

- tempEmulationSchema:

  (string) The temp schema used when the database management system is
  oracle

- cohortDefinitionSchema:

  (string) The name of the database schema with the cohort definition
  tables (defaults to resultSchema).

- tablePrefixCohortDefinitionTables:

  (string) A string that appends to the cohort definition tables

- databaseDefinitionSchema:

  (string) The name of the database schema with the database definition
  tables (defaults to resultSchema).

- tablePrefixDatabaseDefinitionTables:

  (string) A string that appends to the database definition tables

## Value

Returns a list of class 'plpDatabaseResultSchema' with all the database
settings

## Details

This function can be used to specify the database settings used to
upload PatientLevelPrediction results into a database

## Examples

``` r
createDatabaseSchemaSettings(resultSchema = "cdm",
                             tablePrefix = "plp_")
#> $resultSchema
#> [1] "cdm"
#> 
#> $tablePrefix
#> [1] "PLP_"
#> 
#> $targetDialect
#> [1] "sqlite"
#> 
#> $tempEmulationSchema
#> NULL
#> 
#> $cohortDefinitionSchema
#> [1] "cdm"
#> 
#> $tablePrefixCohortDefinitionTables
#> [1] "PLP_"
#> 
#> $databaseDefinitionSchema
#> [1] "cdm"
#> 
#> $tablePrefixDatabaseDefinitionTables
#> [1] "PLP_"
#> 
#> attr(,"class")
#> [1] "plpDatabaseResultSchema"
```
