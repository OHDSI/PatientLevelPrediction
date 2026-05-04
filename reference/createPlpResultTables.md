# Create the results tables to store PatientLevelPrediction models and results into a database

This function executes a large set of SQL statements to create tables
that can store models and results

## Usage

``` r
createPlpResultTables(
  connectionDetails,
  targetDialect = "postgresql",
  resultSchema,
  deleteTables = TRUE,
  createTables = TRUE,
  tablePrefix = "",
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
  testFile = NULL
)
```

## Arguments

- connectionDetails:

  The database connection details

- targetDialect:

  The database management system being used

- resultSchema:

  The name of the database schema that the result tables will be
  created.

- deleteTables:

  If true any existing tables matching the PatientLevelPrediction result
  tables names will be deleted

- createTables:

  If true the PatientLevelPrediction result tables will be created

- tablePrefix:

  A string that appends to the PatientLevelPrediction result tables

- tempEmulationSchema:

  The temp schema used when the database management system is oracle

- testFile:

  (used for testing) The location of an sql file with the table creation
  code

## Value

Returns NULL but creates or deletes the required tables in the specified
database schema(s).

## Details

This function can be used to create (or delete) PatientLevelPrediction
result tables

## Examples

``` r
# create a sqlite database with the PatientLevelPrediction result tables
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "sqlite",
  server = file.path(tempdir(), "test.sqlite"))
createPlpResultTables(connectionDetails = connectionDetails,
                      targetDialect = "sqlite",
                      resultSchema = "main",
                      tablePrefix = "plp_")
#> Connecting using SQLite driver
#> All or some PLP result tables do not exist, tables being recreated
#> Deleting existing tables
#> Creating PLP results tables
#>   |                                                                              |                                                                      |   0%  |                                                                              |==                                                                    |   3%  |                                                                              |=====                                                                 |   7%  |                                                                              |=======                                                               |  10%  |                                                                              |=========                                                             |  13%  |                                                                              |============                                                          |  17%  |                                                                              |==============                                                        |  20%  |                                                                              |================                                                      |  23%  |                                                                              |===================                                                   |  27%  |                                                                              |=====================                                                 |  30%  |                                                                              |=======================                                               |  33%  |                                                                              |==========================                                            |  37%  |                                                                              |============================                                          |  40%  |                                                                              |==============================                                        |  43%  |                                                                              |=================================                                     |  47%  |                                                                              |===================================                                   |  50%  |                                                                              |=====================================                                 |  53%  |                                                                              |========================================                              |  57%  |                                                                              |==========================================                            |  60%  |                                                                              |============================================                          |  63%  |                                                                              |===============================================                       |  67%  |                                                                              |=================================================                     |  70%  |                                                                              |===================================================                   |  73%  |                                                                              |======================================================                |  77%  |                                                                              |========================================================              |  80%  |                                                                              |==========================================================            |  83%  |                                                                              |=============================================================         |  87%  |                                                                              |===============================================================       |  90%  |                                                                              |=================================================================     |  93%  |                                                                              |====================================================================  |  97%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0221 secs
#> PLP result migration being applied
#> Migrating data set
#> Migrator using SQL files in PatientLevelPrediction
#> Connecting using SQLite driver
#> Creating migrations table
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00422 secs
#> Migrations table created
#> Executing migration: Migration_1-store_version.sql
#>   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00364 secs
#> Saving migration: Migration_1-store_version.sql
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00577 secs
#> Migration complete Migration_1-store_version.sql
#> Executing migration: Migration_2-add_hyperparameter_settings.sql
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00702 secs
#> Saving migration: Migration_2-add_hyperparameter_settings.sql
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00511 secs
#> Migration complete Migration_2-add_hyperparameter_settings.sql
#> Executing migration: Migration_3-add_model_name.sql
#>   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0041 secs
#> Saving migration: Migration_3-add_model_name.sql
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00329 secs
#> Migration complete Migration_3-add_model_name.sql
#> Closing database connection
#> Updating version number
#> Connecting using SQLite driver
#>   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00392 secs
# delete the tables
createPlpResultTables(connectionDetails = connectionDetails,
                      targetDialect = "sqlite",
                      resultSchema = "main",
                      deleteTables = TRUE,
                      createTables = FALSE,
                      tablePrefix = "plp_")
#> Connecting using SQLite driver
#> All or some PLP result tables do not exist, tables being recreated
#> Deleting existing tables
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0035 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00334 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00329 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00335 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00319 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0102 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00322 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00338 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0036 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0033 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0033 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00324 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0031 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00354 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00325 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00332 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0033 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00339 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0033 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00328 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00327 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00362 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00333 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00348 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00325 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0033 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0033 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00339 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00327 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00368 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00328 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00334 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00347 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00317 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00298 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00321 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00373 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00319 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00303 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00357 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0031 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00319 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0054 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00336 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00312 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00321 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0031 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00311 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0032 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00312 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00301 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0033 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00305 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0036 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00326 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00324 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00323 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00329 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00302 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00331 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00315 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00324 secs
#> Deleting PLP migration tables
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00297 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00318 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00295 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00409 secs
# clean up the database file
unlink(file.path(tempdir(), "test.sqlite"))
```
