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
#> Executing SQL took 0.0219 secs
#> PLP result migration being applied
#> Migrating data set
#> Migrator using SQL files in PatientLevelPrediction
#> Connecting using SQLite driver
#> Creating migrations table
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00395 secs
#> Migrations table created
#> Executing migration: Migration_1-store_version.sql
#>   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0038 secs
#> Saving migration: Migration_1-store_version.sql
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00502 secs
#> Migration complete Migration_1-store_version.sql
#> Executing migration: Migration_2-add_hyperparameter_settings.sql
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0068 secs
#> Saving migration: Migration_2-add_hyperparameter_settings.sql
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00482 secs
#> Migration complete Migration_2-add_hyperparameter_settings.sql
#> Executing migration: Migration_3-add_model_name.sql
#>   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00415 secs
#> Saving migration: Migration_3-add_model_name.sql
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00333 secs
#> Migration complete Migration_3-add_model_name.sql
#> Closing database connection
#> Updating version number
#> Connecting using SQLite driver
#>   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00382 secs
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
#> Executing SQL took 0.00323 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00341 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00331 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00344 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00263 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00333 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00263 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00274 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00267 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00257 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00248 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00261 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00249 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00267 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00259 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00265 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00248 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00261 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00249 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00264 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00251 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00289 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00259 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00258 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00247 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00262 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00254 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00261 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00246 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00289 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00248 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00272 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00274 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0027 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00249 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00259 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0025 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00254 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00256 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0031 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00263 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00271 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00253 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00255 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00243 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00255 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00258 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00255 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00263 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00274 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00248 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00271 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00247 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00258 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00293 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00256 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00242 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0026 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00235 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00241 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00242 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00274 secs
#> Deleting PLP migration tables
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00234 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00251 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00243 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00364 secs
# clean up the database file
unlink(file.path(tempdir(), "test.sqlite"))
```
