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
#> Executing SQL took 0.0171 secs
#> PLP result migration being applied
#> Migrating data set
#> Migrator using SQL files in PatientLevelPrediction
#> Connecting using SQLite driver
#> Creating migrations table
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00369 secs
#> Migrations table created
#> Executing migration: Migration_1-store_version.sql
#>   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00241 secs
#> Saving migration: Migration_1-store_version.sql
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00361 secs
#> Migration complete Migration_1-store_version.sql
#> Executing migration: Migration_2-add_hyperparameter_settings.sql
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00369 secs
#> Saving migration: Migration_2-add_hyperparameter_settings.sql
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00333 secs
#> Migration complete Migration_2-add_hyperparameter_settings.sql
#> Executing migration: Migration_3-add_model_name.sql
#>   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00278 secs
#> Saving migration: Migration_3-add_model_name.sql
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00215 secs
#> Migration complete Migration_3-add_model_name.sql
#> Closing database connection
#> Updating version number
#> Connecting using SQLite driver
#>   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0029 secs
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
#> Executing SQL took 0.00285 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00226 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00206 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00206 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00187 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0022 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00187 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00201 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00193 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00213 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0021 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00217 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.002 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00204 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.002 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00207 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00203 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00203 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00206 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00206 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00201 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00205 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00201 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00207 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.002 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00202 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.002 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00207 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00201 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00238 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00184 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00186 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00191 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00189 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00191 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0019 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00188 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00202 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00204 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00206 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00191 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00199 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00195 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00217 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00192 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00204 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0019 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00198 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00186 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00194 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00188 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00205 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0019 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00207 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00188 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00195 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00186 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0091 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00173 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00176 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00176 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00177 secs
#> Deleting PLP migration tables
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00179 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00189 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00176 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00192 secs
# clean up the database file
unlink(file.path(tempdir(), "test.sqlite"))
```
