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
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |==                                                                    |   3%
  |                                                                            
  |=====                                                                 |   7%
  |                                                                            
  |=======                                                               |  10%
  |                                                                            
  |=========                                                             |  13%
  |                                                                            
  |============                                                          |  17%
  |                                                                            
  |==============                                                        |  20%
  |                                                                            
  |================                                                      |  23%
  |                                                                            
  |===================                                                   |  27%
  |                                                                            
  |=====================                                                 |  30%
  |                                                                            
  |=======================                                               |  33%
  |                                                                            
  |==========================                                            |  37%
  |                                                                            
  |============================                                          |  40%
  |                                                                            
  |==============================                                        |  43%
  |                                                                            
  |=================================                                     |  47%
  |                                                                            
  |===================================                                   |  50%
  |                                                                            
  |=====================================                                 |  53%
  |                                                                            
  |========================================                              |  57%
  |                                                                            
  |==========================================                            |  60%
  |                                                                            
  |============================================                          |  63%
  |                                                                            
  |===============================================                       |  67%
  |                                                                            
  |=================================================                     |  70%
  |                                                                            
  |===================================================                   |  73%
  |                                                                            
  |======================================================                |  77%
  |                                                                            
  |========================================================              |  80%
  |                                                                            
  |==========================================================            |  83%
  |                                                                            
  |=============================================================         |  87%
  |                                                                            
  |===============================================================       |  90%
  |                                                                            
  |=================================================================     |  93%
  |                                                                            
  |====================================================================  |  97%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.0315 secs
#> PLP result migration being applied
#> Migrating data set
#> Migrator using SQL files in PatientLevelPrediction
#> Connecting using SQLite driver
#> Creating migrations table
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00509 secs
#> Migrations table created
#> Executing migration: Migration_1-store_version.sql
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |===================================                                   |  50%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00363 secs
#> Saving migration: Migration_1-store_version.sql
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00481 secs
#> Migration complete Migration_1-store_version.sql
#> Executing migration: Migration_2-add_hyperparameter_settings.sql
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |==================                                                    |  25%
  |                                                                            
  |===================================                                   |  50%
  |                                                                            
  |====================================================                  |  75%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00527 secs
#> Saving migration: Migration_2-add_hyperparameter_settings.sql
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00404 secs
#> Migration complete Migration_2-add_hyperparameter_settings.sql
#> Executing migration: Migration_3-add_model_name.sql
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |===================================                                   |  50%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00421 secs
#> Saving migration: Migration_3-add_model_name.sql
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00323 secs
#> Migration complete Migration_3-add_model_name.sql
#> Closing database connection
#> Updating version number
#> Connecting using SQLite driver
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |===================================                                   |  50%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.0034 secs
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
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00291 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.288 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00279 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00296 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00287 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00331 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00295 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00301 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00308 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00372 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00541 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00375 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00355 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00374 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00608 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00343 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00312 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00364 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00326 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00356 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00333 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00373 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00322 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00367 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00354 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00348 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00467 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00374 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00364 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00378 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00276 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00294 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00293 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00295 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00273 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00289 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00282 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00312 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.0028 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00307 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.0029 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00291 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00283 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00291 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.0029 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00296 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00289 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00303 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00275 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00322 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00288 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00333 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00279 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00292 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00287 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00304 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00287 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00296 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00311 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00276 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00268 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00281 secs
#> Deleting PLP migration tables
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00273 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00286 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.0029 secs
#> 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%
#> Executing SQL took 0.00305 secs
# clean up the database file
unlink(file.path(tempdir(), "test.sqlite"))
```
