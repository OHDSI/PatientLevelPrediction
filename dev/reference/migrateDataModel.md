# Migrate Data model

Migrate data from current state to next state

It is strongly advised that you have a backup of all data (either sqlite
files, a backup database (in the case you are using a postgres backend)
or have kept the csv/zip files from your data generation.

## Usage

``` r
migrateDataModel(connectionDetails, databaseSchema, tablePrefix = "")
```

## Arguments

- connectionDetails:

  DatabaseConnector connection details object

- databaseSchema:

  String schema where database schema lives

- tablePrefix:

  (Optional) Use if a table prefix is used before table names (e.g.
  "cd\_")

## Value

Nothing. Is called for side effects of migrating data model in the
database
