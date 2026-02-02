# Create a setting that holds the details about the cdmDatabase connection for data extraction

Create a setting that holds the details about the cdmDatabase connection
for data extraction

## Usage

``` r
createDatabaseDetails(
  connectionDetails,
  cdmDatabaseSchema,
  cdmDatabaseName,
  cdmDatabaseId,
  tempEmulationSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cdmDatabaseSchema,
  cohortTable = "cohort",
  outcomeDatabaseSchema = cohortDatabaseSchema,
  outcomeTable = cohortTable,
  targetId = NULL,
  outcomeIds = NULL,
  cdmVersion = 5,
  cohortId = NULL
)
```

## Arguments

- connectionDetails:

  An R object of type `connectionDetails` created using the function
  `createConnectionDetails` in the `DatabaseConnector` package.

- cdmDatabaseSchema:

  The name of the database schema that contains the OMOP CDM instance.
  Requires read permissions to this database. On SQL Server, this should
  specifiy both the database and the schema, so for example
  'cdm_instance.dbo'.

- cdmDatabaseName:

  A string with the name of the database - this is used in the shiny app
  and when externally validating models to name the result list and to
  specify the folder name when saving validation results (defaults to
  cdmDatabaseSchema if not specified)

- cdmDatabaseId:

  A string with a unique identifier for the database and version - this
  is stored in the plp object for future reference and used by the shiny
  app (defaults to cdmDatabaseSchema if not specified)

- tempEmulationSchema:

  For dmbs like Oracle only: the name of the database schema where you
  want all temporary tables to be managed. Requires create/insert
  permissions to this database.

- cohortDatabaseSchema:

  The name of the database schema that is the location where the target
  cohorts are available. Requires read permissions to this database.

- cohortTable:

  The tablename that contains the target cohorts. Expectation is
  cohortTable has format of COHORT table: COHORT_DEFINITION_ID,
  SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.

- outcomeDatabaseSchema:

  The name of the database schema that is the location where the data
  used to define the outcome cohorts is available. Requires read
  permissions to this database.

- outcomeTable:

  The tablename that contains the outcome cohorts. Expectation is
  outcomeTable has format of COHORT table: COHORT_DEFINITION_ID,
  SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.

- targetId:

  An integer specifying the cohort id for the target cohort

- outcomeIds:

  A single integer or vector of integers specifying the cohort ids for
  the outcome cohorts

- cdmVersion:

  Define the OMOP CDM version used: currently support "4" and "5".

- cohortId:

  (depreciated: use targetId) old input for the target cohort id

## Value

A list with the the database specific settings:

- `connectionDetails`: An R object of type `connectionDetails` created
  using the function `createConnectionDetails` in the
  `DatabaseConnector` package.

- `cdmDatabaseSchema`: The name of the database schema that contains the
  OMOP CDM instance.

- `cdmDatabaseName`: A string with the name of the database - this is
  used in the shiny app and when externally validating models to name
  the result list and to specify the folder name when saving validation
  results (defaults to cdmDatabaseSchema if not specified).

- `cdmDatabaseId`: A string with a unique identifier for the database
  and version - this is stored in the plp object for future reference
  and used by the shiny app (defaults to cdmDatabaseSchema if not
  specified).

- `tempEmulationSchema`: The name of a databae schema where you want all
  temporary tables to be managed. Requires create/insert permissions to
  this database.

- `cohortDatabaseSchema`: The name of the database schema that is the
  location where the target cohorts are available. Requires read
  permissions to this schema.

- `cohortTable`: The tablename that contains the target cohorts.
  Expectation is cohortTable has format of COHORT table:
  COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.

- `outcomeDatabaseSchema`: The name of the database schema that is the
  location where the data used to define the outcome cohorts is
  available. Requires read permissions to this database.

- `outcomeTable`: The tablename that contains the outcome cohorts.
  Expectation is outcomeTable has format of COHORT table:
  COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.

- `targetId`: An integer specifying the cohort id for the target cohort

- `outcomeIds`: A single integer or vector of integers specifying the
  cohort ids for the outcome cohorts

- `cdmVersion`: Define the OMOP CDM version used: currently support "4"
  and "5".

## Details

This function simply stores the settings for communicating with the
cdmDatabase when extracting the target cohort and outcomes

## Examples
