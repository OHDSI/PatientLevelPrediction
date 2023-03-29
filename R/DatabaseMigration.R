#' Migrate Data model
#' @description
#' Migrate data from current state to next state
#'
#' It is strongly advised that you have a backup of all data (either sqlite files, a backup database (in the case you
#' are using a postgres backend) or have kept the csv/zip files from your data generation.
#'
#' @param connectionDetails             DatabaseConnector connection details object
#' @param databaseSchema                String schema where database schema lives
#' @param  tablePrefix                  (Optional) Use if a table prefix is used before table names (e.g. "cd_")
#' 
#' @export
migrateDataModel <- function(connectionDetails, databaseSchema, tablePrefix = "") {
  ParallelLogger::logInfo("Migrating data set")
  
  migrator <- getDataMigrator(
    connectionDetails = connectionDetails, 
    databaseSchema = databaseSchema, 
    tablePrefix = tablePrefix
    )
  migrator$executeMigrations()
  migrator$finalize()
  
  ParallelLogger::logInfo("Updating version number")
  updateVersionSql <- SqlRender::loadRenderTranslateSql(
    "UpdateVersionNumber.sql",
    packageName = utils::packageName(),
    database_schema = databaseSchema,
    table_prefix = tablePrefix,
    version_number = utils::packageVersion("PatientLevelPrediction"),
    dbms = "sql server" # this is the same for all dbms so just using sql server
  )
  
  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  DatabaseConnector::executeSql(connection, updateVersionSql)
}


getDataMigrator <- function(connectionDetails, databaseSchema, tablePrefix = "") {
  ensure_installed("ResultModelManager")
  
  ResultModelManager::DataMigrationManager$new(
    connectionDetails = connectionDetails,
    databaseSchema = databaseSchema,
    tablePrefix = tablePrefix,
    migrationPath = "migrations",
    packageName = utils::packageName()
  )
}