# @file DatabaseMigration.R
#
# Copyright 2023 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitatons under the License.
#
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
  rlang::check_installed("ResultModelManager")
  
  ResultModelManager::DataMigrationManager$new(
    connectionDetails = connectionDetails,
    databaseSchema = databaseSchema,
    tablePrefix = tablePrefix,
    migrationPath = "migrations",
    packageName = utils::packageName()
  )
}
