# @file uploadDatabaseDiagnostics.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
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
# limitations under the License.

#' Insert multiple diagnosePlp results saved to a directory into a PLP result schema database
#' @description
#' This function inserts diagnosePlp results into the result schema
#'
#' @details
#' This function can be used to upload diagnosePlp results into a database
#'
#' @param connectionDetails            A connection details created by using the
#'                                     function \code{createConnectionDetails} in the
#'                                     \code{DatabaseConnector} package.
#' @param databaseSchemaSettings       A object created by \code{createDatabaseSchemaSettings} with all the settings specifying the result tables
#' @param cohortDefinitions            (list) A list of cohortDefinitions (each list must contain: name, id)
#' @param databaseList              (Optional) ...
#' @param resultLocation          The location of the diagnostic results
#'
#' @return
#' Returns NULL but uploads multiple diagnosePlp results into the database schema specified in databaseSchemaSettings
#'
#' @export
addMultipleDiagnosePlpToDatabase <- function(
    connectionDetails,
    databaseSchemaSettings,
    cohortDefinitions,
    databaseList = NULL,
    resultLocation) {
  diagnosisFiles <- file.path(resultLocation, dir(resultLocation, pattern = "Analysis_"), "diagnosePlp.rds")

  if (length(diagnosisFiles) == 0) {
    ParallelLogger::logInfo("No diagnostic results found")
    return(NULL)
  }

  for (diagnosisFile in diagnosisFiles) {
    if (file.exists(diagnosisFile)) {
      diagnosePlp <- readRDS(diagnosisFile)
      addDiagnosePlpToDatabase(
        diagnosePlp = diagnosePlp,
        connectionDetails = connectionDetails,
        databaseSchemaSettings = databaseSchemaSettings,
        cohortDefinitions = cohortDefinitions,
        databaseList = databaseList
      )
    }
  }
  return(invisible(NULL))
}

#' Insert a diagnostic result into a PLP result schema database
#' @description
#' This function inserts a diagnostic result into the result schema
#'
#' @details
#' This function can be used to upload a diagnostic result into a database
#'
#' @param diagnosePlp                  An object of class \code{diagnosePlp}
#' @param connectionDetails            A connection details created by using the
#'                                     function \code{createConnectionDetails} in the
#'                                     \code{DatabaseConnector} package.
#' @param databaseSchemaSettings       A object created by \code{createDatabaseSchemaSettings} with all the settings specifying the result tables
#' @param cohortDefinitions            A set of one or more cohorts extracted using ROhdsiWebApi::exportCohortDefinitionSet()
#' @param databaseList              (Optional) If you wish to overwrite the settings in the plp object use \code{createdatabaseList} to specify the databases
#' @param overWriteIfExists            (default: T) Whether to delete existing results and overwrite them
#'
#' @return
#' Returns NULL but uploads the diagnostic into the database schema specified in databaseSchemaSettings
#'
#' @export
addDiagnosePlpToDatabase <- function(
    diagnosePlp,
    connectionDetails,
    databaseSchemaSettings,
    cohortDefinitions,
    databaseList = NULL,
    overWriteIfExists = TRUE) {
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))

  modelDesignId <- insertModelDesignInDatabase(
    object = diagnosePlp$modelDesign,
    conn = conn,
    databaseSchemaSettings = databaseSchemaSettings,
    cohortDefinitions = cohortDefinitions
  )

  databaseId <- addDatabase(
    conn = conn,
    databaseSchemaSettings = databaseSchemaSettings,
    databaseList = databaseList,
    databaseSchema = diagnosePlp$databaseSchema,
    databaseId = diagnosePlp$databaseId
  )

  diagnoseId <- insertDiagnosisToDatabase(
    diagnostics = diagnosePlp,
    conn = conn,
    databaseSchemaSettings = databaseSchemaSettings,
    modelDesignId = modelDesignId,
    databaseId = databaseId,
    overWriteIfExists = overWriteIfExists
  )

  return(invisible(diagnoseId))
}


insertDiagnosisToDatabase <- function(
    diagnostics,
    conn,
    databaseSchemaSettings,
    modelDesignId,
    databaseId,
    overWriteIfExists = TRUE) {
  diagnosticId <- addDiagnostic(
    conn = conn,
    resultSchema = databaseSchemaSettings$resultSchema,
    targetDialect = databaseSchemaSettings$targetDialect,
    modelDesignId = modelDesignId,
    databaseId = databaseId,
    tablePrefix = databaseSchemaSettings$tablePrefix,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  ParallelLogger::logInfo(paste0("diagnosticId: ", diagnosticId))

  # now add the four tables

  ParallelLogger::logInfo("Adding DiagnosticSummary")
  tryCatch(
    {
      addResultTable(
        conn = conn,
        resultSchema = databaseSchemaSettings$resultSchema,
        targetDialect = databaseSchemaSettings$targetDialect,
        tableName = "diagnostic_summary",
        resultIdName = "diagnosticId",
        resultId = diagnosticId,
        object = diagnostics$summary,
        tablePrefix = databaseSchemaSettings$tablePrefix,
        tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema,
        overWriteIfExists = overWriteIfExists
      )
    },
    error = function(e) {
      ParallelLogger::logError(e)
    }
  )

  ParallelLogger::logInfo("Adding DiagnosticParticipants")
  tryCatch(
    {
      addResultTable(
        conn = conn,
        resultSchema = databaseSchemaSettings$resultSchema,
        targetDialect = databaseSchemaSettings$targetDialect,
        tableName = "diagnostic_participants",
        resultIdName = "diagnosticId",
        resultId = diagnosticId,
        object = diagnostics$participants,
        tablePrefix = databaseSchemaSettings$tablePrefix,
        tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema,
        overWriteIfExists = overWriteIfExists
      )
    },
    error = function(e) {
      ParallelLogger::logError(e)
    }
  )

  ParallelLogger::logInfo("Adding DiagnosticPredictors")
  tryCatch(
    {
      addResultTable(
        conn = conn,
        resultSchema = databaseSchemaSettings$resultSchema,
        targetDialect = databaseSchemaSettings$targetDialect,
        tableName = "diagnostic_predictors",
        resultIdName = "diagnosticId",
        resultId = diagnosticId,
        object = diagnostics$predictors,
        tablePrefix = databaseSchemaSettings$tablePrefix,
        tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema,
        overWriteIfExists = overWriteIfExists
      )
    },
    error = function(e) {
      ParallelLogger::logError(e)
    }
  )

  ParallelLogger::logInfo("Adding DiagnosticOutcomes")
  tryCatch(
    {
      addResultTable(
        conn = conn,
        resultSchema = databaseSchemaSettings$resultSchema,
        targetDialect = databaseSchemaSettings$targetDialect,
        tableName = "diagnostic_outcomes",
        resultIdName = "diagnosticId",
        resultId = diagnosticId,
        object = diagnostics$outcomes,
        tablePrefix = databaseSchemaSettings$tablePrefix,
        tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema,
        overWriteIfExists = overWriteIfExists
      )
    },
    error = function(e) {
      ParallelLogger::logError(e)
    }
  )

  ParallelLogger::logInfo("Adding DiagnosticDesigns")
  tryCatch(
    {
      addResultTable(
        conn = conn,
        resultSchema = databaseSchemaSettings$resultSchema,
        targetDialect = databaseSchemaSettings$targetDialect,
        tableName = "diagnostic_designs",
        resultIdName = "diagnosticId",
        resultId = diagnosticId,
        object = diagnostics$designs,
        tablePrefix = databaseSchemaSettings$tablePrefix,
        tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema,
        overWriteIfExists = overWriteIfExists
      )
    },
    error = function(e) {
      ParallelLogger::logError(e)
    }
  )

  return(invisible(diagnosticId))
}



addDiagnostic <- function(
    conn,
    resultSchema,
    targetDialect,
    modelDesignId,
    databaseId,
    tablePrefix,
    tempEmulationSchema) {
  result <- checkTable(
    conn = conn,
    resultSchema = resultSchema,
    tablePrefix = tablePrefix,
    targetDialect = targetDialect,
    tableName = "diagnostics",
    columnNames = c(
      "model_design_id",
      "database_id"
    ),
    values = c(
      modelDesignId,
      databaseId
    ),
    tempEmulationSchema = tempEmulationSchema
  )

  if (nrow(result) == 0) {
    # model
    sql <- "INSERT INTO @my_schema.@string_to_appenddiagnostics (
    model_design_id,
    database_id
  )
  VALUES (
  @model_design_id,
    @database_id
    )"
    sql <- SqlRender::render(sql,
      my_schema = resultSchema,
      model_design_id = modelDesignId,
      database_id = databaseId,
      string_to_append = tablePrefix
    )
    sql <- SqlRender::translate(sql,
      targetDialect = targetDialect,
      tempEmulationSchema = tempEmulationSchema
    )
    DatabaseConnector::executeSql(conn, sql)

    # getId of new
    result <- checkTable(
      conn = conn,
      resultSchema = resultSchema,
      tablePrefix = tablePrefix,
      targetDialect = targetDialect,
      tableName = "diagnostics",
      columnNames = c(
        "model_design_id",
        "database_id"
      ),
      values = c(
        modelDesignId,
        databaseId
      ),
      tempEmulationSchema = tempEmulationSchema
    )
  }

  return(result$diagnosticId[1])
}

# replace the performance inserts with this single function...
addResultTable <- function(
    conn = conn,
    resultSchema,
    targetDialect,
    tableName = "diagnostic_summary",
    resultIdName = "diagnosticId",
    resultId,
    object,
    tablePrefix,
    tempEmulationSchema,
    overWriteIfExists = TRUE) {
  object[resultIdName] <- resultId

  # get column names and check all present in object
  columnNames <- getColumnNames(
    conn = conn,
    resultSchema = resultSchema,
    targetDialect = targetDialect,
    tableName = paste0(tablePrefix, tableName),
    tempEmulationSchema = tempEmulationSchema
  )
  isValid <- sum(colnames(object) %in% columnNames) == length(columnNames)

  exists <- checkResultExists(
    conn = conn,
    resultSchema = resultSchema,
    targetDialect = targetDialect,
    tableName = paste0(tablePrefix, tableName),
    resultIdName = SqlRender::camelCaseToSnakeCase(resultIdName),
    resultId = resultId,
    tempEmulationSchema = tempEmulationSchema
  )

  if (isValid && (!exists || overWriteIfExists)) {
    # REMOVE existing result
    if (exists) {
      sql <- "delete from @result_schema.@table_name where @result_id_name = @result_id;"
      sql <- SqlRender::render(sql,
        result_id_name = SqlRender::camelCaseToSnakeCase(resultIdName),
        result_id = resultId,
        result_schema = resultSchema,
        table_name = paste0(tablePrefix, tableName)
      )
      sql <- SqlRender::translate(sql,
        targetDialect = targetDialect,
        tempEmulationSchema = tempEmulationSchema
      )
      DatabaseConnector::executeSql(conn, sql)
    }

    # add
    DatabaseConnector::insertTable(
      connection = conn,
      databaseSchema = resultSchema,
      tableName = paste0(tablePrefix, tableName),
      data = as.data.frame(object[, columnNames]),
      dropTableIfExists = FALSE, createTable = FALSE, tempTable = FALSE,
      bulkLoad = FALSE, camelCaseToSnakeCase = TRUE, progressBar = T,
      tempEmulationSchema = tempEmulationSchema
    )
  }

  return(invisible(NULL))
}
