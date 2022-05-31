# @file UploadPlpDbResults.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
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

insertRunPlpToSqlite <- function(
  runPlp, 
  externalValidatePlp = NULL, 
  diagnosePlp = NULL
  ){
  
  sqliteLocation <- tempdir()
  
  # create sqlite database
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = 'sqlite',
    server = file.path(sqliteLocation,'databaseFile.sqlite')
  )
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))
  
  createPlpResultTables(
    conn = conn,
    targetDialect = 'sqlite',
    resultSchema = 'main', 
    deleteTables = T, 
    createTables = T,
    stringAppendToTables = ''
  )
  
  cohortDefinitions <- list(
    list(id = runPlp$model$modelDesign$cohortId, name = 'Target'),
    list(id = runPlp$model$modelDesign$outcomeId, name = 'Outcome')
  )
  
  databaseList <- createDatabaseList(
    cdmDatabaseSchemas = c(
      runPlp$model$trainDetails$developmentDatabase,
      unlist(lapply(externalValidatePlp, function(x) x$model$validationDetails$validationDatabase))
    )
  )
  
  addRunPlpToDatabase(
    runPlp = runPlp,
    conn = conn,
    databaseSchemaSettings = createDatabaseSchemaSettings(resultSchema = 'main'),
    cohortDefinitions = cohortDefinitions,
    databaseList = databaseList
  )
  
  # add validation results if entered
  if(!is.null(externalValidatePlp)){
    if(class(externalValidatePlp) == 'list'){
      for(i in 1:length(externalValidatePlp)){
        tryCatch(
          {
            addRunPlpToDatabase(
              runPlp = externalValidatePlp[[i]],
              conn = conn,
              databaseSchemaSettings = createDatabaseSchemaSettings(resultSchema = 'main'),
              cohortDefinitions = cohortDefinitions,
              databaseList = databaseList
            )
          }, error = function(e){ParallelLogger::logError(e)}
        )
      }
    }
  }
  
  # add diagnosis results if entered
  if(!is.null(diagnosePlp)){
    tryCatch(
      {
        addDiagnosePlpToDatabase(
          diagnosePlp = diagnosePlp,
          conn = conn,
          databaseSchemaSettings = createDatabaseSchemaSettings(resultSchema = 'main'),
          cohortDefinitions = cohortDefinitions,
          databaseList = databaseList
        )
      }, error = function(e){ParallelLogger::logError(e)}
    )
  }
  
  
  return(file.path(sqliteLocation,'databaseFile.sqlite'))
}

#' Create sqlite database with the results
#' @description
#' This function create an sqlite database with the PLP result schema and inserts all results
#'
#' @details
#' This function can be used upload PatientLevelPrediction results into an sqlite database
#'
#' @param resultLocation               (string) location of directory where the main package results were saved
#' @param cohortDefinitions            (list) A list of cohortDefinitions (each list must contain: name, id)
#' @param databaseList             A list created by \code{createDatabaseList} to specify the databases
#' @param sqliteLocation               (string) location of directory where the sqlite database will be saved
#'    
#' @return
#' Returns the location of the sqlite database file
#' 
#' @export
insertResultsToSqlite <- function(
  resultLocation, 
  cohortDefinitions,
  databaseList = createDatabaseList(
    cdmDatabaseSchemas = c('cdm_truven_ccae_v123', 'cdm_madeup_v1')
  ),
  sqliteLocation = file.path(resultLocation, 'sqlite')
){
  
  if(!dir.exists(sqliteLocation)){
    dir.create(sqliteLocation, recursive = T)
  }
  
  # create sqlite database
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = 'sqlite',
    server = file.path(sqliteLocation,'databaseFile.sqlite')
  )
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))
  
  createPlpResultTables(
    conn = conn,
    targetDialect = 'sqlite',
    resultSchema = 'main', 
    deleteTables = T, 
    createTables = T,
    stringAppendToTables = ''
  )
  
  # run insert models
  addMultipleRunPlpToDatabase(
    conn = conn, 
    databaseSchemaSettings = createDatabaseSchemaSettings(resultSchema = 'main'),
    cohortDefinitions = cohortDefinitions,
    databaseList = databaseList,
    resultLocation = resultLocation
  )
  
  # run insert diagnosis
  addMultipleDiagnosePlpToDatabase(
    conn = conn, 
    databaseSchemaSettings = createDatabaseSchemaSettings(resultSchema = 'main'),
    cohortDefinitions = cohortDefinitions,
    databaseList = databaseList,
    resultLocation = resultLocation
  )
  
  return(file.path(sqliteLocation,'databaseFile.sqlite'))
}

#' Create the results tables to store PatientLevelPrediction models and results into a database
#' @description
#' This function executes a large set of SQL statements to create tables that can store models and results
#'
#' @details
#' This function can be used to create (or delete) PatientLevelPrediction result tables
#'
#' @param conn                         A connection to a database created by using the
#'                                     function \code{connect} in the
#'                                     \code{DatabaseConnector} package.
#' @param targetDialect                The database management system being used
#' @param resultSchema                 The name of the database schema that the result tables will be created.
#' @param deleteTables                 If true any existing tables matching the PatientLevelPrediction result tables names will be deleted
#' @param createTables                 If true the PatientLevelPrediction result tables will be created
#' @param stringAppendToTables         A string that appends to the PatientLevelPrediction result tables
#' @param tempEmulationSchema          The temp schema used when the database management system is oracle
#' 
#' @param testFile                     (used for testing) The location of an sql file with the table creation code
#'
#' @return
#' Returns NULL but creates the required tables into the specified database schema(s).
#' 
#' @export
createPlpResultTables <- function(
  conn,
  targetDialect = 'postgresql',
  resultSchema, 
  deleteTables = T, 
  createTables = T,
  stringAppendToTables = '',
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
  testFile = NULL
){
  
  
  if(deleteTables){
    ParallelLogger::logInfo('Deleting existing tables')
    
    tableNames <- getPlpResultTables()
    
  deleteTables(
    conn = conn,
    databaseSchema = resultSchema,
    targetDialect = targetDialect,
    tempEmulationSchema = tempEmulationSchema,
    tableNames = tableNames, 
    stringAppendToTables = stringAppendToTables
  )
    
  }
  

  if(createTables){
    ParallelLogger::logInfo('Creating PLP results tables')
    
    if(stringAppendToTables != ''){
      stringAppendToTables <- paste0(toupper(gsub('_','',gsub(' ','', stringAppendToTables))), '_')
    }
    
    if(is.null(testFile)){
      renderedSql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = ifelse(
          targetDialect != 'sqlite',
          "PlpResultTables.sql",
          paste0("PlpResultTables_",targetDialect,".sql")
          ), # need to add others
        packageName = "PatientLevelPrediction",
        dbms = targetDialect,
        tempEmulationSchema = tempEmulationSchema,
        my_schema = resultSchema,
        string_to_append = stringAppendToTables
      )
    } else {
      sql <- readChar(testFile, file.info(testFile)$size) 
      renderedSql <- SqlRender::render(sql = sql[1],
                                       my_schema = resultSchema,
                                       string_to_append = stringAppendToTables)
      renderedSql <- SqlRender::translate(sql = renderedSql,
                                          targetDialect = targetDialect,
                                          tempEmulationSchema = tempEmulationSchema)
      
    }
    
    DatabaseConnector::executeSql(conn, renderedSql)
  }
  
}

#' Populate the PatientLevelPrediction results tables
#' @description
#' This function formats and uploads results that have been generated via an ATLAS prediction package into a database
#'
#' @details
#' This function can be used upload PatientLevelPrediction results into a database
#'
#' @param conn                         A connection to a database created by using the
#'                                     function \code{connect} in the
#'                                     \code{DatabaseConnector} package.
#' @param databaseSchemaSettings       A object created by \code{createDatabaseSchemaSettings} with all the settings specifying the result tables                              
#' @param cohortDefinitions            (list) A list of cohortDefinitions (each list must contain: name, id)
#' @param databaseList              A list created by \code{createDatabaseList} to specify the databases
#' @param resultLocation               (string) location of directory where the main package results were saved
#' @param resultLocationVector         (only used when resultLocation is missing) a vector of locations with development or validation results  
#'    
#' @return
#' Returns NULL but uploads all the results in resultLocation to the PatientLevelPrediction result tables in resultSchema
#' 
#' @export
addMultipleRunPlpToDatabase <- function(conn, 
                                    databaseSchemaSettings = createDatabaseSchemaSettings(resultSchema = 'main'),
                                    cohortDefinitions,
                                    databaseList = createDatabaseList(
                                      cdmDatabaseSchemas = c('cdm_truven_ccae_v123', 'cdm_madeup_v1')
                                    ),
                                    resultLocation = NULL,
                                    resultLocationVector
){
  
  # for each development result add it to the database:
  
  if(missing(resultLocationVector)){
    resultLocationVector <- getResultLocations(resultLocation)
  }
  
  for(runPlpLocation in resultLocationVector){
    ParallelLogger::logInfo(paste0('Inserting result @ ', runPlpLocation, ' into database'))
    
    # TODO edit csv here
    runPlp <- tryCatch(
      {PatientLevelPrediction::loadPlpResult(runPlpLocation)}, 
      error = function(e){ParallelLogger::logInfo(e);return(NULL)}
    )
    
    if(!is.null(runPlp)){
      ParallelLogger::logInfo('result loaded')
      
      #  Add runPlp to the database
      addRunPlpToDatabase(
        runPlp = runPlp,
        conn = conn,
        databaseSchemaSettings = databaseSchemaSettings,
        cohortDefinitions = cohortDefinitions,
        databaseList = databaseList
      )
      
    } #model not null 
    
  } # per model
  
} #end funct


#' Create the PatientLevelPrediction database result schema settings
#' @description
#' This function specifies where the results schema is and lets you pick a different schema for the cohorts and databases
#'
#' @details
#' This function can be used to specify the database settings used to upload PatientLevelPrediction results into a database
#'
#' @param resultSchema                 (string) The name of the database schema with the result tables.
#' @param stringAppendToResultSchemaTables         (string) A string that appends to the PatientLevelPrediction result tables
#' @param targetDialect                (string) The database management system being used
#' @param tempEmulationSchema          (string) The temp schema used when the database management system is oracle
#' @param cohortDefinitionSchema                 (string) The name of the database schema with the cohort definition tables (defaults to resultSchema).
#' @param stringAppendToCohortDefinitionTables         (string) A string that appends to the cohort definition tables
#' @param databaseDefinitionSchema                 (string) The name of the database schema with the database definition tables (defaults to resultSchema).
#' @param stringAppendToDatabaseDefinitionTables         (string) A string that appends to the database definition tables
#'    
#' @return
#' Returns a list of class 'plpDatabaseResultSchema' with all the database settings
#' 
#' @export
createDatabaseSchemaSettings <- function(
  resultSchema = 'main', 
  stringAppendToResultSchemaTables = '',
  targetDialect = 'sqlite',
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
  cohortDefinitionSchema = resultSchema,
  stringAppendToCohortDefinitionTables = stringAppendToResultSchemaTables,
  databaseDefinitionSchema = resultSchema,
  stringAppendToDatabaseDefinitionTables = stringAppendToResultSchemaTables
){
  
  if(missing(resultSchema)){
    stop('resultSchema required')
  }
  if(class(resultSchema) != "character"){
    stop('resultSchema must be a string')
  }
  
  if(stringAppendToResultSchemaTables != ''){
    stringAppendToResultSchemaTables <- paste0(toupper(gsub('_','',gsub(' ','', stringAppendToResultSchemaTables))), '_')
  }
  if(stringAppendToCohortDefinitionTables != ''){
    stringAppendToCohortDefinitionTables <- paste0(toupper(gsub('_','',gsub(' ','', stringAppendToCohortDefinitionTables))), '_')
  }
  if(stringAppendToDatabaseDefinitionTables != ''){
    stringAppendToDatabaseDefinitionTables <- paste0(toupper(gsub('_','',gsub(' ','', stringAppendToDatabaseDefinitionTables))), '_')
  }
  
  result <- list(
    resultSchema = resultSchema,
    stringAppendToResultSchemaTables = stringAppendToResultSchemaTables,
    targetDialect = targetDialect,
    tempEmulationSchema = tempEmulationSchema,
    cohortDefinitionSchema  = cohortDefinitionSchema,
    stringAppendToCohortDefinitionTables = stringAppendToCohortDefinitionTables,
    databaseDefinitionSchema = databaseDefinitionSchema,
    stringAppendToDatabaseDefinitionTables = stringAppendToDatabaseDefinitionTables
  )
  
  class(result) <- 'plpDatabaseResultSchema'
  return(result)
}


#' Create a data frame with the database details
#' @description
#' This function creates a data.frame with the details of the databases used in the study
#'
#' @details
#' This function is used when inserting database details into the PatientLevelPrediction database results schema
#' 
#' @param cdmDatabaseSchemas           (string vector) A vector of the cdmDatabaseSchemas used in the study
#' @param acronyms                     (optional string vector) A vector of the acronyms for the cdmDatabaseSchemas
#' @param versions                     (optional string vector) A vector of the database versions for the cdmDatabaseSchemas
#' @param descriptions                 (optional string vector) A vector of the database descriptions for the cdmDatabaseSchemas
#' @param types                        (optional string vector) A vector of the data types (e.g., claims or EHR) for the cdmDatabaseSchemas
#'
#' @return
#' Returns a data.frame with the database details
#' 
#' @export
createDatabaseList <- function(
  cdmDatabaseSchemas,
  acronyms,
  versions, 
  descriptions,
  types
){
  if(missing(cdmDatabaseSchemas)){
    stop('Need to specify cdmDatabaseSchemas')
  }
  
  if(missing(acronyms)){
    acronyms <- cdmDatabaseSchemas
  }
  if(missing(versions)){
    versions <- unlist(lapply(strsplit(cdmDatabaseSchemas, '_'), function(x) x[length(x)]))
  }
  if(missing(descriptions)){
    descriptions <- rep('', length(cdmDatabaseSchemas))
  }
  if(missing(types)){
    types <- rep('', length(cdmDatabaseSchemas))
  }
  
  result <- lapply(
    1:length(cdmDatabaseSchemas),
    
    function(i) list(
      name = cdmDatabaseSchemas[i],
      acronym = acronyms[i],
      version = versions[i],
      description = descriptions[i],
      type = types[i]
    )
  )
  
  names(result) <- cdmDatabaseSchemas

  return(result)
}



#' Function to add the run plp (development or validation) to database
#' @description
#' This function adds a runPlp or external validation result into a database
#'
#' @details
#' This function is used when inserting results into the PatientLevelPrediction database results schema
#' 
#' @param runPlp           An object of class \code{runPlp} or class \code{externalValidatePlp}
#' @param conn                         A connection to a database created by using the
#'                                     function \code{connect} in the
#'                                     \code{DatabaseConnector} package.
#' @param databaseSchemaSettings       A object created by \code{createDatabaseSchemaSettings} with all the settings specifying the result tables                              
#' @param cohortDefinitions            (list) A list of cohortDefinitions (each list must contain: name, id)
#' @param databaseList              A list created by \code{createDatabaseList} to specify the databases
#'
#' @return
#' Returns a data.frame with the database details
#' 
#' @export
addRunPlpToDatabase <- function(
  runPlp,
  conn,
  databaseSchemaSettings,
  cohortDefinitions,
  databaseList
){
  
  modelDesignId <- insertModelDesignInDatabase(
    object = do.call(PatientLevelPrediction::createModelDesign,runPlp$model$modelDesign), 
    conn = conn, 
    databaseSchemaSettings = databaseSchemaSettings,
    cohortDefinitions = cohortDefinitions 
  )
  
  # Add model if runPlp
  if(class(runPlp) == 'runPlp'){
    includesModel <- T
    developmentDatabase <- runPlp$model$trainDetails$developmentDatabase
    validationDatabase <- runPlp$model$trainDetails$developmentDatabase
    
    populationSettings <- runPlp$model$modelDesign$populationSettings
    cohortId <- runPlp$model$modelDesign$cohortId
    outcomeId <- runPlp$model$modelDesign$outcomeId
    restrictPlpDataSettings <- runPlp$model$modelDesign$restrictPlpDataSettings
    
    attrition <- runPlp$model$trainDetails$attrition
    
  } else{
    includesModel <- F
    developmentDatabase <- runPlp$model$validationDetails$developmentDatabase
    validationDatabase <- runPlp$model$validationDetails$validationDatabase
    
    populationSettings <- runPlp$model$validationDetails$populationSettings
    cohortId <- runPlp$model$validationDetails$cohortId
    outcomeId <- runPlp$model$validationDetails$outcomeId
    restrictPlpDataSettings <- runPlp$model$validationDetails$restrictPlpDataSettings
    
    attrition <- runPlp$model$validationDetails$attrition
    
  }
  
  # Add databases
  developmentDatabaseId <- addDatabase(
    conn = conn, 
    databaseSchemaSettings = databaseSchemaSettings,
    databaseDetail = getDatabaseDetail(
      databaseList = databaseList,
      databaseSchema = developmentDatabase
    )
  )
  validationDatabaseId <- addDatabase(
    conn = conn, 
    databaseSchemaSettings = databaseSchemaSettings,
    databaseDetail = getDatabaseDetail(
      databaseList = databaseList,
      databaseSchema = validationDatabase
    )
  )
  
  # add nodel if the result contains it
  if(includesModel){
    insertModelInDatabase(
      model = runPlp$model,
      conn = conn,
      databaseSchemaSettings = databaseSchemaSettings,
      databaseId = developmentDatabaseId,
      modelDesignId = modelDesignId
    )
  }
  
  # get validation settings
  validationTarId <- addTar(
    conn = conn, 
    resultSchema = databaseSchemaSettings$resultSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    startDay = populationSettings$riskWindowStart, 
    startAnchor = populationSettings$startAnchor,
    endDay = populationSettings$riskWindowEnd,  
    endAnchor = populationSettings$endAnchor, 
    stringAppendToTables = databaseSchemaSettings$stringAppendToResultSchemaTables,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  
  validationCohortId <-  addCohort(
    conn = conn, 
    resultSchema = databaseSchemaSettings$cohortDefinitionSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    cohortDefinition = getCohortDefinitionJson(
      cohortDefinitions = cohortDefinitions,
      cohortId = cohortId
    ),
    stringAppendToTables = databaseSchemaSettings$stringAppendToCohortDefinitionTables,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  
  validationOutcomeId <-  addCohort(
    conn = conn, 
    resultSchema = databaseSchemaSettings$cohortDefinitionSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    cohortDefinition = getCohortDefinitionJson(
      cohortDefinitions = cohortDefinitions,
      cohortId = outcomeId
    ),
    stringAppendToTables = databaseSchemaSettings$stringAppendToCohortDefinitionTables,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  
  validationPopulationId <- addPopulationSetting(
    conn = conn, 
    resultSchema = databaseSchemaSettings$resultSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    json = populationSettings, 
    stringAppendToTables = databaseSchemaSettings$stringAppendToResultSchemaTables,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  
  validationPlpDataId <-  addPlpDataSetting(
    conn = conn, 
    resultSchema = databaseSchemaSettings$resultSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    json = restrictPlpDataSettings, 
    stringAppendToTables = databaseSchemaSettings$stringAppendToResultSchemaTables,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  
  # Add performance
  insertPerformanceInDatabase(
    performanceEvaluation = runPlp$performanceEvaluation,
    covariateSummary = runPlp$covariateSummary,
    attrition = attrition,
    executionDateTime = format(runPlp$executionSummary$ExecutionDateTime, format="%Y-%m-%d"),
    plpVersion = runPlp$executionSummary$PackageVersion$packageVersion, 
    conn = conn,
    databaseSchemaSettings = databaseSchemaSettings,
    
    modelDesignId = modelDesignId,
    developmentDatabaseId = developmentDatabaseId,
    
    validationDatabaseId = validationDatabaseId,
    validationTarId = validationTarId,
    validationPopulationId= validationPopulationId,
    validationPlpDataId = validationPlpDataId,
    validationCohortId = validationCohortId,
    validationOutcomeId = validationOutcomeId
  )
  
  return(invisible(NULL))
}



###################
# INSERT MODEL
####################
insertModelInDatabase <- function(
  model,
  conn,
  databaseSchemaSettings,
  databaseId,
  modelDesignId
){
  
    # create this function
    modelId <- addModel(
      conn = conn, 
      resultSchema = databaseSchemaSettings$resultSchema, 
      targetDialect = databaseSchemaSettings$targetDialect,
      analysisId = ifelse(
        is.null(model$trainDetails$analysisId), 
        'missing',
        model$trainDetails$analysisId
      ), 
      modelDesignId = modelDesignId,
      databaseId = databaseId,
      modelType = model$trainDetails$modelName,
      plpModelFile = as.character(ParallelLogger::convertSettingsToJson(model$model)), # add the json models here?
      trainDetails = as.character(ParallelLogger::convertSettingsToJson(model$trainDetails)),
      preprocessing = as.character(ParallelLogger::convertSettingsToJson(model$preprocessing)),
      
      executionDateTime = format(model$trainDetails$trainingDate, format="%Y-%m-%d"), 
      trainingTime = model$trainDetails$trainingTime, 
      intercept = ifelse(is.list(model$model), model$model$coefficients[1], 0),
      
      stringAppendToTables = databaseSchemaSettings$stringAppendToResultSchemaTables,
      tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
    )
    ParallelLogger::logInfo(
      paste0('modelId: ', modelId, 
             ' inserted for modelDesignId ', modelDesignId, 
             'and databaseId ', databaseId)
    )
    
  return(invisible(modelId))
}

addModel <- function(
  conn, 
  resultSchema, 
  targetDialect,
  stringAppendToTables,
  analysisId,
  modelDesignId,
  databaseId,
  
  modelType,
  plpModelFile,
  trainDetails,
  preprocessing,
  
  executionDateTime,
  trainingTime,
  intercept,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  if(is.null(analysisId)){
    stop('analysisId is null')
  }
  if(is.null(modelDesignId)){
    stop('modelDesignId is null')
  }
  if(is.null(databaseId)){
    stop('databaseId is null')
  }
  
  if(is.null(plpModelFile)){
    stop('plpModelFile is null')
  }
  if(is.null(executionDateTime)){
    stop('executionDateTime is null')
  }
  if(is.null(intercept)){
    stop('intercept is null')
  }
  
  # process json to make it ordered...
  # TODO
  
  result <- checkTable(
    conn = conn, 
    resultSchema = resultSchema, 
    stringAppendToTables = stringAppendToTables,
    targetDialect = targetDialect, 
    tableName = 'models',
    columnNames = c(
      'model_design_id',
      'database_id'
    ), 
    values = c(
      modelDesignId,
      databaseId
    ),
    tempEmulationSchema = tempEmulationSchema
  )
  
  if(nrow(result)==0){
    # model
    sql <- "INSERT INTO @my_schema.@string_to_appendmodels(
    analysis_id,
    model_design_id,
    database_id,
    model_type,
    plp_model_file,
    train_details,
    preprocessing,
    execution_date_time,
    training_time,
    intercept
    ) VALUES 
  ('@analysis_id', 
  @model_design_id,
  @database_id, 
  
  '@model_type',
  '@plp_model_file', 
  '@train_details', 
  '@preprocessing', 
      
  '@execution_date_time', 
  '@training_time', 
   @intercept
  )"
    sql <- SqlRender::render(
      sql, 
      my_schema = resultSchema,
      analysis_id = analysisId,
      model_design_id = modelDesignId,
      database_id = databaseId,
      
      model_type = modelType,
      plp_model_file = plpModelFile,
      train_details = trainDetails,
      preprocessing = preprocessing,
      
      execution_date_time = executionDateTime,
      training_time = trainingTime,
      intercept = intercept,
      
      string_to_append = stringAppendToTables
    )
    sql <- SqlRender::translate(
      sql, 
      targetDialect = targetDialect,
      tempEmulationSchema = tempEmulationSchema
    )
    DatabaseConnector::executeSql(conn, sql)
    
    #getId of new
    result <- checkTable(
      conn = conn, 
      resultSchema = resultSchema, 
      stringAppendToTables = stringAppendToTables,
      targetDialect = targetDialect, 
      tableName = 'models',
      columnNames = c(
        'model_design_id',
        'database_id'
      ), 
      values = c(
        modelDesignId,
        databaseId
      ),
      tempEmulationSchema = tempEmulationSchema
    )
    
  } 
  
  return(result$modelId[1])
}

#======================
# Helpers
#======================

# get a vector with all the result table names
getPlpResultTables <- function(){
  return(
    c(
      "CALIBRATION_SUMMARY", 
      "COVARIATE_SUMMARY", 
      "DEMOGRAPHIC_SUMMARY",
      "EVALUATION_STATISTICS", 
      "PREDICTION_DISTRIBUTION", 
      "THRESHOLD_SUMMARY",
      
      "ATTRITION", #new 
      
      "DIAGNOSTIC_SUMMARY",
      "DIAGNOSTIC_PARTICIPANTS",
      "DIAGNOSTIC_PREDICTORS",
      "DIAGNOSTIC_OUTCOMES",
      "DIAGNOSTIC_DESIGNS",
      
      "DIAGNOSTICS", #new 
      "RECALIBRATIONS", #new 
      
      "PERFORMANCES", 
      
      "MODELS", 
      
      "MODEL_DESIGNS",  
      
      "MODEL_SETTINGS", 
      "COVARIATE_SETTINGS",
      "POPULATION_SETTINGS", 
      "FEATURE_ENGINEERING_SETTINGS", 
      "SPLIT_SETTINGS", 
      "PLP_DATA_SETTINGS", #new
      "SAMPLE_SETTINGS", 
      "TIDY_COVARIATES_SETTINGS", #new 
      "TARS", 
      
      "DATABASE_DETAILS",
      "COHORTS"
    )
  )
}

# get the database detail list for a specified database schema

#' Get the database details
#' @description
#' This function extracts the database details from a list of details
#'
#' @details
#' This function extracts the database details from a list of details
#' 
#' @param databaseList              A list created by \code{createDatabaseList} to specify the databases
#' @param databaseSchema               The database schema string
#'
#' @return
#' Returns a list with the database details
#' 
#' @export
getDatabaseDetail <- function(
  databaseList,
  databaseSchema
){
  
  ind <- which(names(databaseList) == databaseSchema)
  
  if(length(ind) == 0){
    return(
      list(
        name = databaseSchema,
        acronym = '',
        version = '',
        description = '',
        type = ''
      )
    )
  }else{
    return(databaseList[[ind]])
  }
  
}

#' Get the cohort definition from a list of definitions
#' @description
#' This function extracts the cohort definition from a list
#'
#' @details
#' This function extracts the cohort definition from a list
#' 
#' @param cohortDefinitions      A list of cohortDefinitions
#' @param cohortId               The cohortId to extract the cohortDefinition for
#'
#' @return
#' Returns a list with the cohort definition R object
#' 
#' @export
getCohortDefinitionJson <- function(cohortDefinitions, cohortId){
  
  if(is.null(cohortDefinitions)){
    ParallelLogger::logInfo('No cohortDefinitions - not possible to get cohort name')
    return(list(id = cohortId, name = 'Unknown'))
  }
  
  #cohort_name, cohort_id and cohort_json
  ParallelLogger::logInfo(paste0('Adding cohorts from input list'))
  id <- which(unlist(lapply(cohortDefinitions, function(x){x$id == cohortId})))[1]
  
  return(cohortDefinitions[[id]])
}


getResultLocations <- function(resultLocation){
  # get the model locations...
  
  resultLocs <- file.path(
    dir(
      resultLocation, 
      pattern = 'Analysis_', 
      full.names = T
    ), 
    'plpResult'
  )
  
  if(dir.exists(file.path(resultLocation, 'Validation'))){
    validationDatabases <- dir(file.path(resultLocation, 'Validation'))
    
    valLocs <- dir(
      unlist(
        lapply(
          validationDatabases,
          function(x) dir(file.path(resultLocation, 'Validation', x), 
                          pattern = 'Analysis_', 
                          full.names = T
          )
        )
      ),
      full.names = T
    )
    
    resultLocs <- c(resultLocs, valLocs)
    
  }
  return(resultLocs)
  
}

deleteTables <- function(
  conn,
  databaseSchema,
  targetDialect,
  tempEmulationSchema,
  tableNames, 
  stringAppendToTables
){
  
  if(stringAppendToTables != ''){
    tableNames <- paste0(toupper(gsub('_','',gsub(' ','', stringAppendToTables))), '_', tableNames)
  }
  
  alltables <- DatabaseConnector::getTableNames(
    connection = conn, 
    databaseSchema = databaseSchema
  )
  
  for(tb in tableNames){
    if(tb %in% alltables){
      
      if(targetDialect != 'sqlite'){
        sql <- 'TRUNCATE TABLE @my_schema.@table'
        sql <- SqlRender::render(
          sql, 
          my_schema = databaseSchema, 
          table = tb
        )
        sql <- SqlRender::translate(
          sql, 
          targetDialect = targetDialect, 
          tempEmulationSchema = tempEmulationSchema
        )
        DatabaseConnector::executeSql(conn, sql)
      } else{
        sql <- 'DELETE FROM @my_schema.@table'
        sql <- SqlRender::render(
          sql, 
          my_schema = databaseSchema, 
          table = tb
        )
        sql <- SqlRender::translate(
          sql, 
          targetDialect = targetDialect,
          tempEmulationSchema = tempEmulationSchema
        )
        DatabaseConnector::executeSql(conn, sql)
      }
        sql <- 'DROP TABLE @my_schema.@table'
        sql <- SqlRender::render(
          sql, 
          my_schema = databaseSchema, 
          table = tb
        )
        sql <- SqlRender::translate(
          sql, targetDialect = targetDialect,
          tempEmulationSchema = tempEmulationSchema
        )
        DatabaseConnector::executeSql(conn, sql)
    }
    
  }
  
}


## Template Helpers

enc <- function(x){
  return(paste0("'", x, "'"))
}

cleanNum <- function(x){
  types <- unlist(lapply(1:ncol(x), function(i) class(x[,i])))
  
  ids <- which(types%in% c("numeric", "integer" ))
  
  for(id in ids){
    okVals <- is.finite(x[,id])
    
    if(sum(okVals)!=length(okVals)){
      x[!okVals,id] <- NA
    }
    
  }
  return(x)  
}

checkTable <- function(conn,
                       resultSchema, 
                       stringAppendToTables = '',
                       targetDialect,
                       tableName,
                       columnNames, 
                       values,
                       tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  vals <- paste0(paste0(columnNames," = ", values), collapse = " and ")
  
  sql <- "SELECT * from @my_schema.@string_to_append@table where @input_vals;"
  sql <- SqlRender::render(sql, 
                           my_schema = resultSchema,
                           table = tableName,
                           input_vals = vals,
                           string_to_append = stringAppendToTables)
  sql <- SqlRender::translate(sql, targetDialect = targetDialect,
                              tempEmulationSchema = tempEmulationSchema)
  result <- DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = T)
  
  return(result)
}


checkJson <- function(conn,
                      resultSchema, 
                      stringAppendToTables = '',
                      targetDialect,
                      tableName,
                      jsonColumnName,
                      id,
                      json,
                      tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  sql <- "SELECT * from @my_schema.@string_to_append@table;"
  sql <- SqlRender::render(sql, 
                           my_schema = resultSchema,
                           table = tableName,
                           string_to_append = stringAppendToTables)
  sql <- SqlRender::translate(sql, targetDialect = targetDialect,
                              tempEmulationSchema = tempEmulationSchema)
  result <- DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = T)
  
  resultId <- NULL
  if(nrow(result)>0){
    colId <- result[,jsonColumnName] == json
    if(sum(colId)>0){
      resultId <- result[colId,id][1]
    }
  }
  
  return(resultId)
}




# adds json from package unless json is specified
addCohort <- function(conn, resultSchema, targetDialect,
                      stringAppendToTables = '',
                      cohortDefinition, # this is the R list of the cohortDefinition
                      tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  
  # make sure the json has been converted 
  if(class(cohortDefinition)!='character'){
    ParallelLogger::logInfo('converting json to character')
    json <- ParallelLogger::convertSettingsToJson(cohortDefinition)
    json <- as.character(json) # now convert to character
  }
  
  # reduce the size to save
  json <-  substr(json, 1, 4000) # TESTING - FIX THIS [TODO]
  
  #check whether cohort already in table:
  result <- checkTable(conn = conn, 
                       resultSchema = resultSchema, 
                       stringAppendToTables = stringAppendToTables,
                       targetDialect = targetDialect, 
                       tableName = 'cohorts',
                       columnNames = c('cohort_name'), 
                       values = c(paste0("'",cohortDefinition$name,"'")),
                       tempEmulationSchema = tempEmulationSchema
  )
  
  addNew <- F
  if(nrow(result)>0){
    addNew <- json %in% result$cohortJson
    ParallelLogger::logInfo(paste0('json in jsons:', addNew))
  }
  
  if(addNew){
    ParallelLogger::logInfo(paste0('Cohort ',cohortDefinition$name,' exists in result database with id', result$cohortId))
  } else{
    ParallelLogger::logInfo(paste0('Adding cohort ',cohortDefinition$name))
    
    data <- data.frame(cohortName = cohortDefinition$name, 
                       atlasId = cohortDefinition$id,
                       cohortJson = json)
    DatabaseConnector::insertTable(connection = conn, 
                                   databaseSchema = resultSchema, 
                                   tableName = paste0(stringAppendToTables, 'cohorts'),
                                   data = data,
                                   dropTableIfExists = F, 
                                   createTable = F, 
                                   tempTable = F, 
                                   progressBar = T,
                                   camelCaseToSnakeCase = T, 
                                   tempEmulationSchema = tempEmulationSchema
    )
    
    # now check and get id
    result <- checkTable(conn = conn, 
                         resultSchema = resultSchema, 
                         stringAppendToTables = stringAppendToTables,
                         targetDialect = targetDialect, 
                         tableName = 'cohorts',
                         columnNames = c('cohort_name', 'atlas_id'), 
                         values = c(paste0("'",cohortDefinition$name,"'"), cohortDefinition$id),
                         tempEmulationSchema = tempEmulationSchema
    )
    
    jsonInd <- result$cohortJson %in% json
    result <- result[jsonInd,]
    
  }
  
  return(result$cohortId[1])
}



addDatabase <- function(
  conn, 
  databaseSchemaSettings,
  databaseDetail
){
  
  result <- checkTable(conn = conn, 
                       resultSchema = databaseSchemaSettings$resultSchema, 
                       stringAppendToTables = databaseSchemaSettings$stringAppendToResultSchemaTables,
                       targetDialect = databaseSchemaSettings$targetDialect, 
                       tableName = 'database_details',
                       columnNames = c('database_name', 'database_acronym',
                                       'database_version',
                                       'database_description', 'database_type'), 
                       values = c(paste0("'",databaseDetail$name,"'"), 
                                  paste0("'",databaseDetail$acronym,"'"),
                                  paste0("'",databaseDetail$version,"'"),
                                  paste0("'",databaseDetail$description,"'"),
                                  paste0("'",databaseDetail$type,"'")),
                       tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  
  if(nrow(result)>0){
    ParallelLogger::logInfo(paste0('Database ', databaseDetail$name ,' already exists'))
  } else {
    
    sql <- "INSERT INTO @my_schema.@string_to_appenddatabase_details(database_name, database_acronym,
                                  database_version,
                                  database_description, database_type) 
          VALUES ('@dbName','@db', '@version', '@desc', '@type');"
    sql <- SqlRender::render(sql, 
                             my_schema = databaseSchemaSettings$resultSchema,
                             dbName =databaseDetail$name, 
                             db = databaseDetail$acronym,
                             version = databaseDetail$version,
                             desc = databaseDetail$description,
                             type = databaseDetail$type,
                             string_to_append = databaseSchemaSettings$stringAppendToResultSchemaTables)
    sql <- SqlRender::translate(sql, targetDialect = databaseSchemaSettings$targetDialect,
                                tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema)
    DatabaseConnector::executeSql(conn, sql)
    
    result <- checkTable(conn = conn, 
                         resultSchema = databaseSchemaSettings$resultSchema, 
                         stringAppendToTables = databaseSchemaSettings$stringAppendToResultSchemaTables,
                         targetDialect = databaseSchemaSettings$targetDialect, 
                         tableName = 'database_details',
                         columnNames = c('database_name', 'database_acronym', 'database_version',
                                         'database_description', 'database_type'), 
                         values = c(paste0("'",databaseDetail$name,"'"), 
                                    paste0("'",databaseDetail$acronym,"'"),
                                    paste0("'",databaseDetail$version,"'"),
                                    paste0("'",databaseDetail$description,"'"),
                                    paste0("'",databaseDetail$type,"'")),
                         tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
    )
    
  }
  
  return(result$databaseId[1])
  
}









