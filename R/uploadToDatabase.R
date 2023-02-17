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
  
  ensure_installed('RSQLite')
  
  # create sqlite database
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = 'sqlite',
    server = file.path(sqliteLocation,'databaseFile.sqlite')
  )

  createPlpResultTables(
    connectionDetails = connectionDetails,
    targetDialect = 'sqlite',
    resultSchema = 'main', 
    deleteTables = T, 
    createTables = T,
    tablePrefix = ''
  )
  
  #cohortDefinitions <- data.frame(
  #  cohortId = c(runPlp$model$modelDesign$targetId, runPlp$model$modelDesign$outcomeId),
  #  cohortName = c('Target', 'Outcome'),
  #  json = c('{}', '{}')
  #  )
    
  addRunPlpToDatabase(
    runPlp = runPlp,
    connectionDetails = connectionDetails,
    databaseSchemaSettings = createDatabaseSchemaSettings(resultSchema = 'main'),
    cohortDefinitions = NULL,#cohortDefinitions,
    databaseList = NULL, 
    modelSaveLocation = sqliteLocation
  )
  
  # add validation results if entered
  if(!is.null(externalValidatePlp)){
    if(inherits(x = externalValidatePlp, what =  'list')){
      for(i in 1:length(externalValidatePlp)){
        tryCatch(
          {
            addRunPlpToDatabase(
              runPlp = externalValidatePlp[[i]],
              connectionDetails = connectionDetails,
              databaseSchemaSettings = createDatabaseSchemaSettings(resultSchema = 'main'),
              cohortDefinitions = NULL,#cohortDefinitions,
              databaseList = NULL, 
              modelSaveLocation = sqliteLocation
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
          connectionDetails = connectionDetails,
          databaseSchemaSettings = createDatabaseSchemaSettings(resultSchema = 'main'),
          cohortDefinitions = NULL,#cohortDefinitions,
          databaseList = NULL
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
#' @param cohortDefinitions            A set of one or more cohorts extracted using ROhdsiWebApi::exportCohortDefinitionSet()
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
  databaseList = NULL,
  sqliteLocation = file.path(resultLocation, 'sqlite')
){
  
  if(!dir.exists(sqliteLocation)){
    dir.create(sqliteLocation, recursive = T)
  }
  
  ensure_installed('RSQLite')
  
  # create sqlite database
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = 'sqlite',
    server = file.path(sqliteLocation,'databaseFile.sqlite')
  )
  
  # create tables if they dont exist
  createPlpResultTables(
      connectionDetails = connectionDetails,
      targetDialect = 'sqlite',
      resultSchema = 'main', 
      deleteTables = T, 
      createTables = T,
      tablePrefix = ''
    )

  # run insert models
  addMultipleRunPlpToDatabase(
    connectionDetails = connectionDetails, 
    databaseSchemaSettings = createDatabaseSchemaSettings(resultSchema = 'main'),
    cohortDefinitions = cohortDefinitions,
    databaseList = databaseList,
    resultLocation = resultLocation,
    modelSaveLocation = sqliteLocation
  )
  
  # run insert diagnosis
  addMultipleDiagnosePlpToDatabase(
    connectionDetails = connectionDetails,
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
#' @param connectionDetails            The database connection details 
#' @param targetDialect                The database management system being used
#' @param resultSchema                 The name of the database schema that the result tables will be created.
#' @param deleteTables                 If true any existing tables matching the PatientLevelPrediction result tables names will be deleted
#' @param createTables                 If true the PatientLevelPrediction result tables will be created
#' @param tablePrefix         A string that appends to the PatientLevelPrediction result tables
#' @param tempEmulationSchema          The temp schema used when the database management system is oracle
#' 
#' @param testFile                     (used for testing) The location of an sql file with the table creation code
#'
#' @return
#' Returns NULL but creates the required tables into the specified database schema(s).
#' 
#' @export
createPlpResultTables <- function(
    connectionDetails,
  targetDialect = 'postgresql',
  resultSchema, 
  deleteTables = T, 
  createTables = T,
  tablePrefix = '',
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
  testFile = NULL
){
  
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))
  
  tablesExists <- sum(tolower(getPlpResultTables()) %in% tolower(DatabaseConnector::getTableNames(conn)))
  tablesExists <- tablesExists == length(getPlpResultTables())
  
  if(!tablesExists){
    ParallelLogger::logInfo('All or some PLP result tables do not exist, tables being recreated')
    if(deleteTables){
      ParallelLogger::logInfo('Deleting existing tables')
      
      tableNames <- getPlpResultTables()
      
      deleteTables(
        conn = conn,
        databaseSchema = resultSchema,
        targetDialect = targetDialect,
        tempEmulationSchema = tempEmulationSchema,
        tableNames = tableNames, 
        tablePrefix = tablePrefix
      )
      
    }
    
    
    if(createTables){
      ParallelLogger::logInfo('Creating PLP results tables')
      
      if(tablePrefix != ''){
        tablePrefix <- paste0(toupper(gsub('_','',gsub(' ','', tablePrefix))), '_')
      }
      
      pathToSql <- system.file(
        paste("sql/", targetDialect, 
              sep = ""),
        "PlpResultTables.sql", 
        package = "PatientLevelPrediction"
      )
      
      if(!file.exists(pathToSql)){
        # if no dbms specific file use sql_server
        pathToSql <- system.file(
          paste("sql/", 'sql_server', 
                sep = ""),
          "PlpResultTables.sql", 
          package = "PatientLevelPrediction"
        )
      }
      
      sql <- readChar(pathToSql, file.info(pathToSql)$size) 
      renderedSql <- SqlRender::render(
        sql = sql[1],
        my_schema = resultSchema,
        string_to_append = tablePrefix
      )
      renderedSql <- SqlRender::translate(
        sql = renderedSql,
        targetDialect = targetDialect,
        tempEmulationSchema = tempEmulationSchema
      )
      
      DatabaseConnector::executeSql(conn, renderedSql)
    }
    
  } else{
    ParallelLogger::logInfo('PLP result tables already exist')
  }
  
  # then migrate
  ParallelLogger::logInfo('PLP result migrration being applied')
  migrateDataModel(
    connectionDetails = connectionDetails, # input is connection
    databaseSchema = resultSchema,
    tablePrefix = tablePrefix
  )
  
}

#' Populate the PatientLevelPrediction results tables
#' @description
#' This function formats and uploads results that have been generated via an ATLAS prediction package into a database
#'
#' @details
#' This function can be used upload PatientLevelPrediction results into a database
#'
#' @param connectionDetails            A connection details created by using the
#'                                     function \code{createConnectionDetails} in the
#'                                     \code{DatabaseConnector} package.
#' @param databaseSchemaSettings       A object created by \code{createDatabaseSchemaSettings} with all the settings specifying the result tables                              
#' @param cohortDefinitions            A set of one or more cohorts extracted using ROhdsiWebApi::exportCohortDefinitionSet()
#' @param databaseList              (Optional) A list created by \code{createDatabaseList} to specify the databases
#' @param resultLocation               (string) location of directory where the main package results were saved
#' @param resultLocationVector         (only used when resultLocation is missing) a vector of locations with development or validation results  
#' @param modelSaveLocation              The location of the file system for saving the models in a subdirectory  
#'    
#' @return
#' Returns NULL but uploads all the results in resultLocation to the PatientLevelPrediction result tables in resultSchema
#' 
#' @export
addMultipleRunPlpToDatabase <- function(
    connectionDetails, 
    databaseSchemaSettings = createDatabaseSchemaSettings(resultSchema = 'main'),
    cohortDefinitions,
    databaseList = NULL,
    resultLocation = NULL,
    resultLocationVector,
    modelSaveLocation
){
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))
  
  # for each development result add it to the database:
  
  if(missing(resultLocationVector)){
    resultLocationVector <- getResultLocations(resultLocation)
  }
  
  if(length(resultLocationVector) == 0){
    ParallelLogger::logInfo('No results found')
    return(NULL)
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
        connectionDetails = connectionDetails,
        databaseSchemaSettings = databaseSchemaSettings,
        cohortDefinitions = cohortDefinitions,
        databaseList = databaseList,
        modelSaveLocation = modelSaveLocation
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
#' @param tablePrefix         (string) A string that appends to the PatientLevelPrediction result tables
#' @param targetDialect                (string) The database management system being used
#' @param tempEmulationSchema          (string) The temp schema used when the database management system is oracle
#' @param cohortDefinitionSchema                 (string) The name of the database schema with the cohort definition tables (defaults to resultSchema).
#' @param tablePrefixCohortDefinitionTables         (string) A string that appends to the cohort definition tables
#' @param databaseDefinitionSchema                 (string) The name of the database schema with the database definition tables (defaults to resultSchema).
#' @param tablePrefixDatabaseDefinitionTables         (string) A string that appends to the database definition tables
#'    
#' @return
#' Returns a list of class 'plpDatabaseResultSchema' with all the database settings
#' 
#' @export
createDatabaseSchemaSettings <- function(
  resultSchema = 'main', 
  tablePrefix = '',
  targetDialect = 'sqlite',
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
  cohortDefinitionSchema = resultSchema,
  tablePrefixCohortDefinitionTables = tablePrefix,
  databaseDefinitionSchema = resultSchema,
  tablePrefixDatabaseDefinitionTables = tablePrefix
){
  
  if(missing(resultSchema)){
    stop('resultSchema required')
  }
  if(!inherits(x = resultSchema, what = "character")){
    stop('resultSchema must be a string')
  }
  
  if(tablePrefix != ''){
    tablePrefix <- paste0(toupper(gsub('_','',gsub(' ','', tablePrefix))), '_')
  }
  if(tablePrefixCohortDefinitionTables != ''){
    tablePrefixCohortDefinitionTables <- paste0(toupper(gsub('_','',gsub(' ','', tablePrefixCohortDefinitionTables))), '_')
  }
  if(tablePrefixDatabaseDefinitionTables != ''){
    tablePrefixDatabaseDefinitionTables <- paste0(toupper(gsub('_','',gsub(' ','', tablePrefixDatabaseDefinitionTables))), '_')
  }
  
  result <- list(
    resultSchema = resultSchema,
    tablePrefix = tablePrefix,
    targetDialect = targetDialect,
    tempEmulationSchema = tempEmulationSchema,
    cohortDefinitionSchema  = cohortDefinitionSchema, # could be removed
    tablePrefixCohortDefinitionTables = tablePrefixCohortDefinitionTables, # could be removed
    databaseDefinitionSchema = databaseDefinitionSchema, # could be removed
    tablePrefixDatabaseDefinitionTables = tablePrefixDatabaseDefinitionTables # could be removed
  )
  
  class(result) <- 'plpDatabaseResultSchema'
  return(result)
}


#' Create a list with the database details and database meta data entries
#' @description
#' This function creates a list with the database details and database meta data entries used in the study
#'
#' @details
#' This function is used when inserting database details into the PatientLevelPrediction database results schema
#' 
#' @param cdmDatabaseSchemas           (string vector) A vector of the cdmDatabaseSchemas used in the study - if the schemas are not unique per database please also specify databaseRefId
#' @param cdmDatabaseNames             Sharable names for the databases
#' @param databaseRefIds               (string vector) Unique database identifiers - what you specified as cdmDatabaseId in \code{PatientLevelPrediction::createDatabaseDetails()} when developing the models
#'
#' @return
#' Returns a data.frame with the database details
#' 
#' @export
createDatabaseList <- function(
  cdmDatabaseSchemas,
  cdmDatabaseNames,
  databaseRefIds = NULL
){
  if(missing(cdmDatabaseSchemas)){
    stop('Need to specify cdmDatabaseSchemas')
  }
  
  if(is.null(databaseRefIds)){
    ParallelLogger::logInfo('No databaseRefId specified so using schema as unique database identifier')
    databaseRefIds <- removeInvalidString(cdmDatabaseSchemas)
  }
  if(missing(cdmDatabaseNames)){
    cdmDatabaseNames <- removeInvalidString(cdmDatabaseSchemas)
  }
  
  
  result <- lapply(
    1:length(cdmDatabaseSchemas),
    
    function(i) list(
      databaseDetails = list(
        databaseMetaDataId = databaseRefIds[i]
      ),
      databaseMetaData = list(
        databaseId = databaseRefIds[i],
        cdmSourceName = cdmDatabaseSchemas[i],
        cdmSourceAbbreviation = cdmDatabaseNames[i],
        cdmHolder = '', # could get this from CDM_source inside runPlp in future
        sourceDesciption = '',
        sourceDocumentReference = '',
        cdmEtlReference = '', 
        sourceReleaseDate = '',
        cdmReleaseDate = '',
        cdmVersion = '',
        vocabularyVersion = '',
        maxObsPeriodEndDate = ''
      )
    )
  )
  
  names(result) <- databaseRefIds #cdmDatabaseSchemas
  # using id as schema may not be unique
  # id uses schema if it is not set

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
#' @param connectionDetails            A connection details created by using the
#'                                     function \code{createConnectionDetails} in the
#'                                     \code{DatabaseConnector} package.
#' @param databaseSchemaSettings       A object created by \code{createDatabaseSchemaSettings} with all the settings specifying the result tables                              
#' @param cohortDefinitions            A set of one or more cohorts extracted using ROhdsiWebApi::exportCohortDefinitionSet()
#' @param modelSaveLocation         The location of the directory that models will be saved to
#' @param databaseList              (Optional) If you want to change the database name then used \code{createDatabaseList} to specify the database settings but use the same cdmDatabaseId was model development/validation
#' 
#' @return
#' Returns a data.frame with the database details
#' 
#' @export
addRunPlpToDatabase <- function(
  runPlp,
  connectionDetails,
  databaseSchemaSettings,
  cohortDefinitions,
  modelSaveLocation,
  databaseList = NULL
){
  
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))
  
  modelDesignId <- insertModelDesignInDatabase(
    object = runPlp$model$modelDesign, 
    conn = conn, 
    databaseSchemaSettings = databaseSchemaSettings,
    cohortDefinitions = cohortDefinitions 
  )
  
  # Add model if runPlp
  if(inherits(runPlp, 'runPlp')){
    includesModel <- T
    developmentDatabase <- runPlp$model$trainDetails$developmentDatabase
    validationDatabase <- runPlp$model$trainDetails$developmentDatabase
    developmentDatabaseRefId <- runPlp$model$trainDetails$developmentDatabaseId
    validationDatabaseRefId <- runPlp$model$trainDetails$developmentDatabaseId
    
    populationSettings <- runPlp$model$modelDesign$populationSettings
    targetId <- runPlp$model$modelDesign$targetId
    outcomeId <- runPlp$model$modelDesign$outcomeId
    restrictPlpDataSettings <- runPlp$model$modelDesign$restrictPlpDataSettings
    
    modelDevelopment <- 1 #added
    
    attrition <- runPlp$model$trainDetails$attrition

  } else{
    includesModel <- F
    developmentDatabase <- runPlp$model$validationDetails$developmentDatabase
    validationDatabase <- runPlp$model$validationDetails$validationDatabase
    developmentDatabaseRefId <- runPlp$model$validationDetails$developmentDatabaseId
    validationDatabaseRefId <- runPlp$model$validationDetails$validationDatabaseId
    
    populationSettings <- runPlp$model$validationDetails$populationSettings
    targetId <- runPlp$model$validationDetails$targetId
    outcomeId <- runPlp$model$validationDetails$outcomeId
    restrictPlpDataSettings <- runPlp$model$validationDetails$restrictPlpDataSettings
    
    modelDevelopment <- 0 #added
    
    attrition <- runPlp$model$validationDetails$attrition
    
  }
  
  # Add databases
  developmentDatabaseId <- addDatabase(
    conn = conn, 
    databaseSchemaSettings = databaseSchemaSettings,
    databaseList = databaseList,
    databaseSchema = developmentDatabase, 
    databaseId = developmentDatabaseRefId
  )
  
  validationDatabaseId <- addDatabase(
    conn = conn, 
    databaseSchemaSettings = databaseSchemaSettings,
    databaseList = databaseList,
    databaseSchema = validationDatabase,
    databaseId = validationDatabaseRefId
  )
  
  
  # add nodel if the result contains it
  if(includesModel){
    insertModelInDatabase(
      model = runPlp$model,
      conn = conn,
      databaseSchemaSettings = databaseSchemaSettings,
      databaseId = developmentDatabaseId,
      modelDesignId = modelDesignId,
      modelSaveLocation = modelSaveLocation
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
    tablePrefix = databaseSchemaSettings$tablePrefix,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  
  validationTargetId <-  addCohort(
    conn = conn, 
    resultSchema = databaseSchemaSettings$cohortDefinitionSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    cohortDefinition = getCohortDef(cohortDefinitions,targetId),
    tablePrefix = databaseSchemaSettings$tablePrefixCohortDefinitionTables,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  
  validationOutcomeId <-  addCohort(
    conn = conn, 
    resultSchema = databaseSchemaSettings$cohortDefinitionSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    cohortDefinition = getCohortDef(cohortDefinitions,outcomeId),
    tablePrefix = databaseSchemaSettings$tablePrefixCohortDefinitionTables,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  
  validationPopulationId <- addPopulationSetting(
    conn = conn, 
    resultSchema = databaseSchemaSettings$resultSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    json = populationSettings, 
    tablePrefix = databaseSchemaSettings$tablePrefix,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  
  validationPlpDataId <-  addPlpDataSetting(
    conn = conn, 
    resultSchema = databaseSchemaSettings$resultSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    json = restrictPlpDataSettings, 
    tablePrefix = databaseSchemaSettings$tablePrefix,
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
    validationTargetId = validationTargetId,
    validationOutcomeId = validationOutcomeId,
    
    modelDevelopment = modelDevelopment
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
  modelDesignId,
  modelSaveLocation
){
  
  # save the model to the file system
  modelLocation <- file.path(
    modelSaveLocation, 
    'models',
    paste0('folder-', modelDesignId, '-', databaseId)
    )
  if(!dir.exists(modelLocation)){
    dir.create(modelLocation, recursive = T)
  }
  # savign all the model as preprocessing was too large
  # for database
  savePlpModel(
    plpModel = model, 
    dirPath = modelLocation
    )
  
  #saveModelPart(
  #  model = model$model,
  #  savetype = attr(model, 'saveType'),
  #  dirPath = modelLocation
  #)
  
  # need hyperParamSearch for shiny app but the other parts
  # are too large to store into the database
  trainDetails <- list(hyperParamSearch = model$trainDetails$hyperParamSearch)
  
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
      plpModelFile = modelLocation, # save the model to a location and add location here
      trainDetails = as.character(ParallelLogger::convertSettingsToJson(trainDetails)),
      preprocessing = "",#as.character(ParallelLogger::convertSettingsToJson(model$preprocessing)),
      
      executionDateTime = format(model$trainDetails$trainingDate, format="%Y-%m-%d"), 
      trainingTime = model$trainDetails$trainingTime, 
      intercept = ifelse(is.list(model$model) & attr(model, 'saveType') != 'xgboost', model$model$coefficients$betas[1], 0),  # using the param useIntercept?
      
      tablePrefix = databaseSchemaSettings$tablePrefix,
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
  tablePrefix,
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
    tablePrefix = tablePrefix,
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
      
      string_to_append = tablePrefix
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
      tablePrefix = tablePrefix,
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
      "DATABASE_META_DATA",
      "COHORT_DEFINITION",
      "COHORTS"
    )
  )
}

getResultLocations <- function(resultLocation){
  # get the model locations...
  
  resultLocs <- dir(
      resultLocation, 
      pattern = 'Analysis_', 
      full.names = T
  )
  # automatically find Results folder, to handle both plpResult/ and validationResult/
  resultLocs <- file.path(resultLocs, dir(resultLocs, pattern='Result'))
  
  
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
  tablePrefix
){
  
  if(tablePrefix != ''){
    tableNames <- tolower(paste0(gsub('_','',gsub(' ','', tablePrefix)), '_', tableNames))
  }
  
  alltables <- tolower(DatabaseConnector::getTableNames(
    connection = conn, 
    databaseSchema = databaseSchema
  ))
  
  
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
                       tablePrefix = '',
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
                           string_to_append = tablePrefix)
  sql <- SqlRender::translate(sql, targetDialect = targetDialect,
                              tempEmulationSchema = tempEmulationSchema)
  result <- DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = T)
  
  return(result)
}


checkJson <- function(conn,
                      resultSchema, 
                      tablePrefix = '',
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
                           string_to_append = tablePrefix)
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


getCohortDef <- function(cohortDefinitions, cohortId){
  if(!is.null(cohortDefinitions)){
    if(sum(cohortDefinitions$cohortId == cohortId) > 0){
      return(cohortDefinitions[cohortDefinitions$cohortId == cohortId, ])
    }
  }
  return(
    data.frame(
      cohortId = cohortId, 
      cohortName = paste0('Cohort: ', cohortId),
      json = '{}'
    )
  )
}


# adds json from package unless json is specified
addCohort <- function(
  conn, 
  resultSchema, 
  targetDialect,
  tablePrefix = '',
  cohortDefinition, # this is the R data.frame of the cohortDefinition
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  
  # make sure the json has been converted 
  json <- cohortDefinition$json
  if(!inherits(x = json , what = 'character')){
    ParallelLogger::logInfo('converting json to character')
    json <- as.character(json) # now convert to character
  }
  
  # reduce the size to save
  if(!targetDialect %in% c('sqlite', 'postgres')){
    json <-  substr(json, 1, 4000) # TESTING - FIX THIS [TODO]
  }
  
  #check whether cohort already in COHORT_DEFINITION table:
  result <- checkTable(conn = conn, 
                       resultSchema = resultSchema, 
                       tablePrefix = tablePrefix,
                       targetDialect = targetDialect, 
                       tableName = 'cohort_definition',
                       columnNames = c('cohort_name'), 
                       values = c(paste0("'",cohortDefinition$cohortName,"'")),
                       tempEmulationSchema = tempEmulationSchema
  )
  
  addNew <- F
  if(nrow(result)>0){
    addNew <- json %in% result$json
    ParallelLogger::logInfo(paste0('json in jsons:', addNew))
  }
  
  if(addNew){
    cohortDefinitionId <- result$cohortDefinitionId[result$json %in% json]
    ParallelLogger::logInfo(paste0('Cohort ',cohortDefinition$cohortName,' exists in cohort_definition with cohort id', result$cohortDefinitionId[result$json %in% json]))
  } else{
    ParallelLogger::logInfo(paste0('Adding cohort ',cohortDefinition$cohortName))
    
    data <- data.frame(
      cohortName = cohortDefinition$cohortName, 
      cohortDefinitionId = cohortDefinition$cohortId,
      json = json
    )
    DatabaseConnector::insertTable(
      connection = conn, 
      databaseSchema = resultSchema, 
      tableName = paste0(tablePrefix, 'cohort_definition'),
      data = data,
      dropTableIfExists = F, 
      createTable = F, 
      tempTable = F, 
      progressBar = T,
      camelCaseToSnakeCase = T, 
      tempEmulationSchema = tempEmulationSchema
    )
    
    # now check and get id
    result <- checkTable(
      conn = conn, 
      resultSchema = resultSchema, 
      tablePrefix = tablePrefix,
      targetDialect = targetDialect, 
      tableName = 'cohort_definition',
      columnNames = c('cohort_name', 'cohort_definition_id'), 
      values = c(paste0("'",cohortDefinition$cohortName,"'"), cohortDefinition$cohortId),
      tempEmulationSchema = tempEmulationSchema
    )
    
    jsonInd <- result$json %in% json
    cohortDefinitionId <- result$cohortDefinitionId[jsonInd]
  }
  
  # now add to cohorts table
  result <- checkTable(conn = conn, 
                       resultSchema = resultSchema, 
                       tablePrefix = tablePrefix,
                       targetDialect = targetDialect, 
                       tableName = 'cohorts',
                       columnNames = c('cohort_definition_id','cohort_name'), 
                       values = c(cohortDefinitionId, paste0("'",cohortDefinition$cohortName,"'")),
                       tempEmulationSchema = tempEmulationSchema
  )
  
  if(nrow(result)>0){
    ParallelLogger::logInfo(paste0('Cohort ',cohortDefinition$cohortName,' exists in cohorts with cohort id', result$cohortId))
  } else{
    ParallelLogger::logInfo(paste0('Adding cohort ',cohortDefinition$cohortName))
    
    data <- data.frame(
      cohortDefinitionId = cohortDefinitionId,
      cohortName = cohortDefinition$cohortName
    )
    DatabaseConnector::insertTable(
      connection = conn, 
      databaseSchema = resultSchema, 
      tableName = paste0(tablePrefix, 'cohorts'),
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
                         tablePrefix = tablePrefix,
                         targetDialect = targetDialect, 
                         tableName = 'cohorts',
                         columnNames = c('cohort_definition_id','cohort_name'), 
                         values = c(cohortDefinitionId, paste0("'",cohortDefinition$cohortName,"'")),
                         tempEmulationSchema = tempEmulationSchema
    )
  }
  
  return(result$cohortId[1])
}



addDatabase <- function(
  conn, 
  databaseSchemaSettings,
  databaseList = NULL, # list with the database details
  databaseId = NULL, # the database id
  databaseSchema # the database schema
){
  
  if(is.null(databaseId)){
    databaseId <- removeInvalidString(databaseSchema)
  }
  
  # get the database tables for the databaseId 
  if(is.null(databaseList)){
    databaseDataFrames <- createDatabaseList(cdmDatabaseSchemas = databaseSchema, databaseRefIds = databaseId)[[1]]
  } else{
    if(databaseId %in% names(databaseList)){
      databaseDataFrames <- databaseList[[as.character(databaseId)]]
    } else{
      ParallelLogger::logInfo('database ID not found in databaseList so added new entry')
      databaseDataFrames <- createDatabaseList(cdmDatabaseSchemas = databaseSchema, databaseRefIds = databaseId)[[1]]
    }
  }
  
  
  # check the database_meta_data
  result <- checkTable(conn = conn, 
                       resultSchema = databaseSchemaSettings$resultSchema, 
                       tablePrefix = databaseSchemaSettings$tablePrefix,
                       targetDialect = databaseSchemaSettings$targetDialect, 
                       tableName = 'database_meta_data',
                       columnNames = c('database_id'), 
                       values = c(paste0("'",databaseDataFrames$databaseMetaData$databaseId,"'")),
                       tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  
  if(nrow(result)>0){
    ParallelLogger::logInfo(paste0('Database meta data ', databaseDataFrames$database_meta_data$databaseId ,' already exists'))
  } else {
    
    sql <- "INSERT INTO @my_schema.@string_to_appenddatabase_meta_data(
      database_id,
      cdm_source_name,
      cdm_source_abbreviation
    ) 
          VALUES ('@database_id','@cdm_source_name', '@cdm_source_abbreviation');"
    sql <- SqlRender::render(
      sql, 
      my_schema = databaseSchemaSettings$resultSchema,
      database_id = databaseDataFrames$databaseMetaData$databaseId, 
      cdm_source_name = databaseDataFrames$databaseMetaData$cdmSourceName,
      cdm_source_abbreviation = databaseDataFrames$databaseMetaData$cdmSourceAbbreviation,
      string_to_append = databaseSchemaSettings$tablePrefix
    )
    sql <- SqlRender::translate(sql, targetDialect = databaseSchemaSettings$targetDialect,
                                tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema)
    DatabaseConnector::executeSql(conn, sql)
    
    result <- checkTable(conn = conn, 
                         resultSchema = databaseSchemaSettings$resultSchema, 
                         tablePrefix = databaseSchemaSettings$tablePrefix,
                         targetDialect = databaseSchemaSettings$targetDialect, 
                         tableName = 'database_meta_data',
                         columnNames = c('database_id', 'cdm_source_name',
                                         'cdm_source_abbreviation'), 
                         values = c(paste0("'",databaseDataFrames$databaseMetaData$databaseId,"'"), 
                                    paste0("'",databaseDataFrames$databaseMetaData$cdmSourceName,"'"),
                                    paste0("'",databaseDataFrames$databaseMetaData$cdmSourceAbbreviation,"'")),
                         tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
    )
    
  }
  
  result <- checkTable(conn = conn, 
                       resultSchema = databaseSchemaSettings$resultSchema, 
                       tablePrefix = databaseSchemaSettings$tablePrefix,
                       targetDialect = databaseSchemaSettings$targetDialect, 
                       tableName = 'database_details',
                       columnNames = c('database_meta_data_id'), 
                       values = c(paste0("'",databaseDataFrames$databaseDetails$databaseMetaDataId,"'")
                                  ),
                       tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  
  if(nrow(result)>0){
    ParallelLogger::logInfo(paste0('Database', result$databaseId ,' already exists'))
  } else {
    
    sql <- "INSERT INTO @my_schema.@string_to_appenddatabase_details(database_meta_data_id) 
          VALUES ('@database_meta_data_id');"
    sql <- SqlRender::render(sql, 
                             my_schema = databaseSchemaSettings$resultSchema,
                             database_meta_data_id = databaseDataFrames$databaseDetails$databaseMetaDataId, 
                             string_to_append = databaseSchemaSettings$tablePrefix)
    sql <- SqlRender::translate(sql, targetDialect = databaseSchemaSettings$targetDialect,
                                tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema)
    DatabaseConnector::executeSql(conn, sql)
    
    result <- checkTable(conn = conn, 
                         resultSchema = databaseSchemaSettings$resultSchema, 
                         tablePrefix = databaseSchemaSettings$tablePrefix,
                         targetDialect = databaseSchemaSettings$targetDialect, 
                         tableName = 'database_details',
                         columnNames = c('database_meta_data_id'), 
                         values = c(paste0("'",databaseDataFrames$databaseDetails$databaseMetaDataId,"'")
                         ),
                         tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
    )
    
  }
  
  return(result$databaseId[1])
  
}









