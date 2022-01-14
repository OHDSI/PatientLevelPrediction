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
#' @param resultSchema                 The name of the database schema that the result tables will be created.
#' @param targetDialect                The database management system being used
#' @param deleteExistingTables         If true any existing tables matching the PatientLevelPrediction result tables names will be deleted
#' @param createTables                 If true the PatientLevelPrediction result tables will be created
#' @param stringAppendToTables         A string that appends to the PatientLevelPrediction result tables
#' @param tempEmulationSchema          The temp schema used when the database management system is oracle
#' @param testFile                     (used for testing) The location of an sql file with the table creation code
#'
#' @return
#' Returns NULL but creates the required tables into the specified database schema.
#' 
#' @export
createPlpResultTables <- function(conn, 
                                  resultSchema, 
                                  targetDialect = 'postgresql',
                                  deleteExistingTables = T, 
                                  createTables = T,
                                  stringAppendToTables = '',
                                  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                  testFile = NULL){
  
  
  if(deleteExistingTables){
    ParallelLogger::logInfo('Deleting existing tables')
    
    tables <- c("CALIBRATION_SUMMARY", "COVARIATE_SUMMARY", "DEMOGRAPHIC_SUMMARY",
                "EVALUATION_STATISTICS", "PREDICTION_DISTRIBUTION", "THRESHOLD_SUMMARY",
                
                
                "STUDY_MODELS",
                
                "RESULTS", "MODEL_RELATIONSHIP", "MODELS", "STUDIES", 
                
                "MODEL_SETTINGS", "COVARIATE_SETTINGS","POPULATION_SETTINGS", "TRAINING_SETTINGS",
      
      "FEATURE_ENGINEERING_SETTINGS", "SPLIT_SETTINGS", "PLP_DATA_SETTINGS", #new
       "SAMPLE_SETTINGS", "TIDY_COVARIATES_SETTINGS", #new 
      
                "TARS", "COHORTS", "DATABASE_DETAILS", "RESEARCHERS" )
    
    if(stringAppendToTables != ''){
      tables <- paste0(toupper(gsub('_','',gsub(' ','', stringAppendToTables))), '_', tables)
    }
    
    alltables <- DatabaseConnector::getTableNames(connection = conn, 
                                                  databaseSchema = resultSchema)
    
    for(tb in tables){
      if(tb %in%alltables){
        sql <- 'TRUNCATE TABLE @my_schema.@table'
        sql <- SqlRender::render(sql, 
                                 my_schema = resultSchema, 
                                 table=tb)
        sql <- SqlRender::translate(sql, targetDialect = targetDialect, 
                                    tempEmulationSchema = tempEmulationSchema)
        DatabaseConnector::executeSql(conn, sql)
        
        sql <- 'DROP TABLE @my_schema.@table'
        sql <- SqlRender::render(sql, 
                                 my_schema = resultSchema, 
                                 table=tb)
        sql <- SqlRender::translate(sql, targetDialect = targetDialect,
                                    tempEmulationSchema = tempEmulationSchema)
        DatabaseConnector::executeSql(conn, sql)
      }
      
    }
    
  }
  
  if(createTables){
    ParallelLogger::logInfo('Creating PLP results tables')
    
    if(stringAppendToTables != ''){
      stringAppendToTables <- paste0(toupper(gsub('_','',gsub(' ','', stringAppendToTables))), '_')
    }
    
    if(is.null(testFile)){
      renderedSql <- SqlRender::loadRenderTranslateSql(sqlFilename = "PlpResultTables.sql",
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
#' @param resultSchema                 (string) The name of the database schema that the result tables will be created.
#' @param stringAppendToTables         (string) A string that appends to the PatientLevelPrediction result tables
#' @param targetDialect                (string) The database management system being used
#' @param tempEmulationSchema          (string) The temp schema used when the database management system is oracle
#' @param packageName                  (string) The name of the ATLAS R package used to generate the results (this is used to extract cohort jsons)
#' @param studyJsonList                (list) A list of lists per cohort with the cohort_name, cohort_id and cohort_json 
#' @param studyName                    (string) A reference study name 
#' @param studyDescription             (string) A description of the study
#' @param researcherName               (string) Name of the researcher who developed the study
#' @param researcherEmail              (string) Email of the researcher who developed the study
#' @param researcherOrg                (string) Organisation of the researcher who developed the study
#' @param databaseName                 (string) name of the database used to develop the model/s
#' @param databaseAcronym              (string) acronym of the database used to develop the model/s
#' @param databaseVersion              (int) Version of the database used to develop the model/s
#' @param databaseDescription          (string) Description of the database used to develop the model/s
#' @param databaseType                 (string) Type of the database used to develop the model/s (e.g., claims)
#' @param valDatabases                 (list) A named list with details of the external validation databases.  Needs to contain: name, description, version, type.
#' @param resultLocation               (string) location of directory where the main package results were saved
#' @param resultPattern                (string) A string to match to select models of interest  
#' @param validationLocation           (string) location of directory where the validation package results were saved
#' @param addInternalValidation        (boolean) Whether the internval validation results should be uploaded
#' @param addExternalValidation        (boolean) Whether the externval validation results should be uploaded
#' @param gsubVal                      (string) Remove patterns from the result name
#' @param removePattern                (string) Restrict to result names with this pattern
#'    
#' @return
#' Returns NULL but uploads all the results in resultLocation to the PatientLevelPrediction result tables in resultSchema
#' 
#' @export
populatePlpResultTables <- function(conn, 
                                    resultSchema, 
                                    stringAppendToTables = '',
                                    targetDialect = 'postgresql',
                                    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                    packageName,
                                    studyJsonList,
                                    studyName = '',
                                    studyDescription = '',
                                    researcherName = '',
                                    researcherEmail = '',
                                    researcherOrg = '',
                                    databaseName = NULL,
                                    databaseAcronym = NULL,
                                    databaseVersion = 1,
                                    databaseDescription = NULL,
                                    databaseType = NULL,
                                    valDatabases = list(ccae = list(name = 'CCAE', 
                                                                    description = '',
                                                                    version = 1,
                                                                    type = 'US Claims')),
                                    resultLocation = NULL,
                                    resultPattern = '',
                                    validationLocation = file.path(resultLocation,'Validation'),
                                    addInternalValidation = T,
                                    addExternalValidation = T,
                                    gsubVal = NULL,
                                    removePattern = NULL
){

  ensure_installed("jsonlite")
  
  # input checks
  ##TODO
  if(base::missing(packageName)){
    if(base::missing(studyJsonList)){
      stop('Need either packageName or studyJsonList')
    }else{
      if(is.null(studyJsonList)){
        stop('studyJsonList needs to be non-null')
      }
      cohortType <- 'list'
      jsonInput <- studyJsonList
    }
  } else{
    if(is.null(packageName)){
      stop('packageName needs to be non-null')
    }
    cohortType <- 'package'
    jsonInput <- packageName
  }
  
  if(stringAppendToTables != ''){
    stringAppendToTables <- paste0(toupper(gsub('_','',gsub(' ','', stringAppendToTables))), '_')
  }
  
  studyId <- addStudy(conn = conn, 
                      resultSchema = resultSchema, 
                      targetDialect = targetDialect,
                      studyName = studyName,
                      studyDescription = studyDescription,
                      stringAppendToTables = stringAppendToTables,
                      tempEmulationSchema = tempEmulationSchema)
  
  ParallelLogger::logInfo(paste0('studyId: ', studyId))
  
  researcherId <- addResearcher(conn = conn, 
                                resultSchema = resultSchema, 
                                targetDialect = targetDialect,
                                researcherName = researcherName, 
                                researcherEmail = researcherEmail,
                                researcherOrg = researcherOrg, 
                                stringAppendToTables = stringAppendToTables,
                                tempEmulationSchema = tempEmulationSchema)
  
  ParallelLogger::logInfo(paste0('researcherId: ', researcherId))
  
  dbId <- addDatabase(conn = conn, 
                      resultSchema = resultSchema, 
                      targetDialect = targetDialect,
                      databaseName = databaseName,
                      databaseAcronym = databaseAcronym,
                      databaseVersion = databaseVersion,
                      databaseDescription = databaseDescription,
                      databaseType = databaseType, 
                      stringAppendToTables = stringAppendToTables,
                      tempEmulationSchema = tempEmulationSchema)
  ParallelLogger::logInfo(paste0('dbId: ', dbId))
  
  mdls <- dir(resultLocation, pattern = resultPattern)
  removeMdls <- union(grep('.csv', mdls),grep('.txt', mdls))
  if(length(removeMdls)>0){
    mdls <- mdls[-removeMdls] 
  }
  
  # remove pattern
  if(!is.null(removePattern)){
    mdls <- mdls[-grep(removePattern, mdls)]
  }
  
  for(modelRes in mdls){
    ParallelLogger::logInfo(paste0('Adding results for model @ ', modelRes))
    
    # TODO edit csv here
    mdl <- tryCatch({PatientLevelPrediction::loadPlpResult(file.path(resultLocation, modelRes, 'plpResult'))}, 
                    error = function(e){ParallelLogger::logInfo(e);return(NULL)})
    
    if(!is.null(mdl)){
      
      # add TAR
      tarId <- addTar(conn = conn, 
                      resultSchema = resultSchema, 
                      targetDialect = targetDialect,
                      startDay = mdl$model$settings$populationSettings$riskWindowStart, 
                      startAnchor = mdl$model$settings$populationSettings$startAnchor,
                      endDay = mdl$model$settings$populationSettings$riskWindowEnd,  
                      endAnchor = mdl$model$settings$populationSettings$endAnchor, 
                      stringAppendToTables = stringAppendToTables,
                      tempEmulationSchema = tempEmulationSchema
        )
      ParallelLogger::logInfo(paste0('tarId: ', tarId))
      
      tId <- addCohort(conn = conn, 
                       resultSchema = resultSchema, 
                       targetDialect = targetDialect,
                       jsonInput = jsonInput, type = cohortType,
                       cohortId = mdl$model$trainDetails$cohortId, 
                       stringAppendToTables = stringAppendToTables,
                       tempEmulationSchema = tempEmulationSchema)
      ParallelLogger::logInfo(paste0('tId: ', tId))
      
      oId <- addCohort(conn = conn, 
                       resultSchema = resultSchema, 
                       targetDialect = targetDialect,
                       jsonInput = jsonInput, type = cohortType,
                       cohortId = mdl$model$trainDetails$outcomeId, 
                       stringAppendToTables = stringAppendToTables,
                       tempEmulationSchema = tempEmulationSchema)
      ParallelLogger::logInfo(paste0('oId: ', oId))
      
      popSetId <- addPopulationSetting(conn = conn, 
                                       resultSchema = resultSchema, 
                                       targetDialect = targetDialect,
                                       json = mdl$model$settings$populationSettings, 
                                       stringAppendToTables = stringAppendToTables,
                                       tempEmulationSchema = tempEmulationSchema)
      ParallelLogger::logInfo(paste0('popSetId: ', popSetId))
      
      covSetId <- addCovariateSetting(conn = conn, 
                                      resultSchema = resultSchema, 
                                      targetDialect = targetDialect,
                                      json = mdl$model$settings$covariateSettings, 
                                      stringAppendToTables = stringAppendToTables,
                                      tempEmulationSchema = tempEmulationSchema)
      ParallelLogger::logInfo(paste0('covSetId: ', covSetId))
      
      modSetId <- addModelSetting(conn = conn, 
                                  resultSchema = resultSchema, 
                                  targetDialect = targetDialect,
                                  modelType = mdl$model$settings$modelSettings$model,
                                  json = mdl$model$settings$modelSettings, 
                                  stringAppendToTables = stringAppendToTables,
                                  tempEmulationSchema = tempEmulationSchema)
      ParallelLogger::logInfo(paste0('modSetId: ', modSetId))
      
      # NEW: add plp_data_settings
      plpDataSetId <- addPlpDataSetting(conn = conn, 
        resultSchema = resultSchema, 
        targetDialect = targetDialect,
        json = mdl$model$settings$plpDataSettings, 
        stringAppendToTables = stringAppendToTables,
        tempEmulationSchema = tempEmulationSchema)
      ParallelLogger::logInfo(paste0('plpDataSetId: ', plpDataSetId))
      
      # NEW: add FE_settings
      FESetId <- addFESetting(conn = conn, 
        resultSchema = resultSchema, 
        targetDialect = targetDialect,
        json = mdl$model$settings$featureEngineering, 
        stringAppendToTables = stringAppendToTables,
        tempEmulationSchema = tempEmulationSchema)
      ParallelLogger::logInfo(paste0('FESetId: ', FESetId))
      
      # NEW: add sample_settings
      sampleSetId <- addSampleSetting(
        conn = conn, 
        resultSchema = resultSchema, 
        targetDialect = targetDialect,
        json = mdl$model$settings$sampleSettings, 
        stringAppendToTables = stringAppendToTables,
        tempEmulationSchema = tempEmulationSchema
        )
      ParallelLogger::logInfo(paste0('sampleSetId: ', sampleSetId)
      )
      
      # NEW: add tidy_covariate_settings
      tidySetId <- addTidySetting(
        conn = conn, 
        resultSchema = resultSchema, 
        targetDialect = targetDialect,
        json = mdl$model$settings$tidyCovariates, 
        stringAppendToTables = stringAppendToTables,
        tempEmulationSchema = tempEmulationSchema
      )
      ParallelLogger::logInfo(paste0('tidySetId: ', tidySetId))

      
      # this is now split setting - update this function
      splitId <- addSplitSettings(
        conn = conn, 
        resultSchema = resultSchema, 
        targetDialect = targetDialect,
        json = mdl$model$settings$splitSettings,
        stringAppendToTables = stringAppendToTables,
        tempEmulationSchema = tempEmulationSchema
      )
      ParallelLogger::logInfo(paste0('splitId: ', splitId))
      
      # create this function
      modelId <- addModel(
        conn = conn, 
        resultSchema = resultSchema, 
        targetDialect = targetDialect,
        analysisId = mdl$model$trainDetails$analysisId, # trainDetails
        modelName = mdl$model$settings$modelSettings$model, #standardise this
        targetId = tId,
        outcomeId = oId,
        tarId = tarId,
        plpDataSettingId = plpDataSetId,
        populationSettingId = popSetId,
        modelSettingId = modSetId,
        covariateSettingId = covSetId,
        sampleSettingId = sampleSetId,
        splitSettingId = splitId, # changed from trainingId
        featureEngineeringSettingId = FESetId,
        tidyCovariatesSettingId = tidySetId,
        requireDenseMatrix = mdl$model$settings$requireDenseMatrix,
        researcherId = researcherId,
        databaseId = dbId,
        hyperParamSearch = mdl$model$trainDetails$hyperParamSearch, #mdl$trainDetails$hyperParamSearch
        plpModelFile = " ",
        dateTime = format(mdl$executionSummary$ExecutionDateTime, format="%Y-%m-%d"), #mdl$trainDetails$trainingDate
        trainingTime = mdl$model$trainDetails$trainingTime, #mdl$trainDetails$trainingTime
        intercept = ifelse(is.list(mdl$model), mdl$model$model$coefficients[1], 0),
        stringAppendToTables = stringAppendToTables,
        tempEmulationSchema = tempEmulationSchema
      )
      ParallelLogger::logInfo(paste0('modelId: ', modelId))
      
      # add modelId and studyId
      addStudiesModel(conn = conn, 
                      resultSchema = resultSchema, 
                      targetDialect = targetDialect,
                      studyId = studyId,
                      modelId = modelId,
                      stringAppendToTables = stringAppendToTables,
                      tempEmulationSchema = tempEmulationSchema)
      
      # add internalValication
      if(addInternalValidation){
        
        ParallelLogger::logInfo('Adding internal validation results')
        
        ##if exists 
        if(!is.null(mdl)){
          resultId <- addResult(conn = conn, 
                                resultSchema = resultSchema, 
                                targetDialect = targetDialect,
                                modelId = modelId,
                                researcherId = researcherId,
                                databaseId = dbId,
                                targetId = tId,
                                outcomeId = oId,
                                tarId = tarId,
                                populationSettingId = popSetId,
                                executionDateTime = format(mdl$executionSummary$ExecutionDateTime, format="%Y-%m-%d"),
                                plpVersion = mdl$executionSummary$PackageVersion$packageVersion, 
                                stringAppendToTables = stringAppendToTables,
                                tempEmulationSchema = tempEmulationSchema)
          ParallelLogger::logInfo(paste0('resultId: ', resultId))
          
          # add eval
          if(!is.null(mdl$performanceEvaluation)){
            addEvaluation(conn = conn, resultSchema = resultSchema, targetDialect = targetDialect,
                          resultId = resultId,
                          performanceEvaluation = mdl$performanceEvaluation,
                          overWriteIfExists = T, 
                          stringAppendToTables = stringAppendToTables,
                          tempEmulationSchema = tempEmulationSchema)
          }
          if(!is.null(mdl$covariateSummary)){
            addCovariateSummary(conn = conn, resultSchema = resultSchema, targetDialect = targetDialect,
                                resultId = resultId,
                                covariateSummary = mdl$covariateSummary,
                                restrictToIncluded = T,
                                overWriteIfExists = T, 
                                stringAppendToTables = stringAppendToTables,
                                tempEmulationSchema = tempEmulationSchema)
          }
          
        }
      }
      
      # add validation results for this model
      if(addExternalValidation){
        
        ParallelLogger::logInfo('Adding external validation results')
        
        
        if(is.null(validationLocation)){
          validationLocation <- file.path(resultLocation, 'validation')
        }
        valDbs <- dir(validationLocation)
        
        # restrict to the databases with info
        valDbs <- valDbs[valDbs%in%names(valDatabases)]
        
        if(length(valDbs)>0){
          
          valDbs <- valDbs[!valDbs %in% c('plplog.txt')]
          
          for(valDb in valDbs){
            
            #get valDbId
            valDbId <- addDatabase(conn = conn, 
                                   resultSchema = resultSchema, 
                                   targetDialect = targetDialect,
                                   databaseName = valDatabases[[valDb]]$name,
                                   databaseAcronym = valDb,
                                   databaseVersion = valDatabases[[valDb]]$version,
                                   databaseDescription = valDatabases[[valDb]]$description,
                                   databaseType =valDatabases[[valDb]]$type, 
                                   stringAppendToTables = stringAppendToTables,
                                   tempEmulationSchema = tempEmulationSchema)
            
            validationResults <- as.character(dir(file.path(validationLocation, valDb)))
            validationResults <- validationResults[validationResults != 'CohortCounts.csv']
            
            valMods <- data.frame(validationResults = validationResults)
            
            if(!is.null(gsubVal)){
              valModsEdit <- valMods$validationResults
              for(i in 1:nrow(gsubVal)){
                valModsEdit <- gsub(x = valModsEdit, pattern = gsubVal[i,1], replacement = gsubVal[i,2])
              }
              valMods$validationResultsEdit <- valModsEdit
            }else{
              valMods$validationResultsEdit <- valMods$validationResults
            }
            
            # remove pattern
            if(!is.null(removePattern)){
              if(length(grep(removePattern, valMods$validationResultsEdit))>0){
                valMods <- valMods[-grep(removePattern, valMods$validationResultsEdit),]
              }
            }
            
            # restrict to analysis
            ParallelLogger::logInfo(paste0('restricting to ', modelRes))
            valMods <- valMods[grep(modelRes, valMods$validationResultsEdit),]
            
            if(nrow(valMods)>0){
              
              # load each result
              for(valInd in 1:nrow(valMods)){
                
              resultName <- dir(file.path(validationLocation, valDb, valMods$validationResults[valInd]))
              resultName <- resultName[grep('.rds',resultName)]
              
              if(length(resultName)>0){
                vmdl <- readRDS(file.path(validationLocation, valDb, valMods$validationResults[valInd], resultName[1] ))
              } else{
                vmdl <- NULL
              }
                
                if(!is.null(vmdl)){
                  tId <- addCohort(conn = conn, 
                                   resultSchema = resultSchema, 
                                   targetDialect = targetDialect,
                                   jsonInput = jsonInput, type = cohortType,
                                   cohortId = vmdl$model$valdiationDetails$cohortId, 
                                   stringAppendToTables = stringAppendToTables,
                                   tempEmulationSchema = tempEmulationSchema)
                  oId <- addCohort(conn = conn, 
                                   resultSchema = resultSchema, 
                                   targetDialect = targetDialect,
                                   jsonInput = jsonInput, type = cohortType,
                                   cohortId = vmdl$model$valdiationDetails$outcomeId, 
                                   stringAppendToTables = stringAppendToTables,
                                   tempEmulationSchema = tempEmulationSchema)
                  
                  # get tarId TODO - not in these results
                  # popSetId TODO - not in these results
                  
                  # add result
                  resultId <- addResult(conn = conn, 
                                        resultSchema = resultSchema, 
                                        targetDialect = targetDialect,
                                        modelId = modelId,
                                        researcherId = researcherId,
                                        databaseId = valDbId,
                                        targetId = tId,
                                        outcomeId = oId,
                                        tarId = tarId,
                                        populationSettingId = popSetId,
                                        executionDateTime = format(vmdl$executionSummary$ExecutionDateTime, format="%Y-%m-%d"),
                                        plpVersion = vmdl$executionSummary$PackageVersion$packageVersion, 
                                        stringAppendToTables = stringAppendToTables,
                                        tempEmulationSchema = tempEmulationSchema)
                  
                  
                  # add performance 
                  #=============
                  if(!is.null(vmdl$performanceEvaluation)){
                    addEvaluation(conn = conn, resultSchema = resultSchema, targetDialect = targetDialect,
                                  resultId = resultId,
                                  performanceEvaluation = vmdl$performanceEvaluation,
                                  overWriteIfExists = T, 
                                  stringAppendToTables = stringAppendToTables,
                                  tempEmulationSchema = tempEmulationSchema)
                  }
                  
                  if(!is.null(vmdl$covariateSummary)){
                    addCovariateSummary(conn = conn, resultSchema = resultSchema, targetDialect = targetDialect,
                                        resultId = resultId,
                                        covariateSummary = vmdl$covariateSummary,
                                        restrictToIncluded = T,
                                        overWriteIfExists = T, 
                                        stringAppendToTables = stringAppendToTables,
                                        tempEmulationSchema = tempEmulationSchema)
                  }
                  
                }
                
                #+++++++++++++
                
                
              } # end val per database
            } # end if val exists
            
          } # val database
        }
        
      } #externalVal
      
      
    } #model not null 
    
  } # per model
  
} #end funct


#======================
# HELPER FUNCTIONS
#======================
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

# gets the column names in camelCase of a table
getColumnNames <- function(conn, resultSchema, targetDialect, tableName, stringAppendToTables = '',
                           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  sql <- "select top 1 * from @my_schema.@string_to_append@table;"
  sql <- SqlRender::render(sql, 
                           my_schema = resultSchema,
                           table = tableName,
                           string_to_append = stringAppendToTables)
  sql <- SqlRender::translate(sql, targetDialect = targetDialect,
                              tempEmulationSchema = tempEmulationSchema)
  result <- DatabaseConnector::querySql(connection = conn, sql = sql, snakeCaseToCamelCase = T)
  
  return(colnames(result))
}

# True/False check whether results exist in table 
checkResultExists <- function(conn, resultSchema, targetDialect,
                              snakeCaseToCamelCase,
                              tableName,
                              resultId,
                              tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  sql <- "select * from @my_schema.@table where result_id = @result_id;"
  sql <- SqlRender::render(sql, 
                           my_schema = resultSchema,
                           table = tableName,
                           result_id = resultId)
  sql <- SqlRender::translate(sql, targetDialect = targetDialect,
                              tempEmulationSchema = tempEmulationSchema)
  result <- DatabaseConnector::querySql(connection = conn, sql = sql, snakeCaseToCamelCase = T)
  return(nrow(result)>0)
}


#======================
# end helpers
addStudy <- function(conn, resultSchema, targetDialect,
                     studyName, studyDescription,
                     stringAppendToTables = '',
                     tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  result <- checkTable(conn = conn, 
                       resultSchema = resultSchema, 
                       stringAppendToTables = stringAppendToTables,
                       targetDialect = targetDialect, 
                       tableName = 'studies',
                       columnNames = c('study_name', 'study_description'), 
                       values = c(paste0("'",studyName,"'"), 
                                  paste0("'",studyDescription,"'")
                       ),
                       tempEmulationSchema = tempEmulationSchema
  )
  
  if(nrow(result)>0){
    ParallelLogger::logInfo('Study already exists')
  }
  else{
    ParallelLogger::logInfo(paste0('Adding new study: ', studyName ))
    
    # add my detail
    sql <- "INSERT INTO @my_schema.@string_to_appendstudies(study_name, study_description) 
          VALUES ('@name','@desc');"
    sql <- SqlRender::render(sql, 
                             my_schema = resultSchema,
                             name = studyName,
                             desc = studyDescription,
                             string_to_append = stringAppendToTables
    )
    
    sql <- SqlRender::translate(sql, targetDialect = targetDialect, 
                                tempEmulationSchema = tempEmulationSchema)
    
    DatabaseConnector::executeSql(conn, sql)
    
    result <- checkTable(conn = conn, 
                         resultSchema = resultSchema, 
                         stringAppendToTables = stringAppendToTables,
                         targetDialect = targetDialect, 
                         tableName = 'studies',
                         columnNames = c('study_name', 'study_description'), 
                         values = c(paste0("'",studyName,"'"), 
                                    paste0("'",studyDescription,"'")
                         ),
                         tempEmulationSchema = tempEmulationSchema
    )
    
  }
  
  return(result$studyId[1])
  
}

addStudiesModel <- function(conn, 
                            resultSchema, 
                            targetDialect,
                            studyId,
                            modelId,
                            stringAppendToTables,
                            tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  
  result <- checkTable(conn = conn, 
                       resultSchema = resultSchema, 
                       stringAppendToTables = stringAppendToTables,
                       targetDialect = targetDialect, 
                       tableName = 'study_models',
                       columnNames = c('study_id', 'model_id'), 
                       values = c(studyId, modelId),
                       tempEmulationSchema = tempEmulationSchema
  )
  
  if(nrow(result)>0){
    ParallelLogger::logInfo('Study and model already linked')
  }
  else{
    ParallelLogger::logInfo(paste0('Adding link between study: ', studyId, ' and model: ', modelId ))
    
    # add my detail
    sql <- "INSERT INTO @my_schema.@string_to_appendstudy_models(study_id, model_id) 
          VALUES ('@studyid','@modelid');"
    sql <- SqlRender::render(sql, 
                             my_schema = resultSchema,
                             studyid = studyId,
                             modelid = modelId,
                             string_to_append = stringAppendToTables
    )
    
    sql <- SqlRender::translate(sql = sql, targetDialect = targetDialect,
                                tempEmulationSchema = tempEmulationSchema)
    
    DatabaseConnector::executeSql(conn, sql)
    
    result <- checkTable(conn = conn, 
                         resultSchema = resultSchema, 
                         stringAppendToTables = stringAppendToTables,
                         targetDialect = targetDialect, 
                         tableName = 'study_models',
                         columnNames = c('study_id', 'model_id'), 
                         values = c(studyId, modelId),
                         tempEmulationSchema = tempEmulationSchema
    )
    
  }
  
  return(invisible(result$studyId[1]))
  
}


addResearcher <- function(conn, resultSchema, targetDialect,
                          stringAppendToTables = '',
                          researcherName, 
                          researcherEmail,
                          researcherOrg,
                          tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  result <- checkTable(conn = conn, 
                       resultSchema = resultSchema, 
                       stringAppendToTables = stringAppendToTables,
                       targetDialect = targetDialect, 
                       tableName = 'researchers',
                       columnNames = c('researcher_name', 'researcher_email', 'researcher_affiliation'), 
                       values = c(paste0("'",researcherName,"'"), 
                                  paste0("'",researcherEmail,"'"),
                                  paste0("'",researcherOrg,"'")),
                       tempEmulationSchema = tempEmulationSchema
  )
  
  if(nrow(result)>0){
    ParallelLogger::logInfo('Researcher already exists')
  }
  else{
    ParallelLogger::logInfo(paste0('Adding Researcher: ', researcherName ))
    
    # add my detail
    sql <- "INSERT INTO @my_schema.@string_to_appendresearchers(researcher_name, researcher_email, researcher_affiliation) 
          VALUES ('@name','@email', '@org');"
    sql <- SqlRender::render(sql, 
                             my_schema = resultSchema,
                             name = researcherName,
                             email = researcherEmail,
                             org = researcherOrg,
                             string_to_append = stringAppendToTables
    )
    
    sql <- SqlRender::translate(sql = sql, targetDialect = targetDialect,
                                tempEmulationSchema = tempEmulationSchema)
    
    DatabaseConnector::executeSql(conn, sql)
    
    result <- checkTable(conn = conn, 
                         resultSchema = resultSchema, 
                         stringAppendToTables = stringAppendToTables,
                         targetDialect = targetDialect, 
                         tableName = 'researchers',
                         columnNames = c('researcher_name', 'researcher_email', 'researcher_affiliation'), 
                         values = c(paste0("'",researcherName,"'"), 
                                    paste0("'",researcherEmail,"'"),
                                    paste0("'",researcherOrg,"'")),
                         tempEmulationSchema = tempEmulationSchema
    )
    
  }
  
  return(result$researcherId[1])
  
}


addDatabase <- function(conn, resultSchema, targetDialect,
                        stringAppendToTables = '',
                        databaseName,
                        databaseAcronym,
                        databaseVersion = 1,
                        databaseDescription,
                        databaseType,
                        tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  result <- checkTable(conn = conn, 
                       resultSchema = resultSchema, 
                       stringAppendToTables = stringAppendToTables,
                       targetDialect = targetDialect, 
                       tableName = 'database_details',
                       columnNames = c('database_name', 'database_acronym',
                                       'database_version',
                                       'database_description', 'database_type'), 
                       values = c(paste0("'",databaseName,"'"), 
                                  paste0("'",databaseAcronym,"'"),
                                  databaseVersion,
                                  paste0("'",databaseDescription,"'"),
                                  paste0("'",databaseType,"'")),
                       tempEmulationSchema = tempEmulationSchema
  )
  
  if(nrow(result)>0){
    ParallelLogger::logInfo(paste0('Database ', databaseName ,' already exists'))
  } else {
    
    sql <- "INSERT INTO @my_schema.@string_to_appenddatabase_details(database_name, database_acronym,
                                  database_version,
                                  database_description, database_type) 
          VALUES ('@dbName','@db', @version, '@desc', '@type');"
    sql <- SqlRender::render(sql, 
                             my_schema = resultSchema,
                             dbName = databaseName, 
                             db = databaseAcronym,
                             version = databaseVersion,
                             desc = databaseDescription,
                             type = databaseType,
                             string_to_append = stringAppendToTables)
    sql <- SqlRender::translate(sql, targetDialect = targetDialect,
                                tempEmulationSchema = tempEmulationSchema)
    DatabaseConnector::executeSql(conn, sql)
    
    result <- checkTable(conn = conn, 
                         resultSchema = resultSchema, 
                         stringAppendToTables = stringAppendToTables,
                         targetDialect = targetDialect, 
                         tableName = 'database_details',
                         columnNames = c('database_name', 'database_acronym', 'database_version',
                                         'database_description', 'database_type'), 
                         values = c(paste0("'",databaseName,"'"), 
                                    paste0("'",databaseAcronym,"'"),
                                    databaseVersion,
                                    paste0("'",databaseDescription,"'"),
                                    paste0("'",databaseType,"'")),
                         tempEmulationSchema = tempEmulationSchema
    )
    
  }
  
  return(result$databaseId[1])
  
}


addTar <- function(conn, resultSchema, targetDialect,
                   stringAppendToTables = '',
                   startDay, 
                   startAnchor,
                   endDay,  
                   endAnchor,
                   tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  result <- checkTable(conn = conn, 
                       resultSchema = resultSchema, 
                       stringAppendToTables = stringAppendToTables,
                       targetDialect = targetDialect, 
                       tableName = 'tars',
                       columnNames = c('tar_start_day', 'tar_start_anchor',
                                       'tar_end_day', 'tar_end_anchor'), 
                       values = c(startDay, 
                                  paste0("'",startAnchor,"'"),
                                  endDay,
                                  paste0("'",endAnchor,"'")),
                       tempEmulationSchema = tempEmulationSchema
  )
  
  if(nrow(result)==0){
    
    ParallelLogger::logInfo('Adding TAR')
    # tars - id 1
    sql <- "INSERT INTO @my_schema.@string_to_appendtars(tar_start_day, tar_start_anchor,
                           tar_end_day, tar_end_anchor) 
          VALUES (@tar_start_day, @tar_start_anchor, @tar_end_day, @tar_end_anchor);"
    sql <- SqlRender::render(sql, 
                             my_schema = resultSchema,
                             tar_start_day = startDay,
                             tar_start_anchor = paste0("'",startAnchor,"'"),
                             tar_end_day = endDay,
                             tar_end_anchor = paste0("'",endAnchor,"'"),
                             string_to_append = stringAppendToTables)
    
    sql <- SqlRender::translate(sql, targetDialect = targetDialect, 
                                tempEmulationSchema = tempEmulationSchema)
    
    DatabaseConnector::executeSql(conn, sql)
    
    #getId of new
    result <- checkTable(conn = conn, 
                         resultSchema = resultSchema, 
                         stringAppendToTables = stringAppendToTables,
                         targetDialect = targetDialect, 
                         tableName = 'tars',
                         columnNames = c('tar_start_day', 'tar_start_anchor',
                                         'tar_end_day', 'tar_end_anchor'), 
                         values = c(startDay, 
                                    paste0("'",startAnchor,"'"),
                                    endDay,
                                    paste0("'",endAnchor,"'")),
                         tempEmulationSchema = tempEmulationSchema
    )
    
  } else {
    ParallelLogger::logInfo('TAR exists')
  }
  
  
  return(result$tarId[1])
  
}





getCohortFromList <- function(jsonList, cohortId){
  
  #cohort_name, cohort_id and cohort_json
  ParallelLogger::logInfo(paste0('Adding cohorts from input list'))
  id <- which(unlist(lapply(jsonList, function(x){x$cohort_id == cohortId})))[1]
  
  json <- jsonList[[id]]$cohort_json
  
  details <- data.frame(name = jsonList[[id]]$cohort_name,
                        cohortId = jsonList[[id]]$cohort_id,
                        atlasId = jsonList[[id]]$cohort_id)
  
  return(list(json = json,
              cohortTocreate = details))
}

getCohortFromPackage <- function(packageName, cohortId){
  
  ParallelLogger::logInfo(paste0('Adding cohorts from ', packageName))
  # check packageName
  if(!dir.exists(system.file(package = packageName))){
    stop('Package path not found - set pckPath input to the location of the study package you executed')
  } else {
    
    ParallelLogger::logInfo(paste0('Extracting cohort ',cohortId,' json from ', packageName))
    # check required files:
    cohortToCreateLoc <- system.file('settings',
                                     'cohortsToCreate.csv',
                                     package = packageName)
    
    if(!file.exists(cohortToCreateLoc)){
      stop('No cohortsToCreate.csv in package')
    }
    
    if(!dir.exists(file.path(system.file(package = packageName), 'cohorts'))){
      stop('No cohorts in package')
    }
  }
  
  
  # add the cohorts and store the map atlas_id, cohort_id, cohort_name
  cohortsToCreate <- utils::read.csv(cohortToCreateLoc)
  cohortTocreate <- cohortsToCreate[cohortsToCreate$atlasId == cohortId,]
  
  jsonFileName <- file.path(system.file(package = packageName), 'cohorts', paste0(cohortTocreate$name, '.json'))
  json <- readChar(jsonFileName, file.info(jsonFileName)$size)
  
  
  return(list(json = json,
              cohortTocreate = cohortTocreate))
}


# adds json from package unless json is specified
addCohort <- function(conn, resultSchema, targetDialect,
                      stringAppendToTables = '',
                      jsonInput, type = 'package',
                      cohortId,
                      tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  if(type == 'package'){
    object <- getCohortFromPackage(packageName = jsonInput, cohortId)
  } else{
    object <- getCohortFromList(jsonList = jsonInput, cohortId)
  }
  
  json <- object$json
  cohortTocreate <- object$cohortTocreate
  
    # make sure the json has been converted 
    if(class(json)!='character'){
      ParallelLogger::logInfo('converting json to character')
      json <- jsonlite::serializeJSON(json, digits = 23)
    }
    
    # reduce the size to save
    json <-  substr(json, 1, 4000) # TESTING - FIX THIS [TODO]
    
    #check whether cohort already in table:
    result <- checkTable(conn = conn, 
                         resultSchema = resultSchema, 
                         stringAppendToTables = stringAppendToTables,
                         targetDialect = targetDialect, 
                         tableName = 'cohorts',
                         columnNames = c('cohort_name', 'atlas_id'), 
                         values = c(paste0("'",cohortTocreate$name[1],"'"), cohortTocreate$atlasId[1]),
                         tempEmulationSchema = tempEmulationSchema
    )
    
    addNew <- F
    if(nrow(result)>0){
      addNew <- json %in% result$cohortJson
      ParallelLogger::logInfo(paste0('json in jsons:', addNew))
    }
    
    if(addNew){
      ParallelLogger::logInfo(paste0('Cohort ',cohortTocreate$name,' exists in result database with id', result$cohortId))
    } else{
      ParallelLogger::logInfo(paste0('Adding cohort ',cohortTocreate$name[1]))
      
      data <- data.frame(cohortName = cohortTocreate$name, 
                         atlasId = cohortTocreate$atlasId,
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
                           values = c(paste0("'",cohortTocreate$name,"'"), cohortTocreate$atlasId),
                           tempEmulationSchema = tempEmulationSchema
      )
      
      jsonInd <- result$cohortJson %in% json
      result <- result[jsonInd,]
      
    }
  
  return(result$cohortId[1])
}


addPopulationSetting <- function(conn, resultSchema, targetDialect,
                                 stringAppendToTables = '',
                                 json,
                                 tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  # process json to make it ordered...
  # make sure the json has been converted 
  if(class(json)!='character'){
    json <- as.character(jsonlite::serializeJSON(json, digits = 23))
  }
  
  jsonId <- checkJson(conn = conn,
                      resultSchema = resultSchema, 
                      stringAppendToTables = stringAppendToTables,
                      targetDialect = targetDialect, 
                      tableName = 'population_settings',
                      jsonColumnName = 'populationSettingsJson',
                      id = 'populationSettingId',
                      json = json, 
                      tempEmulationSchema = tempEmulationSchema)
  
  if(is.null(jsonId)){
    ParallelLogger::logInfo('Adding new population settings')
    
    data <- data.frame(populationSettingsJson = json)
    DatabaseConnector::insertTable(connection = conn, 
                                   databaseSchema = resultSchema, 
                                   tableName = paste0(stringAppendToTables, 'population_settings'),
                                   data = data, 
                                   dropTableIfExists = F, 
                                   createTable = F, 
                                   tempTable = F, 
                                   progressBar = T,
                                   camelCaseToSnakeCase = T,
                                   tempEmulationSchema = tempEmulationSchema
    )
    
    #getId of new
    jsonId <- checkJson(conn = conn,
                        resultSchema = resultSchema, 
                        stringAppendToTables = stringAppendToTables,
                        targetDialect = targetDialect, 
                        tableName = 'population_settings',
                        jsonColumnName = 'populationSettingsJson',
                        id = 'populationSettingId',
                        json = json,
                        tempEmulationSchema = tempEmulationSchema)
    
  } else{
    ParallelLogger::logInfo('Population settings exists')
  }
  
  return(jsonId)
}


addCovariateSetting <- function(conn, resultSchema, targetDialect,
                                stringAppendToTables = '',
                                json,
                                tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  # process json to make it ordered...
  # make sure the json has been converted 
  if(class(json)!='character'){
    json <- as.character(jsonlite::serializeJSON(json, digits = 23))
  }
  
  jsonId <- checkJson(conn = conn,
                      resultSchema = resultSchema, 
                      stringAppendToTables = stringAppendToTables,
                      targetDialect = targetDialect, 
                      tableName = 'covariate_settings',
                      jsonColumnName = 'covariateSettingsJson',
                      id = 'covariateSettingId',
                      json = json,
                      tempEmulationSchema = tempEmulationSchema)
  
  if(is.null(jsonId)){
    
    ParallelLogger::logInfo('Adding new covariate settings')
    
    data <- data.frame(covariateSettingsJson = json)
    DatabaseConnector::insertTable(connection = conn, 
                                   databaseSchema = resultSchema, 
                                   tableName = paste0(stringAppendToTables, 'covariate_settings'),
                                   data = data, 
                                   dropTableIfExists = F, 
                                   createTable = F, 
                                   tempTable = F, 
                                   progressBar = T,
                                   camelCaseToSnakeCase = T,
                                   tempEmulationSchema = tempEmulationSchema
    )
    
    #getId of new
    jsonId <- checkJson(conn = conn,
                        resultSchema = resultSchema, 
                        stringAppendToTables = stringAppendToTables,
                        targetDialect = targetDialect, 
                        tableName = 'covariate_settings',
                        jsonColumnName = 'covariateSettingsJson',
                        id = 'covariateSettingId',
                        json = json,
                        tempEmulationSchema = tempEmulationSchema)
    
  } else{
    ParallelLogger::logInfo('Covariate setting exists')
  }
  
  return(jsonId)
}


addModelSetting <- function(conn, resultSchema, targetDialect,
                            stringAppendToTables = '',
                            modelType, json,
                            tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  # process json to make it ordered...
  # make sure the json has been converted 
  if(class(json)!='character'){
    json <- as.character(jsonlite::serializeJSON(json, digits = 23))
  }
  
  jsonId <- checkJson(conn = conn,
                      resultSchema = resultSchema, 
                      stringAppendToTables = stringAppendToTables,
                      targetDialect = targetDialect, 
                      tableName = 'model_settings',
                      jsonColumnName = 'modelSettingsJson',
                      id = 'modelSettingId',
                      json = json,
                      tempEmulationSchema = tempEmulationSchema)
  
  if(is.null(jsonId)){
    
    ParallelLogger::logInfo('Adding new model settings')
    
    data <- data.frame(modelType = modelType,
                       modelSettingsJson = json)
    DatabaseConnector::insertTable(connection = conn, 
                                   databaseSchema = resultSchema, 
                                   tableName = paste0(stringAppendToTables, 'model_settings'),
                                   data = data, 
                                   dropTableIfExists = F, 
                                   createTable = F, 
                                   tempTable = F, 
                                   progressBar = T,
                                   camelCaseToSnakeCase = T,
                                   tempEmulationSchema = tempEmulationSchema)
    
    #getId of new
    jsonId <- checkJson(conn = conn,
                        resultSchema = resultSchema, 
                        stringAppendToTables = stringAppendToTables,
                        targetDialect = targetDialect, 
                        tableName = 'model_settings',
                        jsonColumnName = 'modelSettingsJson',
                        id = 'modelSettingId',
                        json = json,
                        tempEmulationSchema = tempEmulationSchema)
    
  } else{
    ParallelLogger::logInfo('Model setting exists')
  }
  
  return(jsonId)
}

addTidySetting <- function(
  conn, 
  resultSchema, 
  targetDialect,
  stringAppendToTables = '',
  json,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  if(class(json)!='character'){
    json <- as.character(jsonlite::serializeJSON(json, digits = 23))
  }
  
  jsonId <- checkJson(conn = conn,
    resultSchema = resultSchema, 
    stringAppendToTables = stringAppendToTables,
    targetDialect = targetDialect, 
    tableName = 'tidy_covariates_settings',
    jsonColumnName = 'tidyCovariatesSettingsJson',
    id = 'tidyCovariatesSettingId',
    json = json,
    tempEmulationSchema = tempEmulationSchema)
  
  if(is.null(jsonId)){
    
    ParallelLogger::logInfo('Adding new tidy covariates settings')
    
    data <- data.frame(
      tidyCovariatesSettingsJson = json
    )
    
    DatabaseConnector::insertTable(
      connection = conn, 
      databaseSchema = resultSchema, 
      tableName = paste0(stringAppendToTables, 'tidy_covariates_settings'),
      data = data, 
      dropTableIfExists = F, 
      createTable = F, 
      tempTable = F, 
      progressBar = T,
      camelCaseToSnakeCase = T,
      tempEmulationSchema = tempEmulationSchema
    )
    
    #getId of new
    jsonId <- checkJson(
      conn = conn,
      resultSchema = resultSchema, 
      stringAppendToTables = stringAppendToTables,
      targetDialect = targetDialect, 
      tableName = 'tidy_covariates_settings',
      jsonColumnName = 'tidyCovariatesSettingsJson',
      id = 'tidyCovariatesSettingId',
      json = json,
      tempEmulationSchema = tempEmulationSchema
      )
    
  } else{
    ParallelLogger::logInfo('tidy covariates setting exists')
  }
  
  return(jsonId)
  
}

addSampleSetting <- function(
  conn, 
  resultSchema, 
  targetDialect,
  stringAppendToTables = '',
  json,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  if(class(json)!='character'){
    json <- as.character(jsonlite::serializeJSON(json, digits = 23))
  }
  
  jsonId <- checkJson(
    conn = conn,
    resultSchema = resultSchema, 
    stringAppendToTables = stringAppendToTables,
    targetDialect = targetDialect, 
    tableName = 'sample_settings',
    jsonColumnName = 'sampleSettingsJson',
    id = 'sampleSettingId',
    json = json,
    tempEmulationSchema = tempEmulationSchema
    )
  
  if(is.null(jsonId)){
    
    ParallelLogger::logInfo('Adding new sample settings')
    
    data <- data.frame(
      sampleSettingsJson = json
    )
    
    DatabaseConnector::insertTable(
      connection = conn, 
      databaseSchema = resultSchema, 
      tableName = paste0(stringAppendToTables, 'sample_settings'),
      data = data, 
      dropTableIfExists = F, 
      createTable = F, 
      tempTable = F, 
      progressBar = T,
      camelCaseToSnakeCase = T,
      tempEmulationSchema = tempEmulationSchema
    )
    
    #getId of new
    jsonId <- checkJson(
      conn = conn,
      resultSchema = resultSchema, 
      stringAppendToTables = stringAppendToTables,
      targetDialect = targetDialect, 
      tableName = 'sample_settings',
      jsonColumnName = 'sampleSettingsJson',
      id = 'sampleSettingId',
      json = json,
      tempEmulationSchema = tempEmulationSchema
      )
    
  } else{
    ParallelLogger::logInfo('sample setting exists')
  }
  
  return(jsonId)
  
}

addPlpDataSetting <- function(
  conn, 
  resultSchema, 
  targetDialect,
  stringAppendToTables = '',
  json,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  if(class(json)!='character'){
    json <- as.character(jsonlite::serializeJSON(json, digits = 23))
  }
  
  jsonId <- checkJson(conn = conn,
    resultSchema = resultSchema, 
    stringAppendToTables = stringAppendToTables,
    targetDialect = targetDialect, 
    tableName = 'plp_data_settings',
    jsonColumnName = 'plpDataSettingsJson',
    id = 'plpDataSettingId',
    json = json,
    tempEmulationSchema = tempEmulationSchema)
  
  if(is.null(jsonId)){
    
    ParallelLogger::logInfo('Adding new plp data settings')
    
    data <- data.frame(
      plpDataSettingsJson = json
    )
    
    DatabaseConnector::insertTable(
      connection = conn, 
      databaseSchema = resultSchema, 
      tableName = paste0(stringAppendToTables, 'plp_data_settings'),
      data = data, 
      dropTableIfExists = F, 
      createTable = F, 
      tempTable = F, 
      progressBar = T,
      camelCaseToSnakeCase = T,
      tempEmulationSchema = tempEmulationSchema
    )
    
    #getId of new
    jsonId <- checkJson(
      conn = conn,
      resultSchema = resultSchema, 
      stringAppendToTables = stringAppendToTables,
      targetDialect = targetDialect, 
      tableName = 'plp_data_settings',
      jsonColumnName = 'plpDataSettingsJson',
      id = 'plpDataSettingId',
      json = json,
      tempEmulationSchema = tempEmulationSchema
      )
    
  } else{
    ParallelLogger::logInfo('Split setting exists')
  }
  
  return(jsonId)
  
}

addFESetting <- function(
  conn, 
  resultSchema, 
  targetDialect,
  stringAppendToTables = '',
  json,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  if(class(json)!='character'){
    json <- as.character(jsonlite::serializeJSON(json, digits = 23))
  }
  
  jsonId <- checkJson(
    conn = conn,
    resultSchema = resultSchema, 
    stringAppendToTables = stringAppendToTables,
    targetDialect = targetDialect, 
    tableName = 'feature_engineering_settings',
    jsonColumnName = 'featureEngineeringSettingsJson',
    id = 'featureEngineeringSettingId',
    json = json,
    tempEmulationSchema = tempEmulationSchema
    )
  
  if(is.null(jsonId)){
    
    ParallelLogger::logInfo('Adding new feature_engineering settings')
    
    data <- data.frame(
      featureEngineeringSettingsJson = json
    )
    
    DatabaseConnector::insertTable(
      connection = conn, 
      databaseSchema = resultSchema, 
      tableName = paste0(stringAppendToTables, 'feature_engineering_settings'),
      data = data, 
      dropTableIfExists = F, 
      createTable = F, 
      tempTable = F, 
      progressBar = T,
      camelCaseToSnakeCase = T,
      tempEmulationSchema = tempEmulationSchema
    )
    
    #getId of new
    jsonId <- checkJson(
      conn = conn,
      resultSchema = resultSchema, 
      stringAppendToTables = stringAppendToTables,
      targetDialect = targetDialect, 
      tableName = 'feature_engineering_settings',
      jsonColumnName = 'featureEngineeringSettingsJson',
      id = 'featureEngineeringSettingId',
      json = json,
      tempEmulationSchema = tempEmulationSchema
      )
    
  } else{
    ParallelLogger::logInfo('feature engineering setting exists')
  }
  
  return(jsonId)
  
}

addSplitSettings <- function(
  conn, 
  resultSchema, 
  targetDialect,
  stringAppendToTables = '',
  json,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  if(class(json)!='character'){
    json <- as.character(jsonlite::serializeJSON(json, digits = 23))
  }
  
  jsonId <- checkJson(
    conn = conn,
    resultSchema = resultSchema, 
    stringAppendToTables = stringAppendToTables,
    targetDialect = targetDialect, 
    tableName = 'split_settings',
    jsonColumnName = 'splitSettingsJson',
    id = 'splitSettingId',
    json = json,
    tempEmulationSchema = tempEmulationSchema
    )
  
  if(is.null(jsonId)){
    
    ParallelLogger::logInfo('Adding new split settings')
    
    data <- data.frame(
      splitSettingsJson = json
      )
    
    DatabaseConnector::insertTable(
      connection = conn, 
      databaseSchema = resultSchema, 
      tableName = paste0(stringAppendToTables, 'split_settings'),
      data = data, 
      dropTableIfExists = F, 
      createTable = F, 
      tempTable = F, 
      progressBar = T,
      camelCaseToSnakeCase = T,
      tempEmulationSchema = tempEmulationSchema
      )
    
    #getId of new
    jsonId <- checkJson(
      conn = conn,
      resultSchema = resultSchema, 
      stringAppendToTables = stringAppendToTables,
      targetDialect = targetDialect, 
      tableName = 'split_settings',
      jsonColumnName = 'splitSettingsJson',
      id = 'splitSettingId',
      json = json,
      tempEmulationSchema = tempEmulationSchema
      )
    
  } else{
    ParallelLogger::logInfo('Split setting exists')
  }
  
  return(jsonId)
  
}


addModel <- function(
  conn, 
  resultSchema, targetDialect,
  stringAppendToTables = stringAppendToTables,
  analysisId,
  modelName,
  targetId,
  outcomeId,
  tarId,
  plpDataSettingId,
  populationSettingId,
  modelSettingId,
  covariateSettingId,
  sampleSettingId,
  splitSettingId,
  featureEngineeringSettingId,
  tidyCovariatesSettingId,
  requireDenseMatrix,
  researcherId,
  databaseId,
  hyperParamSearch,
  plpModelFile,
  dateTime,
  trainingTime,
  intercept,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  if(is.null(analysisId)){
    stop('analysisId is null')
  }
  if(is.null(modelName)){
    stop('modelName is null')
  }
  if(is.null(targetId)){
    stop('targetId is null')
  }
  if(is.null(outcomeId)){
    stop('outcomeId is null')
  }
  if(is.null(tarId)){
    stop('tarId is null')
  }
  
  if(is.null(plpDataSettingId)){
    stop('plpDataSettingId is null')
  }
  if(is.null(populationSettingId)){
    stop('populationSettingId is null')
  }
  if(is.null(modelSettingId)){
    stop('modelSettingId is null')
  }
  if(is.null(covariateSettingId)){
    stop('covariateSettingId is null')
  }
  if(is.null(sampleSettingId)){
    stop('sampleSettingId is null')
  }
  if(is.null(splitSettingId)){
    stop('splitSettingId is null')
  }
  if(is.null(featureEngineeringSettingId)){
    stop('featureEngineeringSettingId is null')
  }
  if(is.null(tidyCovariatesSettingId)){
    stop('tidyCovariatesSettingId is null')
  }
  
  if(is.null(researcherId)){
    stop('researcherId is null')
  }
  if(is.null(databaseId)){
    stop('databaseId is null')
  }
  if(is.null(plpModelFile)){
    stop('plpModelFile is null')
  }
  if(is.null(dateTime)){
    stop('dateTime is null')
  }
  if(is.null(intercept)){
    stop('intercept is null')
  }
  
  if(!is.null(hyperParamSearch)){
    if(class(hyperParamSearch) != 'character'){
      hyperParamSearch <- as.character(jsonlite::serializeJSON(hyperParamSearch, digits = 23))
    }
  }else{
    hyperParamSearch <- '' 
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
      'analysis_id',
      'model_name',
      'target_id',
      'outcome_id',
      'tar_id',
      'plp_data_setting_id',
      'population_setting_id',
      'model_setting_id',
      'covariate_setting_id',
      'sample_setting_id',
      'split_setting_id',
      'feature_engineering_setting_id',
      'tidy_covariates_setting_id',
      'require_dense_matrix',
      'researcher_id',
      'database_id',
      'hyper_param_search',
      'plp_model_file',
      'execution_date_time',
      'training_time',
      'intercept'), 
    values = c(
      enc(analysisId),
      enc(modelName),
      targetId,
      outcomeId,
      tarId,
      plpDataSettingId,
      populationSettingId,
      modelSettingId,
      covariateSettingId,
      sampleSettingId,
      splitSettingId,
      featureEngineeringSettingId,
      tidyCovariatesSettingId,
      ifelse(requireDenseMatrix, "'T'", "'F'"),
      researcherId,
      databaseId,
      enc(hyperParamSearch),
      enc(plpModelFile),
      enc(dateTime),
      enc(trainingTime),
      intercept),
    tempEmulationSchema = tempEmulationSchema
  )
  
  if(nrow(result)==0){
    # model
    sql <- "INSERT INTO @my_schema.@string_to_appendmodels(analysis_id,
    model_name,
    target_id,
    outcome_id,
    tar_id,
    plp_data_setting_id,
                           population_setting_id,
                           model_setting_id,
                           covariate_setting_id,
                           sample_setting_id,
                           split_setting_id,
                           feature_engineering_setting_id,
                           tidy_covariates_setting_id,
                           require_dense_matrix,
    researcher_id,
    database_id,
    hyper_param_search,
    plp_model_file,
    execution_date_time,
    training_time,
    intercept) VALUES 
  ('@analysis_id', '@model_name', @target_id, @outcome_id, @tar_id, @plp_data_setting_id,
  @population_setting_id, @model_setting_id, @covariate_setting_id, 
  @sample_setting_id, @split_setting_id, @feature_engineering_setting_id, @tidy_covariates_setting_id,
  '@require_dense_matrix', @researcher_id, 
  @database_id, '@hyper_param_search', '@plp_model_file', '@date_time', 
    '@training_time', @intercept)"
    sql <- SqlRender::render(
      sql, 
      my_schema = resultSchema,
      analysis_id = analysisId,
      model_name = modelName,
      target_id = targetId,
      outcome_id = outcomeId,
      tar_id = tarId,
      plp_data_setting_id= plpDataSettingId,
      population_setting_id = populationSettingId,
      model_setting_id = modelSettingId,
      covariate_setting_id = covariateSettingId,
      sample_setting_id = sampleSettingId,
      split_setting_id = splitSettingId,
      feature_engineering_setting_id = featureEngineeringSettingId,
      tidy_covariates_setting_id = tidyCovariatesSettingId,
      require_dense_matrix = ifelse(requireDenseMatrix, 'T', 'F'),
      researcher_id = researcherId,
      database_id = databaseId,
      hyper_param_search = hyperParamSearch,
      plp_model_file = plpModelFile,
      date_time = dateTime,
      training_time = trainingTime,
      intercept = intercept,
      string_to_append = stringAppendToTables
    )
    sql <- SqlRender::translate(sql, targetDialect = targetDialect,
                                tempEmulationSchema = tempEmulationSchema)
    DatabaseConnector::executeSql(conn, sql)
    
    #getId of new
    result <- checkTable(conn = conn, 
                         resultSchema = resultSchema, 
                         stringAppendToTables = stringAppendToTables,
                         targetDialect = targetDialect, 
                         tableName = 'models',
                         columnNames = c('analysis_id',
                                         'model_name',
                                         'target_id',
                                         'outcome_id',
                                         'tar_id',
                           'plp_data_setting_id',
                           'population_setting_id',
                           'model_setting_id',
                           'covariate_setting_id',
                           'sample_setting_id',
                           'split_setting_id',
                           'feature_engineering_setting_id',
                           'tidy_covariates_setting_id',
                           'require_dense_matrix',
                                         'researcher_id',
                                         'database_id',
                                         'hyper_param_search',
                                         'plp_model_file',
                                         'execution_date_time',
                                         
                                         'training_time',
                                         'intercept'), 
                         values = c(enc(analysisId),
                                    enc(modelName),
                                    targetId,
                                    outcomeId,
                                    tarId,
                           plpDataSettingId,
                           populationSettingId,
                           modelSettingId,
                           covariateSettingId,
                           sampleSettingId,
                           splitSettingId,
                           featureEngineeringSettingId,
                           tidyCovariatesSettingId,
                           ifelse(requireDenseMatrix, "'T'", "'F'"),
                                    researcherId,
                                    databaseId,
                                    enc(hyperParamSearch),
                                    enc(plpModelFile),
                                    enc(dateTime),
                                    enc(trainingTime),
                                    intercept),
                         tempEmulationSchema = tempEmulationSchema
    )
    
  } 
  
  return(result$modelId[1])
}



addResult <- function(conn, resultSchema, targetDialect,
                      stringAppendToTables = '',
                      modelId,
                      researcherId,
                      databaseId,
                      targetId,
                      outcomeId,
                      tarId,
                      populationSettingId,
                      executionDateTime,
                      plpVersion,
                      tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")){
  
  result <- checkTable(conn = conn, 
                       resultSchema = resultSchema, 
                       stringAppendToTables = stringAppendToTables,
                       targetDialect = targetDialect, 
                       tableName = 'results',
                       columnNames = c('model_id',
                                       'researcher_id',
                                       'database_id',
                                       'target_id',
                                       'outcome_id',
                                       'tar_id',
                                       'population_setting_id',
                                       'execution_date_time',
                                       'plp_version'), 
                       values = c(modelId,
                                  researcherId,
                                  databaseId,
                                  targetId,
                                  outcomeId,
                                  tarId,
                                  populationSettingId,
                                  enc(executionDateTime),
                                  enc(plpVersion)),
                       tempEmulationSchema = tempEmulationSchema
  )
  
  if(nrow(result)==0){
    # model
    sql <- "INSERT INTO @my_schema.@string_to_appendresults (
    model_id,
    researcher_id,
    database_id,
    target_id,
    outcome_id,
    tar_id,
    population_setting_id,
    execution_date_time,
    plp_version
  ) 
  VALUES (@model_id, @researcher_id, @database_id, @target_id, @outcome_id, @tar_id, 
          @population_setting_id, '@execution_date_time', '@plp_version')"
    sql <- SqlRender::render(sql, 
                             my_schema = resultSchema,
                             model_id = modelId,
                             researcher_id = researcherId,
                             database_id = databaseId,
                             target_id = targetId,
                             outcome_id = outcomeId,
                             tar_id = tarId,
                             population_setting_id = populationSettingId,
                             execution_date_time = executionDateTime,
                             plp_version = plpVersion,
                             string_to_append = stringAppendToTables)
    sql <- SqlRender::translate(sql, targetDialect = targetDialect,
                                tempEmulationSchema = tempEmulationSchema)
    DatabaseConnector::executeSql(conn, sql)
    
    #getId of new
    result <- checkTable(conn = conn, 
                         resultSchema = resultSchema, 
                         stringAppendToTables = stringAppendToTables,
                         targetDialect = targetDialect, 
                         tableName = 'results',
                         columnNames = c('model_id',
                                         'researcher_id',
                                         'database_id',
                                         'target_id',
                                         'outcome_id',
                                         'tar_id',
                                         'population_setting_id',
                                         'execution_date_time',
                                         'plp_version'), 
                         values = c(modelId,
                                    researcherId,
                                    databaseId,
                                    targetId,
                                    outcomeId,
                                    tarId,
                                    populationSettingId,
                                    enc(executionDateTime),
                                    enc(plpVersion)),
                         tempEmulationSchema = tempEmulationSchema
    )
    
  } 
  
  return(result$resultId[1])
}


# evals
addEvaluation <- function(conn, resultSchema, targetDialect,
                          stringAppendToTables = '',
                          resultId,
                          performanceEvaluation,
                          overWriteIfExists = T,
                          tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")){
  
  ParallelLogger::logInfo('Adding PredictionDistribution')
  tryCatch({addPredictionDistribution(conn = conn, resultSchema = resultSchema, targetDialect = targetDialect,
                                      stringAppendToTables = stringAppendToTables,
                                      resultId = resultId,
                                      performanceEvaluation = performanceEvaluation,
                                      overWriteIfExists = overWriteIfExists,
                                      tempEmulationSchema = tempEmulationSchema)},
           error = function(e){ParallelLogger::logError(e);})
  
  ParallelLogger::logInfo('Adding ThresholdSummary')
  tryCatch({addThresholdSummary(conn = conn, resultSchema = resultSchema, targetDialect = targetDialect,
                                stringAppendToTables = stringAppendToTables,
                                resultId = resultId,
                                performanceEvaluation = performanceEvaluation,
                                overWriteIfExists = overWriteIfExists,
                                tempEmulationSchema = tempEmulationSchema)},
           error = function(e){ParallelLogger::logError(e);})
  
  ParallelLogger::logInfo('Adding EvaluationStatistics')
  tryCatch({addEvaluationStatistics(conn = conn, resultSchema = resultSchema, targetDialect = targetDialect,
                                    stringAppendToTables = stringAppendToTables,
                                    resultId = resultId,
                                    performanceEvaluation = performanceEvaluation,
                                    overWriteIfExists = overWriteIfExists,
                                    tempEmulationSchema = tempEmulationSchema)},
           error = function(e){ParallelLogger::logError(e);})
  
  ParallelLogger::logInfo('Adding CalibrationSummary')
  tryCatch({addCalibrationSummary(conn = conn, resultSchema = resultSchema, targetDialect = targetDialect,
                                  stringAppendToTables = stringAppendToTables,
                                  resultId = resultId,
                                  performanceEvaluation = performanceEvaluation,
                                  overWriteIfExists = overWriteIfExists,
                                  tempEmulationSchema = tempEmulationSchema)},
           error = function(e){ParallelLogger::logError(e);})
  
  ParallelLogger::logInfo('Adding DemographicSummary')
  tryCatch({addDemographicSummary(conn = conn, resultSchema = resultSchema, targetDialect = targetDialect,
                                  stringAppendToTables = stringAppendToTables,
                                  resultId = resultId,
                                  performanceEvaluation = performanceEvaluation,
                                  overWriteIfExists = overWriteIfExists,
                                  tempEmulationSchema = tempEmulationSchema)},
           error = function(e){ParallelLogger::logError(e);})
  
  return(invisible(NULL))
  
}

addPredictionDistribution <- function(conn, resultSchema, targetDialect,
                                      stringAppendToTables = '',
                                      resultId,
                                      performanceEvaluation,
                                      overWriteIfExists = T,
                                      tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")){
  
  value <- performanceEvaluation$predictionDistribution
  if(is.null(value)){
    return(NULL)
  }
  
  # edit names
  firstLower <- function(x) {
    substr(x, 1, 1) <- tolower(substr(x, 1, 1))
    return(x)
  }
  colnames(value) <- sapply(colnames(value), firstLower )
  if(sum(colnames(value)=='class')>0){
    colnames(value)[colnames(value)=='class'] <- 'classLabel'
  }
  
  value$resultId <- resultId
  
  # get column names and check all present in object
  columnNames <- getColumnNames(conn = conn, 
                                resultSchema = resultSchema, 
                                targetDialect = targetDialect, 
                                tableName = paste0(stringAppendToTables,'prediction_distribution'), 
                                tempEmulationSchema = tempEmulationSchema)
  isValid <- sum(colnames(value)%in%columnNames) == length(columnNames)
  
  exists <- checkResultExists(conn = conn, 
                              resultSchema = resultSchema, 
                              targetDialect = targetDialect, 
                              tableName = paste0(stringAppendToTables,'prediction_distribution'),
                              resultId = resultId,
                              tempEmulationSchema = tempEmulationSchema)
  
  if(isValid && (!exists || overWriteIfExists)){
    
    # REMOVE existing result
    if(exists){
      sql <- "delete from @result_schema.@table_name where result_id = @result_id;"
      sql <- SqlRender::render(sql, 
                               result_id=resultId,
                               result_schema = resultSchema,
                               table_name = paste0(stringAppendToTables,'prediction_distribution'))
      sql <- SqlRender::translate(sql, 
                                  targetDialect = targetDialect,
                                  tempEmulationSchema = tempEmulationSchema)
      DatabaseConnector::executeSql(conn, sql)
    }
    
    # add 
    ParallelLogger::logInfo(paste0('Inserting predictionDistribution for result ',resultId))
    DatabaseConnector::insertTable(connection = conn, 
                                   databaseSchema = resultSchema, 
                                   tableName = paste0(stringAppendToTables,'prediction_distribution'), 
                                   data = value[,columnNames], 
                                   dropTableIfExists = F, createTable = F, tempTable = F, 
                                   bulkLoad = F, camelCaseToSnakeCase = T, progressBar = T,
                                   tempEmulationSchema = tempEmulationSchema)
  }
  
  return(invisible(NULL))
}

addThresholdSummary <- function(conn, resultSchema, targetDialect,
                                stringAppendToTables = '',
                                resultId,
                                performanceEvaluation,
                                overWriteIfExists = T,
                                tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")){
  
  
  value <- performanceEvaluation$thresholdSummary
  if(is.null(value)){
    return(NULL)
  }
  
  #  check numerical columns:
  value <- cleanNum(value)
  
  # edit names
  firstLower <- function(x) {
    substr(x, 1, 1) <- tolower(substr(x, 1, 1))
    return(x)
  }
  colnames(value) <- sapply(colnames(value), firstLower )
  value$resultId <- resultId
  
  # get column names and check all present in object
  columnNames <- getColumnNames(conn = conn, 
                                resultSchema = resultSchema, 
                                targetDialect = targetDialect, 
                                stringAppendToTables = stringAppendToTables,
                                tableName = 'threshold_summary',
                                tempEmulationSchema = tempEmulationSchema)
  isValid <- sum(colnames(value)%in%columnNames) == length(columnNames)
  
  exists <- checkResultExists(conn = conn, 
                              resultSchema = resultSchema, 
                              targetDialect = targetDialect, 
                              tableName = paste0(stringAppendToTables,'threshold_summary'),
                              resultId = resultId,
                              tempEmulationSchema = tempEmulationSchema)
  
  if(isValid && (!exists || overWriteIfExists)){
    
    # REMOVE existing result
    if(exists){
      sql <- "delete from @result_schema.@table_name where result_id = @result_id;"
      sql <- SqlRender::render(sql, 
                               result_schema = resultSchema,
                               result_id = resultId,
                               table_name = paste0(stringAppendToTables,'threshold_summary'))
      sql <- SqlRender::translate(sql, 
                                  targetDialect = targetDialect,
                                  tempEmulationSchema = tempEmulationSchema)
      DatabaseConnector::executeSql(conn, sql)
    }
    
    # add 
    ParallelLogger::logInfo(paste0('Inserting thresholdSummary for result ',resultId))
    DatabaseConnector::insertTable(connection = conn, 
                                   databaseSchema = resultSchema, 
                                   tableName = paste0(stringAppendToTables,'threshold_summary'), 
                                   data = value[,columnNames], 
                                   dropTableIfExists = F, createTable = F, tempTable = F, 
                                   bulkLoad = F, camelCaseToSnakeCase = T, progressBar = T,
                                   tempEmulationSchema = tempEmulationSchema)
  }
  
  return(invisible(NULL))
}


addCalibrationSummary <- function(conn, resultSchema, targetDialect, 
                                  stringAppendToTables = '',
                                  resultId,
                                  performanceEvaluation,
                                  overWriteIfExists = T,
                                  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")){
  
  
  value <- performanceEvaluation$calibrationSummary
  if(is.null(value)){
    return(NULL)
  }
  
  #  check numerical columns:
  value <- cleanNum(value)
  
  # edit names
  firstLower <- function(x) {
    substr(x, 1, 1) <- tolower(substr(x, 1, 1))
    return(x)
  }
  colnames(value) <- sapply(colnames(value), firstLower )
  
  value$resultId <- resultId
  
  # get column names and check all present in object
  columnNames <- getColumnNames(conn = conn, 
                                resultSchema = resultSchema, 
                                targetDialect = targetDialect, 
                                stringAppendToTables = stringAppendToTables,
                                tableName = 'calibration_summary',
                                tempEmulationSchema = tempEmulationSchema)
  isValid <- sum(colnames(value)%in%columnNames) == length(columnNames)
  
  exists <- checkResultExists(conn = conn, 
                              resultSchema = resultSchema, 
                              targetDialect = targetDialect, 
                              tableName = paste0(stringAppendToTables,'calibration_summary'),
                              resultId = resultId,
                              tempEmulationSchema = tempEmulationSchema)
  
  if(isValid && (!exists || overWriteIfExists)){
    
    # REMOVE existing result
    if(exists){
      sql <- "delete from @result_schema.@table_name where result_id = @result_id;"
      sql <- SqlRender::render(sql, 
                               result_schema = resultSchema,
                               result_id=resultId,
                               table_name = paste0(stringAppendToTables,'calibration_summary'))
      sql <- SqlRender::translate(sql, 
                                  targetDialect = targetDialect,
                                  tempEmulationSchema = tempEmulationSchema)
      DatabaseConnector::executeSql(conn, sql)
    }
    
    # add 
    ParallelLogger::logInfo(paste0('Inserting calibrationSummary for result ',resultId))
    DatabaseConnector::insertTable(connection = conn, 
                                   databaseSchema = resultSchema, 
                                   tableName = paste0(stringAppendToTables,'calibration_summary'), 
                                   data = value[,columnNames], 
                                   dropTableIfExists = F, createTable = F, tempTable = F, 
                                   bulkLoad = F, camelCaseToSnakeCase = T, progressBar = T,
                                   tempEmulationSchema = tempEmulationSchema)
  }
  
  return(invisible(NULL))
}

addEvaluationStatistics <- function(conn, resultSchema, targetDialect,
                                    stringAppendToTables = '',
                                    resultId,
                                    performanceEvaluation,
                                    overWriteIfExists = T,
                                    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")){
  
  
  value <- data.frame(
    evaluation = unlist(performanceEvaluation$evaluationStatistics$evaluation),
    metric = unlist(performanceEvaluation$evaluationStatistics$metric),
    value = as.numeric(unlist(performanceEvaluation$evaluationStatistics$value))
      )
  
  if(is.null(value)){
    return(NULL)
  }
  
  # edit names
  firstLower <- function(x) {
    substr(x, 1, 1) <- tolower(substr(x, 1, 1))
    return(x)
  }
  colnames(value) <- sapply(colnames(value), firstLower )
  value$resultId <- resultId
  
  # get column names and check all present in object
  columnNames <- getColumnNames(conn = conn, 
                                resultSchema = resultSchema, 
                                targetDialect = targetDialect, 
                                stringAppendToTables = stringAppendToTables,
                                tableName = 'evaluation_statistics',
                                tempEmulationSchema = tempEmulationSchema)
  isValid <- sum(colnames(value)%in%columnNames) == length(columnNames)
  
  exists <- checkResultExists(conn = conn, 
                              resultSchema = resultSchema, 
                              targetDialect = targetDialect,
                              tableName = paste0(stringAppendToTables,'evaluation_statistics'),
                              resultId = resultId,
                              tempEmulationSchema = tempEmulationSchema)
  
  if(isValid && (!exists || overWriteIfExists)){
    
    # REMOVE existing result
    if(exists){
      sql <- "delete from @result_schema.@table_name where result_id = @result_id;"
      sql <- SqlRender::render(sql, 
                               result_schema = resultSchema,
                               result_id = resultId,
                               table_name = paste0(stringAppendToTables,'evaluation_statistics'))
      sql <- SqlRender::translate(sql, 
                                  targetDialect = targetDialect,
                                  tempEmulationSchema = tempEmulationSchema)
      DatabaseConnector::executeSql(conn, sql)
    }
    
    # add 
    ParallelLogger::logInfo(paste0('Inserting evaluationSummary for result ',resultId))
    DatabaseConnector::insertTable(connection = conn, 
                                   databaseSchema = resultSchema, 
                                   tableName = paste0(stringAppendToTables,'evaluation_statistics'), 
                                   data = value[,columnNames], 
                                   dropTableIfExists = F, createTable = F, tempTable = F, 
                                   bulkLoad = F, camelCaseToSnakeCase = T, progressBar = T,
                                   tempEmulationSchema = tempEmulationSchema)
  }
  
  return(invisible(NULL))
}

addDemographicSummary <- function(conn, resultSchema, targetDialect, 
                                  stringAppendToTables = '',
                                  resultId,
                                  performanceEvaluation,
                                  overWriteIfExists = T,
                                  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")){
  
  
  value <- performanceEvaluation$demographicSummary
  if(is.null(value)){
    return(NULL)
  }
  
  # edit names
  firstLower <- function(x) {
    substr(x, 1, 1) <- tolower(substr(x, 1, 1))
    return(x)
  }
  colnames(value) <- sapply(colnames(value), firstLower )
  if(sum(colnames(value)=="p50PredictedProbability")>0){
    colnames(value)[colnames(value)=="p50PredictedProbability"] <- 'medianPredictedProbability'
  }
  
  value$resultId <- resultId
  
  # get column names and check all present in object
  columnNames <- getColumnNames(conn = conn, 
                                resultSchema = resultSchema, 
                                targetDialect = targetDialect, 
                                stringAppendToTables =  stringAppendToTables,
                                tableName = 'demographic_summary',
                                tempEmulationSchema = tempEmulationSchema)
  isValid <- sum(colnames(value)%in%columnNames) == length(columnNames)
  
  exists <- checkResultExists(conn = conn, 
                              resultSchema = resultSchema, 
                              targetDialect = targetDialect, 
                              tableName = paste0(stringAppendToTables,'demographic_summary'),
                              resultId = resultId,
                              tempEmulationSchema = tempEmulationSchema)
  
  if(isValid && (!exists || overWriteIfExists)){
    
    # REMOVE existing result
    if(exists){
      sql <- "delete from @result_schema.@table_name where result_id = @result_id;"
      sql <- SqlRender::render(sql, 
                               result_schema = resultSchema,
                               result_id = resultId,
                               table_name = paste0(stringAppendToTables,'demographic_summary'))
      sql <- SqlRender::translate(sql, 
                                  targetDialect = targetDialect,
                                  tempEmulationSchema = tempEmulationSchema)
      DatabaseConnector::executeSql(conn, sql)
    }
    
    # add 
    ParallelLogger::logInfo(paste0('Inserting demographicSummary for result ',resultId))
    DatabaseConnector::insertTable(connection = conn, 
                                   databaseSchema = resultSchema, 
                                   tableName = paste0(stringAppendToTables,'demographic_summary'), 
                                   data = value[,columnNames], 
                                   dropTableIfExists = F, createTable = F, tempTable = F, 
                                   bulkLoad = F, camelCaseToSnakeCase = T, progressBar = T,
                                   tempEmulationSchema = tempEmulationSchema)
  }
  
  return(invisible(NULL))
}

addCovariateSummary <- function(conn, resultSchema, targetDialect, 
                                stringAppendToTables = '',
                                resultId,
                                covariateSummary,
                                restrictToIncluded = T,
                                overWriteIfExists = T,
                                tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")){
  
  
  value <- covariateSummary
  if(is.null(value)){
    return(NULL)
  }
  
  # edit names
  firstLower <- function(x) {
    substr(x, 1, 1) <- tolower(substr(x, 1, 1))
    return(x)
  }
  colnames(value) <- sapply(colnames(value), firstLower )
  value$resultId <- resultId
  
  if(restrictToIncluded){
    ParallelLogger::logInfo('Restricting to covariates included in model')
    value <- value[value$covariateValue!=0,] 
  }
  
  # get column names and check all present in object
  columnNames <- getColumnNames(conn = conn, 
                                resultSchema = resultSchema, 
                                targetDialect = targetDialect, 
                                stringAppendToTables = stringAppendToTables,
                                tableName = 'covariate_summary',
                                tempEmulationSchema = tempEmulationSchema)
  isValid <- sum(colnames(value)%in%columnNames) == length(columnNames)
  
  exists <- checkResultExists(conn = conn, 
                              resultSchema = resultSchema, 
                              targetDialect = targetDialect, 
                              tableName = paste0(stringAppendToTables,'covariate_summary'),
                              resultId = resultId,
                              tempEmulationSchema = tempEmulationSchema)
  
  if(isValid && (!exists || overWriteIfExists)){
    
    # REMOVE existing result
    if(exists){
      ParallelLogger::logTrace('Removing existing covariateSummary')
      sql <- "delete from @result_schema.@table_name where result_id = @result_id;"
      sql <- SqlRender::render(sql, 
                               result_schema = resultSchema,
                               result_id = resultId,
                               table_name = paste0(stringAppendToTables,'covariate_summary'))
      sql <- SqlRender::translate(sql, 
                                  targetDialect = targetDialect,
                                  tempEmulationSchema = tempEmulationSchema)
      DatabaseConnector::executeSql(conn, sql)
    }
    
    # add 
    ParallelLogger::logInfo(paste0('Inserting covariateSummary for result ',resultId))
    DatabaseConnector::insertTable(connection = conn, 
                                   databaseSchema = resultSchema, 
                                   tableName = paste0(stringAppendToTables,'covariate_summary'), 
                                   data = value[,columnNames], 
                                   dropTableIfExists = F, createTable = F, tempTable = F, 
                                   bulkLoad = F, camelCaseToSnakeCase = T, progressBar = T,
                                   tempEmulationSchema = tempEmulationSchema)
  }
  
  return(invisible(NULL))
}

