# @file ImportExport.R
#
# Copyright 2017 Observational Health Data Sciences and Informatics
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


#' Export all data in a plpData object to CSV files
#'
#' @details
#' Created a set of CSV files in the output folder with all the data in the plplData object. This
#' function is intended to be used for research into prediction methods. The following files will be
#' created: \describe{ \item{cohort.csv}{Listing all persons and their prediction periods. This file
#' will have these fields: row_id (a unique ID per period), person_id, cohort_start_date, cohort_id,
#' time (number of days in the window).} \item{outcomes.csv}{Listing all outcomes per period. This
#' file will have these fields: row_id, outcome_id, outcome_count, time_to_event.}
#' \item{exclude.csv}{Either not exported or a file listing per outcome ID which windows had the
#' outcome prior to the window and should therefore be removed prior to fitting the model. This object
#' will have these fields: rowId, outcomeId.} \item{covariates.csv}{Listing the baseline covariates
#' per person in the cohorts. This is done using a sparse representation: covariates with a value of 0
#' are omitted to save space. The covariates file will have three columns: rowId, covariateId, and
#' covariateValue. } \item{covariateRef.csv}{A file describing the covariates that have been
#' extracted.} \item{metaData}{Some information on how the plpData object was constructed.} }
#'
#'
#' @param plpData        An object of type \code{plpData}.
#' @param outputFolder   The folder on the file system where the CSV files will be created. If the
#'                       folder does not yet exist it will be created.
#'
#' @examples
#' \dontrun{
#' exportPlpDataToCsv(plpData, "s:/temp/exportTest")
#' }
#' @export
exportPlpDataToCsv <- function(plpData, outputFolder) {
  start <- Sys.time()
  if (!file.exists(outputFolder)) {
    dir.create(outputFolder)
  }
  writeLines("Exporting cohorts.csv")
  ff::write.csv.ffdf(plpData$cohorts, file = file.path(outputFolder, "cohorts.csv"))
  writeLines("Exporting outcomes.csv")
  ff::write.csv.ffdf(plpData$outcomes, file = file.path(outputFolder, "outcomes.csv"))
  if (!is.null(plpData$exclude)) {
    writeLines("Exporting exclude.csv")
    ff::write.csv.ffdf(plpData$exclude, file = file.path(outputFolder, "exclude.csv"))
  }
  writeLines("Exporting covariates.csv")
  ff::write.csv.ffdf(plpData$covariates, file = file.path(outputFolder, "covariates.csv"))
  writeLines("Exporting covariateRef.csv")
  ff::write.csv.ffdf(plpData$covariateRef, file = file.path(outputFolder, "covariateRef.csv"))
  writeLines("Exporting metaData.csv")
  metaData <- data.frame(cohortIds = paste(plpData$metaData$cohortIds, collapse = ","),
                         outcomeIds = paste(plpData$metaData$outcomeIds, collapse = ","),
                         useCohortEndDate = plpData$metaData$useCohortEndDate,
                         windowPersistence = plpData$metaData$windowPersistence)
  write.csv(metaData, file = file.path(outputFolder, "metaData.csv"), row.names = FALSE)
  writeLines("Done exporting")
  delta <- Sys.time() - start
  writeLines(paste("Exporting data to CSV took", signif(delta, 3), attr(delta, "units")))
}




#'Transports a plpResult to a new location and removed sensitive data
#'
#' @details
#' This function is used to 
#'
#' @param plpResult      An object returned by running \code{runPlp}.
#' @param outputFolder   The folder on the file system where the CSV files will be created. If the
#'                       folder does not yet exist it will be created.
#' @param n              The minimum number of people required for each result summary to be included 
#' @param includeEvaluationStatistics  Whether to include the evaluationStatistics
#' @param includeThresholdSummary      Whether to include the thresholdSummary
#' @param includeDemographicSummary    Whether to include the demographicSummary
#' @param includeCalibrationSummary    Whether to include the calibrationSummary
#' @param includePredictionDistribution  Whether to include the predictionDistribution  
#' @param includeCovariateSummary      Whether to include the covariateSummary             
#'
#' @examples
#' \dontrun{
#' transportPlp(plpResult, "s:/temp/exportTest", n=10)
#' }
#' @export
#' 
#' 
transportPlp <- function(plpResult,outputFolder, n=NULL,includeEvaluationStatistics=T,
                         includeThresholdSummary=T, includeDemographicSummary=T, 
                         includeCalibrationSummary =T, includePredictionDistribution=T,
                         includeCovariateSummary=T){
  
  # remove any sensitive data:
  plpResult$inputSetting$dataExtrractionSettings <- NULL
  plpResult$model$metaData$call$connectionDetails <- NULL
  plpResult$model$metaData$call$cdmDatabaseSchema <- NULL
  plpResult$model$metaData$call$cohortDatabaseSchema <- NULL
  plpResult$model$metaData$call$outcomeDatabaseSchema <- NULL
  plpResult$model$index <- NULL
  plpResult$prediction <- NULL
  mod <- get("plpModel", envir = environment(plpResult$model$predict))
  mod$index <- NULL
  mod$metaData$call$connectionDetails <- NULL
  assign("plpModel", mod, envir = environment(plpResult$model$predict))
  
  if(!includeEvaluationStatistics)
   plpResult$performanceEvaluation$evaluationStatistics <- NULL
  if(!includeThresholdSummary)
    plpResult$performanceEvaluation$thresholdSummary <- NULL  
  if(!includeDemographicSummary)
    plpResult$performanceEvaluation$demographicSummary <- NULL 
  if(!includeCalibrationSummary)
    plpResult$performanceEvaluation$calibrationSummary <- NULL 
  if(!includePredictionDistribution)
    plpResult$performanceEvaluation$predictionDistribution <- NULL  
  if(!includeCovariateSummary)
    plpResult$covariateSummary <- NULL
  
  # remove things less than n
  if(!is.null(n)){
    # remove less than n counts from demographicSummary
    
    includeInd <- plpResult$performanceEvaluation$demographicSummary$PersonCountAtRisk>=n &
                  plpResult$performanceEvaluation$demographicSummary$PersonCountWithOutcome >= n
    plpResult$performanceEvaluation$demographicSummary <- plpResult$performanceEvaluation$demographicSummary[includeInd,]
   
    plpResult$covariateSummary <- plpResult$covariateSummary[,colnames(plpResult$covariateSummary)%in%c('covariateId','covariateName', 'analysisId', 'conceptId','CovariateCount', 'covariateValue')]  
    plpResult$covariateSummary <- plpResult$covariateSummary[plpResult$covariateSummary$CovariateCount>=n,]
      
    }
  
  #save to the output location
  PatientLevelPrediction::savePlpResult(plpResult, outputFolder)
  
}

#'Transports a plpModel to a new location and removes sensitive data
#'
#' @details
#' This function is used to 
#'
#' @param plpModel      A trianed model.
#' @param outputFolder   The folder on the file system where the CSV files will be created. If the
#'                       folder does not yet exist it will be created.
#'
#' @examples
#' \dontrun{
#' transportModel(plpModel, "s:/temp/exportTest")
#' }
#' @export
#' 
#' 
transportModel <- function(plpModel,outputFolder){
  
  plpModel$index <- NULL
  plpModel$metaData$call$connectionDetails <- NULL
  plpModel$metaData$call$cdmDatabaseSchema <- NULL
  plpModel$metaData$call$cohortDatabaseSchema <- NULL
  plpModel$metaData$call$outcomeDatabaseSchema <- NULL
  
  # remove any sensitive data:
  mod <- get("plpModel", envir = environment(plpModel$predict))
  mod$index <- NULL
  mod$metaData$call$connectionDetails <- NULL
  assign("plpModel", mod, envir = environment(plpModel$predict))
  
  
  #save to the output location
  PatientLevelPrediction::savePlpModel(plpModel, outputFolder)
  
}


#' Convert logistic regression model to sql code...
#'
#' @details
#' This function is used to create custom covariates for a logistic regression model
#' (currently only supports, demographics/conditions/drug/procedures/observations and measurement concepts)
#'
#' @param models          A trianed plp model.
#' @param modelNames      A name used in the covariate function names (no spaces)
#' @param covariateConstructionName     the name used to for the create covariate function
#' @param model_table     The temporary table name storing the model details
#' @param analysis_id    The covariate analysis_id 
#' @param e              The environment to output the covariate setting functions to
#' @param databaseOutput  If you want to output to go inot a cohort table add the "database.schema.tablename" here

#' @export
#' 
createLrSql <- function(models, modelNames, covariateConstructionName='prediction', model_table='#model_table',
                         analysis_id=111, e=environment(),
                        databaseOutput=NULL){
  
  # test model inputs
  #====================================
  if(!exists("models"))
    stop('Need to input models')
  if(!exists("modelNames"))
    stop('Need to input model names')
  if(length(models)!=length(modelNames) & class(models)!='plpModel')
    stop('model names not same length as models')
  if(class(models)!='list' & class(models)!='plpModel')
    stop('wrong models class')
  if(class(modelNames)!='character')
    stop('wrong model names class')
  #====================================
  
  # create the settings function
  #====================================
  createCovs <- function() {
    covariateSettings <- list(model_table=model_table, modelNames=modelNames,
                              covariateConstructionName = covariateConstructionName,
                              analysis_id=analysis_id, databaseOutput=databaseOutput)
    attr(covariateSettings, "fun") <- paste0('get',covariateConstructionName,'CovariateSettings')
    class(covariateSettings) <- "covariateSettings"
    return(covariateSettings)
  }
  
  
  getCovs <- function(connection,
                      oracleTempSchema = NULL,
                      cdmDatabaseSchema,
                      cohortTable = "#cohort_person",
                      cohortId = -1,
                      cdmVersion = "5",
                      rowIdField = "subject_id",
                      covariateSettings,
                      aggregated = FALSE){ 
    
    if (aggregated)
      stop("Aggregation not supported")
    
    if(!is.null(covariateSettings$model_table)){
      model_table = covariateSettings$model_table} else{
        model_table='#model_table'
      }
    
    if(is.null(covariateSettings$analysis_id)){
      analysis_id <- 111
    }
    
    # test the covariates use the same time settings so we can do call at the same time
    # use model$metaData$call$covariateSettings to check equality
    #if(class(models)!='plpModel'){
    #  if(sum(unlist(lapply(models, function(x) sameCovs(x, models[[1]]))))!=length(models))
    #    stop('Covariates are inconsistent between models')
    #}
    
    
    # =================================
    #==================================
    #  INSERT THE INTERCEPTS FOR THE MODELS: MODEL_ID, INTERCEPT
    if(class(models)=='plpModel'){ 
      intercepts <- data.frame(modelId=1,
                      intercept= models$model$coefficients[names(models$model$coefficients)=="(Intercept)"])
      
    } else{
      intercepts <- cbind(modelId=1:length(models),
                          intercept= unlist(lapply(models, 
                                                   function(x) x$model$coefficients[names(x$model$coefficients)=="(Intercept)"]))
      )}
    
    colnames(intercepts) <- SqlRender::camelCaseToSnakeCase(colnames(intercepts))
    DatabaseConnector::insertTable(connection, tableName='#intercepts', data = intercepts, tempTable = T)
    
    
    # =================================
    #==================================
    #  CREATE THE COVARIATE SETTINGS 
    
    # create the table with: model_id, covariate_id, covariateValue 
    if(class(models)=='plpModel'){ 
      allVars <- cbind(model_id=rep(1, sum(models$varImp$covariateValue!=0) ),
                       models$varImp[models$varImp$covariateValue!=0,])
      allVars <- merge(allVars, models$metaData$normFactors, by.x="covariateId", by.y="bins", all.x=T)
      allVars$maxs[is.na(allVars$maxs)] <- 1
      allVars$covariateValue <- allVars$covariateValue/allVars$maxs
      
    } else{
      allVars <- c()
      for( i in 1:length(models)){
        vars <- cbind(model_id=rep(i, sum(models[[i]]$varImp$covariateValue!=0) ),
                      models[[i]]$varImp[models[[i]]$varImp$covariateValue!=0,])
        
        # update the vars value based on normalisation ((raw value/norm factor)*coef), revised_coef = coef/norm factor
        vars <- merge(vars, models[[i]]$metaData$normFactors, by.x="covariateId", by.y="bins", all.x=T)
        vars$maxs[is.na(vars$maxs)] <- 1
        vars$covariateValue <- vars$covariateValue/vars$maxs
        
        allVars <- rbind(allVars,vars)
      }
    }
    
    # insert the model table
    colnames(allVars) <- SqlRender::camelCaseToSnakeCase(colnames(allVars))
    DatabaseConnector::insertTable(connection, tableName=model_table, data = allVars, tempTable = T)
    
    # =================================
    #==================================
    #  RUN GETPLPDATA (BUT WITHOUT REMOVING THE COVARIATE TABLE)
    if(class(models)=='plpModel'){ 
      covSettings <- models$metaData$call$covariateSettings
    } else{
      covSettings <- models[[1]]$metaData$call$covariateSettings
    }
    #update some of the settings and only include variables in the model
    
    covSettings$includedCovariateIds <-  as.double(unique(allVars$covariate_id))
    covariates <- getPredictionCovariateData(connection = connection,
                                                oracleTempSchema = oracleTempSchema,
                                                cdmDatabaseSchema = cdmDatabaseSchema,
                                                cohortTable = "#cohort_person",
                                                cdmVersion = cdmVersion,
                                                rowIdField = "row_id",
                                                covariateSettings = covSettings,
                                             analysisId = covariateSettings$analysis_id,
                                             databaseOutput= covariateSettings$databaseOutput)
    
    # clean the model_table... [TODO]
    covariateRef <- data.frame(covariateId = 1000*(1:length(covariateSettings$modelNames))+covariateSettings$analysis_id,
                               covariateName = covariateSettings$modelNames,
                               analysisId = rep(covariateSettings$analysis_id,length(covariateSettings$modelNames)),
                               conceptId = rep(0, length(covariateSettings$modelNames)))
    covariateRef <- ff::as.ffdf(covariateRef)
    # Construct analysis reference:
    analysisRef <- data.frame(analysisId = covariateSettings$analysis_id,
                              analysisName = "Prediction Models",
                              domainId = "Demographics",
                              startDay = 0,
                              endDay = 0,
                              isBinary = "N",
                              missingMeansZero = "N")
    analysisRef <- ff::as.ffdf(analysisRef)
    # Construct analysis reference:
    metaData <- list(sql = sql, call = match.call())
    result <- list(covariates = covariates,
                   covariateRef = covariateRef,
                   analysisRef = analysisRef,
                   metaData = metaData)
    class(result) <- "covariateData"
    
    return(result)
  }
  
  assign(paste0('create',covariateConstructionName,'CovariateSettings'), createCovs,envir = e)
  assign(paste0('get',covariateConstructionName,'CovariateSettings'), getCovs,envir = e)
  
  return(T)
}


# helper function
sameCovs <- function(model1, model2){
  test1 <- model1$varImp[,c('covariateId', 'covariateName')]
  colnames(test1) <- c('covariateId', 'covariateName1')
  test2 <- model2$varImp[,c('covariateId', 'covariateName')]
  colnames(test2) <- c('covariateId', 'covariateName2')
  vars <-merge(test1,test2, 
               by='covariateId')
  if(nrow(vars)==0)
    return(FALSE)
  
  return(sum(vars$covariateName1==vars$covariateName2)==nrow(vars))
}


getPredictionCovariateData <- function(connection,
                                       oracleTempSchema = NULL,
                                       cdmDatabaseSchema,
                                       cohortTable = "#cohort_person",
                                       cohortId = -1,
                                       cdmVersion = "5",
                                       rowIdField = "subject_id",
                                       covariateSettings,
                                       aggregated = FALSE,
                                       analysisId=111,
                                       databaseOutput=NULL) {
  if (!is(covariateSettings, "covariateSettings")) {
    stop("Covariate settings object not of type covariateSettings")
  }
  if (cdmVersion == "4") {
    stop("Common Data Model version 4 is not supported")
  }
  FeatureExtraction::getDbDefaultCovariateData(connection = connection,
                                               oracleTempSchema = oracleTempSchema,
                                               cdmDatabaseSchema = cdmDatabaseSchema,
                                               cohortTable = cohortTable,
                                               cohortId = cohortId,
                                               rowIdField = rowIdField,
                                               covariateSettings = covariateSettings,
                                               targetCovariateTable = "#cov_temp",
                                               aggregated = aggregated)
  
  # =================================
  #==================================
  #  CALCULATE THE RISK
  writeLines("Calculating the risk scores...")
  
  if(is.null(databaseOutput)){
  # now sum the temp_covariates table t get val and then 1/(1+exp(-val))
  sql <- "select #cov_temp.row_id, #model_table.model_id*1000+@analysis_id as covariate_id, 
  1/(1+exp(-1.0*(max(#intercepts.intercept)+sum(#cov_temp.covariate_value*#model_table.covariate_value)))) covariate_value
  from #cov_temp inner join #model_table on #cov_temp.covariate_id=#model_table.covariate_id
  inner join #intercepts on #intercepts.model_id=#model_table.model_id
  group by #cov_temp.row_id,  #model_table.model_id"
  sql <- SqlRender::renderSql(sql, 
                              analysis_id = analysisId
  )$sql
  sql <- SqlRender::translateSql(sql, targetDialect ='pdw')$sql
  
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql.ffdf(connection, sql)
  # Convert colum names to camelCase:
  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))
} else {
  # TO ADD TO A COHORT TABLE 
  covariates <- NULL
  sql <- " select a.subject_id, a.cohort_start_date, a.cohort_start_date as cohort_end_date,
  b.covariate_id cohort_definition_id, b.covariate_value as risk 
  into @databaseOutput
  from #cohort_person a inner join 
  
  (select #cov_temp.row_id, #model_table.model_id*1000+@analysis_id as covariate_id, 
  1/(1+exp(-1.0*(max(#intercepts.intercept)+sum(#cov_temp.covariate_value*#model_table.covariate_value)))) covariate_value
  from #cov_temp inner join #model_table on #cov_temp.covariate_id=#model_table.covariate_id
  inner join #intercepts on #intercepts.model_id=#model_table.model_id
  group by #cov_temp.row_id,  #model_table.model_id) b on a.row_id=b.row_id"
  
  sql <- SqlRender::renderSql(sql, 
                              analysis_id = analysisId,
                              databaseOutput=databaseOutput
  )$sql
  sql <- SqlRender::translateSql(sql, targetDialect ='pdw')$sql
  DatabaseConnector::executeSql(connection, sql)
}
  
  return(covariates)
}


#' Apply an existing logistic regression prediction model
#'
#' @details
#' This function is used to create custom covariates corresponding to existing models
#'
#' @param modelTable         A dataframe or list of dataframes with columns: modelId, modelCovariateId, coefficientValue all doubles
#' @param modelNames      A name used in the covariate function names (no spaces)
#' @param interceptTable     A dataframe or list of dataframes with the columns: modelId, interceptValue
#' @param covariateTable     A dataframe or list of dataframes with columns: modelCovariateId, covariateId (the mapping of covariate_id to standard covariates)
#' @param type             The type of model: logistic or linear/score 
#' @param analysisId    The covariate analysis_id (default 112)
#' @param covariateSettings  The settings for the standard covariates (needs for temporal settings)
#' @param asFunctions        If T then return two functions
#' @param e              The environment to output the covariate setting functions to
#' @export
#' 
createExistingModelSql <- function(modelTable, modelNames, interceptTable,
                                   covariateTable, type='logistic',
                                   analysisId=112, covariateSettings, 
                                   asFunctions=F,
                                   e=environment()
                                   ){
  
  # test model inputs
  #====================================
  if(!exists("modelTable"))
    stop('Need to input modelTable')
  if(!exists("modelNames"))
    stop('Need to input model names')
  if(length(modelTable)!=length(modelNames) & class(modelTable)!='data.frame')
    stop('model names not same length as model table')
  if(class(modelTable)!='list' & class(modelTable)!='data.frame')
    stop('wrong modelTable class')
  if(class(modelNames)!='character')
    stop('wrong model names class')
  #====================================
  
  # create the settings function
  #====================================
  createCovs <- function() {
    createExistingmodelsCovariateSettings <- list(modelTable=modelTable, covariateTable=covariateTable,
                                                  interceptTable=interceptTable,
                                                  modelNames=modelNames,
                                                  analysisId=analysisId,
                                                  covariateSettings=covariateSettings,
                                                  type=type)
    attr(createExistingmodelsCovariateSettings, "fun") <- paste0('getExistingmodelsCovariateSettings')
    class(createExistingmodelsCovariateSettings) <- "covariateSettings"
    return(createExistingmodelsCovariateSettings)
  }
  
  getCovs <- function(connection,
                      oracleTempSchema = NULL,
                      cdmDatabaseSchema,
                      cohortTable = "#cohort_person",
                      cohortId = -1,
                      cdmVersion = "5",
                      rowIdField = "subject_id",
                      covariateSettings,
                      aggregated = FALSE){ 
    
    if (aggregated)
      stop("Aggregation not supported")
    
    model_table <- '#model_table'
    covariate_table <- '#covariate_table'
    
    if(is.null(covariateSettings$analysisId)){
      analysisId <- 112
    }
    
    interceptTable <- covariateSettings$interceptTable
    modelTable <- covariateSettings$modelTable
    covariateTable <- covariateSettings$covariateTable
    
    # =================================
    #==================================
    #  INSERT THE INTERCEPTS FOR THE MODELS: MODEL_ID, INTERCEPT_VALUE
    if(class(interceptTable)=='data.frame'){ 
      intercepts <- interceptTable
    } else{
      intercepts <- do.call(rbind, interceptTable)
      }
    
    colnames(intercepts) <- SqlRender::camelCaseToSnakeCase(colnames(intercepts))
    DatabaseConnector::insertTable(connection, tableName='#intercepts', data = intercepts, tempTable = T)
    
    
    # =================================
    #==================================
    #  INSERT THE MODEL TABLE
    
    # create the table with: model_id, model_covariate_id, coefficientValue 
    if(class(modelTable)=='data.frame'){ 
      allVars <- modelTable
      
    } else{
      allVars <- do.call(rbind, modelTable)
    }
    
    # adding age with 0 coef to make sure everyone gets value
    allVars <- rbind(allVars, data.frame(modelId=unique(allVars$modelId),
                                modelCovariateId=rep(-1,length(unique(allVars$modelId))), 
                                coefficientValue=rep(0,length(unique(allVars$modelId)))
                                ))
    
    
    # insert the model table
    colnames(allVars) <- SqlRender::camelCaseToSnakeCase(colnames(allVars))
    DatabaseConnector::insertTable(connection, tableName='model_table', data = allVars, tempTable = T)
    
    # =================================
    #==================================
    #  INSERT THE COVARIATE TABLE
    
    # create the table with: modelCovariateId, covariateId
    if(class(covariateTable)=='data.frame'){ 
      allVars <- covariateTable
      
    } else{
      allVars <- do.call(rbind, covariateTable)
    }
    
    # adding age with 0 coef to make sure everyone gets value
    allVars <- rbind(allVars, data.frame(modelCovariateId=-1, covariateId=1002))
    
    # insert the model table
    colnames(allVars) <- SqlRender::camelCaseToSnakeCase(colnames(allVars))
    DatabaseConnector::insertTable(connection, tableName='covariate_table', data = allVars, tempTable = T)
    
    
    # =================================
    #==================================
    #  RUN GETPLPDATA (BUT WITHOUT REMOVING THE COVARIATE TABLE)
    covSettings <- covariateSettings$covariateSettings
    covSettings$DemographicsAge <- T #making sure to include variable everyone has but setting value to zero 
    
    covSettings$includedCovariateIds <-  as.double(unique(allVars$covariate_id)) #[unique(allVars$covariate_id)!='1002'])
    covariates <- getExistingmodelsCovariateData(connection = connection,
                                             oracleTempSchema = oracleTempSchema,
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             cohortTable = "#cohort_person",
                                             cdmVersion = cdmVersion,
                                             rowIdField = "row_id",
                                             covariateSettings = covSettings,
                                             analysisId = covariateSettings$analysisId,
                                             type=covariateSettings$type)
    
    # clean the model_table... [TODO]
    covariateRef <- data.frame(covariateId = 1000*(1:length(covariateSettings$modelNames))+covariateSettings$analysisId,
                               covariateName = covariateSettings$modelNames,
                               analysisId = rep(covariateSettings$analysisId,length(covariateSettings$modelNames)),
                               conceptId = rep(0, length(covariateSettings$modelNames)))
    covariateRef <- ff::as.ffdf(covariateRef)
    # Construct analysis reference:
    analysisRef <- data.frame(analysisId = covariateSettings$analysisId,
                              analysisName = "Prediction Models",
                              domainId = "Demographics",
                              startDay = 0,
                              endDay = 0,
                              isBinary = "N",
                              missingMeansZero = "N")
    analysisRef <- ff::as.ffdf(analysisRef)
    # Construct analysis reference:
    #metaData <- list(sql = sql, call = match.call())
    metaData <- list()
    result <- list(covariates = covariates,
                   covariateRef = covariateRef,
                   analysisRef = analysisRef,
                   metaData = metaData)
    class(result) <- "covariateData"
    
    return(result)
  }
  
  if(asFunctions==T){
    return(list(createExistingmodelsCovariateSettings=createCovs,
                getExistingmodelsCovariateSettings=getCovs))
  }
  
  assign(paste0('createExistingmodelsCovariateSettings'), createCovs,envir = e)
  assign(paste0('getExistingmodelsCovariateSettings'), getCovs,envir = e)
  return(T)
}



getExistingmodelsCovariateData <- function(connection,
                                               oracleTempSchema = NULL,
                                               cdmDatabaseSchema,
                                               cohortTable = "#cohort_person",
                                               cohortId = -1,
                                               cdmVersion = "5",
                                               rowIdField = "subject_id",
                                               covariateSettings,
                                               aggregated = FALSE,
                                               analysisId=112,
                                               type='logistic') {
  if (!is(covariateSettings, "covariateSettings")) {
    stop("Covariate settings object not of type covariateSettings")
  }
  if (cdmVersion == "4") {
    stop("Common Data Model version 4 is not supported")
  }
  
  FeatureExtraction::getDbDefaultCovariateData(connection = connection,
                            oracleTempSchema = oracleTempSchema,
                            cdmDatabaseSchema = cdmDatabaseSchema,
                            cohortTable = cohortTable,
                            cohortId = cohortId,
                            rowIdField = rowIdField,
                            covariateSettings = covariateSettings,
                            targetCovariateTable = "#cov_temp",
                            aggregated = aggregated)
  
  # =================================
  #==================================
  #  CALCULATE THE RISK
  writeLines("Calculating the risk scores...")
  
  if(type=='logistic'){
    # now sum the temp_covariates table t get val and then 1/(1+exp(-val))
    sql <- "select row_id, model_id*1000+@analysis_id as covariate_id, 
    1/(1+exp(-1.0*(max(intercept_value)+sum(covariate_value*coefficient_value)))) covariate_value
    from 
    
    (select #cov_temp.row_id, #model_table.model_id, #model_table.model_covariate_id,
    max(#model_table.coefficient_value) as coefficient_value, max(#cov_temp.covariate_value) as covariate_value, 
    max(isnull(#intercepts.intercept_value,0)) as intercept_value
    from
    #cov_temp inner join #covariate_table on #cov_temp.covariate_id=#covariate_table.covariate_id
    inner join #model_table on #covariate_table.model_covariate_id=#model_table.model_covariate_id
    left outer join #intercepts on #intercepts.model_id=#model_table.model_id 
    group by #cov_temp.row_id, #model_table.model_id, #model_table.model_covariate_id) temp 
    
    group by row_id,  model_id"}else{
      sql <- "select row_id, model_id*1000+@analysis_id as covariate_id, 
      (max(intercept_value)+sum(covariate_value*coefficient_value)) covariate_value
      from 
      
      (select #cov_temp.row_id, #model_table.model_id, #model_table.model_covariate_id,
      max(#model_table.coefficient_value) as coefficient_value, max(#cov_temp.covariate_value) as covariate_value, 
      max(isnull(#intercepts.intercept_value,0)) as intercept_value
      from
      #cov_temp inner join #covariate_table on #cov_temp.covariate_id=#covariate_table.covariate_id
      inner join #model_table on #covariate_table.model_covariate_id=#model_table.model_covariate_id
      left outer join #intercepts on #intercepts.model_id=#model_table.model_id 
      group by #cov_temp.row_id, #model_table.model_id, #model_table.model_covariate_id) temp 
      
      group by row_id,  model_id" 
  }
  
  sql <- SqlRender::renderSql(sql, 
                              analysis_id = analysisId
  )$sql
  sql <- SqlRender::translateSql(sql, targetDialect ='pdw')$sql
  
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql.ffdf(connection, sql)
  # Convert colum names to camelCase:
  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))
  
  # Drop temp tables
  #sql <- SqlRender::translateSql(sql = todo$sqlCleanup,
  #                               targetDialect = attr(connection, "dbms"),
  #                               oracleTempSchema = oracleTempSchema)$sql
  #DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)
  
  
  return(covariates)
  }

getExistingmodelsCovariateData_old <- function(connection,
                                           oracleTempSchema = NULL,
                                           cdmDatabaseSchema,
                                           cohortTable = "#cohort_person",
                                           cohortId = -1,
                                           cdmVersion = "5",
                                           rowIdField = "subject_id",
                                           covariateSettings,
                                           aggregated = FALSE,
                                           analysisId=112,
                                           type='logistic') {
  if (!is(covariateSettings, "covariateSettings")) {
    stop("Covariate settings object not of type covariateSettings")
  }
  if (cdmVersion == "4") {
    stop("Common Data Model version 4 is not supported")
  }
  settings <- FeatureExtraction:::.toJson(covariateSettings)
  rJava::J("org.ohdsi.featureExtraction.FeatureExtraction")$init(system.file("", package = "FeatureExtraction"))
  json <- rJava::J("org.ohdsi.featureExtraction.FeatureExtraction")$createSql(settings, aggregated, cohortTable, rowIdField, as.integer(cohortId), cdmDatabaseSchema)
  todo <- FeatureExtraction:::.fromJson(json)
  if (length(todo$tempTables) != 0) {
    writeLines("Sending temp tables to server")
    for (i in 1:length(todo$tempTables)) {
      DatabaseConnector::insertTable(connection,
                                     tableName = names(todo$tempTables)[i],
                                     data = as.data.frame(todo$tempTables[[i]]),
                                     dropTableIfExists = TRUE,
                                     createTable = TRUE,
                                     tempTable = TRUE,
                                     oracleTempSchema = oracleTempSchema)
    }
  }
  
  writeLines("Constructing features on server")
  
  sql <- SqlRender::translateSql(sql = todo$sqlConstruction,
                                 targetDialect = attr(connection, "dbms"),
                                 oracleTempSchema = oracleTempSchema)$sql
  profile <- (!is.null(getOption("dbProfile")) && getOption("dbProfile") == TRUE)
  DatabaseConnector::executeSql(connection, sql, profile = profile)
  
  
  # edited to create table on server rather than dowload...
  todo$sqlQueryFeatures <- gsub('FROM \\(','into #cov_temp FROM \\(',todo$sqlQueryFeatures)
  if (!is.null(todo$sqlQueryFeatures)) {
    sql <- SqlRender::translateSql(sql = todo$sqlQueryFeatures,
                                   targetDialect = attr(connection, "dbms"),
                                   oracleTempSchema = oracleTempSchema)$sql
    DatabaseConnector::executeSql(connection, sql)
  }
  
  # =================================
  #==================================
  #  CALCULATE THE RISK
  writeLines("Calculating the risk scores...")
  
  if(type=='logistic'){
  # now sum the temp_covariates table t get val and then 1/(1+exp(-val))
  sql <- "select row_id, model_id*1000+@analysis_id as covariate_id, 
  1/(1+exp(-1.0*(max(intercept_value)+sum(covariate_value*coefficient_value)))) covariate_value
  from 
  
  (select #cov_temp.row_id, #model_table.model_id, #model_table.model_covariate_id,
  max(#model_table.coefficient_value) as coefficient_value, max(#cov_temp.covariate_value) as covariate_value, 
  max(isnull(#intercepts.intercept_value,0)) as intercept_value
  from
  #cov_temp inner join #covariate_table on #cov_temp.covariate_id=#covariate_table.covariate_id
  inner join #model_table on #covariate_table.model_covariate_id=#model_table.model_covariate_id
  left outer join #intercepts on #intercepts.model_id=#model_table.model_id 
  group by #cov_temp.row_id, #model_table.model_id, #model_table.model_covariate_id) temp 
  
  group by row_id,  model_id"}else{
    sql <- "select row_id, model_id*1000+@analysis_id as covariate_id, 
  (max(intercept_value)+sum(covariate_value*coefficient_value)) covariate_value
    from 
    
    (select #cov_temp.row_id, #model_table.model_id, #model_table.model_covariate_id,
    max(#model_table.coefficient_value) as coefficient_value, max(#cov_temp.covariate_value) as covariate_value, 
    max(isnull(#intercepts.intercept_value,0)) as intercept_value
    from
    #cov_temp inner join #covariate_table on #cov_temp.covariate_id=#covariate_table.covariate_id
    inner join #model_table on #covariate_table.model_covariate_id=#model_table.model_covariate_id
    left outer join #intercepts on #intercepts.model_id=#model_table.model_id 
    group by #cov_temp.row_id, #model_table.model_id, #model_table.model_covariate_id) temp 
    
    group by row_id,  model_id" 
  }
  
  sql <- SqlRender::renderSql(sql, 
                              analysis_id = analysisId
  )$sql
  sql <- SqlRender::translateSql(sql, targetDialect ='pdw')$sql
  
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql.ffdf(connection, sql)
  # Convert colum names to camelCase:
  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))

  # Drop temp tables
  sql <- SqlRender::translateSql(sql = todo$sqlCleanup,
                                 targetDialect = attr(connection, "dbms"),
                                 oracleTempSchema = oracleTempSchema)$sql
  DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)
  
  
  return(covariates)
}


#' Convert matrix into plpData
#'
#' @description
#' Converts a matrix (rows = people, columns=variables) into the standard plpData
#'
#' @details
#' This function converts matrix into plpData
#' 
#' @param data                          An data.frame or matrix.
#' @param columnInfo                    A dataframe with three columns, column 1 contains columnId, column 2 contains columnName for each column id and column 3 contains the columnTime - the time prior to index the variable was recorded  
#' @param outcomeId                     The column id containing the outcome
#' @param outcomeThreshold              The outcome value must be higher or equal to this for the person to have the outcome
#' @param indexTime                     The time defining the index date
#' @param includeIndexDay               Boolean - whether to include variables recorded on index date
#' @examples
#' #TODO
#'
#' @return
#' Returns an object of class plpData
#' @export
toPlpData <- function(data, columnInfo, outcomeId, outcomeThreshold=0.5,
                      indexTime =0, includeIndexDay=T ){
  
  if(!class(data)%in%c("data.frame","matrix"))
    stop('data needs to be matrix of data.frame')
  
  if(nrow(columnInfo)!=ncol(data))
    stop('Column Names missing')
  
  if(missing(outcomeId))
    stop('outcomeId not entered')
  
  plpData <- list()
  
  # first order the column info by columnId
  columnInfo <- columnInfo[order(columnInfo[,'columnId']), ]
  
  meltData <- function(i, data, columnInfo, includeIndexDay,indexTime){
    ind <- columnInfo[,'columnTime']<= (indexTime + ifelse(includeIndexDay,0,-1))
    
    return(data.frame(rowId=rep(i, sum(ind)), 
                      covariateId=columnInfo[ind, 'columnId'],
                      covariateValue = unlist(c(data[i,ind]))))
  } 
  
  melted <- sapply(1:nrow(data), function(x) meltData(x, data, 
                                                      columnInfo, 
                                                      includeIndexDay, indexTime ), simplify = F )
  melted <- do.call(rbind, melted)
  
  #remove outcomeId
  melted <- melted[melted$covariateId!=outcomeId,]
  
  plpData$covariates <- ff::as.ffdf(melted)
  rownames(plpData$covariates) <- NULL
  plpData$covariateRef <-   ff::as.ffdf(data.frame(covariateId = columnInfo[,'columnId'], 
                                                   covariateName = columnInfo[,'columnName'], 
                                                   analysisId = rep(0, nrow(columnInfo)), 
                                                   conceptId = rep(0, nrow(columnInfo))
  ))
  
  subjectId <- 1:nrow(data)
  if(!is.null(rownames(data))){
    subjectId <- rownames(data)
  }
  
  plpData$cohorts <- data.frame(rowId = 1:nrow(data), 
                                subjectId = subjectId,
                                cohortId = rep(1, nrow(data)),
                                cohortStartDate = indexTime ,
                                daysFromObsStart = rep(9999, nrow(data)) ,
                                daysToCohortEnd = rep(9999, nrow(data)) ,
                                daysToObsEnd = rep(9999, nrow(data)) )
  
  attr(plpData$cohorts, "metaData") <- list(cohortId=1,
                                            outcomeIds=2)
  
  plpData$outcomes <-  data.frame(rowId = which(data[,outcomeId]>=outcomeThreshold),
                                  outcomeId = rep(2, length(which(data[,outcomeId]>=outcomeThreshold))), 
                                  daysToEvent = rep( columnInfo$columnTime[columnInfo$columnId==outcomeId]-indexTime,
                                                     length(which(data[,outcomeId]>=outcomeThreshold)))
  )
  
  plpData$metaData <- list(columnInfo=columnInfo, 
                           cohortId=1,
                           outcomeId=2, 
                           indexTime =indexTime, 
                           includeIndexDay=includeIndexDay, 
                           call=list(1,1,1,1,1,1,1,1,1, 
                                     cdmDatabaseSchema=1,
                                     studyStartDate=1,
                                     studyEndDate=1))
  
  class(plpData) <- 'plpData'
  
  return(plpData) 
}


#' Create a dataframe with the summary details of the population cohort for publications
#'
#' @details
#' This function is used to create a summary table for population to be inserted into publications
#'
#' @param plpData        An object returned by running \code{getPlpData}.
#' @param population     The population you want the summary table for
#' @param connectionDetails  The connection details used to connect to the CDM database
#' @param cohortTable     The name of the temp table that will store the popualtion cohort             
#'
#' @examples
#' \dontrun{
#' getTable1 (plpData, population, connectionDetails)
#' }
#' @export
#' 
getPlpTable <- function(plpData, population, connectionDetails,
                      cohortTable='#temp_person'){
  if(is.missing(plpData))
    stop('Need to enter plpData')
  if(is.missing(population))
    stop('Need to enter population')
  if(is.missing(connectionDetails))
    stop('Need to enter connectionDetails')
  
  if(class(plpData)!='plpData')
    stop('Wrong class for plpData')
  if(class(population)!='data.frame')
    stop('wrong population class')
  if(sum(c('cohortId','subjectId','cohortStartDate')%in%colnames(population))!=3)  # need to remember column names
    stop('population missing required column')
  
  # add population to database in cohort table format
  connection <- DatabaseConnector::connect(connectionDetails)
  #insert pop table into '#temp_person'
  popCohort <- population[,c('cohortId','subjectId','cohortStartDate','cohortStartDate')]
  colnames(popCohort)[4] <- 'cohortEndDate'
  DatabaseConnector::insertTable(connection = connection,
                                 tableName = cohortTable, 
                                 data = popCohort, 
                                 tempTable = T)
  
  settings <- plpData$metaData$call
  settings[[1]] <- NULL # remove function
  settings$connection <- connection
  settings$cohortTableIsTemp <- T
  class(settings) <- 'list'
  for(val in c("outcomeIds","studyStartDate","studyEndDate",
               "outcomeDatabaseSchema", "outcomeTable","firstExposureOnly",
               "washoutPeriod","sampleSize", "connectionDetails")){
    ind <- which(names(settings)==val)
    settings[[val]] <- NULL
  }
  settings$aggregated <- T
  # need to update below with the population cohort...
  settings$cohortTable <- cohortTable
  settings$cohortId <- -1
  covariateData <- do.call(FeatureExtraction::getDbCovariateData, settings)
  
  #FeatureExtraction::getDbCovariateData()
  tab1 <- FeatureExtraction::createTable1(covariateData1 = covariateData)
  
  return(tab1)
}