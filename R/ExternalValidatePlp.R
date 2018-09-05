# @file ExternalValidatePlp.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
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

#' externalValidatePlp - Validate a model on new databases
#'
#' @description
#' This function extracts data using a user specified connection and cdm_schema, applied the model and then calcualtes the performance
#' @details
#' Users need to input a trained model (the output of runPlp()) and new database connections. The function will return a list of length equal to the 
#' number of cdm_schemas input with the performance on the new data
#' 
#' @param plpResult                        The object returned by runPlp() containing the trained model
#' @param connectionDetails                The connection details for extracting the new data 
#' @param validationSchemaTarget         A string or list of strings specifying the database containing the target cohorts
#' @param validationSchemaOutcome       A string or list of strings specifying the database containing the outcome cohorts
#' @param validationSchemaCdm            A string or list of strings specifying the database containing the cdm
#' @param databaseNames                  A string of lift of strings specifying sharing friendly database names corresponding to validationSchemaCdm
#' @param validationTableTarget          A string or list of strings specifying the table containing the target cohorts
#' @param validationTableOutcome        A string or list of strings specifying the table containing the outcome cohorts
#' @param validationIdTarget             An iteger or list of integers specifying the cohort id for the target cohorts
#' @param validationIdOutcome           An iteger or list of integers specifying the cohort id for the outcome cohorts
#' @param oracleTempSchema                 The temp oracle schema requires read/write 
#' @param verbosity                        Sets the level of the verbosity. If the log level is at or higher in priority than the logger threshold, a message will print. The levels are:
#'                                         \itemize{
#'                                         \item{DEBUG}{Highest verbosity showing all debug statements}
#'                                         \item{TRACE}{Showing information about start and end of steps}
#'                                         \item{INFO}{Show informative information (Default)}
#'                                         \item{WARN}{Show warning messages}
#'                                         \item{ERROR}{Show error messages}
#'                                         \item{FATAL}{Be silent except for fatal errors}
#'                                         }
#' @param keepPrediction                   Whether to keep the predicitons for the new data                                         
#' @return
#' A list containing the performance for each validation_schema 
#'
#'
#' @export
externalValidatePlp <- function(plpResult,
                                connectionDetails, 
                                validationSchemaTarget,
                                validationSchemaOutcome, 
                                validationSchemaCdm, databaseNames,
                                validationTableTarget='cohort', validationTableOutcome='cohort',
                                validationIdTarget = NULL, validationIdOutcome = NULL,
                                oracleTempSchema=NULL,#validationSchemaCdm,
                                verbosity="INFO", keepPrediction=F){
  
  # TODO:: ADD LOGGING, MORE INOUT TESTS, ADD TEST CASE IN PACKAGE... 
  if(missing(plpResult))
    stop('Need to input a plpResult')
  if(class(plpResult)!="runPlp")
    stop('Need to input a plpResult of class runPlp')
  
  if(missing(connectionDetails))
    stop('Need to enter connection details')
  
  if(missing(validationSchemaTarget))
    stop('Need to enter validationSchemaTarget ')
  if(missing(validationSchemaOutcome))
    stop('Need to enter validationSchemaOutcome ')
  if(missing(validationSchemaCdm))
    stop('Need to enter validationSchemaCdm ')
  
  if(class(validationSchemaTarget)=='list'){
    if(class(validationSchemaOutcome)!='list')
      stop('If target schema is list outcomes must be list...')
    if(class(validationSchemaCdm)!='list')
      stop('If target schema is list cdm must be list...')    
    
    if(length(validationSchemaTarget)!=length(validationSchemaOutcome))
      stop('Length of database schemas not matching...')
    if(length(validationSchemaTarget)!=length(validationSchemaCdm))
      stop('Length of database schemas not matching...')
  }
  
  if(class(validationSchemaTarget)!=class(validationSchemaOutcome))
    stop('validationSchemaTarget and validationSchemaOutcome not same class')
  if(class(validationSchemaTarget)!=class(validationSchemaCdm))
    stop('validationSchemaTarget and validationSchemaCdm not same class')
  
  if(!missing(databaseNames)){
    if(length(validationSchemaCdm)!=length(databaseNames)){
      stop('DatabaseNames not same length as validationSchemaCdm')
    }
  }
  
  
  # add lots of test for tables and ids :(
  
  
  if(is.null(validationIdTarget))
    validationIdTarget <- plpResult$inputSetting$populationSettings$cohortId# set the model ids
  if(is.null(validationIdOutcome))
    validationIdOutcome <- plpResult$inputSetting$populationSettings$outcomeId# set the model ids
  
  
  results <- list()
  length(results) <- length(validationSchemaCdm)
  for(i in 1:length(validationSchemaCdm)){
    # Now extract the data:
    targetTable <- validationTableTarget
    outcomeTable <- validationTableOutcome
    
    if(length(validationTableTarget)>1)
      targetTable <- validationTableTarget[i]
    if(length(validationTableOutcome)>1)
      outcomeTable <- validationTableOutcome[i]
    newData <- PatientLevelPrediction::similarPlpData(plpModel= plpResult$model, createCohorts = F, 
                                                      newConnectionDetails = connectionDetails, 
                                                      newCdmDatabaseSchema = validationSchemaCdm[[i]], 
                                                      newCohortDatabaseSchema = validationSchemaTarget[[i]], 
                                                      newCohortTable = targetTable, 
                                                      newCohortId = validationIdTarget, 
                                                      newOutcomeDatabaseSchema = validationSchemaOutcome[[i]], 
                                                      newOutcomeTable = outcomeTable, 
                                                      newOutcomeId = validationIdOutcome, 
                                                      newOracleTempSchema = oracleTempSchema,
                                                      sample = NULL, 
                                                      createPopulation = T )
    
    if(sum(newData$population$outcomeCount>0)<20){
      warning('Outcome count is less than 20... external validation may be inaccurate')
    }
    if(sum(newData$population$outcomeCount>0)<5){
      warning('Outcome count is less than 5... external validation stopped')
      results[[i]] <- 'not run due to outcome count less than 5'
    } else{
      
      results[[i]] <- PatientLevelPrediction::applyModel(population=newData$population, plpData = newData$plpData, 
                                                         calculatePerformance = T, plpModel = plpResult$model)
      
      
      if(!keepPrediction){
        results[[i]]$prediction <- NULL
      }
      
      if(missing(databaseNames)){
        niceName <-   rep('Not Entered', length(validationSchemaCdm))
      } else{
        niceName <-   databaseNames
      }
      results[[i]]$inputSetting<- list(newCdmDatabaseSchema = validationSchemaCdm[[i]],
                                       databaseNames = niceName[i], #added [i]
                                    newCohortDatabaseSchema = validationSchemaTarget[[i]], 
                                    newCohortTable = targetTable, 
                                    cohortId = validationIdTarget, 
                                    newOutcomeDatabaseSchema = validationSchemaOutcome[[i]], 
                                    newOutcomeTable = outcomeTable, 
                                    outcomeId = validationIdOutcome,
                                    newOracleTempSchema = oracleTempSchema,
                                    # added the below
                                    populationSettings = plpResult$inputSetting$populationSettings,
                                    dataExtrractionSettings = list(covariateSettings = plpResult$inputSetting$dataExtrractionSettings$covariateSettings)
                                    )
      
      results[[i]]$executionSummary <- list(PackageVersion = list(rVersion= R.Version()$version.string,
                                                                  packageVersion = utils::packageVersion("PatientLevelPrediction")),
                                            PlatformDetails= list(platform= R.Version()$platform,
                                                                  cores= Sys.getenv('NUMBER_OF_PROCESSORS'),
                                                                  RAM=utils::memory.size()), #  test for non-windows needed
                                            # Sys.info()
                                            TotalExecutionElapsedTime = NULL,
                                            ExecutionDateTime = Sys.Date())
      
    }
    
  }
  
  if(!missing(databaseNames)){
    names(results) <- databaseNames
    # do summary
    summary <- do.call(rbind, lapply(1:length(results), function(i) summariseVal(results[[i]], 
                                                                                 database=databaseNames[[i]])))
  } else{
    names(results) <- validationSchemaCdm # do I want to paste ids onto this?
    # do summary
    summary <- do.call(rbind, lapply(1:length(results), function(i) summariseVal(results[[i]], 
                                                                                 database=validationSchemaCdm[[i]])))
  }
  
  
  summary <- reshape2::dcast(summary, Database ~ Metric, value.var="Value" )
  
  
  result <- list(summary=summary,
                 validation=results)
  
  class(result) <- 'validatePlp'
  
  # Now return results
  return(result)
}




summariseVal <- function(result, database){
  row.names(result$performanceEvaluation$evaluationStatistics) <- NULL
  result <- as.data.frame(result$performanceEvaluation$evaluationStatistics)
  result$performanceEvaluation$evaluationStatistics$Metric <- gsub('.auc','',result$performanceEvaluation$evaluationStatistics$Metric)
  result$Database <- database
  return(result)
}


#' evaluateExistingModel
#'
#' @description
#' This function implements an existing model
#' @details
#' Implements an existing model and evaluates its performance
#' 
#' @param modelTable                       The model covariates and scores
#' @param covariateTable                   The mapping from model covariates to standard covariates
#' @param interceptTable                   The model intercepts
#' @param type                             Model type (score or logistic)
#' @param covariateSettings                The standard covariate settings (specify covariate lookback time)
#' @param customCovariates                 A table of covariateId, sql (sql creates the custom covariate)
#' @param riskWindowStart                  The day after index to start predicting the outcome
#' @param addExposureDaysToEnd             riskWindomEnd relative to the cohort end date instead of the cohort start date?
#' @param riskWindowEnd                    The day after index to stop predicting the outcome
#' @param requireTimeAtRisk                Do you want to ignore people who leave the database some point between the riskWindowStart and riskWindowEnd 
#' @param minTimeAtRisk                    If requireTimeAtRisk is TRUE, how many days must they be observed before leaving to get included (default recommendation is all risk period: riskWindowEnd-riskWindowStart)    
#' @param includeAllOutcomes               Setting this to TRUE means people with the outcome who leave the data during the risk period are still included, so only non-outcome people who leave during the risk period are removed 
#' @param removeSubjectsWithPriorOutcome   Remove people from the target population if they have the outcome prior to target cohort start date
#' @param connectionDetails                The details to connect to the CDM
#' @param cohortDatabaseSchema             A string specifying the database containing the target cohorts
#' @param outcomeDatabaseSchema            A string specifying the database containing the outcome cohorts
#' @param cdmDatabaseSchema                A string specifying the database containing the cdm
#' @param cohortTable                      A string specifying the table containing the target cohorts
#' @param outcomeTable                     A string specifying the table containing the outcome cohorts
#' @param cohortId                         An iteger specifying the cohort id for the target cohorts
#' @param outcomeId                        An iteger specifying the cohort id for the outcome cohorts
#' @param oracleTempSchema                 The temp oracle schema 
#' @param modelName                        The name of the model
#' @param calibrationPopulation            A data.frame of subjectId, cohortStartDate, indexes used to recalibrate the model on new data
#' 
#' @return
#' The performance of the existing model and prediction
#'
#'
#' @export
evaluateExistingModel <- function(modelTable, 
                                   covariateTable, 
                                   interceptTable=NULL, 
                                   type='score',
                                   covariateSettings,
                                  customCovariates=NULL,
                                  riskWindowStart = 1, 
                                  addExposureDaysToEnd = F,
                                  riskWindowEnd = 365,
                                   requireTimeAtRisk = T, 
                                  minTimeAtRisk = 364,
                                  includeAllOutcomes = T,
                                  removeSubjectsWithPriorOutcome=T,
                                   connectionDetails,
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema, cohortTable, cohortId,
                                   outcomeDatabaseSchema, outcomeTable, outcomeId,
                                   oracleTempSchema = cdmDatabaseSchema,
                                  modelName='existingModel',
                                  calibrationPopulation=NULL
                                
                                   ){
  
  #input tests
  if(missing(modelTable))
    stop('Need to input modelTable')
  if(sum(colnames(modelTable)%in%c('modelId','modelCovariateId','coefficientValue'))!=3)
    stop('Missing required column in modelTable')
  
  if(missing(covariateTable))
    stop('Need to input covariateTable')
  if(sum(colnames(covariateTable)%in%c('modelCovariateId','covariateId'))!=2)
    stop('Missing required column in covariateTable')
  
  if(!is.null(interceptTable)){
    if(sum(colnames(interceptTable)%in%c('modelId','interceptValue'))!=2)
      stop('Missing required column in interceptTable')
  }
  
  if(is.null(interceptTable))
    interceptTable <- data.frame(modelId=1, interceptValue=0)
  
  if(!type%in%c('score','logistic'))
    stop('Incorrect type specified')
  
  if(missing(connectionDetails))
    stop('Need to enter connection details')
  
  if(missing(covariateSettings))
    stop('Need to enter covariates settings')
  
  if(class(covariateSettings)!='covariateSettings')
    stop('Incorrect covariateSettings class')
  
  if(riskWindowStart < 0)
    warning('riskWindowStart is negative - is this correct?')
  
  if(riskWindowEnd < 0)
    warning('riskWindowEnd is negative - is this correct?')
  
  if(riskWindowEnd < riskWindowStart)
    warning('riskWindowEnd is less than the Start - is this correct?')
  
  if(minTimeAtRisk > riskWindowEnd - riskWindowStart)
    warning('minTimeAtRisk is greater than time at risk - is this correct?')
  
  if(!is.null(calibrationPopulation)){
    if(sum(c('subjectId','cohortStartDate','indexes')%in%colnames(calibrationPopulation))!=3){
      stop("Need 'subjectId','cohortStartDate','indexes' in data.frame")
    }
    calibrationPopulation <- calibrationPopulation[,c('subjectId','cohortStartDate','indexes')]
  }
  
  custCovs <- PatientLevelPrediction::createExistingModelSql(modelTable= modelTable, 
                                                 modelNames = modelName, 
                                                 interceptTable = interceptTable,
                                                 covariateTable=covariateTable, 
                                                 type='score',
                                                 analysisId=919, 
                                                 covariateSettings=covariateSettings, 
                                                 customCovariates=customCovariates,
                                                 asFunctions=T)
  
  createExistingmodelsCovariateSettings <- custCovs$createExistingmodelsCovariateSettings
  getExistingmodelsCovariateSettings <- custCovs$getExistingmodelsCovariateSettings
  assign(paste0('getExistingmodelsCovariateSettings'), custCovs$getExistingmodelsCovariateSettings
         ,envir = globalenv())
  
  plpData <- PatientLevelPrediction::getPlpData(connectionDetails, 
                                                cdmDatabaseSchema = cdmDatabaseSchema,
                                                oracleTempSchema=oracleTempSchema,
                                                cohortId = cohortId ,
                                                outcomeIds = outcomeId, 
                                                cohortDatabaseSchema = cohortDatabaseSchema, 
                                                cohortTable =  cohortTable , 
                                                outcomeDatabaseSchema = outcomeDatabaseSchema, 
                                                outcomeTable = outcomeTable , 
                                                covariateSettings =  createExistingmodelsCovariateSettings(),
                                                sampleSize = NULL)
  
  population <- PatientLevelPrediction::createStudyPopulation(plpData = plpData, outcomeId = outcomeId,
                                                              includeAllOutcomes = includeAllOutcomes, 
                                                              requireTimeAtRisk = requireTimeAtRisk, 
                                                              minTimeAtRisk = minTimeAtRisk, 
                                                              riskWindowStart = riskWindowStart,
                                                              addExposureDaysToEnd = addExposureDaysToEnd,
                                                              riskWindowEnd = riskWindowEnd, 
                                                              removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome
                                                              )
  prediction <- merge(population, ff::as.ram(plpData$covariates), by='rowId', all.x=T)
  
  if(!is.null(calibrationPopulation)){
    #re-calibrate model:
    prediction <- base::merge(calibrationPopulation, prediction, by=c('subjectId','cohortStartDate'))
    recalModel <- stats::glm(y ~ x,
                             family=stats::binomial(link='logit'),
                             data=data.frame(x=prediction$covariateValue, 
                                             y=as.factor(prediction$outcomeCount))[prediction$indexes>0,])
    value <- stats::predict(recalModel, data.frame(x=prediction$covariateValue),
                            type = "response")
    prediction$value <- value
  } else {
    colnames(prediction)[colnames(prediction)=='covariateValue'] <- 'value'
    prediction$value <- prediction$value/max(prediction$value)
  }
  
  attr(prediction, "metaData")$predictionType <- "binary"
  performance <- PatientLevelPrediction::evaluatePlp(prediction, plpData)
  
  # reformatting the performance 
  analysisId <-   '000000'
  nr1 <- length(unlist(performance$evaluationStatistics[-1]))
  performance$evaluationStatistics <- cbind(analysisId= rep(analysisId,nr1),
                                            Eval=rep('validation', nr1),
                                            Metric = names(unlist(performance$evaluationStatistics[-1])),
                                            Value = unlist(performance$evaluationStatistics[-1])
  )
  nr1 <- nrow(performance$thresholdSummary)
  performance$thresholdSummary <- cbind(analysisId=rep(analysisId,nr1),
                                        Eval=rep('validation', nr1),
                                        performance$thresholdSummary)
  nr1 <- nrow(performance$demographicSummary)
  if(!is.null(performance$demographicSummary)){
    performance$demographicSummary <- cbind(analysisId=rep(analysisId,nr1),
                                            Eval=rep('validation', nr1),
                                            performance$demographicSummary)
  }
  nr1 <- nrow(performance$calibrationSummary)
  performance$calibrationSummary <- cbind(analysisId=rep(analysisId,nr1),
                                          Eval=rep('validation', nr1),
                                          performance$calibrationSummary)
  nr1 <- nrow(performance$predictionDistribution)
  performance$predictionDistribution <- cbind(analysisId=rep(analysisId,nr1),
                                              Eval=rep('validation', nr1),
                                              performance$predictionDistribution)
  
  executionSummary <- list(PackageVersion = list(rVersion= R.Version()$version.string,
                                                 packageVersion = utils::packageVersion("PatientLevelPrediction")),
                           PlatformDetails= list(platform= R.Version()$platform,
                                                 cores= Sys.getenv('NUMBER_OF_PROCESSORS'),
                                                 RAM=utils::memory.size()), #  test for non-windows needed
                           # Sys.info()
                           TotalExecutionElapsedTime = NULL,
                           ExecutionDateTime = Sys.Date())
  
  return(list(performanceEvaluation=performance, 
              prediction=prediction,
              inputSetting = list(outcomeId=outcomeId, 
                                  cohortId=cohortId,
                                  database = cdmDatabaseSchema),
              executionSummary = executionSummary,
              model = list(model='existing model',
                           modelName=modelName,
                           modelTable=modelTable, 
                           covariateTable=covariateTable, 
                           interceptTable=interceptTable),
              analysisRef=list(analysisId=NULL,
                               analysisName=NULL,
                               analysisSettings= NULL),
              covariateSummary = NULL
              ))
}