# @file externalValidatePlp.R
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
#' @param validation_schema_target         A string or list of strings specifying the database containing the target cohorts
#' @param validation_schema_outcome       A string or list of strings specifying the database containing the outcome cohorts
#' @param validation_schema_cdm            A string or list of strings specifying the database containing the cdm
#' @param validation_table_target          A string or list of strings specifying the table containing the target cohorts
#' @param validation_table_outcome        A string or list of strings specifying the table containing the outcome cohorts
#' @param validation_id_target             An iteger or list of integers specifying the cohort id for the target cohorts
#' @param validation_id_outcome           An iteger or list of integers specifying the cohort id for the outcome cohorts
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
                                validation_schema_target,
                                validation_schema_outcome, validation_schema_cdm,
                                validation_table_target='cohort', validation_table_outcome='cohort',
                                validation_id_target = NULL, validation_id_outcome = NULL,
                                verbosity=futile.logger::INFO, keepPrediction=F){
  
  # TODO:: ADD LOGGING, MORE INOUT TESTS, ADD TEST CASE IN PACKAGE... 
  if(missing(plpResult))
    stop('Need to input a plpResult')
  if(class(plpResult)!="runPlp")
    stop('Need to input a plpResult of class runPlp')
  
  if(missing(connectionDetails))
    stop('Need to enter connection details')
  
  if(missing(validation_schema_target))
    stop('Need to enter alidation_schema_target ')
  if(missing(validation_schema_outcome))
    stop('Need to enter alidation_schema_outcome ')
  if(missing(validation_schema_cdm))
    stop('Need to enter alidation_schema_cdm ')
  
  if(class(validation_schema_target)=='list'){
    if(class(validation_schema_outcome)!='list')
      stop('If target schema is list outcomes must be list...')
    if(class(validation_schema_cdm)!='list')
      stop('If target schema is list cdm must be list...')    
    
    if(length(validation_schema_target)!=length(validation_schema_outcome))
      stop('Length of database schemas not matching...')
    if(length(validation_schema_target)!=length(validation_schema_cdm))
      stop('Length of database schemas not matching...')
  }
  
  if(class(validation_schema_target)!=class(validation_schema_outcome))
    stop('validation_schema_target and validation_schema_outcome not same class')
  if(class(validation_schema_target)!=class(validation_schema_cdm))
    stop('validation_schema_target and validation_schema_cdm not same class')
  
  
  # add lots of test for tables and ids :(
  
  
  if(is.null(validation_id_target))
    validation_id_target <- plpResult$inputSetting$populationSettings$cohortId# set the model ids
  if(is.null(validation_id_outcome))
    validation_id_outcome <- plpResult$inputSetting$populationSettings$outcomeId# set the model ids
  
  
  results <- list()
  length(results) <- length(validation_schema_cdm)
  for(i in 1:length(validation_schema_cdm)){
    # Now extract the data:
    target_table <- validation_table_target
    outcome_table <- validation_table_outcome
    
    if(length(validation_table_target)>1)
      target_table <- validation_table_target[i]
    if(length(validation_table_outcome)>1)
      outcome_table <- validation_table_outcome[i]
    newData <- PatientLevelPrediction::similarPlpData(plpModel= plpResult$model, createCohorts = F, 
                                                      newConnectionDetails = connectionDetails, 
                                                      newCdmDatabaseSchema = validation_schema_cdm[[i]], 
                                                      newCohortDatabaseSchema = validation_schema_target[[i]], 
                                                      newCohortTable = target_table, 
                                                      newCohortId = validation_id_target, 
                                                      newOutcomeDatabaseSchema = validation_schema_outcome[[i]], 
                                                      newOutcomeTable = outcome_table, 
                                                      newOutcomeId = validation_id_outcome, 
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
      
      results[[i]]$settings <- list(newCdmDatabaseSchema = validation_schema_cdm[i], 
                                    newCohortDatabaseSchema = validation_schema_target[i], 
                                    newCohortTable = target_table, 
                                    newCohortId = validation_id_target, 
                                    newOutcomeDatabaseSchema = validation_schema_outcome[i], 
                                    newOutcomeTable = outcome_table, 
                                    newOutcomeId = validation_id_outcome)
      
    }
    
  }
  
  names(results) <- validation_schema_cdm # do I want to paste ids onto this?
  
  # do summary
  summary <- do.call(rbind, lapply(1:length(results), function(i) summariseVal(results[[i]], 
                                                                    database=validation_schema_cdm[[i]])))
  summary <- reshape2::dcast(summary, Database ~ Metric, value.var="Value" )
  
  
  result <- list(summary=summary,
                 validation=results)
  
  class(result) <- 'validatePlp'
  
  # Now return results
  return(result)
}




summariseVal <- function(result, database){
  row.names(result$performance$evaluationStatistics) <- NULL
  result <- as.data.frame(result$performance$evaluationStatistics)
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
#' @param riskWindowStart                  The day after index to start predicting the outcome
#' @param riskWindowEnd                    The day after index to stop predicting the outcome
#' @param requireTimeAtRisk                Do you want to ignore people who leave the database some point between the riskWindowStart and riskWindowEnd 
#' @param minTimeAtRisk                    If requireTimeAtRisk is TRUE, how many days must they be observed before leaving to get included (default recommendation is all risk period: riskWindowEnd-riskWindowStart)    
#' @param includeAllOutcomes               Setting this to TRUE means people with the outcome who leave the data during the risk period are still included, so only non-outcome people who leave during the risk period are removed 
#' @param connectionDetails                The details to connect to the CDM
#' @param cohortDatabaseSchema             A string specifying the database containing the target cohorts
#' @param outcomeDatabaseSchema            A string specifying the database containing the outcome cohorts
#' @param cdmDatabaseSchema                A string specifying the database containing the cdm
#' @param cohortTable                      A string specifying the table containing the target cohorts
#' @param outcomeTable                     A string specifying the table containing the outcome cohorts
#' @param cohortId                         An iteger specifying the cohort id for the target cohorts
#' @param outcomeId                        An iteger specifying the cohort id for the outcome cohorts
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
                                  riskWindowStart = 1, 
                                  riskWindowEnd = 365,
                                   requireTimeAtRisk = T, 
                                  minTimeAtRisk = 364,
                                  includeAllOutcomes = T,
                                   connectionDetails,
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema, cohortTable, cohortId,
                                   outcomeDatabaseSchema, outcomeTable, outcomeId
                                
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
  
  custCovs <- PatientLevelPrediction::createExistingModelSql(modelTable= modelTable, 
                                                 modelNames ='existingModel', 
                                                 interceptTable = interceptTable,
                                                 covariateTable=covariateTable, 
                                                 type='score',
                                                 analysisId=919, 
                                                 covariateSettings=covariateSettings, 
                                                 asFunctions=T)
  
  createExistingmodelsCovariateSettings <- custCovs$createExistingmodelsCovariateSettings
  getExistingmodelsCovariateSettings <- custCovs$getExistingmodelsCovariateSettings
  assign(paste0('getExistingmodelsCovariateSettings'), custCovs$getExistingmodelsCovariateSettings
         ,envir = globalenv())
  
  plpData <- PatientLevelPrediction::getPlpData(connectionDetails, 
                                                cdmDatabaseSchema = cdmDatabaseSchema,
                                                cohortId = cohortId ,
                                                outcomeIds = outcomeId, 
                                                cohortDatabaseSchema = cohortDatabaseSchema, 
                                                cohortTable =  cohortTable , 
                                                outcomeDatabaseSchema = outcomeDatabaseSchema, 
                                                outcomeTable = outcomeTable , 
                                                covariateSettings =  createExistingmodelsCovariateSettings(),
                                                sampleSize = NULL)
  
  population <- PatientLevelPrediction::createStudyPopulation(plpData = plpData, outcomeId = outcomeId,
                                                              includeAllOutcomes = includeAllOutcomes, requireTimeAtRisk = requireTimeAtRisk, 
                                                              minTimeAtRisk = minTimeAtRisk, riskWindowStart = riskWindowStart, 
                                                              riskWindowEnd = riskWindowEnd
                                                              )
  prediction <- merge(population, ff::as.ram(plpData$covariates), by='rowId', all.x=T)
  colnames(prediction)[colnames(prediction)=='covariateValue'] <- 'value'
  prediction$value <- prediction$value/max(prediction$value)
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
  
  return(list(performance=performance, prediction=prediction))
}