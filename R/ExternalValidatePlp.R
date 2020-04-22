# @file ExternalValidatePlp.R
#
# Copyright 2019 Observational Health Data Sciences and Informatics
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
#' @param validationSchemaTarget         A string or vector of strings specifying the database containing the target cohorts
#' @param validationSchemaOutcome       A string or vector of strings specifying the database containing the outcome cohorts
#' @param validationSchemaCdm            A string or vector of strings specifying the database containing the cdm
#' @param databaseNames                  A string of vector of strings specifying sharing friendly database names corresponding to validationSchemaCdm
#' @param validationTableTarget          A string or vector of strings specifying the table containing the target cohorts
#' @param validationTableOutcome        A string or vector of strings specifying the table containing the outcome cohorts
#' @param validationIdTarget             An iteger specifying the cohort id for the target cohort
#' @param validationIdOutcome           An iteger specifying the cohort id for the outcome cohort
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
#' @param sampleSize                       If not NULL, the number of people to sample from the target cohort
#' @param outputFolder                     If you want to save the results enter the directory to save here
#' 
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
                                verbosity="INFO", keepPrediction=F,
                                sampleSize = NULL,
                                outputFolder){
  
  # TODO:: ADD LOGGING, MORE INOUT TESTS, ADD TEST CASE IN PACKAGE... 
  if(missing(plpResult))
    stop('Need to input a plpResult')
  if(class(plpResult)!="runPlp")
    stop('Need to input a plpResult of class runPlp')
  
  if(!missing(outputFolder)){
    if(missing(databaseNames)){
      stop('Need to enter databaseNames if saving results to outputFolder')
    }}
  
  if(missing(connectionDetails))
    stop('Need to enter connection details')
  
  if(missing(validationSchemaTarget))
    stop('Need to enter validationSchemaTarget ')
  if(missing(validationSchemaOutcome))
    stop('Need to enter validationSchemaOutcome ')
  if(missing(validationSchemaCdm))
    stop('Need to enter validationSchemaCdm ')
  
  # convert the lists to vectors (to keep backwards compat)
  if(class(validationSchemaTarget)=='list'){
    validationSchemaTarget <- unlist(validationSchemaTarget)
  }
  if(class(validationSchemaOutcome)=='list'){
    validationSchemaOutcome <- unlist(validationSchemaOutcome)
  }
  if(class(validationSchemaCdm )=='list'){
    validationSchemaCdm  <- unlist(validationSchemaCdm)
  }
  if(class(databaseNames)=='list'){
    databaseNames  <- unlist(databaseNames)
  }
  if(class(validationTableTarget)=='list'){
    validationTableTarget  <- unlist(validationTableTarget)
  }
  if(class(validationTableOutcome)=='list'){
    validationTableOutcome  <- unlist(validationTableOutcome)
  }
  if(class(validationIdTarget)=='list'){
    validationIdTarget  <- unlist(validationIdTarget)
  }
  if(class(validationIdOutcome)=='list'){
    validationIdOutcome  <- unlist(validationIdOutcome)
  }
  
  
  # check lengths
  if(length(validationSchemaTarget) != length(validationSchemaOutcome)){
      stop('validationSchemaTarget and validationSchemaOutcome need to be the same length')
    }
  if(length(validationSchemaTarget) != length(validationSchemaCdm)){
    stop('validationSchemaTarget and validationSchemaCdm need to be the same length')
  }
  
  # check class
  if(class(validationSchemaTarget)!=class(validationSchemaOutcome)){
    stop('validationSchemaTarget and validationSchemaOutcome not same class')
  }
  if(class(validationSchemaTarget)!=class(validationSchemaCdm)){
    stop('validationSchemaTarget and validationSchemaCdm not same class')
  }
  
  if(!missing(databaseNames)){
    if(length(validationSchemaTarget)!=length(databaseNames)){
      stop('DatabaseNames not same length as validationSchemaTarget')
    }
  }
  
  
  # add lots of test for tables and ids -  TODO?
  
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
    newData <- similarPlpData(plpModel= plpResult$model, createCohorts = F, 
                                                      newConnectionDetails = connectionDetails, 
                                                      newCdmDatabaseSchema = validationSchemaCdm[i], 
                                                      newCohortDatabaseSchema = validationSchemaTarget[i], 
                                                      newCohortTable = targetTable, 
                                                      newCohortId = validationIdTarget, 
                                                      newOutcomeDatabaseSchema = validationSchemaOutcome[i], 
                                                      newOutcomeTable = outcomeTable, 
                                                      newOutcomeId = validationIdOutcome, 
                                                      newOracleTempSchema = oracleTempSchema,
                                                      sample = sampleSize, 
                                                      createPopulation = T )
    
    if(sum(newData$population$outcomeCount>0)<20){
      warning('Outcome count is less than 20... external validation may be inaccurate')
    }
    if(sum(newData$population$outcomeCount>0)<5){
      warning('Outcome count is less than 5... external validation stopped')
      results[[i]] <- 'not run due to outcome count less than 5'
    } else{
      
      results[[i]] <- applyModel(population=newData$population, plpData = newData$plpData, 
                                                         calculatePerformance = T, plpModel = plpResult$model)
      
      
      if(!keepPrediction){
        results[[i]]$prediction <- NULL
      }
      
      if(missing(databaseNames)){
        niceName <-   rep('Not Entered', length(validationSchemaCdm))
      } else{
        niceName <-   databaseNames
      }
      results[[i]]$inputSetting<- list(databaseNames = niceName[i],
                                    cohortId = validationIdTarget, 
                                    outcomeId = validationIdOutcome,
                                    # added the below
                                    modelSettings = plpResult$model$modelSettings,
                                    testSplit = 'NA',
                                    testFraction = -1,
                                    nfold = -1, 
                                    splitSeed = -1,
                                    populationSettings = plpResult$model$populationSettings,
                                    dataExtrractionSettings = list(covariateSettings = plpResult$model$metaData$call$covariateSettings,
                                                                   cdmDatabaseSchema = validationSchemaCdm[[i]],
                                                                   databaseNames = niceName[i], #added [i]
                                                                   cohortDatabaseSchema = validationSchemaTarget[[i]], 
                                                                   cohortTable = targetTable, 
                                                                   cohortId = validationIdTarget, 
                                                                   outcomeDatabaseSchema = validationSchemaOutcome[[i]], 
                                                                   outcomeTable = outcomeTable, 
                                                                   outcomeIds = validationIdOutcome,
                                                                   oracleTempSchema = oracleTempSchema,
                                                                   sampleSize = sampleSize)
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
  
  # save results if not missing:
  if(!missing(outputFolder)){
    if(!dir.exists(outputFolder)){
      dir.create(outputFolder)
    }
    for(i in 1:length(databaseNames)){
      if(!dir.exists(file.path(outputFolder,databaseNames[i],result$validation[[i]]$model$modelAnalysisId))){
        dir.create(file.path(outputFolder,databaseNames[i],result$validation[[i]]$model$modelAnalysisId), recursive = T)
      }
    saveRDS(result$validation[[i]], file.path(outputFolder,databaseNames[i],result$validation[[i]]$model$modelAnalysisId,'validationResult.rds'))
    }
  }
  
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

