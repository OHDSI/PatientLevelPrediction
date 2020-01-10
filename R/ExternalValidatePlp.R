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
    newData <- PatientLevelPrediction::similarPlpData(plpModel= plpResult$model, createCohorts = F, 
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
#' @param addExposureDaysToStart           riskWindowStart relative to the cohort end date instead of the cohort start date?
#' @param riskWindowStart                  The day after index to start predicting the outcome
#' @param addExposureDaysToEnd             riskWindowEnd relative to the cohort end date instead of the cohort start date?
#' @param riskWindowEnd                    The day after index to stop predicting the outcome
#' @param requireTimeAtRisk                Do you want to ignore people who leave the database some point between the riskWindowStart and riskWindowEnd 
#' @param minTimeAtRisk                    If requireTimeAtRisk is TRUE, how many days must they be observed before leaving to get included (default recommendation is all risk period: riskWindowEnd-riskWindowStart)    
#' @param includeAllOutcomes               Setting this to TRUE means people with the outcome who leave the data during the risk period are still included, so only non-outcome people who leave during the risk period are removed 
#' @param removeSubjectsWithPriorOutcome   Remove people from the target population if they have the outcome prior to target cohort start date
#' @param priorOutcomeLookback             Lookback for removeSubjectsWithPriorOutcome
#' @param verbosity                        The study population creation verbosity
#' @param washoutPeriod                    Remove patients from the population with less than washoutPeriod of days prior observation
#' @param firstExposureOnly                If patients are in the target population multiple times, use only the first date
#' @param binary                           Binary classificsation (T or F)
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
#' @param scoreToProb                      Mapping from the score to the probabilities
#' @param recalibrate                      Whether to recalibrate the model on new data
#' @param calibrationPopulation            A data.frame of subjectId, cohortStartDate, indexes used to recalibrate the model on new data
#' @param covariateSummary                 Whether to calculate the covariateSummary
#' @param cdmVersion                       The CDM version being used 
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
                                  addExposureDaysToStart = F,
                                  riskWindowStart = 1, 
                                  addExposureDaysToEnd = F,
                                  riskWindowEnd = 365,
                                   requireTimeAtRisk = T, 
                                  minTimeAtRisk = 364,
                                  includeAllOutcomes = T,
                                  removeSubjectsWithPriorOutcome=T,
                                  priorOutcomeLookback = 99999,
                                  verbosity = 'INFO', 
                                  washoutPeriod = 0,
                                  firstExposureOnly= F, 
                                  binary = T,
                                   connectionDetails,
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema, cohortTable, cohortId,
                                   outcomeDatabaseSchema, outcomeTable, outcomeId,
                                   oracleTempSchema = cdmDatabaseSchema,
                                  modelName='existingModel',
                                  scoreToProb = NULL,
                                  recalibrate = F,
                                  calibrationPopulation=NULL,
                                  covariateSummary = T,
                                   cdmVersion = 5
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
                                                 asFunctions=T, 
                                                 covariateValues = covariateSummary)
  
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
                                                sampleSize = NULL, 
                                                cdmVersion = cdmVersion)
  
  population <- PatientLevelPrediction::createStudyPopulation(plpData = plpData, outcomeId = outcomeId,
                                                              includeAllOutcomes = includeAllOutcomes, 
                                                              requireTimeAtRisk = requireTimeAtRisk, 
                                                              minTimeAtRisk = minTimeAtRisk, 
                                                              riskWindowStart = riskWindowStart,
                                                              addExposureDaysToEnd = addExposureDaysToEnd,
                                                              riskWindowEnd = riskWindowEnd, 
                                                              removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
                                                              verbosity = verbosity, 
                                                              washoutPeriod = washoutPeriod,
                                                              firstExposureOnly = firstExposureOnly, 
                                                              binary = binary
                                                              )
  prediction <- merge(population, ff::as.ram(plpData$covariates$risks), by='rowId', all.x=T)
  
  covSum <- NULL
  if(covariateSummary){
    plpData$covariates <- plpData$covariates$covariateValues
    plpData$covariateRef <- ff::as.ffdf(data.frame(covariateId = modelTable$modelCovariateId,
                                                   covariateName = rep(' ', length(modelTable$modelCovariateId))))
    covSum <- covariateSummary(plpData, population)
  }
  
  # map score to probability
  if(!is.null(scoreToProb)){
    prediction <- merge(prediction, scoreToProb, by.x = 'covariateValue', by.y = 'score', all.x=T)
    prediction$probability[is.na(prediction$probability)] <- 0 
    prediction <- prediction[, !colnames(prediction%in%c('covariateValue','score') )]
    colnames(prediction)[colnames(prediction)=='probability'] <- 'value'
  }
  
  recalModel <- NULL
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
  } else if(recalibrate & is.null(calibrationPopulation)){
    recalModel <- stats::glm(y ~ x,
                             family=stats::binomial(link='logit'),
                             data=data.frame(x=prediction$covariateValue,
                                             y=as.factor(prediction$outcomeCount)))
    value <- stats::predict(recalModel, data.frame(x=prediction$covariateValue),
                            type = "response")
    prediction$value <- value
    
  } else {
    colnames(prediction)[colnames(prediction)=='covariateValue'] <- 'value'
    
    if(max(prediction$value)>1){
      attr(prediction, "metaData")$scale <- max(prediction$value)
      prediction$value <- prediction$value/max(prediction$value)
    }
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
              inputSetting = list(dataExtrractionSettings = list(outcomeId=outcomeId, 
                                  cohortId=cohortId,
                                  database = cdmDatabaseSchema),
                                  populationSettings = attr(population,'metaData')),
              executionSummary = executionSummary,
              model = list(model=list(name= 'existing model', 
                           type=type,
                           modelName=modelName,
                           modelTable=modelTable, 
                           covariateTable=covariateTable, 
                           interceptTable=interceptTable,
                           covariateSettings=covariateSettings,
                           recalModel = recalModel)),
              analysisRef=list(analysisId=NULL,
                               analysisName=NULL,
                               analysisSettings= NULL),
              covariateSummary = covSum
              ))
}
