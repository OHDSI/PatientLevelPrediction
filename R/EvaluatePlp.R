# @file Evaluate.R
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

#' evaluatePlp
#'
#' @description
#' Evaluates the performance of the patient level prediction model
#' @details
#' The function calculates various metrics to measure the performance of the model
#' @param prediction                         The patient level prediction model's prediction
#' @param typeColumn                         The column name in the prediction object that is used to 
#'                                           stratify the evaluation
#' @return
#' A list containing the performance values
#'

#' @export
evaluatePlp <- function(prediction, typeColumn = 'evaluationType'){

  # checking inputs
  #========================================
  modelType <- attr(prediction, "metaData")$modelType
  
  # could remove the bit below to let people add custom types (but currently
  # we are thinking this should be set - so people should add a new type 
  # evaluation into the package rather than use custom
  if (!modelType %in% c("binary","survival")) {
    stop('Currently only support binary or survival classification models')
  }
  
  if(is.null(prediction$outcomeCount)){
    stop('No outcomeCount column present')
  }
  if(length(unique(prediction$value))==1){
    stop('Cannot evaluate as predictions all the same value')
  }
  
  # 1) evaluationSummary
  ParallelLogger::logTrace(paste0('Calulating evaluation summary Started @ ',Sys.time()))
  evaluationStatistics <- getEvaluationStatistics(
    prediction = prediction, 
    predictionType = modelType,
    typeColumn = typeColumn
  )
      
  # 2) thresholdSummary
  # need to update thresholdSummary this with all the requested values
  ParallelLogger::logTrace(paste0('Calulating threshold summary Started @ ',Sys.time()))
  thresholdSummary <- tryCatch({
    getThresholdSummary(
      prediction = prediction,
      predictionType = modelType,
      typeColumn = typeColumn
    ) 
  },
    error = function(e){ParallelLogger::logInfo('getThresholdSummary error');ParallelLogger::logInfo(e);return(NULL)}
  )
      
  # 3) demographicSummary
  ParallelLogger::logTrace(paste0('Calulating Demographic Based Evaluation Started @ ',Sys.time()))
  demographicSummary <- tryCatch({
    getDemographicSummary(
      prediction = prediction,
      predictionType = modelType,
      typeColumn = typeColumn
      )
    },
    error = function(e){ParallelLogger::logInfo('getDemographicSummary error');ParallelLogger::logInfo(e);return(NULL)}
  )
  
  # 4) calibrationSummary
  ParallelLogger::logTrace(paste0('Calculating Calibration Summary Started @ ',Sys.time()))
  calibrationSummary <- tryCatch({
    getCalibrationSummary(
      prediction = prediction,
      predictionType = modelType,
      typeColumn = typeColumn,
      numberOfStrata = 100,
      truncateFraction = 0.01
    )
  },
    error = function(e){ParallelLogger::logInfo('getCalibrationSummary error');ParallelLogger::logInfo(e);return(NULL)}
  )
  
  
  # 5) predictionDistribution - done
  ParallelLogger::logTrace(paste0('Calculating Quantiles Started @ ',Sys.time()))
  predictionDistribution <- tryCatch({
    getPredictionDistribution(
      prediction = prediction,
      predictionType = modelType,
      typeColumn = typeColumn
    )
  },
    error = function(e){ParallelLogger::logInfo('getPredictionDistribution error');ParallelLogger::logInfo(e);return(NULL)}
  )
      
  result <- list(
    evaluationStatistics = evaluationStatistics,
    thresholdSummary = thresholdSummary,
    demographicSummary = demographicSummary,
    calibrationSummary = calibrationSummary,
    predictionDistribution = predictionDistribution
  )

  class(result) <- 'plpEvaluation'
  
  return(result)

}




#' Calculate the model-based concordance, which is a calculation of the expected discrimination performance of a model under the assumption the model predicts the "TRUE" outcome
#' as detailed in van Klaveren et al. https://pubmed.ncbi.nlm.nih.gov/27251001/
#' 
#' @details
#' Calculate the model-based concordance
#'
#' @param prediction         the prediction object found in the plpResult object
#' 
#' @return
#' model-based concordance value
#'
#' @export
modelBasedConcordance <- function(prediction){
  if (!length(prediction$value >0)){
    stop("Prediction object not found")
  }
  prediction <- prediction$value
  n<-length(prediction)
  ord<-order(prediction)
  prediction<-prediction[ord]
  q.hat<-1-prediction
  V1<-(prediction*(cumsum(q.hat)-q.hat)+q.hat*(sum(prediction)-cumsum(prediction)))/(n-1)
  V2<-(prediction*(sum(q.hat)-q.hat)+q.hat*(sum(prediction)-prediction))/(n-1)
  mb.c<-sum(V1)/sum(V2)
  return(mb.c)
}


#' Evaluate a subgroup of the population
#' 
#' @details
#' run evaluatePlp in a specific subgroup
#'
#' @param prediction         the prediction object found in the plpResult object
#' @param subPopulation      the subPopulation of interestof type 'population' 
#' @return
#' plpResult evaluation object for the subgroup
#'
#' @export
subgroupEvaluation <- function(prediction, subPopulation){
  subPrediction <- prediction[prediction$subjectId %in% subPopulation$subjectId,]
  result <- evaluatePlp(subPrediction)
  return(result)
}

#' run multiple evaluations of the population
#' 
#' @details
#' create subgroups on database instance and run the subgroup analyses
#' 
#' @param plpResult                    The result of running runPlp()
#' @param connectionDetails            An R object of type\cr\code{connectionDetails} created using the
#'                                     function \code{createConnectionDetails} in the
#'                                     \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema            The name of the database schema that contains the OMOP CDM
#'                                     instance.  Requires read permissions to this database. On SQL
#'                                     Server, this should specifiy both the database and the schema,
#'                                     so for example 'cdm_instance.dbo'.
#' @param cohortDatabaseSchema         The name of the database schema that is the location where the
#'                                     cohort data used to define the at risk cohort is available.
#'                                     Requires read permissions to this database. 
#' @param cohortIds                    the cohort ids which correspond to the relvant subgroups
#' @param outputFolder                 The folder to save the results too
#' 
#' @return
#' plpResult evaluation object for the subgroup
#'
#' @export
getEvaluateSubgroup <- function(plpResult,connectionDetails = NULL, connection = NULL,
                                   cdmDatabaseSchema, cohortDatabaseSchema,
                                   cohortTable, cohortIds, outputFolder,
                                   # cohortJsonDirectory,
                                   ){
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  # First construct a data frame with the cohorts to generate
  # cohortsToCreate <- CohortGenerator::createEmptyCohortSet()
  # cohortJsonFiles <- list.files(cohortJsonDirectory, full.names = T)
  #test with celecoxib patients
  # cohortJsonFiles <- list.files(path = system.file("cohorts", package = "CohortGenerator"), full.names = TRUE)
  # 
  # for (i in 1:length(cohortJsonFiles)) {
  #   cohortJsonFileName <- cohortJsonFiles[i]
  #   cohortFullName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
  #   cohortJson <- CohortGenerator::readCirceExpressionJsonFile(cohortJsonFileName)
  #   cohortExpression <- CohortGenerator::createCirceExpressionFromFile(cohortJsonFileName)
  #   cohortsToCreate <- rbind(cohortsToCreate, data.frame(cohortId = i,
  #                                                        cohortFullName = cohortFullName, 
  #                                                        sql = CirceR::buildCohortQuery(cohortExpression, 
  #                                                                                       options = CirceR::createGenerateOptions(generateStats = FALSE)),
  #                                                        json = cohortJson,
  #                                                        stringsAsFactors = FALSE))
  # }
  # 
  # # Generate the cohort set against Eunomia. 
  # # cohortsGenerated contains a list of the cohortIds 
  # # successfully generated against the CDM
  # cohortsGenerated <- CohortGenerator::generateCohortSet(connectionDetails = connectionDetails,
  #                                                        cdmDatabaseSchema = cdmDatabaseSchema,
  #                                                        cohortDatabaseSchema = cohortDatabaseSchema,
  #                                                        cohortTable = cohortTable,
  #                                                        cohortSet = cohortsToCreate,
  #                                                        createCohortTable = TRUE,
  #                                                        incremental = FALSE,
  #                                                        incrementalFolder = file.path(outputFolder, "RecordKeeping"),
  #                                                        inclusionStatisticsFolder = outputFolder)
  prediction <- plpResult$prediction
  # for (cohortId in cohortsToCreate$cohortId){ 
  for (cohortId in cohortIds){
    getPatientIdsSql <- SqlRender::render("SELECT COHORT_DEFINITION_ID,  SUBJECT_ID FROM @cohortDatabaseSchema.@cohortTable WHERE COHORT_DEFINITION_ID = @cohortId", cohortDatabaseSchema = cohortDatabaseSchema, cohortTable = cohortTable, cohortId = cohortId )
    patientSubset <- DatabaseConnector::querySql(connection, getPatientIdsSql, snakeCaseToCamelCase = T)
    result <- subgroupEvaluation(prediction, subPopulation = patientSubset)
    saveRDS(result, file = file.path(outputFolder, paste0(cohortId, ".rds")))
  }
  
}
