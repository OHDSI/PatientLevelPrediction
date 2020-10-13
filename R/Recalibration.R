# @file Recalibration.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
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

#' recalibratePlp
#'
#' @description
#' Train various models using a default parameter gird search or user specified parameters
#'
#' @details
#' The user can define the machine learning model to train (regularised logistic regression, random forest,
#' gradient boosting machine, neural network and )
#' 
#' @param plpResult                        The output from \code{runPlp}
#' @param population                       The population created using createStudyPopulation() who will have their risks predicted
#' @param validationData                   An object of type \code{plpData} - the patient level prediction
#'                                         data extracted from the CDM.
#' @param testFraction                    Fraction of data to used for internal validation
#' @param method                          Method used to recalibrate ('RecalibrationintheLarge' or 'weakRecalibration' or 'reestimate')
#' @return
#' An object of class \code{runPlp} that is recalibrated on the new data
#'

#' @export
recalibratePlp <- function(plpResult,population, validationData, testFraction = 0.25, method = c('RecalibrationintheLarge', 'weakRecalibration', 'reestimate')){
  # check input:
  if (is.null(population))
    stop("NULL population")
  if (class(validationData) != "plpData")
    stop("Incorrect plpData class")
  if (class(plpResult) != "runPlp")
    stop("plpResult is not of class runPlp")
  if(!method  %in% c('RecalibrationintheLarge', 'weakRecalibration', 'reestimate'))
    stop("Unknown recalibration method type. must be of type: RecalibrationintheLarge, weakRecalibration, reestimate")
  if(method == 'RecalibrationintheLarge'){
    
    misCalVal <- applyModel(population=population, plpData = validationData, 
                                      calculatePerformance = T, plpModel = plpResult$model)
    misCal <- PatientLevelPrediction:::calibrationInLarge(misCalVal$prediction)
    obsOdds <- misCal$observedRisk/ (1-misCal$observedRisk)
    predOdds <- misCal$meanPredictionRisk/ (1 -  misCal$meanPredictionRisk)
    correctionFactor <- log(obsOdds / predOdds)
    recalResult <- plpResult
    recalResult$model$model$coefficients["(Intercept)"] <- plpResult$model$model$coefficients["(Intercept)"] + correctionFactor 
    recalResult$model$predict <- PatientLevelPrediction:::createTransform(recalResult$model)
    #recalculate predictions
    
    
    result <- applyModel(population=population, plpData = validationData, 
                               calculatePerformance = T, plpModel = recalResult$model)
    
    
    result$executionSummary <- list(PackageVersion = list(rVersion= R.Version()$version.string,
                                                                packageVersion = utils::packageVersion("PatientLevelPrediction")),
                                          PlatformDetails= list(platform= R.Version()$platform,
                                                                cores= Sys.getenv('NUMBER_OF_PROCESSORS'),
                                                                RAM=utils::memory.size()), #  test for non-windows needed
                                          # Sys.info()
                                          TotalExecutionElapsedTime = NULL,
                                          ExecutionDateTime = Sys.Date())
    
  }
  
  if(method == 'weakRecalibration'){
    # adjust slope intercept to 1,0
    extVal <- PatientLevelPrediction::applyModel(population = population, plpData = validationData, plpModel = plpResult$model)
    
    # edit the 0 and 1 values
    extVal$prediction$value[extVal$prediction$value==0] <- 0.000000000000001
    extVal$prediction$value[extVal$prediction$value==1] <- 1-0.000000000000001
    
    y <- ifelse(population$outcomeCount>0, 1, 0)
    inverseLog <- log(extVal$prediction$value/(1-extVal$prediction$value))
    refit <- suppressWarnings(stats::glm(y ~ inverseLog, family = 'binomial'))
    recalResult <- plpResult
    recalResult$model$model$coefficients <- recalResult$model$model$coefficients * refit$coefficients[2]
    recalResult$model$model$coefficients[1] <- recalResult$model$model$coefficients[1] + refit$coefficients[1]
    recalResult$model$predict <- PatientLevelPrediction:::createTransform(recalResult$model)
    #recalculate predictions
    result <- applyModel(population=population, plpData = validationData, 
                         calculatePerformance = T, plpModel = recalResult$model)
    
    
    result$executionSummary <- list(PackageVersion = list(rVersion= R.Version()$version.string,
                                                          packageVersion = utils::packageVersion("PatientLevelPrediction")),
                                    PlatformDetails= list(platform= R.Version()$platform,
                                                          cores= Sys.getenv('NUMBER_OF_PROCESSORS'),
                                                          RAM=utils::memory.size()), #  test for non-windows needed
                                    # Sys.info()
                                    TotalExecutionElapsedTime = NULL,
                                    ExecutionDateTime = Sys.Date())
  }

  if(method == 'reestimate'){
    #get selected covariates
    covs <- plpResult$model$model$coefficients[plpResult$model$model$coefficients != 0]
    includeCovariateIds <- names(covs[-1])
    # check which covariates are included in new data
    containedIds <- RSQLite::dbGetQuery(validationData$covariateData, "SELECT DISTINCT covariateId FROM covariateRef")
    ##setdiff(includeCovariateIds, containedIds$covariateId)
    noShrinkage <- intersect(includeCovariateIds, containedIds$covariateId)
    # add intercept
    noShrinkage <- append(noShrinkage,0, 0)
    setLassoRefit <- setLassoLogisticRegression(includeCovariateIds = includeCovariateIds,
                                                noShrinkage = noShrinkage)
    
    result <- runPlp(population = population, 
                     plpData = validationData, 
                     modelSettings = setLassoRefit, 
                     testFraction = testFraction, 
                     savePlpData = F, savePlpResult = F,savePlpPlots = F, saveEvaluation = F)
  }
 
  class(result) <- 'recalibratePlp'
  return(result)
}


