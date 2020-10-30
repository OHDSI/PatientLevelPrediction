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

recalibratePlp <- function(plpResult, validationData, testFraction = 25, population, method = c('RecalibrationintheLarge', 'weakRecalibration', 'reestimate')){
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
    y <- ifelse(population2$outcomeCount>0, 1, 0)
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
    setdiff(includeCovariateIds, containedIds$covariateId)
    noShrinkage <- intersect(includeCovariateIds, containedIds$covariateId)
    # add intercept
    noShrinkage <- append(noShrinkage,0, 0)
    setLassoRefit <- setLassoLogisticRegression(includeCovariateIds = includeCovariateIds,
                                                noShrinkage = noShrinkage)
    
    result <- runPlp(population = population, 
                     plpData = validationData, 
                     modelSettings = setLassoRefit, 
                     testFraction = testFraction )
  }
  if (method == "populationRisk"){
    #get dev database (use test or train?)
    eval <- as.data.frame(plpResult$performanceEvaluation$evaluationStatistics)
    eval <- eval[eval$Eval %in% c('train',"validation"),]
    devPopRisk <- as.double(as.character(eval$Value[eval$Metric=='outcomeCount']))/as.double(as.character(eval$Value[eval$Metric=='populationSize']))

    valPopRisk <- sum(population$outcomeCount) / length(population$outcomeCount)
    correctionFactor <- log(devPopRisk / valPopRisk)
    recalResult <- plpResult
    recalResult$model$model$coefficients["(Intercept)"] <- plpResult$model$model$coefficients["(Intercept)"] + correctionFactor 
    recalResult$model$predict <- PatientLevelPrediction:::createTransform(recalResult$model)
    #recalculate predictions
     
    
    result <- applyModel(population=population2, plpData = validationData, 
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
  class(result) <- 'recalibratePlp'
  return(result)
}


