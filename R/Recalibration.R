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

recalibratePlp <- function(plpResult, validationData, testFraction = 25, population, method = c('slopeintercept', 'reestimate', 'RecalibrationintheLarge')){
  # check input:
  if (is.null(population))
    stop("NULL population")
  if (class(validationData) != "plpData")
    stop("Incorrect plpData class")
  if (class(plpResult) != "runPlp")
    stop("plpResult is not of class runPlp")
  if(!method  %in% c('intercept', 'reestimate'))
    stop("Unknown recalibration method type")
  if(method == 'RecalibrationintheLarge'){
    observedFreq <- sum(population$outcomeCount>0) / nrow(population)
    predictedFreq <- mean(plpResult$prediction$value)
    misCal <- observedFreq - predictedFreq
    recalResult <- plpResult
    recalResult$prediction$value
    recalResult$model$model$coefficients["(Intercept)"] <- plpResult$model$model$coefficients["(Intercept)"] + misCal 

        #recalculate predictions
    predictionOld <- plpResult$model$predict(plpData = plpData2, population = population2)
    prediction <- recalResult$model$predict(plpData=plpData2,population=population2)
    return(recalResult)
  }
  if(method == 'slopeintercept'){
    # adjust slope intercept to 1,0
  }

  if(method == 'reestimate'){
    covs <- plpResult$model$model$coefficients[plpResult$model$model$coefficients != 0]
    includeCovariateIds <- names(covs[-1])
    noShrinkage <- append(0,includeCovariateIds)
    setLassoRefit <- setLassoLogisticRegression(includeCovariateIds = includeCovariateIds,
                                                noShrinkage = noShrinkage)
    validationData$covariateData$covariates
    containedIds <- RSQLite::dbGetQuery(validationData$covariateData, "SELECT DISTINCT covariateId FROM covariateRef")
    setdiff(noShrinkage, containedIds$covariateId)
    result <- runPlp(population = population, 
                     plpData = validationData, 
                     modelSettings = setLassoRefit, 
                     testFraction = testFraction )
  }
  return(result)
}


