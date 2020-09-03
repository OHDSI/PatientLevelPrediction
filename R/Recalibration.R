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

recalibratePlp <- function(plpModel, validationData, testFraction = 25, population, method = c('slopeintercept', 'reestimate', 'intheLarge')){
  # check input:
  if (is.null(population))
    stop("NULL population")
  if (class(validationData) != "plpData")
    stop("Incorrect plpData class")
  if (class(plpModel) != "plpModel")
    stop("Incorrect plpModel class")
  if(!method  %in% c('intercept', 'reestimate'))
    stop("Unknown recalibration method type")
  if(method == 'slopeintercept'){
    # adjust slope intercept to 1,0
  }
  if(method == 'recalibrationintheLarge'){
    #match avg pred to obs risk, adjust intercept
  }
  if(method == 'reestimate'){
    covs <- plpModel$model$coefficients[plpModel$model$coefficients != 0]
    includeCovariateIds <- names(covs[-1])
    setLassoRefit <- setLassoLogisticRegression()
    result <- runPlp(population = population, 
                     plpData = validationData, 
                     includeCovariateIds = includeCovariateIds,
                     modelSettings = setLassoRefit, 
                     testFraction = testFraction )
  }
  return(result)
}


