# @file Predict.R
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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

#' Create predictive probabilities
#' 
#' @details Note that the cohortsData and covariateData objects need to come from the same population.
#' 
#' @param predictiveModel       An object of type \code{predictiveModel} as generated using \code\link{{fitPredictiveModel}}.
#' @param cohortsData           An object of type \code{cohortsData} as generated using \code\link{{getDbCohortsData}}.
#' @param covariateData         An object of type \code{covariateData} as generated using \code\link{{getDbCovarteData}}. 
#'
#' @export
predict.predictiveModel <- function(predictiveModel, cohortsData, covariateData){
  intercept <- predictiveModel$coefficients[1]
  coefficients <- predictiveModel$coefficients[2:length(predictiveModel$coefficients)]
  coefficients <- data.frame(beta = as.numeric(coefficients), covariateId = as.numeric(names(predictiveModel$coefficients)[2:length(predictiveModel$coefficients)]))
  coefficients <- coefficients[coefficients$beta != 0,]
  cohortConceptId = predictiveModel$cohortConceptId
  covariates <- ffbase::subset.ffdf(covariateData$covariates, cohortConceptId == cohortConceptId, select = c("personId", "cohortStartDate", "covariateId", "covariateValue"))
  cohorts <- ffbase::subset.ffdf(cohortsData$cohorts, cohortConceptId == cohortConceptId, select = c("personId", "cohortStartDate", "time"))
  cohorts$rowId <- ff::ff(1:nrow(cohorts))
  covariates <- merge(covariates, cohorts, by=c("cohortStartDate", "personId"))
  prediction <- merge(covariates, ff::as.ffdf(coefficients), by = "covariateId")
  prediction$value <- prediction$covariateValue * prediction$beta 
  # Sum value over rowIds: (can't fit in memory, so do in chunks)
  chunks <- chunk(prediction)
  parts <- result <- vector("list", length(chunks))
  for (i in 1:length(chunks)){
    data <- prediction[chunks[[i]],,drop=FALSE]
    sums <- ffbase::bySum(data$value, by = as.factor(data$rowId))
    if (i > 1) # Probably the last personId of the previous chunk wasn't finished yet
      if (names(parts[[i-1]])[length(parts[[i-1]])] == names(sums)[1]){
        parts[[i-1]][length(parts[[i-1]])] = parts[[i-1]][length(parts[[i-1]])]  + sums[i] 
        sums<- sums[2:length(sums)]                                               
      }
    parts[[i]] <- sums
  }
  prediction <- do.call(c, parts)
  prediction <- data.frame(value = prediction, rowId = names(prediction))
  prediction <- merge(ff::as.ram(cohorts[,c("personId", "cohortStartDate", "time", "rowId")]), prediction, all.x = TRUE)
  prediction$rowId <- NULL
  prediction$value[is.na(prediction$value)] <- 0
  prediction$value <- prediction$value + intercept
  if (predictiveModel$modelType == "logistic"){
    link <- function(x) {
      return(1/(1+exp(0-x)))
    }
    prediction$value <- link(prediction$value)
  } else if (predictiveModel$modelType == "logistic"){
    prediction$value <- exp(prediction$value)  
    prediction$value <- prediction$value * prediction$time
  } else { #modelType == "survival"
    prediction$value <- exp(prediction$value)  
  }
  prediction$time <- NULL
  return(prediction)
}