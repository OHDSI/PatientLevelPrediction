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
#' @details
#' Note that the cohortData and covariateData objects need to come from the same population.
#'
#' @param predictiveModel   An object of type \code{predictiveModel} as generated using
#'                          \code{\link{fitPredictiveModel}}.
#' @param cohortData        An object of type \code{cohortData} as generated using
#'                          \code{\link{getDbCohortData}}.
#' @param covariateData     An object of type \code{covariateData} as generated using
#'                          \code{\link{getDbCovarteData}}.
#' @export
predictProbabilities <- function(predictiveModel, cohortData, covariateData) {
  cohortId <- predictiveModel$cohortId
  covariates <- ffbase::subset.ffdf(covariateData$covariates,
                                    cohortId == cohortId,
                                    select = c("personId",
                                               "cohortStartDate",
                                               "covariateId",
                                               "covariateValue"))
  cohorts <- ffbase::subset.ffdf(cohortData$cohorts,
                                 cohortId == cohortId,
                                 select = c("personId", "cohortStartDate", "time"))
  cohorts$rowId <- ff::ff(1:nrow(cohorts))
  covariates <- merge(covariates, cohorts, by = c("cohortStartDate", "personId"))
  prediction <- predictFfdf(predictiveModel$coefficients,
                            cohorts,
                            covariates,
                            predictiveModel$modelType)
  prediction$time <- NULL
  attr(prediction, "modelType") <- predictiveModel$modelType
  attr(prediction, "cohortConceptId") <- predictiveModel$cohortConceptId
  attr(prediction, "outcomeConceptId") <- predictiveModel$outcomeConceptId
  return(prediction)
}

#' Generated predictions from a regression model
#'
#' @param coefficients   A names numeric vector where the names are the covariateIds, except for the
#'                       first value which is expected to be the intercept.
#' @param outcomes       A data frame or ffdf object containing the outcomes with predefined columns
#'                       (see below).
#' @param covariates     A data frame or ffdf object containing the covariates with predefined columns
#'                       (see below).
#' @param modelType      Current supported types are "logistic", "poisson", or "survival".
#'
#' @details
#' These columns are expected in the outcome object: \tabular{lll}{ \verb{rowId} \tab(integer) \tab
#' Row ID is used to link multiple covariates (x) to a single outcome (y) \cr \verb{time} \tab(real)
#' \tab For models that use time (e.g. Poisson or Cox regression) this contains time \cr \tab
#' \tab(e.g. number of days) \cr }
#' These columns are expected in the covariates object: \tabular{lll}{ \verb{rowId} \tab(integer) \tab
#' Row ID is used to link multiple covariates (x) to a single outcome (y) \cr \verb{covariateId}
#' \tab(integer) \tab A numeric identifier of a covariate \cr \verb{covariateValue} \tab(real) \tab
#' The value of the specified covariate \cr }
#'
#' @export
predictFfdf <- function(coefficients, outcomes, covariates, modelType = "logistic") {
  if (!(modelType %in% c("logistic", "poisson", "survival"))) {
    stop(paste("Unknown modelType:", modelType))
  }
  if (class(outcomes) != "ffdf") {
    stop("Outcomes should be of type ffdf")
  }
  if (class(covariates) != "ffdf") {
    stop("Covariates should be of type ffdf")
  }
  intercept <- coefficients[1]
  coefficients <- coefficients[2:length(coefficients)]
  coefficients <- data.frame(beta = as.numeric(coefficients),
                             covariateId = as.numeric(names(coefficients)))
  coefficients <- coefficients[coefficients$beta != 0, ]
  prediction <- merge(covariates, ff::as.ffdf(coefficients), by = "covariateId")
  prediction$value <- prediction$covariateValue * prediction$beta
  prediction <- bySumFf(prediction$value, prediction$rowId)
  colnames(prediction) <- c("rowId", "value")
  prediction <- merge(ff::as.ram(outcomes), prediction, by = "rowId", all.x = TRUE)
  prediction$value[is.na(prediction$value)] <- 0
  prediction$value <- prediction$value + intercept
  if (modelType == "logistic") {
    link <- function(x) {
      return(1/(1 + exp(0 - x)))
    }
    prediction$value <- link(prediction$value)
  } else if (modelType == "poisson") {
    
    prediction$value <- exp(prediction$value)
    prediction$value <- prediction$value * prediction$time
  } else {
    # modelType == 'survival'
    prediction$value <- exp(prediction$value)
  }
  return(prediction)
}

#' Compute sum of values binned by a second variable
#'
#' @param values   An ff object containing the numeric values to be summed
#' @param bins     An ff object containing the numeric values to bin by
#'
#' @examples
#' values <- ff::as.ff(c(1, 1, 2, 2, 1))
#' bins <- ff::as.ff(c(1, 1, 1, 2, 2))
#' bySumFf(values, bins)
#'
#' @export
bySumFf <- function(values, bins) {
  .bySum(values, bins)
  # .Call("PatientLevelPrediction_bySum", PACKAGE = "PatientLevelPrediction", values, bins)
}
