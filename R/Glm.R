# @file Glm.R 
#
# Copyright 2025 Observational Health Data Sciences and Informatics
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

#' createGlmModel
#'
#' @description
#' Create a generalized linear model that can be used in the 
#' PatientLevelPrediction package.
#' @param coefficients A dataframe containing two columns, coefficients and
#' covariateId, both of type numeric. The covariateId column must contain
#' valid covariateIds that match those used in the \code{FeatureExtraction}
#' package.
#' @param intercept A numeric value representing the intercept of the model.
#' @param mapping A string representing the mapping from the 
#' linear predictors to outcome probabilities. For generalized linear models
#' this is the inverse of the link function. Supported values is only
#' "logistic" for logistic regression model at the moment. 
#' @return A model object containing the model (Coefficients and intercept)
#' and the prediction function.
#' @examples
#' coefficients <- data.frame(
#'   covariateId = c(1002),
#'   coefficient = c(0.05))
#' model <- createGlmModel(coefficients, intercept = -2.5)
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n=50)
#' prediction <- predictPlp(model, plpData, plpData$cohorts)
#' # see the predicted risk values
#' prediction$value
#' @export
createGlmModel <- function(coefficients,
                           intercept = 0,
                           mapping = "logistic") {

  checkDataframe(coefficients, 
    c("covariateId", "coefficient"), 
    c("numeric", "numeric"))
  checkHigherEqual(coefficients$covariateId, 0)
  checkIsClass(intercept, c("numeric"))
  checkIsClass(mapping, c("character"))
  checkIsEqual(mapping, "logistic")

  model <- list(
    intercept = intercept,
    coefficients = coefficients,
    mapping = mapping,
    predictionFunction = "PatientLevelPrediction::predictGlm"
  )
  existingModel <- list(model = "existingGlm")
  class(existingModel) <- "modelSettings"

  plpModel <- list(
    preprocessing = list(
      tidyCovariates = NULL,
      requireDenseMatrix = FALSE
    ),
    covariateImportance = NULL,
    modelDesign = PatientLevelPrediction::createModelDesign(
      targetId = NULL,
      outcomeId = NULL,
      modelSettings = existingModel
    ),
    model = model,
    trainDetails = NULL
  )
  attr(plpModel, "modelType") <- "binary"
  attr(plpModel, "saveType") <- "RToJson"
  attr(plpModel, "predictionFunction") <- "PatientLevelPrediction::predictGlm"
  class(plpModel) <- "plpModel"
  return(plpModel)
}

#' predict using a logistic regression model
#' 
#' @description
#' Predict risk with a given plpModel containing a generalized linear model.
#' 
#' @param plpModel An object of type \code{plpModel} - a patient level 
#' prediction model
#' @param data An object of type \code{plpData} - the patient level prediction
#' data extracted from the CDM.
#' @param cohort The population dataframe created using
#' \code{createStudyPopulation} who will have their risks predicted or a cohort
#' without the outcome known
#' @export
#' @examples
#' coefficients <- data.frame(
#'   covariateId = c(1002),
#'   coefficient = c(0.05))
#' model <- createGlmModel(coefficients, intercept = -2.5)
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n=50)
#' prediction <- predictGlm(model, plpData, plpData$cohorts)
#' # see the predicted risk values
#' head(prediction)
#' @export
#' @return A dataframe containing the prediction for each person in the 
#' population
predictGlm <- function(plpModel, data, cohort) {
  start <- Sys.time()
  
  ParallelLogger::logInfo("predict risk probabilities using predictGlm")
  
  data$covariateData$coefficients <- plpModel$model$coefficients
  on.exit(data$covariateData$coefficients <- NULL)
  
  prediction <- data$covariateData$covariates %>%
    dplyr::inner_join(data$covariateData$coefficients, by = "covariateId") %>%
    dplyr::mutate(values = .data$covariateValue * .data$coefficient) %>%
    dplyr::group_by(.data$rowId) %>%
    dplyr::summarise(value = sum(.data$values, na.rm = TRUE)) %>%
    dplyr::select("rowId", "value")
  
  prediction <- as.data.frame(prediction)
  prediction <- merge(cohort, prediction, by = "rowId", all.x = TRUE, fill = 0)
  prediction$value[is.na(prediction$value)] <- 0
  prediction$value <- prediction$value + plpModel$model$intercept
  
  if (plpModel$model$mapping == "linear") {
    prediction$value <- prediction$value
  } else if (plpModel$model$mapping == "logistic") {
    prediction$value <- 1 / (1 + exp(-prediction$value))
  } else if (plpModel$model$mapping == "square") {
    prediction$value <- prediction$value^2
  } else if (plpModel$model$mapping == "exponential") {
    prediction$value <- exp(prediction$value)
  }
  
  attr(prediction, "metaData")$modelType <- "binary"
  
  delta <- Sys.time() - start
  ParallelLogger::logInfo("Prediction took ", signif(delta, 3), " ", attr(delta, "units"))
  return(prediction)
}
