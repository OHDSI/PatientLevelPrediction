# @file Fit.R
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

#' fitPlp
#'
#' @description
#' Train various models using a default parameter grid search or user specified 
#' parameters
#'
#' @details
#' The user can define the machine learning model to train 

#' @param trainData An object of type \code{trainData} created using \code{splitData}
#' data extracted from the CDM.
#' @param modelSettings An object of class \code{modelSettings} created using 
#' one of the \code{createModelSettings} functions
#' @param hyperparameterSettings An object of class \code{hyperparameterSettings}'
#' @param search The search strategy for the hyper-parameter selection (currently not used)
#' @param analysisId                       The id of the analysis
#' @param analysisPath                     The path of the analysis
#' @return
#' An object of class \code{plpModel} containing:
#'
#' \item{model}{The trained prediction model}
#' \item{preprocessing}{The preprocessing required when applying the model}
#' \item{prediction}{The cohort data.frame with the predicted risk column added}
#' \item{modelDesign}{A list specifiying the modelDesign settings used to fit the model}
#' \item{trainDetails}{The model meta data}
#' \item{covariateImportance}{The covariate importance for the model}
#' @examples
#' \donttest{ \dontshow{ # takes too long }
#' # simulate data
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' # create study population, split into train/test and preprocess with default settings
#' population <- createStudyPopulation(plpData, outcomeId = 3)
#' data <- splitData(plpData, population, createDefaultSplitSetting())
#' data$Train$covariateData <- preprocessData(data$Train$covariateData)
#' saveLoc <- file.path(tempdir(), "fitPlp")
#' # fit a lasso logistic regression model using the training data
#' plpModel <- fitPlp(data$Train, modelSettings=setLassoLogisticRegression(seed=42),
#'                    analysisId=1, analysisPath=saveLoc)
#' # show evaluationSummary for model
#' evaluatePlp(plpModel$prediction)$evaluationSummary
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
fitPlp <- function(
    trainData,
    modelSettings,
    hyperparameterSettings = createHyperparameterSettings(),
    search = "grid",
    analysisId,
    analysisPath) {
  start <- Sys.time()
  if (is.null(trainData)) {
    stop("trainData is NULL")
  }
  if (is.null(trainData$covariateData)) {
    stop("covariateData is NULL")
  }
  checkIsClass(trainData$covariateData, "CovariateData")
  if (is.null(modelSettings$fitFunction)) {
    stop("No model specified")
  }
  checkIsClass(modelSettings, "modelSettings")

  # =========================================================
  # run through pipeline list and apply:
  # =========================================================

  # Now apply the classifier:
  fun <- eval(parse(text = modelSettings$fitFunction))
  args <- list(
    trainData = trainData,
    modelSettings = modelSettings, # old: param = modelSettings$param, # make this model settings?
    hyperparameterSettings = hyperparameterSettings,
    search = search,
    analysisId = analysisId,
    analysisPath = analysisPath
  )
  plpModel <- do.call(fun, args)
  ParallelLogger::logTrace("Returned from classifier function")

  # adding trainDetails databaseId to all classifiers
  # TODO - move other details into fit
  plpModel$trainDetails$developmentDatabaseId <- attr(trainData, "metaData")$cdmDatabaseId
  class(plpModel) <- "plpModel"
  delta <- Sys.time() - start
  ParallelLogger::logInfo("Time to fit model: ", 
    signif(delta, 3), " ", attr(delta, "units"))

  return(plpModel)
}
