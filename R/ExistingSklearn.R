# @file ExistingSklearn.R
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


#' Plug an existing scikit learn python model into the
#' PLP framework
#'
#' @details
#' This function lets users add an existing scikit learn model that is saved as 
#' model.pkl into PLP format.  covariateMap is a mapping between standard 
#' covariateIds and the model columns. The user also needs to specify the 
#' covariate settings and population settings as these are used to determine 
#' the standard PLP model design.
#'
#' @param modelLocation  The location of the folder that contains the model as
#' model.pkl
#' @param covariateMap  A data.frame with the columns: columnId and covariateId. 
#' `covariateId` from FeatureExtraction is the standard OHDSI covariateId.
#' `columnId` is the column location the model expects that covariate to be in.
#' For example, if you had a column called 'age' in your model and this was the 
#' 3rd column when fitting the model, then the values for columnId would be 3, 
#' covariateId would be 1002 (the covariateId for age in years) and
#' @param covariateSettings  The settings for the standardized covariates
#' @param populationSettings  The settings for the population, this includes the 
#' time-at-risk settings and inclusion criteria.
#' @param isPickle If the model should be saved as a pickle set this to TRUE if
#' it should be saved as json set this to FALSE.
#'
#' @return
#' An object of class plpModel, this is a list that contains: 
#'        model (the location of the model.pkl),
#'        preprocessing (settings for mapping the covariateIds to the model 
#'                       column mames), 
#'        modelDesign (specification of the model design), 
#'        trainDetails (information about the model fitting) and 
#'        covariateImportance.  
#' 
#' You can use the output as an input in PatientLevelPrediction::predictPlp to 
#' apply the model and calculate the risk for patients.
#' @export
createSklearnModel <- function(
    modelLocation = "/model", # model needs to be saved here as "model.pkl"
    covariateMap = data.frame(
      columnId = 1:2,
      covariateId = c(1, 2),
    ),
    covariateSettings, # specify the covariates
    populationSettings, # specify time at risk used to develop model
    isPickle = TRUE) {
  checkSklearn()
  checkFileExists(modelLocation)
  checkIsClass(covariateMap, "data.frame")
  checkIsClass(covariateSettings, "covariateSettings")
  checkIsClass(populationSettings, "populationSettings")
  checkBoolean(isPickle)
  checkDataframe(covariateMap, c("columnId", "covariateId"),
    columnTypes = list(c("numeric", "integer"), c("numeric", "integer"))
  )
  existingModel <- list(model = "existingSklearn")
  class(existingModel) <- "modelSettings"

  plpModel <- list(
    preprocessing = list(
      tidyCovariates = NULL,
      requireDenseMatrix = FALSE
    ),
    covariateImportance = data.frame(
      columnId = covariateMap$columnId,
      covariateId = covariateMap$covariateId,
      included = TRUE
    ),
    modelDesign = PatientLevelPrediction::createModelDesign(
      targetId = 1,
      outcomeId = 2,
      restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
      covariateSettings = covariateSettings,
      populationSettings = populationSettings,
      sampleSettings = PatientLevelPrediction::createSampleSettings(),
      preprocessSettings = PatientLevelPrediction::createPreprocessSettings(
        minFraction = 0,
        normalize = FALSE,
        removeRedundancy = FALSE
      ),
      modelSettings = existingModel,
      splitSettings = PatientLevelPrediction::createDefaultSplitSetting()
    ),
    model = modelLocation,
    trainDetails = list(
      analysisId = "existingSklearn",
      developmentDatabase = "unknown",
      developmentDatabaseId = "unknown",
      trainingTime = -1,
      modelName = "existingSklearn"
    )
  )

  attr(plpModel, "modelType") <- "binary"
  attr(plpModel, "saveType") <- "file"
  attr(plpModel, "predictionFunction") <- "predictPythonSklearn"
  attr(plpModel, "saveToJson") <- !isPickle
  class(plpModel) <- "plpModel"
  return(plpModel)
}
