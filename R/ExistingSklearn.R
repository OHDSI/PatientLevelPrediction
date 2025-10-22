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
#' @param isPickle If the model should be saved as a pickle set this to TRUE if
#' it should be saved as json set this to FALSE.
#' @param targetId Add the development targetId here
#' @param outcomeId Add the development outcomeId here
#' @param populationSettings Add development population settings (this includes the time-at-risk settings).
#' @param restrictPlpDataSettings Add development restriction settings
#' @param covariateSettings Add the covariate settings here to specify how the model covariates are created from the OMOP CDM
#' @param featureEngineering Add any feature engineering here (e.g., if you need to modify the covariates before applying the model)
#'   This is a list of lists containing a string named funct specifying the engineering function to call and settings that are inputs to that 
#'   function. funct must take as input trainData (a plpData object) and settings (a list).
#' @param tidyCovariates Add any tidyCovariates mappings here (e.g., if you need to normalize the covariates)
#' @param requiresDenseMatrix Specify whether the model needs a dense matrix (TRUE or FALSE)
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
    isPickle = TRUE,
    targetId = NULL,
    outcomeId = NULL,
    populationSettings = createStudyPopulationSettings(),
    restrictPlpDataSettings = createRestrictPlpDataSettings(),
    covariateSettings = FeatureExtraction::createDefaultCovariateSettings(),
    featureEngineering = NULL,
    tidyCovariates = NULL,
    requiresDenseMatrix = FALSE
    ) {
  checkSklearn()
  checkFileExists(modelLocation)
  checkIsClass(covariateMap, "data.frame")
  checkBoolean(isPickle)
  checkDataframe(covariateMap, c("columnId", "covariateId"),
    columnTypes = list(c("numeric", "integer"), c("numeric", "integer"))
  )
  
  checkIsClass(targetId, c("numeric", "NULL"))
  checkIsClass(outcomeId, c("numeric", "NULL"))
  
  checkIsClass(populationSettings, c("NULL", "populationSettings"))
  checkIsClass(restrictPlpDataSettings , c("NULL", "restrictPlpDataSettings"))
  checkIsClass(covariateSettings, c("list", "NULL", "covariateSettings"))
  
  checkIsClass(requiresDenseMatrix, c("logical"))
  
  # start to make the plpModel
  # add param with modelType attribute 
  param <- list()
  attr(param, "settings") <- list(modelType = 'Sklearn')
  existingModel <- list(
    model = "existingSklearn",
    param = param
  )
  class(existingModel) <- "modelSettings"

  plpModel <- list(
    preprocessing = list(
      featureEngineering = featureEngineering,
      tidyCovariates = tidyCovariates,
      requiresDenseMatrix = requiresDenseMatrix
    ),
    covariateImportance = data.frame(
      columnId = covariateMap$columnId,
      covariateId = covariateMap$covariateId,
      included = TRUE
    ),
    modelDesign = PatientLevelPrediction::createModelDesign(
      targetId = targetId,
      outcomeId = outcomeId,
      modelSettings = existingModel,
      covariateSettings = covariateSettings,
      populationSettings = populationSettings,
      restrictPlpDataSettings = restrictPlpDataSettings,
      preprocessSettings = PatientLevelPrediction::createPreprocessSettings(
        minFraction = 0,
        normalize = FALSE,
        removeRedundancy = FALSE
      ),
      splitSettings = PatientLevelPrediction::createDefaultSplitSetting(splitSeed = 123)
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
  attr(plpModel, "predictionFunction") <- "predictSklearn" # TODO check this works?
  attr(plpModel, "saveToJson") <- !isPickle
  class(plpModel) <- "plpModel"
  return(plpModel)
}
