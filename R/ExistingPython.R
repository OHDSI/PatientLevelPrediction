# @file ExistingPython.R
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
#' This function lets users add an existing scikit learn that is saved as model.pkl
#' into PLP format.  covariateMap is a mapping between standard covariateIds and the model column names
#' and order are required in addition to pythonModelLocation, the location of the model that must be saved
#' as `model.pkl`.  The user also needs to specify the covariate settings and population settings as these
#' are used to determine the standard PLP model design.
#'
#' @param modelLocation  The location of the folder that contains the model as model.pkl
#' @param covariateMap  A data.frame with the columns: columnId specifying the column order for the
#' covariate, covariateId the covariate ID from FeatureExtraction and modelCovariateIdName which is the
#' column name used when fitting the model.  For example, if you had a column called 'age' in your model and this was the 3rd
#' column when fitting the model, then the values for columnId would be 3, covariateId would be 1002 (the covariateId for age in years) and
#' modelCovariateIdName would be 'age'.
#' @param covariateSettings  The settings for the standardized covariates
#' @param populationSettings  The settings for the population, this includes the time-at-risk settings and
#' and inclusion criteria.
#' @param isPickle If the model is saved as a pickle set this to T if it is a json set this to F
#'
#' @return
#' An object of class plpModel, this is a list that contains: model (the location of the model.pkl),
#' preprocessing (settings for mapping the covariateIds to the model column mames), modelDesign (specification
#' of the model design), trainDetails (information about the model fitting) and covariateImportance.  You can use the output
#' as an input in PatientLevelPrediction::predictPlp to apply the model and calculate the risk for patients.
#'
#' @export
createSciKitLearnModel <- function(
    modelLocation = "/model", # model needs to be saved here as "model.pkl"
    covariateMap = data.frame(
      columnId = 1:2,
      covariateId = c(1, 2),
      modelCovariateIdName = c("pred_1", "pred_2")
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
  checkDataframe(covariateMap, c("columnId", "covariateId", "modelCovariateIdName"),
    columnTypes = list(c("numeric", "integer"), "numeric", "character")
  )
  existingModel <- list(model = "existingPython")
  class(existingModel) <- "modelSettings"

  plpModel <- list(
    # use plpModel$preprocessing$featureEngineering to rename columns
    # set plpModel$preprocessing$tidyCovariates to NULL
    preprocessing = list(
      featureEngineering = list(
        funct = "mapColumns",
        settings = list(
          featureEngineeringSettings = createFeatureEngineeringMapColumnsSettings(
            columnMap = covariateMap
          )
        )
      ),
      tidyCovariates = NULL,
      requireDenseMatrix = FALSE
    ),
    covariateImportance = data.frame(
      columnId = covariateMap$columnId,
      covariateId = covariateMap$modelCovariateIdName,
      included = TRUE
    ),
    modelDesign = PatientLevelPrediction::createModelDesign(
      targetId = 1,
      outcomeId = 2,
      restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
      covariateSettings = covariateSettings,
      populationSettings = populationSettings,
      sampleSettings = PatientLevelPrediction::createSampleSettings(),
      featureEngineeringSettings = createFeatureEngineeringMapColumnsSettings(
        columnMap = covariateMap
      ),
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
      analysisId = "exisitingPython",
      developmentDatabase = "nonOMOP",
      developmentDatabaseId = "nonOMOP",
      trainingTime = -1,
      modelName = "existing"
    )
  )

  attr(plpModel, "modelType") <- "binary"
  attr(plpModel, "saveType") <- "file"
  attr(plpModel, "predictionFunction") <- "predictPythonSklearn"
  attr(plpModel, "saveToJson") <- !isPickle
  class(plpModel) <- "plpModel"
  return(plpModel)
}


#' Create settings that enable you to convert from standard covariateIds to
#' model covariate names - this is useful when implementing a model developed
#' outside of the OHDSI tools.
#'
#' @details
#' This function creates settings that let you rename the covariates in the plpData object
#'
#' @param columnMap A data.frame containing the columns: covariateId the covariate ID from FeatureExtraction and
#' modelCovariateIdName which is the column name used when fitting the model.
#'
#' @return
#' An object of class \code{featureEngineeringSettings} that will convert column names
#'
#' @export
createFeatureEngineeringMapColumnsSettings <- function(
    columnMap) {
  featureEngineeringSettings <- list(
    columnMap = columnMap
  )

  attr(featureEngineeringSettings, "fun") <- "mapColumns"
  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
}

mapColumns <- function(
    trainData,
    featureEngineeringSettings) {
  ParallelLogger::logInfo("Changing column names")

  # map the columns - swap the covariateId with the modelCovariateIdName
  trainData$covariateData$columnMap <- featureEngineeringSettings$columnMap %>%
    dplyr::select("covariateId", "modelCovariateIdName")
  trainData$covariateData$covariates <- trainData$covariateData$covariates %>%
    dplyr::rename(newId = "rowId") %>% # duckdb issue with implicit rowid
    dplyr::compute() %>% 
    dplyr::inner_join(
      trainData$covariateData$columnMap,
      by = "covariateId"
    ) %>%
    dplyr::select("newId", "modelCovariateIdName", "covariateValue") %>%
    dplyr::rename(
      rowId = "newId", # duckdb issue with implicit rowid
      covariateId = "modelCovariateIdName"
    )

  trainData$covariateData$covariateRef <- dplyr::inner_join(
    trainData$covariateData$covariateRef,
    trainData$covariateData$columnMap,
    by = "covariateId"
  ) %>%
    dplyr::select(-"covariateId") %>%
    dplyr::rename(
      covariateId = "modelCovariateIdName"
    )

  # remove the columnMap
  trainData$covariateData$columnMap <- NULL

  # add attribute for FE
  featureEngineering <- list(
    funct = "mapColumns",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings
    )
  )

  attr(trainData, "metaData")$featureEngineering <- listAppend(
    attr(trainData, "metaData")$featureEngineering,
    featureEngineering
  )

  return(trainData)
}
