# @file SaveLoadPlp.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of CohortMethod
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

#' Save the plpData to folder
#'
#' @description
#' \code{savePlpData} saves an object of type plpData to folder.
#'
#' @param plpData   An object of type \code{plpData} as generated using
#'                           \code{getPlpData}.
#' @param file               The name of the folder where the data will be written. The folder should
#'                           not yet exist.
#' @param envir              The environment for to evaluate variables when saving
#' @param overwrite          Whether to force overwrite an existing file
#' @return
#' Called for its side effect, the data will be written to a set of files in the
#' folder specified by the user.
#'
#' @examples
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 500, seed = 42)
#' saveLoc <- file.path(tempdir(), "savePlpData")
#' savePlpData(plpData, saveLoc)
#  # This should show the files saved
#' dir(saveLoc, full.names = TRUE)
#'
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' @export
savePlpData <- function(plpData, file, envir = NULL, overwrite = FALSE) {
  if (missing(plpData)) {
    stop("Must specify plpData")
  }
  if (missing(file)) {
    stop("Must specify file")
  }
  if (!inherits(x = plpData, what = c("plpData"))) {
    stop("Data not of class plpData")
  }
  if (dir.exists(file.path(file, "covariates"))) {
    stop("Folder to save covariates already exists...")
  }

  if (!dir.exists(file)) {
    dir.create(file, recursive = TRUE)
  }

  # save the actual values in the metaData
  # TODO - only do this if exists in parent or environ
  if (is.null(plpData$metaData$call$sampleSize)) { # fixed a bug when sampleSize is NULL
    plpData$metaData$call$sampleSize <- "NULL"
  }

  Andromeda::saveAndromeda(plpData$covariateData, file = file.path(file, "covariates"), maintainConnection = TRUE)
  saveRDS(plpData$timeRef, file = file.path(file, "timeRef.rds"))
  saveRDS(plpData$cohorts, file = file.path(file, "cohorts.rds"))
  saveRDS(plpData$outcomes, file = file.path(file, "outcomes.rds"))
  saveRDS(plpData$metaData, file = file.path(file, "metaData.rds"))
}

#' Load the plpData from a folder
#'
#' @description
#' \code{loadPlpData} loads an object of type plpData from a folder in the file
#' system.
#'
#' @param file       The name of the folder containing the data.
#' @param readOnly   If true, the data is opened read only.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @return
#' An object of class plpData.
#' @examples
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 500, seed = 42)
#' saveLoc <- file.path(tempdir(), "loadPlpData")
#' savePlpData(plpData, saveLoc)
#' dir(saveLoc)
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' @export
loadPlpData <- function(file, readOnly = TRUE) {
  if (!file.exists(file)) {
    stop(paste("Cannot find folder", file))
  }
  if (!file.info(file)$isdir) {
    stop(paste("Not a folder", file))
  }

  result <- list(
    covariateData = FeatureExtraction::loadCovariateData(file = file.path(file, "covariates")),
    timeRef = readRDS(file.path(file, "timeRef.rds")),
    cohorts = readRDS(file.path(file, "cohorts.rds")),
    outcomes = readRDS(file.path(file, "outcomes.rds")),
    metaData = readRDS(file.path(file, "metaData.rds"))
  )

  class(result) <- "plpData"

  return(result)
}

#' Saves the plp model
#'
#' @details
#' Saves the plp model to a user specificed folder
#'
#' @param plpModel                   A trained classifier returned by running \code{runPlp()$model}
#' @param dirPath                  A location to save the model to
#' @return                         The directory path where the model was saved
#'
#' @examples
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' saveLoc <- file.path(tempdir(), "savePlpModel")
#' plpResult <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#' path <- savePlpModel(plpResult$model, file.path(saveLoc, "savedModel"))
#' # show the saved model
#' dir(path, full.names = TRUE)
#'
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
savePlpModel <- function(plpModel, dirPath) {
  if (missing(plpModel)) {
    stop("Must specify plpModel")
  }
  if (missing(dirPath)) {
    stop("Must specify directory path")
  }
  if (!inherits(x = plpModel, what = "plpModel")) {
    stop("Not a plpModel")
  }

  if (!dir.exists(dirPath)) {
    ParallelLogger::logInfo("Creating directory to save model")
    dir.create(dirPath, recursive = TRUE)
  }

  # save the covariateImportance
  utils::write.csv(
    x = plpModel$covariateImportance,
    file = file.path(dirPath, "covariateImportance.csv"),
    row.names = FALSE
  )

  # save the trainDetails
  if (!is.null(plpModel$trainDetails)) {
    ParallelLogger::saveSettingsToJson(
      object = plpModel$trainDetails,
      fileName = file.path(dirPath, "trainDetails.json")
    )
  }

  # save the validationDetails
  if (!is.null(plpModel$validationDetails)) {
    ParallelLogger::saveSettingsToJson(
      object = plpModel$validationDetails,
      fileName = file.path(dirPath, "validationDetails.json")
    )
  }


  # save the settings
  ParallelLogger::saveSettingsToJson(
    object = plpModel$modelDesign,
    fileName = file.path(dirPath, "modelDesign.json")
  )

  if (!is.null(plpModel$preprocessing)) {
    # cheap fix to get past bug in ParallelLogger::saveSettingsToJson with tibbles
    if (!is.null(plpModel$preprocessing$tidyCovariates)) {
      plpModel$preprocessing$tidyCovariates$normFactors <-
        as.data.frame(plpModel$preprocessing$tidyCovariates$normFactors)
    }

    ParallelLogger::saveSettingsToJson(
      object = plpModel$preprocessing,
      fileName = file.path(dirPath, "preprocessing.json")
    )
  }


  # save the model part function to file
  saveModelPart(
    model = plpModel$model,
    savetype = attr(plpModel, "saveType"),
    dirPath = dirPath
  )

  # save the attributes of plpModel
  modelAttributes <- attributes(plpModel)
  modelAttributes$names <- NULL
  ParallelLogger::saveSettingsToJson(
    object = modelAttributes,
    fileName = file.path(dirPath, "attributes.json")
  )

  return(dirPath)
}


saveModelPart <- function(model, savetype, dirPath) {
  # save the model based on saveType
  if (savetype == "xgboost") {
    xgboost::xgb.save(
      model = model,
      fname = file.path(dirPath, "model.json")
    )
  } else if (savetype == "lightgbm") {
    lightgbm::lgb.save(
      booster = model,
      filename = file.path(dirPath, "model.json")
    )
  } else if (savetype == "RtoJson") {
    ParallelLogger::saveSettingsToJson(
      object = model,
      fileName = file.path(dirPath, "model.json")
    )
  } else if (savetype == "file") {
    # move the model into model
    if (!dir.exists(file.path(dirPath, "model"))) {
      dir.create(file.path(dirPath, "model"), recursive = TRUE)
    }
    for (file in dir(model)) {
      file.copy(
        file.path(model, file),
        file.path(dirPath, "model"),
        overwrite = TRUE,
        recursive = FALSE,
        copy.mode = TRUE,
        copy.date = FALSE
      )
    }
  } else {
    ParallelLogger::logWarn("Not sure how to save model - invalid saveType")
  }
}


#' loads the plp model
#'
#' @details
#' Loads a plp model that was saved using \code{savePlpModel()}
#'
#' @param dirPath                  The location of the model
#' @return                         The plpModel object
#' @examples
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' saveLoc <- file.path(tempdir(), "loadPlpModel")
#' plpResult <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#' savePlpModel(plpResult$model, file.path(saveLoc, "savedModel"))
#' loadedModel <- loadPlpModel(file.path(saveLoc, "savedModel"))
#' # show design of loaded model
#' str(loadedModel$modelDesign)
#'
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
loadPlpModel <- function(dirPath) {
  if (!file.exists(dirPath)) {
    stop(paste("Cannot find folder", dirPath))
  }
  if (!file.info(dirPath)$isdir) {
    stop(paste("Not a folder", dirPath))
  }

  plpModel <- list()
  modelAttributes <- tryCatch(
    ParallelLogger::loadSettingsFromJson(file.path(dirPath, "attributes.json")),
    error = function(e) {
      NULL
    }
  )

  if (is.null(modelAttributes)) {
    ParallelLogger::logWarn("Incorrect plpModel object - is this an old model?")
    return(NULL)
  }

  attributes(plpModel) <- modelAttributes

  plpModel$covariateImportance <- tryCatch(
    utils::read.csv(file.path(dirPath, "covariateImportance.csv")),
    error = function(e) {
      NULL
    }
  )

  if (file.exists(file.path(dirPath, "trainDetails.json"))) {
    plpModel$trainDetails <- tryCatch(
      ParallelLogger::loadSettingsFromJson(file.path(dirPath, "trainDetails.json")),
      error = function(e) {
        NULL
      }
    )
  }
  if (file.exists(file.path(dirPath, "validationDetails.json"))) {
    plpModel$validationDetails <- tryCatch(
      ParallelLogger::loadSettingsFromJson(file.path(dirPath, "validationDetails.json")),
      error = function(e) {
        NULL
      }
    )
  }

  plpModel$modelDesign <- tryCatch(
    ParallelLogger::loadSettingsFromJson(file.path(dirPath, "modelDesign.json")),
    error = function(e) {
      NULL
    }
  )

  # we don't use "preprocess" anymore, should be "preprocessing",
  # but leave this here if loading an older model
  if (file.exists(file.path(dirPath, "preprocess.json"))) {
    plpModel$preprocessing <- tryCatch(
      ParallelLogger::loadSettingsFromJson(file.path(dirPath, "preprocess.json")),
      error = function(e) {
        NULL
      }
    )
  }
  if (file.exists(file.path(dirPath, "preprocessing.json")) && is.null(plpModel$preprocessing)) {
    plpModel$preprocessing <- tryCatch(
      ParallelLogger::loadSettingsFromJson(file.path(dirPath, "preprocessing.json")),
      error = function(e) {
        NULL
      }
    )
  }


  if (attr(plpModel, "saveType") == "xgboost") {
    rlang::check_installed("xgboost")
    plpModel$model <- xgboost::xgb.load(file.path(dirPath, "model.json"))
  } else if (attr(plpModel, "saveType") == "lightgbm") {
    rlang::check_installed("lightgbm")
    plpModel$model <- lightgbm::lgb.load(file.path(dirPath, "model.json"))
  } else if (attr(plpModel, "saveType") %in% c("RtoJson")) {
    plpModel$model <- ParallelLogger::loadSettingsFromJson(file.path(dirPath, "model.json"))
  } else {
    plpModel$model <- file.path(dirPath, "model")
  }

  return(plpModel)
}


#' Saves the prediction dataframe to a json file
#'
#' @details
#' Saves the prediction data frame returned by predict.R to an json file and
#' returns the fileLocation where the prediction is saved
#'
#' @param prediction                  The prediciton data.frame
#' @param dirPath                     The directory to save the prediction json
#' @param fileName                    The name of the json file that will be saved
#' @return                            The file location where the prediction was saved
#'
#' @examples
#' prediction <- data.frame(
#'   rowIds = c(1, 2, 3),
#'   outcomeCount = c(0, 1, 0),
#'   value = c(0.1, 0.9, 0.2)
#' )
#' saveLoc <- file.path(tempdir())
#' savePrediction(prediction, saveLoc)
#' dir(saveLoc)
#'
#' # clean up
#' unlink(file.path(saveLoc, "prediction.json"))
#' @export
savePrediction <- function(prediction, dirPath, fileName = "prediction.json") {
  ParallelLogger::saveSettingsToJson(
    object = prediction,
    fileName = file.path(dirPath, fileName)
  )

  return(file.path(dirPath, fileName))
}

#' Loads the prediction dataframe to json
#'
#' @details
#' Loads the prediciton json file
#'
#' @param fileLocation                     The location with the saved prediction
#' @return                                 The prediction data.frame
#' @examples
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' saveLoc <- file.path(tempdir(), "loadPrediction")
#' results <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#' savePrediction(results$prediction, saveLoc)
#' dir(saveLoc)
#' loadedPrediction <- loadPrediction(file.path(saveLoc, "prediction.json"))
#' }
#' @export
loadPrediction <- function(fileLocation) {
  prediction <- ParallelLogger::loadSettingsFromJson(fileName = fileLocation)
  return(prediction)
}

#' Saves the result from runPlp into the location directory
#'
#' @details
#' Saves the result from runPlp into the location directory
#'
#' @param result                      The result of running runPlp()
#' @param dirPath                     The directory to save the csv
#' @return                            The directory path where the results were saved
#'
#' @examples
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' saveLoc <- file.path(tempdir(), "savePlpResult")
#  # develop a model with default settings
#' results <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#' # save the results
#' newSaveLoc <- file.path(tempdir(), "savePlpResult", "saved")
#' savePlpResult(results, newSaveLoc)
#' # show the saved results
#' dir(newSaveLoc, recursive = TRUE, full.names = TRUE)
#'
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' unlink(newSaveLoc, recursive = TRUE)
#' }
#' @export
savePlpResult <- function(result, dirPath) {
  if (missing(result)) {
    stop("Must specify runPlp output")
  }
  if (missing(dirPath)) {
    stop("Must specify directory location")
  }

  if (!dir.exists(dirPath)) {
    dir.create(dirPath, recursive = TRUE)
  }

  savePlpModel(result$model, dirPath = file.path(dirPath, "model"))
  result$model <- NULL
  saveRDS(result, file = file.path(dirPath, "runPlp.rds"))
}

#' Loads the evalaution dataframe
#'
#' @details
#' Loads the evaluation
#'
#' @param dirPath                     The directory where the evaluation was saved
#' @return                            The runPlp object
#' @examples
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' saveLoc <- file.path(tempdir(), "loadPlpResult")
#' results <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#' savePlpResult(results, saveLoc)
#' loadedResults <- loadPlpResult(saveLoc)
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
loadPlpResult <- function(dirPath) {
  if (!file.exists(dirPath)) {
    stop(paste("Cannot find folder", dirPath))
  }
  if (!file.info(dirPath)$isdir) {
    stop(paste("Not a folder", dirPath))
  }

  result <- readRDS(file.path(dirPath, "runPlp.rds"))
  result$model <- loadPlpModel(file.path(dirPath, "model"))

  if (is.null(class(result))) {
    class(result) <- "runPlp"
  }

  return(result)
}


#' Save the plp result as json files and csv files for transparent sharing
#'
#' @details
#' Saves the main results json/csv files (these files can be read by the shiny app)
#'
#' @param result                      An object of class runPlp with development or validation results
#' @param saveDirectory               The directory the save the results as csv files
#' @param minCellCount                Minimum cell count for the covariateSummary and certain evaluation results
#' @return                            The directory path where the results were saved
#' @examples
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' saveLoc <- file.path(tempdir(), "savePlpShareable")
#' results <- runPlp(plpData, saveDirectory = saveLoc)
#' newSaveLoc <- file.path(tempdir(), "savePlpShareable", "saved")
#' path <- savePlpShareable(results, newSaveLoc)
#' # show the saved result
#' dir(newSaveLoc, full.names = TRUE, recursive = TRUE)
#'
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' unlink(newSaveLoc, recursive = TRUE)
#' }
#' @export
savePlpShareable <- function(result, saveDirectory, minCellCount = 10) {
  if (!dir.exists(saveDirectory)) dir.create(saveDirectory, recursive = TRUE)

  # executionSummary
  result$executionSummary$PackageVersion$packageVersion <- as.character(result$executionSummary$PackageVersion$packageVersion)
  result$executionSummary$PlatformDetails$RAM <- as.character(result$executionSummary$PlatformDetails$RAM)
  ParallelLogger::saveSettingsToJson(
    object = result$executionSummary,
    fileName = file.path(saveDirectory, "executionSummary.json")
  )

  # save model as json files
  savePlpModel(result$model, file.path(saveDirectory, "model"))

  # performanceEvaluation
  if (!dir.exists(file.path(saveDirectory, "performanceEvaluation"))) {
    dir.create(file.path(saveDirectory, "performanceEvaluation"), recursive = TRUE)
  }
  utils::write.csv(removeList(result$performanceEvaluation$evaluationStatistics), file = file.path(saveDirectory, "performanceEvaluation", "evaluationStatistics.csv"), row.names = FALSE)
  utils::write.csv(result$performanceEvaluation$thresholdSummary, file = file.path(saveDirectory, "performanceEvaluation", "thresholdSummary.csv"), row.names = FALSE)
  utils::write.csv(
    removeCellCount(
      result$performanceEvaluation$demographicSummary,
      minCellCount = minCellCount,
      filterColumns = c("PersonCountAtRisk", "PersonCountWithOutcome")
    ),
    file = file.path(saveDirectory, "performanceEvaluation", "demographicSummary.csv"),
    row.names = FALSE
  )
  utils::write.csv(result$performanceEvaluation$calibrationSummary, file = file.path(saveDirectory, "performanceEvaluation", "calibrationSummary.csv"), row.names = FALSE)
  utils::write.csv(result$performanceEvaluation$predictionDistribution, file = file.path(saveDirectory, "performanceEvaluation", "predictionDistribution.csv"), row.names = FALSE)

  if (!is.null(result$covariateSummary)) {
    # covariateSummary
    utils::write.csv(
      removeCellCount(
        result$covariateSummary,
        minCellCount = minCellCount,
        filterColumns = c("CovariateCount", "WithOutcome_CovariateCount", "WithNoOutcome_CovariateCount"),
        extraCensorColumns = c("WithOutcome_CovariateMean", "WithNoOutcome_CovariateMean"),
        restrictColumns = c("covariateId", "covariateName", "analysisId", "conceptId", "CovariateCount", "covariateValue", "WithOutcome_CovariateCount", "WithNoOutcome_CovariateCount", "WithOutcome_CovariateMean", "WithNoOutcome_CovariateMean", "StandardizedMeanDiff")
      ),
      file = file.path(saveDirectory, "covariateSummary.csv"),
      row.names = FALSE
    )
  }

  # analysisRef
  ParallelLogger::saveSettingsToJson(
    object = result$analysisRef,
    fileName = file.path(saveDirectory, "analysisRef.json")
  )

  return(invisible(saveDirectory))
}

removeList <- function(x) {
  if (is.null(x)) {
    return(x)
  }

  for (i in 1:ncol(x)) {
    x[, i] <- unlist(x[, i])
  }

  if ("value" %in% colnames(x)) {
    x$value <- as.double(x$value)
  }

  return(x)
}

#' Loads the plp result saved as json/csv files for transparent sharing
#'
#' @details
#' Load the main results from json/csv files into a runPlp object
#'
#' @param loadDirectory                     The directory with the results as json/csv files
#' @return                                  The runPlp object
#' @examples
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' saveLoc <- file.path(tempdir(), "loadPlpShareable")
#' results <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#' savePlpShareable(results, saveLoc)
#' dir(saveLoc)
#' loadedResults <- loadPlpShareable(saveLoc)
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
loadPlpShareable <- function(loadDirectory) {
  result <- list()
  objects <- gsub(".json", "", gsub(".csv", "", dir(loadDirectory)))
  if (sum(!c("covariateSummary", "executionSummary", "performanceEvaluation", "model", "analysisRef") %in% objects) > 0) {
    stop("Incorrect results file")
  }

  length(result) <- length(objects)
  names(result) <- objects

  # load model settings
  result$model <- loadPlpModel(file.path(loadDirectory, "model"))

  # executionSummary
  result$executionSummary <- tryCatch(
    {
      ParallelLogger::loadSettingsFromJson(fileName = file.path(loadDirectory, "executionSummary.json"))
    },
    error = function(e) {
      return(NULL)
    }
  )

  # performanceEvaluation
  result$performanceEvaluation <- list()
  result$performanceEvaluation$evaluationStatistics <- tryCatch(
    {
      utils::read.csv(file = file.path(loadDirectory, "performanceEvaluation", "evaluationStatistics.csv"))
    },
    error = function(e) {
      return(NULL)
    }
  )
  result$performanceEvaluation$thresholdSummary <- tryCatch(
    {
      utils::read.csv(file = file.path(loadDirectory, "performanceEvaluation", "thresholdSummary.csv"))
    },
    error = function(e) {
      return(NULL)
    }
  )
  result$performanceEvaluation$demographicSummary <- tryCatch(
    {
      utils::read.csv(file = file.path(loadDirectory, "performanceEvaluation", "demographicSummary.csv"))
    },
    error = function(e) {
      return(NULL)
    }
  )
  result$performanceEvaluation$calibrationSummary <- tryCatch(
    {
      utils::read.csv(file = file.path(loadDirectory, "performanceEvaluation", "calibrationSummary.csv"))
    },
    error = function(e) {
      return(NULL)
    }
  )
  result$performanceEvaluation$predictionDistribution <- tryCatch(
    {
      utils::read.csv(file = file.path(loadDirectory, "performanceEvaluation", "predictionDistribution.csv"))
    },
    error = function(e) {
      return(NULL)
    }
  )

  # covariateSummary
  result$covariateSummary <- utils::read.csv(file = file.path(loadDirectory, "covariateSummary.csv"))

  # analysisRef
  result$analysisRef <- tryCatch(
    {
      ParallelLogger::loadSettingsFromJson(fileName = file.path(loadDirectory, "analysisRef.json"))
    },
    error = function(e) {
      return(NULL)
    }
  )

  class(result) <- "runPlp"
  return(result)
}


removeCellCount <- function(
    data,
    minCellCount = minCellCount,
    filterColumns = c("CovariateCount", "WithOutcome_CovariateCount", "WithNoOutcome_CovariateCount"),
    extraCensorColumns = c("WithOutcome_CovariateMean", "WithNoOutcome_CovariateMean"),
    restrictColumns = NULL) {
  # first restrict to certain columns if required
  if (!is.null(restrictColumns)) {
    data <- data[, restrictColumns]
  }

  # next find the rows that need censoring
  ind <- rep(FALSE, nrow(data))
  for (i in 1:length(filterColumns)) {
    data[, filterColumns[i]][is.na(data[, filterColumns[i]])] <- 0
    ind <- ind | (data[, filterColumns[i]] < minCellCount)
  }

  # now replace these value with -1

  removeColumns <- c(filterColumns, extraCensorColumns)[c(filterColumns, extraCensorColumns) %in% colnames(data)]

  for (i in 1:length(removeColumns)) {
    data[ind, removeColumns[i]] <- NA
  }

  return(data)
}


#' Exports all the results from a database into csv files
#'
#' @details
#' Extracts the results from a database into a set of csv files
#'
#' @param conn  The connection to the database with the results
#' @param connectionDetails                    The connectionDetails for the result database
#' @param databaseSchemaSettings         The result database schema settings
#' @param csvFolder      Location to save the csv files
#' @param minCellCount   The min value to show in cells that are sensitive (values less than this value will be replaced with -1)
#' @param sensitiveColumns A named list (name of table columns belong to) with a list of columns to apply the minCellCount to.
#' @param fileAppend     If set to a string this will be appended to the start of the csv file names
#' @return The directory path where the results were saved
#' @examples
#' \donttest{ \dontshow{ # takes too long }
#' # develop a simple model on simulated data
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 500, seed = 42)
#' saveLoc <- file.path(tempdir(), "extractDatabaseToCsv")
#' results <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#' # now upload the results to a sqlite database
#' databasePath <- insertResultsToSqlite(saveLoc)
#' # now extract the results to csv
#' connectionDetails <- 
#'   DatabaseConnector::createConnectionDetails(dbms = "sqlite", 
#'                                              server = databasePath)
#' extractDatabaseToCsv(
#'   connectionDetails = connectionDetails,
#'   csvFolder = file.path(saveLoc, "csv")
#' )
#' # show csv file
#' list.files(file.path(saveLoc, "csv"))
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
extractDatabaseToCsv <- function(
    conn = NULL,
    connectionDetails,
    databaseSchemaSettings = createDatabaseSchemaSettings(resultSchema = "main"),
    csvFolder,
    minCellCount = 5,
    sensitiveColumns = getPlpSensitiveColumns(),
    fileAppend = NULL) {
  rlang::check_installed("readr")

  # check inputs
  if (!is.null(fileAppend)) {
    fileAppend <- paste0(gsub("_", "", gsub(" ", "", fileAppend)), "_")
  }

  if (is.null(conn)) {
    # connect
    conn <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(conn))
  }

  # create the folder to save the csv files
  if (!dir.exists(csvFolder)) {
    dir.create(csvFolder, recursive = TRUE)
  }

  # get the table names using the function in uploadToDatabase.R
  tables <- getPlpResultTables()

  # extract result per table - give option to extract from different cohort/database tables?
  modelLocations <- list()
  for (table in tables) {
    sql <- "select * from @resultSchema.@appendtotable@tablename"
    sql <- SqlRender::render(
      sql,
      resultSchema = databaseSchemaSettings$resultSchema,
      appendtotable = databaseSchemaSettings$stringAppendToResultSchemaTables,
      tablename = table
    )
    sql <- SqlRender::translate(
      sql = sql,
      targetDialect = databaseSchemaSettings$targetDialect,
      tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
    )
    result <- DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = TRUE)

    # get the model locations
    if (table == "MODELS") {
      modelLocations <- result$plpModelFile
    }

    # lower case for consistency in sharing csv results
    colnames(result) <- tolower(SqlRender::camelCaseToSnakeCase(colnames(result)))

    # TODO: add min cell count filter here
    if (tolower(table) %in% names(sensitiveColumns)) {
      result <- applyMinCellCount(
        tableName = table,
        sensitiveColumns = sensitiveColumns,
        result = result,
        minCellCount = minCellCount
      )
    }

    # save the results as a csv
    readr::write_excel_csv(
      x = result,
      file = file.path(csvFolder, paste0(fileAppend, tolower(table), ".csv"))
    )
  }


  # load plpModels from database file and save into csv file
  if (length(modelLocations) > 0) {
    if (!dir.exists(file.path(csvFolder, "models"))) {
      dir.create(file.path(csvFolder, "models"), recursive = TRUE)
    }
    for (modelLocation in modelLocations) {
      modelLocAppend <- strsplit(x = modelLocation, split = "/")[[1]][length(strsplit(x = modelLocation, split = "/")[[1]])]
      plpModel <- tryCatch(
        {
          PatientLevelPrediction::loadPlpModel(file.path(modelLocation))
        },
        error = function(e) {
          ParallelLogger::logInfo(e)
          return(NULL)
        }
      )
      if (!is.null(plpModel)) {
        PatientLevelPrediction::savePlpModel(plpModel, file.path(csvFolder, "models", modelLocAppend))
      }
    }
  }

  return(invisible(NULL))
}


getPlpSensitiveColumns <- function() {
  result <- list(
    prediction_distribution = list(
      c("person_count")
    ),
    covariate_summary = list(
      c("covariate_count"),
      c("with_no_outcome_covariate_count", "with_outcome_covariate_count")
    ),
    calibration_summary = list(
      c("person_count_at_risk", "person_count_with_outcome")
    ),
    demographic_summary = list(
      c("person_count_at_risk"),
      c("person_count_with_outcome")
    )
  )

  return(result)
}


applyMinCellCount <- function(
    tableName,
    sensitiveColumns,
    result,
    minCellCount) {
  columnsToCensor <- sensitiveColumns[[tableName]]

  for (columns in columnsToCensor) {
    rowInd <- apply(result[, columns, drop = FALSE] < minCellCount, 1, sum) > 0
    if (sum(rowInd) > 0) {
      result[rowInd, columns] <- -1
    }
  }

  return(result)
}
