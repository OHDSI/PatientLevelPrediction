# @file Sampling.R
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

#' Create the settings for defining how the trainData from \code{splitData} are sampled using
#' default sample functions.
#'
#' @details
#' Returns an object of class \code{sampleSettings} that specifies the sampling function that will be called and the settings
#'
#' @param type              (character) Choice of:  \itemize{
#'                                         \item 'none' No sampling is applied - this is the default
#'                                         \item 'underSample' Undersample the non-outcome class to make the data more balanced
#'                                         \item 'overSample' Oversample the outcome class by adding in each outcome multiple times
#'                                         }
#' @param numberOutcomestoNonOutcomes   (numeric) A numeric specifying the required number of outcomes per non-outcomes
#' @param sampleSeed         (numeric) A seed to use when splitting the data for reproducibility (if not set a random number will be generated)
#'
#' @return
#' An object of class \code{sampleSettings}
#' @examplesIf rlang::is_installed("Eunomia")
#' \donttest{
#' # sample even rate of outcomes to non-outcomes
#' sampleSetting <- createSampleSettings(
#'   type = "underSample",
#'   numberOutcomestoNonOutcomes = 1,
#'   sampleSeed = 42
#' )
#' }
#' @export
createSampleSettings <- function(type = "none",
                                 numberOutcomestoNonOutcomes = 1,
                                 sampleSeed = sample(10000, 1)) {
  checkIsClass(numberOutcomestoNonOutcomes, c("numeric", "integer"))
  checkHigher(numberOutcomestoNonOutcomes, 0)
  checkIsClass(sampleSeed, c("numeric", "integer"))
  checkIsClass(type, c("character"))
  if (!type %in% c("none", "underSample", "overSample")) {
    stop("Incorrect type.  Pick: none/underSample/overSample")
  }

  if (type %in% c("underSample", "overSample")) {
    ParallelLogger::logWarn("The previous documentation for `numberOutcomestoNonOutcomes` used to not reflect the functionality and has now been changed. The user needs to make sure the code is not relying on what was in the docs previously.")
  }

  sampleSettings <- list(
    numberOutcomestoNonOutcomes = numberOutcomestoNonOutcomes,
    sampleSeed = ifelse(type == "none", 1, sampleSeed) # to make it the same for none
  )

  if (type == "none") {
    attr(sampleSettings, "fun") <- "sameData"
  }
  if (type == "underSample") {
    attr(sampleSettings, "fun") <- "underSampleData"
  }
  if (type == "overSample") {
    attr(sampleSettings, "fun") <- "overSampleData" # TODO
  }
  class(sampleSettings) <- "sampleSettings"
  return(sampleSettings)
}

# code to run the sampling - add desc
sampleData <- function(trainData, sampleSettings) {
  start <- Sys.time()
  metaData <- attr(trainData, "metaData")

  ParallelLogger::logInfo("Starting data sampling")

  # if a single setting, make it a list
  if (inherits(sampleSettings, "sampleSettings")) {
    sampleSettings <- list(sampleSettings)
  }

  for (sampleSetting in sampleSettings) {
    fun <- attr(sampleSetting, "fun")
    args <- list(
      trainData = trainData,
      sampleSettings = sampleSetting
    )
    ParallelLogger::logInfo(paste0("Applying ", fun))
    trainData <- do.call(eval(parse(text = fun)), args)
  }

  ParallelLogger::logInfo("Finished data sampling")

  metaData$sampleSettings <- sampleSettings

  attr(trainData, "metaData") <- metaData
  delta <- Sys.time() - start
  ParallelLogger::logInfo(
    "Sampling took ",
    signif(delta, 3), " ", attr(delta, "units")
  )
  return(trainData)
}



sameData <- function(trainData, ...) {
  ParallelLogger::logInfo("No sampling - returning same data")

  # add attribute for FE
  featureEngeering <- list(
    funct = "sameData",
    settings = list(
      none = TRUE
    )
  )
  attr(trainData, "metaData")$featureEngineering <- listAppend(
    attr(trainData, "metaData")$featureEngineering,
    featureEngeering
  )

  return(trainData)
}

underSampleData <- function(trainData, sampleSettings) {
  ParallelLogger::logInfo(paste0("sampleSeed: ", sampleSettings$sampleSeed))
  ParallelLogger::logInfo(paste0("numberOutcomestoNonOutcomes: ", sampleSettings$numberOutcomestoNonOutcomes))

  set.seed(sampleSettings$sampleSeed)
  ParallelLogger::logInfo(paste0("Starting undersampling with seed ", sampleSettings$sampleSeed))

  population <- trainData$labels %>% dplyr::collect()
  folds <- trainData$folds %>% dplyr::collect()

  population <- merge(population, folds, by = "rowId")

  ParallelLogger::logInfo(paste0(
    "Initial train data has ", sum(population$outcomeCount > 0), " outcomes to ",
    sum(population$outcomeCount == 0), " non-outcomes"
  ))

  pplOfInterest <- c()
  for (i in unique(folds$index)) {
    outcomeIds <- population$rowId[population$outcomeCount > 0 & population$index == i]
    nonoutcomeIds <- population$rowId[population$outcomeCount == 0 & population$index == i]

    sampleSize <- length(outcomeIds) / sampleSettings$numberOutcomestoNonOutcomes

    if (sampleSize > length(nonoutcomeIds)) {
      ParallelLogger::logWarn("Non-outcome count less that require sample size")
      sampleSize <- length(nonoutcomeIds)
    }

    # randomly pick non-outcome people
    sampleNonoutcomeIds <- sample(nonoutcomeIds, sampleSize)

    pplOfInterest <- c(pplOfInterest, outcomeIds, sampleNonoutcomeIds)
  }

  # filter to these patients
  sampleTrainData <- list()
  class(sampleTrainData) <- "plpData"
  sampleTrainData$labels <- trainData$labels %>% dplyr::filter(.data$rowId %in% pplOfInterest)
  sampleTrainData$folds <- trainData$folds %>% dplyr::filter(.data$rowId %in% pplOfInterest)



  sampleTrainData$covariateData <- Andromeda::andromeda()
  sampleTrainData$covariateData$covariateRef <- trainData$covariateData$covariateRef
  sampleTrainData$covariateData$covariates <- trainData$covariateData$covariates %>%
    dplyr::filter(.data$rowId %in% pplOfInterest)

  # update metaData$populationSize = nrow(trainData$labels)
  metaData <- attr(trainData$covariateData, "metaData")
  metaData$populationSize <- nrow(sampleTrainData$labels)
  attr(sampleTrainData$covariateData, "metaData") <- metaData

  class(sampleTrainData$covariateData) <- "CovariateData"

  return(sampleTrainData)
}

overSampleData <- function(trainData, sampleSettings) {
  ParallelLogger::logInfo(paste0("sampleSeed: ", sampleSettings$sampleSeed))
  ParallelLogger::logInfo(paste0("numberOutcomestoNonOutcomes: ", sampleSettings$numberOutcomestoNonOutcomes))

  set.seed(sampleSettings$sampleSeed)
  ParallelLogger::logInfo(paste0("Starting oversampling with seed ", sampleSettings$sampleSeed))

  population <- trainData$labels %>% dplyr::collect()
  folds <- trainData$folds %>% dplyr::collect()

  population <- merge(population, folds, by = "rowId")

  ParallelLogger::logInfo(paste0(
    "Initial train data has ", sum(population$outcomeCount > 0), " outcomes to ",
    sum(population$outcomeCount == 0), " non-outcomes"
  ))

  sampleTrainData <- list()
  class(sampleTrainData) <- "plpData"
  sampleTrainData$labels <- trainData$labels %>% dplyr::collect()
  sampleTrainData$folds <- trainData$folds %>% dplyr::collect()

  sampleTrainData$covariateData <- Andromeda::andromeda()
  sampleTrainData$covariateData$covariateRef <- trainData$covariateData$covariateRef
  sampleTrainData$covariateData$covariates <- trainData$covariateData$covariates

  for (i in unique(folds$index)) {
    outcomeIds <- population$rowId[population$outcomeCount > 0 & population$index == i]
    nonoutcomeIds <- population$rowId[population$outcomeCount == 0 & population$index == i]

    sampleSize <- floor(length(nonoutcomeIds) * sampleSettings$numberOutcomestoNonOutcomes)

    if (sampleSize > length(nonoutcomeIds)) {
      ParallelLogger::logWarn("Non-outcome count less than required sample size")
      sampleSize <- length(nonoutcomeIds) - length(outcomeIds)
    } else {
      sampleSize <- sampleSize - length(outcomeIds)
    }

    while (sampleSize > 0) {
      tempSampleSize <- min(length(outcomeIds), sampleSize)
      # randomly oversample outcome people
      sampleOutcomeIds <- sample(outcomeIds, tempSampleSize, replace = TRUE)
      pplOfInterest <- unique(sampleOutcomeIds) # to enable oversampling with replacement
      sampleSize <- sampleSize - length(pplOfInterest)

      addTrainData <- Andromeda::andromeda()
      addTrainData$covariates <- trainData$covariateData$covariates %>%
        dplyr::filter(.data$rowId %in% pplOfInterest)
      addTrainData$folds <- trainData$folds %>%
        dplyr::filter(.data$rowId %in% pplOfInterest)

      addTrainData$labels <- trainData$labels %>%
        dplyr::filter(.data$rowId %in% pplOfInterest) %>%
        dplyr::mutate(newRowId = dplyr::row_number()) %>%
        dplyr::mutate(rowId = .data$newRowId + max(sampleTrainData$labels$rowId))

      addTrainData$folds <- addTrainData$folds %>%
        dplyr::inner_join(addTrainData$labels %>% 
          dplyr::select("rowId", "newRowId"), by = "rowId") %>%
        dplyr::mutate(rowId = .data$newRowId) %>%
        dplyr::select(-"newRowId")

      addTrainData$covariates <- addTrainData$covariates %>%
        dplyr::inner_join(addTrainData$labels %>% dplyr::select("rowId", "newRowId"),
          by = "rowId"
        ) %>%
        dplyr::mutate(rowId = .data$newRowId) %>%
        dplyr::select(-"newRowId")
      labels <- addTrainData$labels %>% dplyr::collect()

      labels <- labels %>%
        dplyr::collect() %>%
        dplyr::mutate(rowId = .data$newRowId) %>%
        dplyr::select(-"newRowId")

      sampleTrainData$labels <- dplyr::bind_rows(
        sampleTrainData$labels,
        labels
      )
      sampleTrainData$folds <- dplyr::bind_rows(
        sampleTrainData$folds,
        addTrainData$folds %>% dplyr::collect()
      )
      Andromeda::appendToTable(
        sampleTrainData$covariateData$covariates,
        addTrainData$covariates
      )
    }
  }

  # update metaData$populationSize = nrow(trainData$labels)
  metaData <- attr(trainData$covariateData, "metaData")
  metaData$populationSize <- nrow(sampleTrainData$labels)
  attr(sampleTrainData$covariateData, "metaData") <- metaData

  class(sampleTrainData$covariateData) <- "CovariateData"

  return(sampleTrainData)
}
