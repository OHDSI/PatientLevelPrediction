# @file simulation.R
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

#' Create simulation profile from PLP data and an outcome model
#'
#' @param plpData An object of type \code{plpData} as generated using the \cr\code{getPlpData} function.'
#' @param outcomeModels Optional outcome model specification. This can be a named numeric vector,
#'   a list of named numeric vectors, a Cyclops/GLM coefficient data frame, a trained model, a
#'   \code{plpModel}, or a \code{runPlp} result. When a \code{plpModel} or \code{runPlp}
#'   result is supplied, the model's saved preprocessing is applied before generating outcome risks.
#' @return An object of type \code{plpDataSimulationProfile}.
#' @examplesIf rlang::is_installed("Eunomia") && rlang::is_installed("curl") && curl::has_internet()
#' \donttest{ \dontshow{ # takes too long }
#' plpData <- getEunomiaPlpData()
#' saveLoc <- file.path(tempdir(), "simulationProfile")
#' plpResult <- runPlp(
#'   plpData = plpData,
#'   outcomeId = 3,
#'   analysisId = 1,
#'   saveDirectory = saveLoc
#' )
#' simulationProfile <- PatientLevelPrediction:::createSimulationProfile(
#'   plpData = plpData,
#'   outcomeModels = plpResult
#' )
#' simulatedData <- simulatePlpData(simulationProfile, n = 100, seed = 42)
#' }
#' @keywords internal
#' @noRd
createSimulationProfile <- function(plpData, outcomeModels = NULL) {
  writeLines("Computing covariate prevalence") # (Note: currently assuming binary covariates)
  sums <- plpData$covariateData$covariates %>%
    dplyr::group_by(.data$covariateId) %>%
    dplyr::summarize(sums = dplyr::n()) %>%
    dplyr::collect()
  covariatePrevalence <- sums$sums / nrow(plpData$cohorts)
  attr(covariatePrevalence, "names") <- sums$covariateId

  isContinuous <- plpData$covariateData$covariateRef %>%
    dplyr::inner_join(plpData$covariateData$analysisRef, by = "analysisId") %>%
    dplyr::filter(.data$isBinary == "N") %>%
    dplyr::pull("covariateId")
  # get mean, sd, min and max for continuous covariates
  continuousCovariates <- plpData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId %in% isContinuous) %>%
    dplyr::group_by(.data$covariateId) %>%
    dplyr::summarize(
      mean = mean(.data$covariateValue, na.rm = TRUE),
      sd = stats::sd(.data$covariateValue, na.rm = TRUE),
      min = min(.data$covariateValue, na.rm = TRUE),
      max = max(.data$covariateValue, na.rm = TRUE)
    ) %>%
    dplyr::collect()

  outcomeRate <- nrow(plpData$outcomes) / nrow(plpData$cohorts)
  writeLines("Fitting outcome model(s)")
  outcomeIds <- plpData$metaData$databaseDetails$outcomeIds
  if (is.null(outcomeModels)) {
    outcomeModels <- vector("list", length(outcomeIds))
    outcomeModelPlpModels <- vector("list", length(outcomeIds))
    for (i in seq_along(outcomeIds)) {
      outcomeModels[[i]] <- createDefaultSimulationOutcomeModel(
        covariatePrevalence = covariatePrevalence
      )
    }
  } else {
    normalizedOutcomeModels <- normalizeSimulationOutcomeModels(outcomeModels, length(outcomeIds))
    outcomeModels <- normalizedOutcomeModels$outcomeModels
    outcomeModelPlpModels <- normalizedOutcomeModels$outcomeModelPlpModels
  }

  timeMax <- max(plpData$outcomes$daysToEvent)

  result <- list(
    covariateInfo = list(
      covariatePrevalence = covariatePrevalence,
      continuousCovariates = continuousCovariates
    ),
    timeMax = timeMax,
    outcomeRate = outcomeRate,
    outcomeModels = outcomeModels,
    outcomeModelPlpModels = outcomeModelPlpModels,
    metaData = plpData$metaData,
    covariateRef = plpData$covariateData$covariateRef %>% dplyr::collect()
  )
  class(result) <- "plpDataSimulationProfile"
  validateSimulationOutcomeModels(result)
  return(result)
}

normalizeSimulationOutcomeModels <- function(outcomeModels, numberOfOutcomeIds) {
  outcomeModel <- coerceSimulationOutcomeModel(outcomeModels)
  if (!is.null(outcomeModel)) {
    outcomeModels <- list(outcomeModel$outcomeModel)
    outcomeModelPlpModels <- list(outcomeModel$plpModel)
  } else if (is.data.frame(outcomeModels)) {
    outcomeModels <- list(outcomeModels)
    outcomeModelPlpModels <- list(NULL)
  } else if (is.list(outcomeModels)) {
    coercedOutcomeModels <- lapply(outcomeModels, function(outcomeModel) {
      coercedOutcomeModel <- coerceSimulationOutcomeModel(outcomeModel)
      if (is.null(coercedOutcomeModel)) {
        return(list(outcomeModel = outcomeModel, plpModel = NULL))
      }
      return(coercedOutcomeModel)
    })
    outcomeModels <- lapply(coercedOutcomeModels, function(outcomeModel) outcomeModel$outcomeModel)
    outcomeModelPlpModels <- lapply(coercedOutcomeModels, function(outcomeModel) outcomeModel$plpModel)
  }

  if (!is.list(outcomeModels)) {
    stop(paste(
      "outcomeModels must be a named numeric vector, a list of named numeric vectors,",
      "a supported model object, or a list of supported model objects"
    ))
  }

  if (length(outcomeModels) == 1 && numberOfOutcomeIds > 1) {
    outcomeModels <- rep(outcomeModels, numberOfOutcomeIds)
    outcomeModelPlpModels <- rep(outcomeModelPlpModels, numberOfOutcomeIds)
  }

  if (length(outcomeModels) != numberOfOutcomeIds) {
    stop("outcomeModels must contain one model for each outcomeId")
  }

  for (i in seq_along(outcomeModels)) {
    if (!is.numeric(outcomeModels[[i]])) {
      stop(sprintf("Outcome model %s must be a named numeric vector", i))
    }
    coefficientIds <- names(outcomeModels[[i]])
    if (is.null(coefficientIds) || any(is.na(coefficientIds)) || any(coefficientIds == "")) {
      stop(sprintf("Outcome model %s must have names for all coefficients", i))
    }
  }

  return(list(
    outcomeModels = outcomeModels,
    outcomeModelPlpModels = outcomeModelPlpModels
  ))
}

coerceSimulationOutcomeModel <- function(outcomeModel) {
  if (is.numeric(outcomeModel)) {
    return(list(outcomeModel = outcomeModel, plpModel = NULL))
  }

  if (is.data.frame(outcomeModel)) {
    coefficients <- coerceSimulationCoefficientDataFrame(outcomeModel)
    if (is.null(coefficients)) {
      return(NULL)
    }
    return(list(outcomeModel = coefficients, plpModel = NULL))
  }

  if (!is.list(outcomeModel)) {
    return(NULL)
  }

  if (inherits(outcomeModel, "runPlp") && inherits(outcomeModel$model, "plpModel")) {
    return(coerceSimulationPlpModel(outcomeModel$model))
  }

  if (inherits(outcomeModel, "plpModel")) {
    return(coerceSimulationPlpModel(outcomeModel))
  }

  if (!is.null(outcomeModel$coefficients)) {
    coefficients <- coerceSimulationCoefficientDataFrame(
      outcomeModel$coefficients,
      intercept = outcomeModel$intercept
    )
    if (is.null(coefficients)) {
      return(NULL)
    }
    return(list(outcomeModel = coefficients, plpModel = NULL))
  }

  if (!is.null(outcomeModel$model)) {
    return(coerceSimulationOutcomeModel(outcomeModel$model))
  }

  return(NULL)
}

coerceSimulationPlpModel <- function(plpModel) {
  outcomeModel <- coerceSimulationOutcomeModel(plpModel$model)
  if (is.null(outcomeModel)) {
    return(NULL)
  }
  outcomeModel$plpModel <- plpModel
  return(outcomeModel)
}

coerceSimulationCoefficientDataFrame <- function(coefficients, intercept = NULL) {
  if (!is.data.frame(coefficients)) {
    return(NULL)
  }

  if (all(c("betas", "covariateIds") %in% colnames(coefficients))) {
    outcomeModel <- stats::setNames(
      as.numeric(coefficients$betas),
      as.character(coefficients$covariateIds)
    )
  } else if (all(c("coefficient", "covariateId") %in% colnames(coefficients))) {
    outcomeModel <- stats::setNames(
      as.numeric(coefficients$coefficient),
      as.character(coefficients$covariateId)
    )
  } else {
    return(NULL)
  }

  if (!is.null(intercept) && !"(Intercept)" %in% names(outcomeModel)) {
    outcomeModel <- c("(Intercept)" = as.numeric(intercept)[[1]], outcomeModel)
  }

  return(outcomeModel)
}

createDefaultSimulationOutcomeModel <- function(covariatePrevalence) {
  availableCovariateIds <- names(covariatePrevalence)

  preferredCoefficients <- c(
    "1002" = 0.04, # age
    "8532001" = 0.5, # female
    "81893102" = 0.5, # ulcerative colitis
    "4266809102" = 1.0, # diverticular disease
    "4310024102" = 1.5, # angiodysplasia of stomach
    "1112807402" = 0.7, # aspirin
    "1177480402" = 0.6, # ibuprofen
    "439777102" = 0.8 # anemia
  )
  coefficients <- c(
    "(Intercept)" = -2,
    preferredCoefficients[names(preferredCoefficients) %in% availableCovariateIds]
  )

  missingIds <- setdiff(names(preferredCoefficients), availableCovariateIds)
  if (length(missingIds) > 0) {
    warning(
      sprintf(
        paste(
          "Default outcome model omitted %s coefficient covariateId(s)",
          "not available in covariateInfo$covariatePrevalence: %s"
        ),
        length(missingIds),
        paste(head(missingIds, 10), collapse = ", ")
      )
    )
  }
  if (length(coefficients) == 1) {
    warning("Default outcome model contains only an intercept; simulated outcomes may have no covariate signal")
  }

  return(coefficients)
}

validateSimulationOutcomeModels <- function(plpDataSimulationProfile) {
  availableCovariateIds <- names(plpDataSimulationProfile$covariateInfo$covariatePrevalence)
  for (i in seq_along(plpDataSimulationProfile$outcomeModels)) {
    coefficientIds <- names(plpDataSimulationProfile$outcomeModels[[i]])
    coefficientIds <- coefficientIds[!is.na(coefficientIds) & coefficientIds != "(Intercept)"]
    missingIds <- setdiff(coefficientIds, availableCovariateIds)
    if (length(missingIds) > 0) {
      stop(
        sprintf(
          paste(
            "Outcome model %s references %s covariateId(s) that are not available",
            "in covariateInfo$covariatePrevalence: %s"
          ),
          i,
          length(missingIds),
          paste(head(missingIds, 10), collapse = ", ")
        )
      )
    }
  }
}

#' Generate simulated data
#'
#' @description
#' \code{simulateplpData} creates a plpData object with simulated data.
#'
#' @param plpDataSimulationProfile   An object of type \code{plpDataSimulationProfile} as generated
#'                                   using the \cr\code{createplpDataSimulationProfile} function.
#' @param n                          The size of the population to be generated.
#' @param seed                       An optional seed for the random number generator. If provided
#'
#' @details
#' This function generates simulated data that is in many ways similar to the original data on which
#' the simulation profile is based.
#'
#' @return
#' An object of type \code{plpData}.
#'
#' @examples
#' # first load the simulation profile to use
#' data("simulationProfile")
#' # then generate the simulated data
#' plpData <- simulatePlpData(simulationProfile, n = 100, seed = 42)
#' nrow(plpData$cohorts)
#' @export
simulatePlpData <- function(plpDataSimulationProfile, n = 10000, seed = NULL) {
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || seed %% 1 != 0) {
      stop("Seed must be a single integer")
    }
    oldSeedExists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (oldSeedExists) oldSeed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(as.integer(seed))
    on.exit({
      if (oldSeedExists) {
        assign(".Random.seed", oldSeed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
  }

  # Note: currently, simulation is done completely in-memory. Could easily do batch-wise
  covariatePrevalence <- plpDataSimulationProfile$covariateInfo$covariatePrevalence
  validateSimulationOutcomeModels(plpDataSimulationProfile)

  writeLines("Generating covariates")
  personsPerCov <- stats::rpois(n = length(covariatePrevalence), lambda = covariatePrevalence * n)
  personsPerCov[personsPerCov > n] <- n
  rowId <- unlist(sapply(personsPerCov[personsPerCov > 0], function(x, n) sample.int(size = x, n), n = n))
  covariateIds <- as.numeric(names(covariatePrevalence))
  covariateId <- rep(covariateIds, personsPerCov)

  covariateValue <- rep(1, length(covariateId))
  covariateData <- Andromeda::andromeda(
    covariates = data.frame(
      rowId = rowId,
      covariateId = covariateId,
      covariateValue = covariateValue
    ),
    covariateRef = plpDataSimulationProfile$covariateRef,
    analysisRef = data.frame(analysisId = 1)
  )

  class(covariateData) <- "CovariateData"
  attr(class(covariateData), "package") <- "FeatureExtraction"

  continuousCovariates <- plpDataSimulationProfile$covariateInfo$continuousCovariates
  continuousCovSim <- do.call(rbind, lapply(seq_len(nrow(continuousCovariates)), function(i) {
    info <- continuousCovariates[i, ]
    # Generate a value for every subject:
    simValues <- round(stats::rnorm(n, mean = info$mean, sd = info$sd))
    simValues <- pmin(pmax(simValues, info$min), info$max)
    data.frame(
      rowId = 1:n,
      covariateId = info$covariateId,
      covariateValue = simValues
    )
  }))
  covariateData$covariates <- covariateData$covariates %>%
    dplyr::filter(!.data$covariateId %in% !!continuousCovariates$covariateId)
  Andromeda::appendToTable(covariateData$covariates, continuousCovSim)

  writeLines("Generating cohorts")
  cohorts <- data.frame(rowId = 1:n, subjectId = 2e+10 + (1:n), targetId = 1)
  cohorts$cohortStartDate <- sample(-1000:1000, n, replace = TRUE) + as.Date("2010-01-01")
  cohorts$daysFromObsStart <- sample(1:1000, n, replace = TRUE)
  cohorts$daysToCohortEnd <- sample(1:1000, n, replace = TRUE)
  cohorts$daysToObsEnd <- cohorts$daysToCohortEnd + sample(1:1000, n, replace = TRUE)
  cohorts$ageYear <- covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 1002) %>%
    dplyr::arrange(.data$rowId) %>%
    dplyr::pull(.data$covariateValue)
  cohorts$gender <- 8532 # female
  cohorts$gender[sample((seq_len(nrow(cohorts))), nrow(cohorts) / 2)] <- 8507

  writeLines("Generating outcomes")
  allOutcomes <- vector("list", length(plpDataSimulationProfile$metaData$databaseDetails$outcomeIds))
  allSimulationTruth <- vector("list", length(plpDataSimulationProfile$metaData$databaseDetails$outcomeIds))
  outcomeIds <- plpDataSimulationProfile$metaData$databaseDetails$outcomeIds
  timeMax <- max(plpDataSimulationProfile$timeMax)
  for (i in seq_along(outcomeIds)) {
    prediction <- predictSimulationOutcomeRisk(
      outcomeModel = plpDataSimulationProfile$outcomeModels[[i]],
      plpModel = plpDataSimulationProfile$outcomeModelPlpModels[[i]],
      cohorts = cohorts,
      covariateData = covariateData,
      metaData = plpDataSimulationProfile$metaData
    )
    outcomeCount <- stats::rbinom(nrow(prediction), size = 1, prob = prediction$value)
    daysToEvent <- rep(NA_integer_, nrow(prediction))
    eventRows <- outcomeCount != 0
    daysToEvent[eventRows] <- round(stats::runif(sum(eventRows), 0, timeMax))
    linearPredictor <- rep(NA_real_, nrow(prediction))
    if ("rawValue" %in% colnames(prediction)) {
      linearPredictor <- prediction$rawValue
    }
    simulationTruth <- data.frame(
      rowId = prediction$rowId,
      outcomeId = outcomeIds[i],
      linearPredictor = linearPredictor,
      trueRisk = prediction$value,
      outcomeCount = outcomeCount,
      daysToEvent = daysToEvent
    )
    outcomes <- simulationTruth[simulationTruth$outcomeCount != 0, c("rowId", "outcomeId", "outcomeCount", "daysToEvent")]
    allOutcomes[[i]] <- outcomes
    allSimulationTruth[[i]] <- simulationTruth
  }
  allOutcomes <- dplyr::bind_rows(allOutcomes)
  simulationTruth <- dplyr::bind_rows(allSimulationTruth)

  covariateData$coefficients <- NULL

  # Remove rownames else they will be copied to the ffdf objects:
  metaData <- list()

  metaData$databaseDetails <- list(
    cdmDatabaseSchema = "CDM_SCHEMA",
    cdmDatabaseName = "CDM_NAME",
    outcomeDatabaseSchema = NULL,
    cohortDatabaseSchema = NULL,
    connectionDetails = NULL,
    outcomeTable = NULL,
    cohortTable = NULL,
    cdmVersion = 5,
    targetId = 1,
    outcomeIds = 3
  )
  metaData$restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings()
  metaData$covariateSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsAge = TRUE,
    useDemographicsGender = TRUE,
    useConditionOccurrenceLongTerm = TRUE,
    useDrugEraLongTerm = TRUE
  )


  attrition <- data.frame(
    outcomeId = 3, description = "Simulated data",
    targetCount = nrow(cohorts), uniquePeople = nrow(cohorts),
    outcomes = nrow(outcomes)
  )
  attr(cohorts, "metaData") <- list(
    targetId = 1,
    attrition = attrition
  )

  attr(allOutcomes, "metaData") <- data.frame(outcomeIds = 3)

  attr(covariateData, "metaData") <- list(populationSize = n, cohortId = 1)

  result <- list(
    cohorts = cohorts,
    outcomes = allOutcomes,
    simulationTruth = simulationTruth,
    covariateData = covariateData,
    timeRef = NULL,
    metaData = metaData
  )

  class(result) <- "plpData"
  return(result)
}

predictSimulationOutcomeRisk <- function(outcomeModel, plpModel, cohorts, covariateData, metaData) {
  if (!is.null(plpModel)) {
    plpData <- list(
      cohorts = cohorts,
      outcomes = data.frame(),
      covariateData = covariateData,
      metaData = metaData
    )
    class(plpData) <- "plpData"
    return(predictPlp(
      plpModel = plpModel,
      plpData = plpData,
      population = cohorts
    ))
  }

  coefficients <- data.frame(
    betas = as.numeric(outcomeModel),
    covariateIds = names(outcomeModel)
  )
  return(predictCyclopsType(coefficients,
    cohorts,
    covariateData,
    modelType = "logistic"
  ))
}
