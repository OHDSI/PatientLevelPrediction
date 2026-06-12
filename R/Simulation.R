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
        paste(utils::head(missingIds, 10), collapse = ", ")
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
          paste(utils::head(missingIds, 10), collapse = ", ")
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
#' Generate benchmark PLP data with known outcome risk
#'
#' @description
#' \code{simulatePlpBenchmarkData} creates semi-synthetic PLP data by resampling rows
#' from an existing \code{plpData} object and generating outcomes from a known logistic
#' outcome model. The source covariate patterns are preserved, so this function is
#' intended for benchmark simulations rather than realistic synthetic patient generation.
#' Rows without observable time at \code{riskWindowStart} are excluded before
#' resampling, and generated event times are sampled within each row's observed
#' risk window. When \code{daysToCohortEnd} is present and contains positive
#' follow-up, event times are also kept within the target cohort end.
#'
#' The attached truth is defined for each simulated row's observable risk window.
#' For benchmark evaluation against this truth, use population settings with
#' \code{requireTimeAtRisk = FALSE}; otherwise PLP can drop short-follow-up
#' non-outcome rows and change the estimand. For a strict fixed-horizon benchmark,
#' first restrict the source \code{plpData} to rows with complete follow-up through
#' \code{riskWindowEnd}.
#'
#' Rows are sampled with replacement, as in standard plasmode simulation. A source
#' row can therefore appear more than once with different simulated \code{rowId} and
#' \code{subjectId} values. The truth table records \code{sourceRowId} and
#' \code{sourceSubjectId} so duplicate source rows can be identified. This is not a
#' problem when using the simulated data to study known risks in realistic covariate
#' distributions, but ordinary row-level train/test splitting can put the same source
#' covariate pattern in both partitions and can make held-out performance optimistic.
#'
#' @param plpData An object of type \code{plpData}.
#' @param outcomeModel A PLP model, a \code{runPlp()} result, or a named numeric
#'                     vector of logistic model coefficients. When a PLP model or
#'                     result is supplied, outcomes are generated from
#'                     \code{predictPlp()}, so the model's feature engineering and
#'                     preprocessing are applied before calculating true risks.
#'                     Named numeric vectors are used directly and must already
#'                     match the scale and feature space of \code{plpData}; use
#'                     \code{"(Intercept)"} for the intercept and covariate IDs for
#'                     covariate coefficients.
#' @param n The number of rows to generate.
#' @param riskWindowStart The earliest generated \code{daysToEvent} for outcome cases.
#' @param riskWindowEnd The latest generated \code{daysToEvent} for outcome cases.
#' @param outcomeId The outcome ID to use. If \code{NULL}, the first outcome ID in
#'                  \code{plpData$metaData$databaseDetails$outcomeIds} is used.
#' @param targetOutcomeRate Optional target mean true risk. If provided, the intercept
#'                          is shifted so the generated population has this mean risk.
#' @param seed An optional seed for the random number generator.
#' @param returnTruth If \code{TRUE}, a truth table with source row and subject IDs,
#'                    linear predictors, true risks, and generated outcome status is
#'                    returned as \code{simulationTruth}.
#'
#' @return
#' An object of type \code{plpData}. The returned object includes
#' \code{simulationSettings}; when \code{returnTruth = TRUE}, it also includes
#' \code{simulationTruth}.
#'
#' @examples
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 100, seed = 42)
#' benchmarkData <- simulatePlpBenchmarkData(
#'   plpData = plpData,
#'   outcomeModel = c("(Intercept)" = -2, "1002" = 0.04),
#'   n = 100,
#'   riskWindowStart = 1,
#'   riskWindowEnd = 365,
#'   seed = 42
#' )
#' benchmarkData$simulationTruth
#' @export
simulatePlpBenchmarkData <- function(plpData,
                                     outcomeModel,
                                     n = nrow(plpData$cohorts),
                                     riskWindowStart = 1,
                                     riskWindowEnd = 365,
                                     outcomeId = NULL,
                                     targetOutcomeRate = NULL,
                                     seed = NULL,
                                     returnTruth = TRUE) {
  benchmarkOutcomeModel <- normalizeBenchmarkOutcomeModel(outcomeModel)
  validateBenchmarkSimulationInputs(
    plpData = plpData,
    outcomeModel = benchmarkOutcomeModel,
    n = n,
    riskWindowStart = riskWindowStart,
    riskWindowEnd = riskWindowEnd,
    targetOutcomeRate = targetOutcomeRate,
    seed = seed,
    returnTruth = returnTruth,
    outcomeId = outcomeId
  )

  if (!is.null(seed)) {
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

  n <- as.integer(n)
  riskWindowStart <- as.integer(riskWindowStart)
  riskWindowEnd <- as.integer(riskWindowEnd)

  cohorts <- as.data.frame(plpData$cohorts)
  targetId <- unique(cohorts$targetId)
  outcomeId <- resolveBenchmarkOutcomeId(plpData, outcomeId)
  simulationSettings <- list(
    outcomeId = outcomeId,
    targetId = targetId,
    n = n,
    riskWindowStart = riskWindowStart,
    riskWindowEnd = riskWindowEnd,
    targetOutcomeRate = targetOutcomeRate,
    seed = seed,
    sampleWithReplacement = TRUE,
    returnTruth = returnTruth,
    estimand = "observable risk window"
  )
  cohorts <- restrictBenchmarkCohortsToObservableRiskWindow(
    cohorts = cohorts,
    riskWindowStart = riskWindowStart
  )
  sampledIndexes <- sample.int(nrow(cohorts), size = n, replace = TRUE)
  sampledCohorts <- cohorts[sampledIndexes, , drop = FALSE]
  rowMap <- data.frame(
    sourceRowId = sampledCohorts$rowId,
    sourceSubjectId = sampledCohorts$subjectId,
    newRowId = seq_len(n)
  )

  sampledCohorts$rowId <- seq_len(n)
  sampledCohorts$subjectId <- 2e+10 + seq_len(n)
  attr(sampledCohorts, "metaData") <- list(
    targetId = targetId,
    outcomeId = outcomeId
  )

  covariateData <- resampleBenchmarkCovariateData(
    covariateData = plpData$covariateData,
    rowMap = rowMap,
    populationSize = n,
    cohortId = targetId
  )

  truth <- calculateBenchmarkTruth(
    plpData = plpData,
    covariateData = covariateData,
    population = sampledCohorts,
    rowMap = rowMap,
    outcomeModel = benchmarkOutcomeModel,
    targetOutcomeRate = targetOutcomeRate
  )

  generatedOutcome <- stats::rbinom(n = n, size = 1, prob = truth$trueRisk)
  truth$outcomeCount <- generatedOutcome
  if (sum(generatedOutcome) > 0) {
    truth$daysToEvent <- NA_integer_
    eventRows <- which(generatedOutcome == 1)
    eventWindowEnd <- getBenchmarkObservableWindowEnd(sampledCohorts, riskWindowEnd)
    truth$daysToEvent[eventRows] <- vapply(
      eventRows,
      function(rowIndex) {
        sample.int(
          n = eventWindowEnd[rowIndex] - riskWindowStart + 1L,
          size = 1L
        ) + riskWindowStart - 1L
      },
      integer(1)
    )
  } else {
    truth$daysToEvent <- NA_integer_
  }

  outcomes <- truth[truth$outcomeCount == 1, c("rowId", "outcomeCount", "daysToEvent")]
  outcomes$outcomeId <- rep(outcomeId, nrow(outcomes))
  outcomes <- outcomes[, c("rowId", "outcomeId", "outcomeCount", "daysToEvent")]

  metaData <- plpData$metaData
  metaData$databaseDetails$outcomeIds <- outcomeId

  attrition <- data.frame(
    outcomeId = outcomeId,
    description = "Benchmark simulated data",
    targetCount = nrow(sampledCohorts),
    uniquePeople = length(unique(sampledCohorts$subjectId)),
    outcomes = nrow(outcomes)
  )
  attr(sampledCohorts, "metaData") <- list(
    targetId = targetId,
    attrition = attrition
  )
  attr(outcomes, "metaData") <- data.frame(outcomeIds = outcomeId)

  result <- list(
    cohorts = sampledCohorts,
    outcomes = outcomes,
    covariateData = covariateData,
    timeRef = NULL,
    metaData = metaData,
    simulationSettings = simulationSettings
  )
  class(result) <- "plpData"
  if (returnTruth) {
    result$simulationTruth <- truth
  }
  return(result)
}

normalizeBenchmarkOutcomeModel <- function(outcomeModel) {
  if (inherits(outcomeModel, "runPlp")) {
    if (is.null(outcomeModel$model)) {
      stop("outcomeModel is a runPlp result but does not contain a model")
    }
    outcomeModel <- outcomeModel$model
  }
  if (inherits(outcomeModel, "plpModel")) {
    return(list(type = "plpModel", model = outcomeModel))
  }
  return(list(type = "coefficients", coefficients = outcomeModel))
}

validateBenchmarkSimulationInputs <- function(plpData,
                                              outcomeModel,
                                              n,
                                              riskWindowStart,
                                              riskWindowEnd,
                                              targetOutcomeRate,
                                              seed,
                                              returnTruth,
                                              outcomeId) {
  if (is.null(plpData$cohorts) || is.null(plpData$covariateData$covariates)) {
    stop("plpData must contain cohorts and covariateData$covariates")
  }
  if (!"daysToObsEnd" %in% colnames(plpData$cohorts)) {
    stop("plpData$cohorts must contain daysToObsEnd")
  }
  if (!"targetId" %in% colnames(plpData$cohorts)) {
    stop("plpData$cohorts must contain targetId")
  }
  targetIds <- unique(plpData$cohorts$targetId)
  if (!is.numeric(targetIds) || length(targetIds) != 1L ||
      is.na(targetIds) || !is.finite(targetIds) || targetIds %% 1 != 0) {
    stop("plpData$cohorts must contain exactly one finite targetId")
  }
  if ("timeId" %in% colnames(plpData$covariateData$covariates) ||
      !is.null(plpData$covariateData$timeRef) ||
      !is.null(plpData$timeRef)) {
    stop("simulatePlpBenchmarkData does not support temporal covariateData")
  }
  if (!is.list(outcomeModel) || is.null(outcomeModel$type)) {
    stop("outcomeModel must be a PLP model, runPlp result, or named finite numeric vector")
  }
  if (identical(outcomeModel$type, "coefficients")) {
    coefficients <- outcomeModel$coefficients
    if (!is.numeric(coefficients)) {
      stop("outcomeModel must be a PLP model, runPlp result, or named finite numeric vector")
    }
    coefficientIds <- names(coefficients)
    if (is.null(coefficientIds) || any(is.na(coefficientIds)) || any(coefficientIds == "")) {
      stop("outcomeModel must have names for all coefficients")
    }
    if (anyNA(coefficients) || any(!is.finite(coefficients))) {
      stop("outcomeModel must be a named finite numeric vector")
    }
    if (anyDuplicated(coefficientIds)) {
      stop("outcomeModel coefficient names must be unique")
    }
    covariateCoefficientIds <- coefficientIds[coefficientIds != "(Intercept)"]
    if (length(covariateCoefficientIds) > 0 &&
        any(is.na(suppressWarnings(as.numeric(covariateCoefficientIds))))) {
      stop("All non-intercept outcomeModel names must be numeric covariate IDs")
    }
    covariateRef <- plpData$covariateData$covariateRef %>%
      dplyr::collect()
    missingCovariateIds <- setdiff(covariateCoefficientIds, as.character(covariateRef$covariateId))
    if (length(missingCovariateIds) > 0) {
      stop(sprintf(
        "outcomeModel references covariate ID(s) not available in covariateRef: %s",
        paste(utils::head(missingCovariateIds, 10), collapse = ", ")
      ))
    }
  } else if (identical(outcomeModel$type, "plpModel")) {
    if (is.null(outcomeModel$model)) {
      stop("outcomeModel must contain a PLP model")
    }
  } else {
    stop("outcomeModel must be a PLP model, runPlp result, or named finite numeric vector")
  }
  checkBenchmarkScalarInteger(n, "n", positive = TRUE)
  checkBenchmarkScalarInteger(riskWindowStart, "riskWindowStart")
  checkBenchmarkScalarInteger(riskWindowEnd, "riskWindowEnd")
  if (riskWindowEnd < riskWindowStart) {
    stop("riskWindowEnd must be greater than or equal to riskWindowStart")
  }
  if (!is.null(targetOutcomeRate) &&
      (!is.numeric(targetOutcomeRate) || length(targetOutcomeRate) != 1L ||
       is.na(targetOutcomeRate) || !is.finite(targetOutcomeRate) ||
       targetOutcomeRate <= 0 || targetOutcomeRate >= 1)) {
    stop("targetOutcomeRate must be NULL or a single number between 0 and 1")
  }
  if (!is.null(seed)) {
    checkBenchmarkScalarInteger(seed, "Seed")
  }
  if (!is.logical(returnTruth) || length(returnTruth) != 1L || is.na(returnTruth)) {
    stop("returnTruth must be TRUE or FALSE")
  }
  resolveBenchmarkOutcomeId(plpData, outcomeId)
}

checkBenchmarkScalarInteger <- function(x, name, positive = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x %% 1 != 0 || (positive && x <= 0)) {
    if (positive) {
      stop(sprintf("%s must be a positive finite scalar integer", name))
    }
    stop(sprintf("%s must be a finite scalar integer", name))
  }
}

resolveBenchmarkOutcomeId <- function(plpData, outcomeId) {
  if (is.null(outcomeId)) {
    outcomeIds <- plpData$metaData$databaseDetails$outcomeIds
    if (is.null(outcomeIds) || !is.numeric(outcomeIds) || length(outcomeIds) != 1L ||
        is.na(outcomeIds) || !is.finite(outcomeIds) || outcomeIds %% 1 != 0) {
      stop("outcomeId must be specified when plpData metadata does not contain exactly one finite outcome ID")
    }
    return(as.integer(outcomeIds))
  }
  checkBenchmarkScalarInteger(outcomeId, "outcomeId", positive = TRUE)
  as.integer(outcomeId)
}

restrictBenchmarkCohortsToObservableRiskWindow <- function(cohorts, riskWindowStart) {
  if (any(is.na(cohorts$daysToObsEnd))) {
    stop("plpData$cohorts$daysToObsEnd must not contain missing values")
  }
  observableRows <- getBenchmarkObservableWindowEnd(cohorts) >= riskWindowStart
  if (!any(observableRows)) {
    stop("No cohort rows have observation time at riskWindowStart")
  }
  if (sum(!observableRows) > 0) {
    warning(sprintf(
      "Excluding %s cohort row(s) without observable time at riskWindowStart",
      sum(!observableRows)
    ))
  }
  cohorts[observableRows, , drop = FALSE]
}

getBenchmarkObservableWindowEnd <- function(cohorts, riskWindowEnd = NULL) {
  observableEnd <- cohorts$daysToObsEnd
  if ("daysToCohortEnd" %in% colnames(cohorts)) {
    if (any(is.na(cohorts$daysToCohortEnd))) {
      stop("plpData$cohorts$daysToCohortEnd must not contain missing values")
    }
    if (max(cohorts$daysToCohortEnd) > 0) {
      observableEnd <- pmin(observableEnd, cohorts$daysToCohortEnd)
    }
  }
  if (!is.null(riskWindowEnd)) {
    observableEnd <- pmin(riskWindowEnd, observableEnd)
  }
  observableEnd
}

resampleBenchmarkCovariateData <- function(covariateData, rowMap, populationSize, cohortId) {
  covariateData$benchmarkRowMap <- rowMap
  on.exit(covariateData$benchmarkRowMap <- NULL, add = TRUE)

  if ("analysisRef" %in% names(covariateData)) {
    result <- Andromeda::andromeda(
      covariateRef = covariateData$covariateRef,
      analysisRef = covariateData$analysisRef
    )
  } else {
    covariateRef <- covariateData$covariateRef %>% dplyr::collect()
    result <- Andromeda::andromeda(
      covariateRef = covariateRef,
      analysisRef = data.frame(analysisId = unique(covariateRef$analysisId))
    )
  }

  result$covariates <- covariateData$covariates %>%
    dplyr::inner_join(covariateData$benchmarkRowMap, by = c("rowId" = "sourceRowId")) %>%
    dplyr::transmute(
      rowId = .data$newRowId,
      covariateId = .data$covariateId,
      covariateValue = .data$covariateValue
    )
  if (inherits(result, "RSQLiteConnection")) {
    Andromeda::createIndex(
      tbl = result$covariates,
      columnNames = "rowId",
      indexName = "covariates_rowId"
    )
    Andromeda::createIndex(
      tbl = result$covariates,
      columnNames = "covariateId",
      indexName = "covariates_covariateId"
    )
  }

  class(result) <- "CovariateData"
  attr(class(result), "package") <- "FeatureExtraction"
  metaData <- attr(covariateData, "metaData")
  if (is.null(metaData)) {
    metaData <- list()
  }
  metaData$populationSize <- populationSize
  metaData$cohortIds <- cohortId
  metaData$cohortId <- cohortId
  attr(result, "metaData") <- metaData
  return(result)
}

calculateBenchmarkTruth <- function(plpData, covariateData, population, rowMap, outcomeModel, targetOutcomeRate) {
  if (identical(outcomeModel$type, "plpModel")) {
    linearPredictor <- calculateBenchmarkTruthFromPlpModel(
      plpData = plpData,
      covariateData = covariateData,
      population = population,
      plpModel = outcomeModel$model
    )
  } else {
    linearPredictor <- calculateBenchmarkTruthFromCoefficients(
      covariateData = covariateData,
      rowMap = rowMap,
      coefficients = outcomeModel$coefficients
    )
  }
  linearPredictor <- adjustBenchmarkLinearPredictor(
    linearPredictor = linearPredictor,
    targetOutcomeRate = targetOutcomeRate
  )
  interceptAdjustment <- attr(linearPredictor, "interceptAdjustment")

  data.frame(
    rowId = rowMap$newRowId,
    sourceRowId = rowMap$sourceRowId,
    sourceSubjectId = rowMap$sourceSubjectId,
    linearPredictor = as.numeric(linearPredictor),
    trueRisk = as.numeric(stats::plogis(linearPredictor)),
    interceptAdjustment = interceptAdjustment
  )
}

calculateBenchmarkTruthFromCoefficients <- function(covariateData, rowMap, coefficients) {
  outcomeModel <- coefficients
  intercept <- outcomeModel[names(outcomeModel) == "(Intercept)"]
  if (length(intercept) == 0) intercept <- 0
  covariateCoefficients <- outcomeModel[names(outcomeModel) != "(Intercept)"]

  linearPredictor <- rep(as.numeric(intercept[1]), nrow(rowMap))
  if (length(covariateCoefficients) > 0) {
    covariateData$benchmarkCoefficients <- data.frame(
      covariateId = as.numeric(names(covariateCoefficients)),
      beta = as.numeric(covariateCoefficients)
    )
    on.exit(covariateData$benchmarkCoefficients <- NULL, add = TRUE)

    rowContributions <- covariateData$covariates %>%
      dplyr::inner_join(covariateData$benchmarkCoefficients, by = "covariateId") %>%
      dplyr::mutate(contribution = .data$covariateValue * .data$beta) %>%
      dplyr::group_by(.data$rowId) %>%
      dplyr::summarise(contribution = sum(.data$contribution, na.rm = TRUE)) %>%
      dplyr::collect()
    if (nrow(rowContributions) > 0) {
      linearPredictor[rowContributions$rowId] <- linearPredictor[rowContributions$rowId] + rowContributions$contribution
    }
  }
  linearPredictor
}

calculateBenchmarkTruthFromPlpModel <- function(plpData, covariateData, population, plpModel) {
  predictionData <- plpData
  predictionData$cohorts <- population
  predictionData$covariateData <- covariateData
  predictionData$outcomes <- data.frame(
    rowId = integer(),
    outcomeId = integer(),
    outcomeCount = integer(),
    daysToEvent = integer()
  )
  prediction <- predictPlp(
    plpModel = plpModel,
    plpData = predictionData,
    population = population
  )
  prediction <- as.data.frame(prediction)
  if (!"rowId" %in% colnames(prediction)) {
    stop("predictPlp() did not return a rowId column for outcomeModel")
  }
  prediction <- prediction[match(population$rowId, prediction$rowId), , drop = FALSE]
  if (any(is.na(prediction$rowId))) {
    stop("predictPlp() did not return predictions for every simulated row")
  }
  if ("rawValue" %in% colnames(prediction)) {
    linearPredictor <- prediction$rawValue
  } else if ("value" %in% colnames(prediction)) {
    if (anyNA(prediction$value) || any(prediction$value < 0 | prediction$value > 1)) {
      stop("predictPlp() returned values outside [0, 1] and no rawValue column for outcomeModel")
    }
    risk <- pmin(pmax(prediction$value, 1e-15), 1 - 1e-15)
    linearPredictor <- stats::qlogis(risk)
  } else {
    stop("predictPlp() must return rawValue or value for outcomeModel")
  }
  if (anyNA(linearPredictor) || any(!is.finite(linearPredictor))) {
    stop("predictPlp() returned non-finite linear predictors for outcomeModel")
  }
  linearPredictor
}

adjustBenchmarkLinearPredictor <- function(linearPredictor, targetOutcomeRate) {
  interceptAdjustment <- 0
  if (!is.null(targetOutcomeRate)) {
    if (any(!is.finite(linearPredictor))) {
      stop("Cannot target outcome rate because the linear predictor contains non-finite values")
    }
    targetLogit <- stats::qlogis(targetOutcomeRate)
    rootInterval <- c(
      targetLogit - max(linearPredictor) - 20,
      targetLogit - min(linearPredictor) + 20
    )
    interceptAdjustment <- stats::uniroot(
      f = function(adjustment) mean(stats::plogis(linearPredictor + adjustment)) - targetOutcomeRate,
      interval = rootInterval
    )$root
    linearPredictor <- linearPredictor + interceptAdjustment
  }
  attr(linearPredictor, "interceptAdjustment") <- interceptAdjustment
  linearPredictor
}
