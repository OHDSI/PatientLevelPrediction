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

#' Create simulation profile using eunomia data and a fixed outcome model
#'
#' @param plpData An object of type \code{plpData} as generated using the \cr\code{getPlpData} function.'
#' @return An object of type \code{plpDataSimulationProfile}.
#' @keywords internal
#' @noRd
createSimulationProfile <- function(plpData) {
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
      sd = sd(.data$covariateValue, na.rm = TRUE),
      min = min(.data$covariateValue, na.rm = TRUE),
      max = max(.data$covariateValue, na.rm = TRUE)
    ) %>%
    dplyr::collect()

  outcomeRate <- nrow(plpData$outcomes) / nrow(plpData$cohorts)
  writeLines("Fitting outcome model(s)")
  outcomeModels <- vector("list", length(plpData$metaData$databaseDetails$outcomeIds))
  for (i in seq_along(plpData$metaData$databaseDetails$outcomeIds)) {
   coefficients <- c("(Intercept)" = -2, 
       "1002" = 0.04,  # age
       "8532" = 0.5,   # male 
       "81893102" = 0.5, # ulcerative colitis
       "4266809102" = 1.0, # Diverticular disease
       "4310024102" = 1.5, #Angiodysplasia of stomach
       "1112807402" = 0.7, # Aspirin
       "1177480402" = 0.6, # Ibuprofen
       "439777102" = 0.8)  # Anemia
    outcomeModels[[i]] <- coefficients
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
    metaData = plpData$metaData,
    covariateRef = plpData$covariateData$covariateRef %>% dplyr::collect()
  )
  class(result) <- "plpDataSimulationProfile"
  return(result)
}
#' Generate simulated data
#'
#' @description
#' \code{simulateplpData} creates a plpData object with simulated data.
#'
#' @param plpDataSimulationProfile   An object of type \code{plpDataSimulationProfile} as generated
#'                                   using the \cr\code{createplpDataSimulationProfile} function.
#' @param n                          The size of the population to be generated.
#'
#' @details
#' This function generates simulated data that is in many ways similar to the original data on which
#' the simulation profile is based.
#'
#' @return
#' An object of type \code{plpData}.
#'
#' @export
simulatePlpData <- function(plpDataSimulationProfile, n = 10000) {
  # Note: currently, simulation is done completely in-memory. Could easily do batch-wise
  writeLines("Generating covariates")
  covariatePrevalence <- plpDataSimulationProfile$covariateInfo$covariatePrevalence

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

  continuousCovariates <- plpDataSimulationProfile$covariateInfo$continuousCovariates
  continuousCovSim <- do.call(rbind, lapply(seq_len(nrow(continuousCovariates)), function(i) {
    info <- continuousCovariates[i, ]
    # Generate a value for every subject:
    simValues <- round(rnorm(n, mean = info$mean, sd = info$sd))
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
  outcomeIds <- plpDataSimulationProfile$metaData$databaseDetails$outcomeIds
  timeMax <- max(plpDataSimulationProfile$timeMax)
  for (i in seq_along(outcomeIds)) {
    coefficients <- data.frame(
      betas = as.numeric(plpDataSimulationProfile$outcomeModels[[i]]),
      covariateIds = names(plpDataSimulationProfile$outcomeModels[[i]])
    )
    prediction <- predictCyclopsType(coefficients,
      cohorts,
      covariateData,
      modelType = "logistic"
    )
    outcomes <- prediction
    outcomes$outcomeCount <- rbinom(nrow(outcomes), size = 1, prob = outcomes$value)
    outcomes <- outcomes[outcomes$outcomeCount != 0, ]
    outcomes$outcomeId <- outcomeIds[i]
    outcomes$daysToEvent <- round(stats::runif(nrow(outcomes), 0, timeMax))
    outcomes <- outcomes[, c("rowId", "outcomeId", "outcomeCount", "daysToEvent")]
    allOutcomes[[i]] <- outcomes
  }
  allOutcomes <- dplyr::bind_rows(allOutcomes)

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
    covariateData = covariateData,
    timeRef = NULL,
    metaData = metaData
  )

  class(result) <- "plpData"
  return(result)
}
