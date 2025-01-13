# @file simulation.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
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
#' the simulation profile is based. The contains same outcome, comparator, and outcome concept IDs,
#' and the covariates and their 1st order statistics should be comparable.
#'
#' @return
#' An object of type \code{plpData}.
#'
#' @export
simulatePlpData <- function(plpDataSimulationProfile, n = 10000) {
  # Note: currently, simulation is done completely in-memory. Could easily do batch-wise
  writeLines("Generating covariates")
  covariatePrevalence <- plpDataSimulationProfile$covariatePrevalence

  personsPerCov <- stats::rpois(n = length(covariatePrevalence), lambda = covariatePrevalence * n)
  personsPerCov[personsPerCov > n] <- n
  rowId <- sapply(personsPerCov, function(x, n) sample.int(size = x, n), n = n)
  rowId <- do.call("c", rowId)
  covariateIds <- as.numeric(names(covariatePrevalence))
  covariateId <- unlist(sapply(1:length(personsPerCov),
    function(x, personsPerCov, covariateIds) {
      rep(covariateIds[x], personsPerCov[x])
    },
    personsPerCov = personsPerCov,
    covariateIds = covariateIds
  ))

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

  writeLines("Generating cohorts")
  cohorts <- data.frame(rowId = 1:n, subjectId = 2e+10 + (1:n), targetId = 1)
  breaks <- cumsum(plpDataSimulationProfile$timePrevalence)
  r <- stats::runif(n)
  cohorts$time <- as.numeric(as.character(cut(r, breaks = c(0, breaks), labels = names(breaks))))
  cohorts$cohortStartDate <- sample(-1000:1000, n, replace = TRUE) + as.Date("2010-01-01")
  cohorts$daysFromObsStart <- sample(1:1000, n, replace = TRUE)
  cohorts$daysToCohortEnd <- sample(1:1000, n, replace = TRUE)
  cohorts$daysToObsEnd <- cohorts$daysToCohortEnd + sample(1:1000, n, replace = TRUE)
  cohorts$ageYear <- sample(0:95, n, replace = TRUE)
  cohorts$gender <- 8532 # female
  cohorts$gender[sample((1:nrow(cohorts)), nrow(cohorts) / 2)] <- 8507

  writeLines("Generating outcomes")
  allOutcomes <- data.frame()
  for (i in 1:length(plpDataSimulationProfile$metaData$outcomeIds)) {
    coefficients <- data.frame(
      betas = as.numeric(plpDataSimulationProfile$outcomeModels[[i]]),
      covariateIds = names(plpDataSimulationProfile$outcomeModels[[i]])
    )
    prediction <- predictCyclopsType(coefficients,
      cohorts,
      covariateData,
      modelType = "poisson"
    )
    outcomes <- merge(prediction, cohorts[, c("rowId", "time")])
    outcomes$value <- outcomes$value * outcomes$time # Value is lambda
    outcomes$outcomeCount <- as.numeric(stats::rpois(n, outcomes$value))
    outcomes <- outcomes[outcomes$outcomeCount != 0, ]
    outcomes$outcomeId <- plpDataSimulationProfile$metaData$outcomeIds[i]
    outcomes$daysToEvent <- round(stats::runif(nrow(outcomes), 0, outcomes$time))
    outcomes <- outcomes[, c("rowId", "outcomeId", "outcomeCount", "daysToEvent")]
    allOutcomes <- rbind(allOutcomes, outcomes)
  }

  covariateData$coefficients <- NULL

  # add indexes for covariate summary
  Andromeda::createIndex(tbl = covariateData$covariates, columnNames = "rowId", indexName = "covsum_rowId")
  Andromeda::createIndex(tbl = covariateData$covariates, columnNames = "covariateId", indexName = "covsum_covariateId")

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
    outcomeIds = c(2, 3)
  )
  metaData$restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings()
  metaData$covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsAgeGroup = TRUE)


  attrition <- data.frame(
    outcomeId = 2, description = "Simulated data",
    targetCount = nrow(cohorts), uniquePeople = nrow(cohorts),
    outcomes = nrow(outcomes)
  )
  attr(cohorts, "metaData") <- list(
    targetId = 1,
    attrition = attrition
  )

  attr(allOutcomes, "metaData") <- data.frame(outcomeIds = c(2, 3))

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
