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


test_that("pfi feature importance returns data.frame", {
  skip_if_offline()
  # limit to a sample of 2 covariates for faster test
  covariates <- plpResult$model$covariateImportance %>%
    dplyr::filter("covariateValue" != 0) %>%
    dplyr::select("covariateId") %>%
    dplyr::arrange(desc("covariateValue")) %>%
    dplyr::pull()

  # if the model had non-zero covariates
  if (length(covariates) > 0) {
    if (length(covariates) > 2) {
      covariates <- covariates[1:2]
    }
    pfiTest <- pfi(plpResult, population, plpData,
      repeats = 1,
      covariates = covariates, cores = 1, log = NULL,
      logthreshold = "INFO"
    )

    expect_equal(class(pfiTest), "data.frame")
    expect_equal(sum(names(pfiTest) %in% c("covariateId", "pfi")), 2)
    expect_true(all(!is.nan(pfiTest$pfi)))
  }
})

test_that("pfi feature importance works with logger or without covariates", {
  skip_if_offline()
  pfiTest <- pfi(tinyResults, population, nanoData,
    cores = 1,
    covariates = NULL, log = file.path(tempdir(), "pfiLog")
  )

  expect_equal(class(pfiTest), "data.frame")
  expect_equal(sum(names(pfiTest) %in% c("covariateId", "pfi")), 2)
  expect_true(all(!is.nan(pfiTest$pfi)))
})

# Saves a synthetic plpData so permute() can load it the way pfi() does.
permutePlpDataLocation <- function(covariates, covariateRef, analysisRef, nRows, env = parent.frame()) {
  location <- withr::local_tempdir(.local_envir = env)
  covariateData <- Andromeda::andromeda(
    covariates = covariates,
    covariateRef = covariateRef,
    analysisRef = analysisRef
  )
  withr::defer(Andromeda::close(covariateData), envir = env)
  attr(covariateData, "metaData") <- list()
  class(covariateData) <- "CovariateData"
  plpData <- list(
    covariateData = covariateData,
    cohorts = data.frame(rowId = seq_len(nRows), subjectId = seq_len(nRows), targetId = 1),
    outcomes = data.frame(rowId = integer(), outcomeId = integer(), daysToEvent = integer()),
    timeRef = NULL,
    metaData = list()
  )
  class(plpData) <- "plpData"
  savePlpData(plpData, file = file.path(location, "plpData"))
  file.path(location, "plpData")
}

permutedCovariates <- function(location, cId, population) {
  result <- permute(location, cId, population)
  covariates <- as.data.frame(dplyr::collect(result$covariateData$covariates))
  Andromeda::close(result$covariateData)
  covariates
}

continuousRef <- function(covariateId) {
  data.frame(covariateId = covariateId, covariateName = "c", analysisId = 702, conceptId = 1)
}
continuousAnalysisRef <- function() {
  data.frame(analysisId = 702, analysisName = "a", domainId = "Condition", isBinary = "N", missingMeansZero = "Y")
}

ageGroupRef <- function() {
  data.frame(
    covariateId = c(1003, 2003, 3003, 5002),
    covariateName = c("a", "b", "c", "other"),
    analysisId = c(3, 3, 3, 1),
    conceptId = 0
  )
}
ageGroupAnalysisRef <- function() {
  data.frame(analysisId = c(1, 3), analysisName = c("o", "a"), domainId = "Demographics", isBinary = "Y", missingMeansZero = "Y")
}

test_that("permute only reassigns feature rows of the evaluated population", {
  population <- data.frame(rowId = 1:100)
  covariates <- data.frame(
    rowId = c(1, seq(200, 1000, by = 100)),
    covariateId = 1101,
    covariateValue = 1:10
  )
  location <- permutePlpDataLocation(covariates, continuousRef(1101), continuousAnalysisRef(), 1000)
  for (seed in 1:5) {
    set.seed(seed)
    permuted <- permutedCovariates(location, 1101, population)
    inPopulation <- permuted[permuted$rowId %in% population$rowId, ]
    expect_equal(nrow(inPopulation), 1)
    expect_equal(inPopulation$covariateValue, 1)
  }
})

test_that("permute keeps the empirical distribution of a feature inside the population", {
  n <- 100
  population <- data.frame(rowId = 1:n)
  # 40 population rows carry varied values, plus rows outside the population that must not join in
  covariates <- data.frame(
    rowId = c(1:40, seq(500, 900, by = 100)),
    covariateId = 1101,
    covariateValue = c(seq(1, 40), rep(99, 5))
  )
  location <- permutePlpDataLocation(covariates, continuousRef(1101), continuousAnalysisRef(), 1000)
  set.seed(2)
  permuted <- permutedCovariates(location, 1101, population)
  inPopulation <- permuted[permuted$rowId %in% population$rowId, ]
  expect_equal(sort(inPopulation$covariateValue), seq(1, 40))
  expect_false(99 %in% inPopulation$covariateValue)
})

test_that("permute handles a feature absent from the evaluated population", {
  population <- data.frame(rowId = 1:50)
  covariates <- data.frame(rowId = c(200, 300), covariateId = 1101, covariateValue = 1)
  location <- permutePlpDataLocation(covariates, continuousRef(1101), continuousAnalysisRef(), 500)
  permuted <- permutedCovariates(location, 1101, population)
  expect_equal(sum(permuted$rowId %in% population$rowId), 0)
})
