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
  skip_if_no_eunomia()
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
  skip_if_no_eunomia()
  # a model on two covariates keeps pfi() quick
  tinyResults <- runPlp(
    plpData = nanoData,
    populationSettings = populationSettings,
    outcomeId = outcomeId,
    analysisId = "tinyFit",
    executeSettings = createExecuteSettings(
      runSplitData = TRUE,
      runSampleData = FALSE,
      runFeatureEngineering = FALSE,
      runPreprocessData = TRUE,
      runModelDevelopment = TRUE,
      runCovariateSummary = FALSE
    ),
    saveDirectory = file.path(saveLoc, "tinyResults")
  )
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
  covariateData <- makeCovariateData(covariates, covariateRef, analysisRef)
  withr::defer(Andromeda::close(covariateData), envir = env)
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

test_that("permute keeps unrelated predictors when permuting a categorical block", {
  n <- 100
  population <- data.frame(rowId = 1:n)
  ageGroup <- data.frame(
    rowId = 1:n,
    covariateId = ifelse(1:n <= 30, 1003, ifelse(1:n <= 70, 2003, 3003)),
    covariateValue = 1
  )
  unrelated <- data.frame(rowId = 1:n, covariateId = 5002, covariateValue = 2:(n + 1))
  location <- permutePlpDataLocation(rbind(ageGroup, unrelated), ageGroupRef(), ageGroupAnalysisRef(), n)

  for (seed in 1:5) {
    set.seed(seed)
    permuted <- permutedCovariates(location, 1003, population)
    keptOther <- permuted[permuted$covariateId == 5002, ]
    expect_equal(keptOther$covariateValue[order(keptOther$rowId)], 2:(n + 1))

    block <- permuted[permuted$covariateId %in% c(1003, 2003, 3003), ]
    expect_equal(sum(block$covariateId == 1003), 30)
    expect_equal(sum(block$covariateId == 2003), 40)
    expect_equal(sum(block$covariateId == 3003), 30)
    expect_equal(max(table(block$rowId)), 1)
    expect_equal(nrow(block), n)
  }
})

test_that("permute handles missing category rows and holders outside the population", {
  n <- 100
  population <- data.frame(rowId = 1:60)
  # rows 91:100 have no race row at all; rows 61:90 are outside the evaluated population
  race <- data.frame(
    rowId = 1:90,
    covariateId = ifelse(1:90 <= 30 | 1:90 > 75, 1003, 2003),
    covariateValue = 1
  )
  unrelated <- data.frame(rowId = 1:100, covariateId = 5002, covariateValue = 1)
  ref <- ageGroupRef()
  ref$analysisId[ref$covariateId %in% c(1003, 2003, 3003)] <- 4
  location <- permutePlpDataLocation(rbind(race, unrelated), ref, ageGroupAnalysisRef(), n)

  set.seed(4)
  permuted <- permutedCovariates(location, 1003, population)
  block <- permuted[permuted$covariateId %in% c(1003, 2003), ]
  inPop <- block[block$rowId %in% population$rowId, ]
  # only the population's own 30 + 30 category rows move
  expect_equal(sum(inPop$covariateId == 1003), 30)
  expect_equal(sum(inPop$covariateId == 2003), 30)
  expect_equal(max(table(inPop$rowId)), 1)
  # holders outside the evaluated population keep their rows
  outside <- block[block$rowId > 60, ]
  expect_equal(sort(outside$rowId), 61:90)
  expect_equal(unname(outside$covariateId[order(outside$rowId)]), ifelse(61:90 > 75, 1003, 2003))
  expect_equal(sum(permuted$covariateId == 5002), 100)
})

test_that("permute leaves a categorical block unchanged when nobody needs to swap", {
  n <- 20
  population <- data.frame(rowId = 1:n)
  location <- permutePlpDataLocation(
    data.frame(rowId = 1:n, covariateId = 1003, covariateValue = 1),
    ageGroupRef(), ageGroupAnalysisRef(), n
  )
  set.seed(1)
  permuted <- permutedCovariates(location, 1003, population)
  expect_equal(sort(permuted$rowId), 1:n)
  expect_equal(unique(permuted$covariateId), 1003)
})
