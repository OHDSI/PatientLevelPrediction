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
createDefaultSettings <- function(
    minCovariateFraction = 0.001,
    normalizeData = TRUE,
    removeRedundancy = TRUE) {
  result <- createPreprocessSettings(
    minFraction = minCovariateFraction,
    normalize = normalizeData,
    removeRedundancy = removeRedundancy
  )
  return(result)
}

test_that("createPreprocessSettings", {
  normalizeDataTest <- sample(c(TRUE, FALSE), 1)
  removeRedundancyTest <- sample(c(TRUE, FALSE), 1)
  minCovariateFractionTest <- 1 / (1000 + sample(1000, 1))

  settings <- createDefaultSettings(
    minCovariateFraction = minCovariateFractionTest,
    normalizeData = normalizeDataTest,
    removeRedundancy = removeRedundancyTest
  )

  expect_s3_class(settings, "preprocessSettings")
  expect_equal(settings$minFraction, minCovariateFractionTest)
  expect_equal(settings$normalize, normalizeDataTest)
  expect_equal(settings$removeRedundancy, removeRedundancyTest)

  expect_error(createDefaultSettings(minCovariateFraction = -1))
  expect_error(createDefaultSettings(minCovariateFraction = "dfdfdf"))

  expect_error(createDefaultSettings(removeRedundancy = "dfdfdf"))
  expect_error(createDefaultSettings(removeRedundancy = NULL))

  expect_error(createDefaultSettings(normalizeData = "dfdfdf"))
  expect_error(createDefaultSettings(normalizeData = NULL))
})

test_that("createPreprocessSettings", {
  skip_if_not_installed("Eunomia")
  skip_if_offline()
  attr(trainData$covariateData, "metaData")$preprocessSettings <- NULL # removing for test
  metaData <- attr(trainData$covariateData, "metaData")
  metaLength <- length(metaData)
  covSize <- trainData$covariateData$covariates %>%
    dplyr::tally() %>%
    dplyr::pull()
  oldFeatureCount <- trainData$covariateData$covariateRef %>%
    dplyr::tally() %>%
    dplyr::pull()

  preprocessSettings <- createDefaultSettings(
    minCovariateFraction = 0.01,
    normalizeData = FALSE,
    removeRedundancy = FALSE
  )
  newData <- preprocessData(trainData$covariateData, preprocessSettings)

  expect_s4_class(newData, "CovariateData")
  expect_true(newData$covariates %>% dplyr::tally() %>% dplyr::pull() < covSize)

  # metaData should have tidyCovariateDataSettings + preprocessSettings (so 2 bigger)
  expect_equal(length(attr(newData, "metaData")), metaLength + 2)

  expect_true(length(attr(newData, "metaData")$tidyCovariateDataSettings$deletedInfrequentCovariateIds) >= 0)
  expect_equal(attr(newData, "metaData")$tidyCovariateDataSettings$deletedRedundantCovariateIds, NULL)
  expect_equal(attr(newData, "metaData")$tidyCovariateDataSettings$normFactors, NULL)

  newFeatureCount <- newData$covariateRef %>%
    dplyr::tally() %>%
    dplyr::pull() + length(attr(newData, "metaData")$tidyCovariateDataSettings$deletedInfrequentCovariateIds)

  expect_equal(newFeatureCount, oldFeatureCount)

  oldFeatureCount <- trainData$covariateData$covariateRef %>%
    dplyr::tally() %>%
    dplyr::pull()

  metaData <- attr(trainData$covariateData, "metaData")
  preprocessSettings <- createDefaultSettings(
    minCovariateFraction = 0,
    normalizeData = TRUE,
    removeRedundancy = TRUE
  )
  newData <- preprocessData(trainData$covariateData, preprocessSettings)
  expect_true(length(attr(newData, "metaData")$tidyCovariateDataSettings$deletedInfrequentCovariateIds) == 0)
  expect_true(length(attr(newData, "metaData")$tidyCovariateDataSettings$deletedRedundantCovariateIds) >= 0)
  expect_true(length(attr(newData, "metaData")$tidyCovariateDataSettings$normFactors) >= 0)

  newFeatureCount <- newData$covariateRef %>%
    dplyr::tally() %>%
    dplyr::pull() +
    length(attr(newData, "metaData")$tidyCovariateDataSettings$deletedRedundantCovariateIds)

  expect_equal(newFeatureCount, oldFeatureCount) # sometimes differ?

  # check settings are saved
  expect_equal(attr(newData, "metaData")$preprocessSettings, preprocessSettings)
})

test_that("Did tidy on test", {
  skip_if_not_installed("Eunomia")
  skip_if_offline()
  expect_true(attr(plpResult$prediction, "metaData")$tidyCovariates)
})

test_that("apply tidy with temporal data", { 
  covs <- Andromeda::andromeda(
    covariates = data.frame(
      covariateId = c(1, 2, 1, 2, 2),
      rowId = c(1, 1, 2, 2, 2),
      covariateValue = c(1, 1, 1, 1, 1),
      timeId = c(1, 1, 1, 1, 2)
    ),
    covariateRef = data.frame(
      covariateId = c(1, 2),
      covariateName = c("cov1", "cov2"),
      analysisId = c(1, 2)
    ),
    analysisRef = data.frame(
      analysisId = c(1, 2),
      analysisName = c("analysis1", "analysis2")
    ),
    timeRef = data.frame(
      timePart = "day",
      timeInterval = 1,
      sequenceStartDay = 0,
      sequenceEndDay = 1
    )
  )
  class(covs) <- "CovariateData"
  preprocessSettings <- list(
    populationSize = 2,
    cohortIds = -1,
    deletedRedundantCovariateIds = c(1))

  tidied <- applyTidyCovariateData(covs, preprocessSettings)

  expect_equal(tidied$covariates %>% dplyr::pull(.data$covariateId) %>% dplyr::n_distinct(), 1)
  expect_true(!is.null(tidied$timeRef))
}) 

createTemporalData <- function() {
  covariateData <- FeatureExtraction::createEmptyCovariateData(1, FALSE, TRUE)
  covariateData$covariates <- data.frame(
    cohortDefinitionId = 1,
    rowId = c(1, 2, 3, 1),
    covariateId = 1001,
    covariateValue = 1,
    timeId = c(1, 1, 1, 2)
  )
  covariateData$covariateRef <- data.frame(
    covariateId = 1001,
    covariateName = "Binary feature",
    analysisId = 1,
    conceptId = 100
  )
  covariateData$analysisRef <- data.frame(
    analysisId = 1,
    analysisName = "Binary analysis",
    domainId = "Condition",
    startDay = -30,
    endDay = 0,
    isBinary = "Y",
    missingMeansZero = "Y"
  )
  covariateData$timeRef <- data.frame(timeId = 1:2, startDay = c(-30, -15), endDay = c(-16, 0))
  attr(covariateData, "metaData") <- list(populationSize = 3, cohortIds = 1)
  return(covariateData)
}

# FeatureExtraction removes redundant covariateId/timeId pairs in temporal data;
# the tidy settings carry them as deletedRedundantCovariateTimeIds once it records them.
recordedTidySettings <- function() {
  raw <- createTemporalData()
  on.exit(Andromeda::close(raw), add = TRUE)
  trained <- preprocessData(raw, createPreprocessSettings())
  settings <- attr(trained, "metaData")$tidyCovariateDataSettings
  settings$deletedRedundantCovariateTimeIds <- data.frame(covariateId = 1001, timeId = 1)
  list(settings = settings, trainedTimeIds = as.data.frame(trained$covariates)$timeId)
}

test_that("recorded temporal covariate time deletions are replayed at prediction", {
  recorded <- recordedTidySettings()
  newData <- createTemporalData()
  on.exit(Andromeda::close(newData), add = TRUE)

  replayed <- applyTidyCovariateData(newData, recorded$settings)

  expect_equal(as.data.frame(replayed$covariates)$timeId, 2)
  expect_equal(as.data.frame(replayed$covariates)$timeId, recorded$trainedTimeIds)
})

test_that("recorded temporal deletions survive a JSON round trip of the settings", {
  settings <- recordedTidySettings()$settings
  jsonFile <- tempfile(fileext = ".json")
  on.exit(unlink(jsonFile), add = TRUE)
  settings$normFactors <- as.data.frame(settings$normFactors)
  ParallelLogger::saveSettingsToJson(settings, jsonFile)
  loaded <- ParallelLogger::loadSettingsFromJson(jsonFile)

  newData <- createTemporalData()
  on.exit(Andromeda::close(newData), add = TRUE)
  replayed <- applyTidyCovariateData(newData, loaded)

  expect_equal(as.data.frame(replayed$covariates)$timeId, 2)
})

test_that("preprocessData carries recorded temporal deletions into the tidy settings", {
  raw <- createTemporalData()
  on.exit(Andromeda::close(raw), add = TRUE)
  originalTidy <- FeatureExtraction::tidyCovariateData
  tidyWithRecord <- function(...) {
    result <- originalTidy(...)
    metaData <- attr(result, "metaData")
    metaData$deletedRedundantCovariateTimeIds <- data.frame(covariateId = 1001, timeId = 1)
    attr(result, "metaData") <- metaData
    result
  }
  trained <- with_mocked_bindings(
    preprocessData(raw, createPreprocessSettings()),
    tidyCovariateData = tidyWithRecord,
    .package = "FeatureExtraction"
  )
  expect_equal(
    attr(trained, "metaData")$tidyCovariateDataSettings$deletedRedundantCovariateTimeIds,
    data.frame(covariateId = 1001, timeId = 1)
  )
})

test_that("tidy settings without recorded temporal deletions replay as before", {
  raw <- createTemporalData()
  on.exit(Andromeda::close(raw), add = TRUE)
  trained <- preprocessData(raw, createPreprocessSettings())
  settings <- attr(trained, "metaData")$tidyCovariateDataSettings
  settings$deletedRedundantCovariateTimeIds <- NULL

  newData <- createTemporalData()
  on.exit(Andromeda::close(newData), add = TRUE)
  replayed <- applyTidyCovariateData(newData, settings)

  # only whole-covariate deletions are replayed, so every time point stays
  expect_equal(nrow(as.data.frame(replayed$covariates)), 4)
})

test_that("recorded covariate time deletions are ignored for non-temporal data", {
  covariateData <- Andromeda::andromeda(
    covariates = data.frame(rowId = 1:2, covariateId = 1, covariateValue = c(1, 2)),
    covariateRef = data.frame(covariateId = 1, covariateName = "a", analysisId = 1, conceptId = 0),
    analysisRef = data.frame(analysisId = 1, analysisName = "a", domainId = "d", startDay = 0, endDay = 0, isBinary = "N", missingMeansZero = "N")
  )
  class(covariateData) <- "CovariateData"
  attr(covariateData, "metaData") <- list(populationSize = 2)
  on.exit(Andromeda::close(covariateData), add = TRUE)

  trained <- preprocessData(covariateData, createPreprocessSettings(minFraction = 0, removeRedundancy = FALSE))
  settings <- attr(trained, "metaData")$tidyCovariateDataSettings
  settings$deletedRedundantCovariateTimeIds <- data.frame(covariateId = 1, timeId = 1)

  replayed <- applyTidyCovariateData(covariateData, settings)
  expect_equal(nrow(as.data.frame(replayed$covariates)), 2)
})
