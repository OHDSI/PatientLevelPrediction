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

test_that("createPreprocessSettings stores its inputs and checks them", {
  # fixed values that differ from the defaults, so a failure can be reproduced
  normalizeDataTest <- FALSE
  removeRedundancyTest <- FALSE
  minCovariateFractionTest <- 1 / 1500

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

test_that("preprocessData removes infrequent covariates and records its settings", {
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
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

  # fewer covariate rows remain, so some infrequent covariates were removed
  expect_gt(length(attr(newData, "metaData")$tidyCovariateDataSettings$deletedInfrequentCovariateIds), 0)
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
  skip_if_no_eunomia()
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
  covs <- asCovariateData(covs)
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

# The tidy settings preprocessData() stores for createTemporalData(): the
# covariate is kept and normalized by its maximum of 1 ("preprocessData carries
# recorded temporal deletions into the tidy settings" checks that they match).
# FeatureExtraction removes redundant covariateId/timeId pairs in temporal data;
# the settings carry them as deletedRedundantCovariateTimeIds once it records
# them (here time 1, which training dropped).
temporalTidySettings <- function(recordDeletions = TRUE) {
  settings <- list(
    populationSize = 3,
    cohortIds = 1,
    deletedInfrequentCovariateIds = numeric(0),
    normFactors = data.frame(covariateId = 1001, maxValue = 1)
  )
  if (recordDeletions) {
    settings$deletedRedundantCovariateTimeIds <- data.frame(covariateId = 1001, timeId = 1)
  }
  settings
}

test_that("recorded temporal covariate time deletions are replayed at prediction", {
  newData <- createTemporalData()
  on.exit(Andromeda::close(newData), add = TRUE)

  replayed <- applyTidyCovariateData(newData, temporalTidySettings())

  # the time point training kept
  expect_equal(as.data.frame(replayed$covariates)$timeId, 2)
})

test_that("recorded temporal deletions survive a JSON round trip of the settings", {
  settings <- temporalTidySettings()
  jsonFile <- tempfile(fileext = ".json")
  on.exit(unlink(jsonFile), add = TRUE)
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
  settings <- attr(trained, "metaData")$tidyCovariateDataSettings
  expect_equal(
    settings$deletedRedundantCovariateTimeIds,
    data.frame(covariateId = 1001, timeId = 1)
  )
  # the hand-built settings the replay tests use
  settings$normFactors <- as.data.frame(settings$normFactors)
  expect_equal(settings[names(temporalTidySettings())], temporalTidySettings())
  expect_setequal(names(settings), names(temporalTidySettings()))
  # training dropped time 1
  expect_equal(as.data.frame(trained$covariates)$timeId, 2)
})

test_that("tidy settings without recorded temporal deletions replay as before", {
  newData <- createTemporalData()
  on.exit(Andromeda::close(newData), add = TRUE)
  replayed <- applyTidyCovariateData(newData, temporalTidySettings(recordDeletions = FALSE))

  # only whole-covariate deletions are replayed, so every time point stays
  expect_equal(nrow(as.data.frame(replayed$covariates)), 4)
})

test_that("recorded covariate time deletions are ignored for non-temporal data", {
  covariateData <- makeCovariateData(
    covariates = data.frame(rowId = 1:2, covariateId = 1, covariateValue = c(1, 2)),
    analysisRef = defaultAnalysisRef(data.frame(analysisId = 1), missingMeansZero = "N"),
    metaData = list(populationSize = 2)
  )
  on.exit(Andromeda::close(covariateData), add = TRUE)

  # as preprocessData() stores them for this data without removing anything
  settings <- list(
    populationSize = 2,
    deletedInfrequentCovariateIds = numeric(0),
    normFactors = data.frame(covariateId = 1, maxValue = 2),
    deletedRedundantCovariateTimeIds = data.frame(covariateId = 1, timeId = 1)
  )

  replayed <- applyTidyCovariateData(covariateData, settings)
  expect_equal(nrow(as.data.frame(replayed$covariates)), 2)
})

test_that("tidying in place gives the same covariates as tidying into new data", {
  settings <- temporalTidySettings()
  settings$normFactors <- data.frame(covariateId = 1001, maxValue = 2)
  separate <- createTemporalData()
  on.exit(Andromeda::close(separate), add = TRUE)
  owned <- createTemporalData()
  on.exit(Andromeda::close(owned), add = TRUE)

  tidied <- applyTidyCovariateData(separate, settings)
  on.exit(Andromeda::close(tidied), add = TRUE)
  inPlace <- applyTidyCovariateData(owned, settings, inPlace = TRUE)

  sortedRows <- function(x) {
    x <- as.data.frame(x)
    x <- x[do.call(order, x[c("rowId", "covariateId", "timeId")]), ]
    rownames(x) <- NULL
    x
  }
  # time 1 is deleted and the values are divided by the normalization factor
  expect_equal(sortedRows(tidied$covariates)$timeId, 2)
  expect_equal(sortedRows(tidied$covariates)$covariateValue, 0.5)
  expect_equal(sortedRows(inPlace$covariates), sortedRows(tidied$covariates))
  expect_equal(as.data.frame(inPlace$covariateRef), as.data.frame(tidied$covariateRef))
  # in place rewrites the given data; otherwise it is left as it was
  expect_identical(inPlace, owned)
  expect_equal(nrow(as.data.frame(separate$covariates)), 4)
})
