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
