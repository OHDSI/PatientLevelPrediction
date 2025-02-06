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

test_that("createNormalizer works", {
  normalizer <- createNormalizer(type = "minmax")
  expect_equal(normalizer$type, "minmax")
  expect_equal(attr(normalizer, "fun"), "minMaxNormalize")
  expect_s3_class(normalizer, "featureEngineeringSettings")

  normalizer <- createNormalizer(type = "robust", settings = list(clip = TRUE))
  expect_equal(normalizer$type, "robust")

  expect_error(createNormalizer(type = "mean"))
  expect_error(createNormalizer(type = "median"))
  expect_error(createNormalizer(type = "zscore"))
  expect_error(createNormalizer(type = "none"))
  expect_error(createNormalizer(
    type = "robust",
    settings = list(clip = 3)
  ))
})

test_that("normalization works", {
  skip_if_not_installed("RSQLite") # until Andromeda 1.0.0 is released
  addFeature <- function(data, covariateId, minValue,
                         maxValue, outliers = FALSE) {
    data$covariateData <- Andromeda::copyAndromeda(data$covariateData)
    nSubjects <- nrow(data$labels)
    Andromeda::appendToTable(
      data$covariateData$covariates,
      data.frame(
        rowId = data$labels$rowId,
        covariateId = rep(covariateId, nSubjects),
        covariateValue = runif(nSubjects, minValue, maxValue)
      )
    )
    if (outliers) {
      nOutliers <- floor(nSubjects / 10)
      outlierRows <- sample(data$labels$rowId, nOutliers)
      outlierValues <- runif(nOutliers, minValue * 10, maxValue * 10)
      outlierData <- data.frame(
        rowId = outlierRows,
        covariateId = covariateId,
        covariateValue = outlierValues
      )
      data$covariateData$covariates <- data$covariateData$covariates %>%
        dplyr::filter(!(.data$rowId %in% outlierRows &
          .data$covariateId == covariateId))
      Andromeda::appendToTable(
        data$covariateData$covariates,
        outlierData
      )
    }

    Andromeda::appendToTable(
      data$covariateData$covariateRef,
      data.frame(
        covariateId = covariateId,
        covariateName = "testCovariate",
        analysisId = 999,
        conceptId = 1
      )
    )
    Andromeda::appendToTable(
      data$covariateData$analysisRef,
      data.frame(
        analysisId = 999,
        analysisName = "testAnalysis",
        domainId = "testDomain",
        startDay = 0,
        endDay = 0,
        isBinary = "N",
        missingMeansZero = "N"
      )
    )
    return(data)
  }

  testNormalizer <- function(data, testData, normalizer, outliers = FALSE) {
    newData <- addFeature(data, 12101, -10, 10, outliers = outliers)
    newTestData <- addFeature(testData, 12101, -10, 10, outliers = outliers)
    if (normalizer$type == "minmax") {
      normalizeFn <- minMaxNormalize
      manualFn <- function(data, testData) {
        testFeature <- testData$covariateData$covariates %>%
          dplyr::filter(.data$covariateId == 12101) %>%
          dplyr::pull(.data$covariateValue)
        trainMin <- min(data$covariateData$covariates %>% dplyr::filter(.data$covariateId == 12101) %>% dplyr::pull(.data$covariateValue))
        trainMax <- max(data$covariateData$covariates %>% dplyr::filter(.data$covariateId == 12101) %>% dplyr::pull(.data$covariateValue))
        testNormFeature <- (testFeature - trainMin) / (trainMax - trainMin)
        return(testNormFeature)
      }
      minLimit <- 0
      maxLimit <- 1
    } else if (normalizer$type == "robust") {
      normalizeFn <- robustNormalize
      manualFn <- function(data, testData) {
        testFeature <- testData$covariateData$covariates %>%
          dplyr::filter(.data$covariateId == 12101) %>%
          dplyr::pull(.data$covariateValue)
        trainMedian <- median(data$covariateData$covariates %>%
          dplyr::filter(.data$covariateId == 12101) %>%
          dplyr::pull(.data$covariateValue))
        if (inherits(data$covariateData, "SQLiteConnection")) {
          trainIQR <- IQR(data$covariateData$covariates %>%
            dplyr::filter(.data$covariateId == 12101) %>%
            dplyr::pull(.data$covariateValue), type = 1)
        } else {
          trainIQR <- IQR(data$covariateData$covariates %>%
            dplyr::filter(.data$covariateId == 12101) %>%
            dplyr::pull(.data$covariateValue))
        }
        testNormFeature <- (testFeature - trainMedian) / trainIQR
        if (clip) {
          testNormFeature <- testNormFeature / sqrt(1 + (testNormFeature / 3)^2)
        }
        return(testNormFeature)
      }
      if (clip) {
        minLimit <- -3
        maxLimit <- 3
      } else {
        minLimit <- -100
        maxLimit <- 100
      }
    } else {
      stop("Unknown normalizer type")
    }
    trainNormalizedData <- normalizeFn(newData, normalizer)
    metaData <- attr(trainNormalizedData$covariateData, "metaData")
    if (normalizer$type == "minmax") {
      testSettings <- metaData$featureEngineering$minMaxNormalize$settings$featureEngineeringSettings
    } else if (normalizer$type == "robust") {
      testSettings <- metaData$featureEngineering$robustNormalize$settings$featureEngineeringSettings
    }
    testNormalizedData <- normalizeFn(newTestData, testSettings, done = TRUE)

    trainFeature <- trainNormalizedData$covariateData$covariates %>%
      dplyr::filter(.data$covariateId == 12101) %>%
      dplyr::pull(.data$covariateValue)
    expect_true(all(trainFeature >= minLimit) && all(trainFeature <= maxLimit))
    newTestFeature <- testNormalizedData$covariateData$covariates %>%
      dplyr::filter(.data$covariateId == 12101) %>%
      dplyr::pull(.data$covariateValue)
    testNormFeature <- manualFn(newData, newTestData)
    expect_equal(newTestFeature, testNormFeature)
  }
  normalizer <- createNormalizer(type = "minmax")
  testNormalizer(tinyTrainData, testData, normalizer)

  normalizer <- createNormalizer(type = "robust", settings = list(clip = TRUE))
  clip <- TRUE
  testNormalizer(tinyTrainData, testData, normalizer, outliers = TRUE)

  normalizer <- createNormalizer(type = "robust", settings = list(clip = FALSE))
  clip <- FALSE
  testNormalizer(tinyTrainData, testData, normalizer, outliers = TRUE)
})

