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
        trainIQR <- IQR(data$covariateData$covariates %>%
          dplyr::filter(.data$covariateId == 12101) %>%
          dplyr::pull(.data$covariateValue))
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

test_that("normalization with constant column doesn't produce NaNs", {
  addConstantFeature <- function(data, covariateId, value) {
    data$covariateData <- Andromeda::copyAndromeda(data$covariateData)
    nSubjects <- nrow(data$labels)
    Andromeda::appendToTable(
      data$covariateData$covariates,
      data.frame(
        rowId = data$labels$rowId,
        covariateId = rep(covariateId, nSubjects),
        covariateValue = rep(value, nSubjects)
      )
    )
    Andromeda::appendToTable(
      data$covariateData$covariateRef,
      data.frame(
        covariateId = covariateId,
        covariateName = "constantCovariate",
        analysisId = 999,
        conceptId = 1
      )
    )
    Andromeda::appendToTable(
      data$covariateData$analysisRef,
      data.frame(
        analysisId = 999,
        analysisName = "constantAnalysis",
        domainId = "testDomain",
        startDay = 0,
        endDay = 0,
        isBinary = "N",
        missingMeansZero = "N"
      )
    )
    return(data)
  }

  normalizer <- createNormalizer(type = "minmax")
  newData <- addConstantFeature(tinyTrainData, 12101, 5)
  newTestData <- addConstantFeature(testData, 12101, 5)
  normalizedData <- minMaxNormalize(newData, normalizer)
  testNormalizedData <- minMaxNormalize(newTestData, attr(normalizedData$covariateData, "metaData")$featureEngineering$minMaxNormalize$settings$featureEngineeringSettings, done = TRUE)

  trainFeature <- normalizedData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 12101) %>%
    dplyr::pull(.data$covariateValue)
  expect_true(all(is.finite(trainFeature)))
  newTestFeature <- testNormalizedData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 12101) %>%
    dplyr::pull(.data$covariateValue)
  expect_true(all(is.finite(newTestFeature)))
  expect_equal(newTestFeature, rep(0, nrow(newTestData$labels)))
  # now same with robust normalization
  normalizer <- createNormalizer(type = "robust", settings = list(clip = TRUE))
  newData <- addConstantFeature(tinyTrainData, 12101, 5)
  newTestData <- addConstantFeature(testData, 12101, 5)
  normalizedData <- robustNormalize(newData, normalizer)
  testNormalizedData <- robustNormalize(newTestData, attr(normalizedData$covariateData, "metaData")$featureEngineering$robustNormalize$settings$featureEngineeringSettings, done = TRUE)
  trainFeature <- normalizedData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 12101) %>%
    dplyr::pull(.data$covariateValue)
  expect_true(all(is.finite(trainFeature)))
  newTestFeature <- testNormalizedData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 12101) %>%
    dplyr::pull(.data$covariateValue)
  expect_true(all(is.finite(newTestFeature)))
  expect_equal(newTestFeature, rep(0, nrow(newTestData$labels)))
})

createZeroData <- function(values, missingMeansZero = "Y", rowIds = seq_along(values), plain = FALSE) {
  stored <- values != 0
  covariateData <- Andromeda::andromeda(
    covariates = data.frame(
      rowId = rowIds[stored],
      covariateId = 1001,
      covariateValue = values[stored]
    ),
    covariateRef = data.frame(
      covariateId = 1001,
      covariateName = "score",
      analysisId = 1,
      conceptId = 0
    ),
    analysisRef = data.frame(
      analysisId = 1,
      analysisName = "score",
      domainId = "Condition",
      startDay = 0,
      endDay = 0,
      isBinary = "N",
      missingMeansZero = missingMeansZero
    )
  )
  class(covariateData) <- "CovariateData"
  attr(covariateData, "metaData") <- list(populationSize = length(rowIds))
  if (plain) {
    return(structure(
      list(cohorts = data.frame(rowId = rowIds), outcomes = data.frame(), covariateData = covariateData),
      class = "plpData"
    ))
  }
  structure(
    list(
      labels = data.frame(rowId = rowIds, outcomeCount = 0),
      folds = data.frame(rowId = rowIds, index = 1),
      covariateData = covariateData
    ),
    class = "plpData"
  )
}

matrixValues <- function(data, rowIds) {
  mapped <- toSparseM(data, data.frame(rowId = rowIds))
  values <- as.numeric(mapped$dataMatrix[, 1])
  values[order(mapped$labels$originalRowId)]
}

savedNormalizer <- function(data, name) {
  attr(data$covariateData, "metaData")$featureEngineering[[name]]$settings$featureEngineeringSettings
}

test_that("min-max normalization includes implicit zeros", {
  data <- createZeroData(c(0, 1, 2))
  on.exit(Andromeda::close(data$covariateData), add = TRUE)

  normalized <- minMaxNormalize(data, createNormalizer("minmax"))

  expect_equal(matrixValues(normalized, 1:3), c(0, 0.5, 1))
  # zeros that stay zero are not stored
  expect_equal(normalized$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull(), 2)
  expect_equal(
    data$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull(),
    2
  )
})

test_that("robust normalization includes implicit zeros and stores non-zero results", {
  data <- createZeroData(c(0, 1, 2))
  on.exit(Andromeda::close(data$covariateData), add = TRUE)

  normalized <- robustNormalize(data, createNormalizer("robust", settings = list(clip = FALSE)))

  expect_equal(matrixValues(normalized, 1:3), c(-1, 0, 1))
  rows <- as.data.frame(normalized$covariateData$covariates)
  expect_equal(rows$covariateValue[rows$rowId == 1], -1)
})

test_that("robust normalization clips the implicit zero value when requested", {
  data <- createZeroData(c(0, 1, 2))
  on.exit(Andromeda::close(data$covariateData), add = TRUE)

  normalized <- robustNormalize(data, createNormalizer("robust", settings = list(clip = TRUE)))

  expect_equal(matrixValues(normalized, 1:3)[1], -1 / sqrt(1 + (-1 / 3)^2))
})

test_that("saved normalizers apply implicit zeros to new data", {
  for (norm in list(
    list(create = createNormalizer("minmax"), fun = minMaxNormalize, name = "minMaxNormalize", expected = c(0.5, 0, 1)),
    list(
      create = createNormalizer("robust", settings = list(clip = FALSE)),
      fun = robustNormalize, name = "robustNormalize", expected = c(0, -1, 1)
    )
  )) {
    train <- createZeroData(c(0, 1, 2))
    on.exit(Andromeda::close(train$covariateData), add = TRUE)
    fitted <- norm$fun(train, norm$create)
    settings <- savedNormalizer(fitted, norm$name)

    test <- createZeroData(c(1, 0, 2))
    on.exit(Andromeda::close(test$covariateData), add = TRUE)
    applied <- norm$fun(test, settings, done = TRUE)

    expect_equal(matrixValues(applied, 1:3), norm$expected)
  }
})

test_that("saved normalizers apply implicit zeros to plain plpData", {
  train <- createZeroData(c(0, 1, 2))
  on.exit(Andromeda::close(train$covariateData), add = TRUE)
  fitted <- minMaxNormalize(train, createNormalizer("minmax"))
  settings <- savedNormalizer(fitted, "minMaxNormalize")

  plain <- createZeroData(c(0, 1, 2), plain = TRUE)
  on.exit(Andromeda::close(plain$covariateData), add = TRUE)
  applied <- minMaxNormalize(plain, settings, done = TRUE)

  expect_equal(names(applied), names(plain))
  expect_equal(matrixValues(applied, 1:3), c(0, 0.5, 1))
})

test_that("normalizers treat absent rows as missing when missingMeansZero is N", {
  data <- createZeroData(c(0, 1, 2), missingMeansZero = "N")
  on.exit(Andromeda::close(data$covariateData), add = TRUE)

  normalized <- minMaxNormalize(data, createNormalizer("minmax"))
  rows <- as.data.frame(normalized$covariateData$covariates)

  # statistics come from stored rows, so min=1 and max=2
  expect_equal(rows$covariateValue[order(rows$rowId)], c(0, 1))
  expect_equal(nrow(rows), 2)
})

test_that("models saved before implicit zeros were handled replay unchanged", {
  data <- createZeroData(c(0, 1, 2))
  on.exit(Andromeda::close(data$covariateData), add = TRUE)
  settings <- createNormalizer("minmax")
  attr(settings, "minMaxs") <- data.frame(covariateId = 1001, max = 2, min = 1)

  applied <- minMaxNormalize(data, settings, done = TRUE)
  rows <- as.data.frame(applied$covariateData$covariates)

  expect_equal(nrow(rows), 2)
  expect_equal(rows$covariateValue[order(rows$rowId)], c(0, 1))
})

test_that("implicit zero covariates survive a JSON round trip of the settings", {
  train <- createZeroData(c(0, 1, 2))
  on.exit(Andromeda::close(train$covariateData), add = TRUE)
  fitted <- robustNormalize(train, createNormalizer("robust", settings = list(clip = FALSE)))
  jsonFile <- tempfile(fileext = ".json")
  on.exit(unlink(jsonFile), add = TRUE)
  ParallelLogger::saveSettingsToJson(
    attr(fitted$covariateData, "metaData")$featureEngineering,
    jsonFile
  )
  loaded <- ParallelLogger::loadSettingsFromJson(jsonFile)

  test <- createZeroData(c(1, 0, 2))
  on.exit(Andromeda::close(test$covariateData), add = TRUE)
  applied <- applyFeatureEngineering(test, loaded)

  expect_equal(matrixValues(applied, 1:3), c(0, -1, 1))
})
