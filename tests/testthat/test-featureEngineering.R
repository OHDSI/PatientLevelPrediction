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

library("testthat")
context("FeatureEngineering")


testFEFun <- function(type = "none") {
  result <- createFeatureEngineeringSettings(type = type)
  return(result)
}

test_that("createFeatureEngineeringSettings correct class", {
  featureEngineeringSettings <- testFEFun()

  expect_is(featureEngineeringSettings, "featureEngineeringSettings")

  checkFun <- "sameData"
  expect_equal(attr(featureEngineeringSettings, "fun"), checkFun)
})

testUniFun <- function(k = 100) {
  result <- createUnivariateFeatureSelection(k = k)
  return(result)
}

test_that("createUnivariateFeatureSelection correct class", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  k <- sample(1000, 1)
  featureEngineeringSettings <- testUniFun(k = k)

  expect_is(featureEngineeringSettings, "featureEngineeringSettings")
  expect_equal(featureEngineeringSettings$k, k)
  expect_equal(attr(featureEngineeringSettings, "fun"), "univariateFeatureSelection")

  expect_error(testUniFun(k = "ffdff"))
  expect_error(testUniFun(k = NULL))
  expect_error(testUniFun(k = -1))
})


test_that("univariateFeatureSelection", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  k <- 20 + sample(10, 1)
  featureEngineeringSettings <- testUniFun(k = k)
  newTrainData <- copyTrainData(trainData)

  trainDataCovariateSize <- newTrainData$covariateData$covariates %>%
    dplyr::tally() %>%
    dplyr::pull()

  reducedTrainData <- univariateFeatureSelection(
    trainData = newTrainData,
    featureEngineeringSettings = featureEngineeringSettings,
    covariateIdsInclude = NULL
  )

  newDataCovariateSize <- reducedTrainData$covariateData$covariates %>%
    dplyr::tally() %>%
    dplyr::pull()
  expect_true(newDataCovariateSize <= trainDataCovariateSize)

  # expect k many covariates left
  expect_equal(k, reducedTrainData$covariateData$covariateRef %>% dplyr::tally() %>% dplyr::pull())
})


test_that("createRandomForestFeatureSelection correct class", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  ntreesTest <- sample(1000, 1)
  maxDepthTest <- sample(20, 1)
  featureEngineeringSettings <- createRandomForestFeatureSelection(
    ntrees = ntreesTest,
    maxDepth = maxDepthTest
  )

  expect_is(featureEngineeringSettings, "featureEngineeringSettings")
  expect_equal(featureEngineeringSettings$ntrees, ntreesTest)
  expect_equal(featureEngineeringSettings$max_depth, maxDepthTest)
  expect_equal(attr(featureEngineeringSettings, "fun"), "randomForestFeatureSelection")

  # error due to params
  expect_error(
    createRandomForestFeatureSelection(
      ntrees = -1,
      maxDepth = maxDepthTest
    )
  )

  expect_error(
    createRandomForestFeatureSelection(
      ntrees = "dfdfd",
      maxDepth = maxDepthTest
    )
  )

  expect_error(
    createRandomForestFeatureSelection(
      ntrees = 50,
      maxDepth = "maxDepthTest"
    )
  )

  expect_error(
    createRandomForestFeatureSelection(
      ntrees = 50,
      maxDepth = -1
    )
  )
})


test_that("randomForestFeatureSelection", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  ntreesTest <- sample(1000, 1)
  maxDepthTest <- sample(20, 1)
  featureEngineeringSettings <- createRandomForestFeatureSelection(
    ntrees = ntreesTest,
    maxDepth = maxDepthTest
  )

  newTrainData <- copyTrainData(trainData)
  trainDataCovariateSize <- newTrainData$covariateData$covariates %>%
    dplyr::tally() %>%
    dplyr::pull()

  reducedTrainData <- randomForestFeatureSelection(
    trainData = newTrainData,
    featureEngineeringSettings = featureEngineeringSettings,
    covariateIdsInclude = NULL
  )

  newDataCovariateSize <- reducedTrainData$covariateData$covariates %>%
    dplyr::tally() %>%
    dplyr::pull()
  expect_true(newDataCovariateSize < trainDataCovariateSize)
})

test_that("featureSelection is applied on test_data", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  k <- 20
  featureEngineeringSettings <- testUniFun(k = k)
  newTrainData <- copyTrainData(trainData)
  newTrainData <- univariateFeatureSelection(
    trainData = newTrainData,
    featureEngineeringSettings = featureEngineeringSettings,
    covariateIdsInclude = NULL
  )

  modelSettings <- setLassoLogisticRegression()

  # added try catch due to model sometimes not fitting
  plpModel <- tryCatch(
    {
      fitPlp(newTrainData, modelSettings, analysisId = "FE")
    },
    error = function(e) {
      return(NULL)
    }
  )

  if (!is.null(plpModel)) { # if the model fit then check this
    prediction <- predictPlp(plpModel, testData, population)
    expect_true(attr(prediction, "metaData")$featureEngineering)
  }
})

test_that("createSplineSettings correct class", {
  featureEngineeringSettings <- createSplineSettings(
    continousCovariateId = 12,
    knots = 4
  )

  expect_is(featureEngineeringSettings, "featureEngineeringSettings")
  expect_equal(featureEngineeringSettings$knots, 4)
  expect_equal(featureEngineeringSettings$continousCovariateId, 12)
  expect_equal(attr(featureEngineeringSettings, "fun"), "splineCovariates")

  expect_error(createSplineSettings(knots = "ffdff"))
  expect_error(createSplineSettings(knots = NULL))
})

test_that("createSplineSettings correct class", {
  knots <- 4
  featureEngineeringSettings <- createSplineSettings(
    continousCovariateId = 12101,
    knots = knots
  )
  data(plpDataSimulationProfile)
  trainData <- simulatePlpData(plpDataSimulationProfile, n = 200)

  N <- 50
  trainData$covariateData$covariates <- data.frame(
    rowId = sample(trainData$cohorts$rowId, N),
    covariateId = rep(12101, N),
    covariateValue = sample(10, N, replace = TRUE)
  )

  trainData$covariateData$analysisRef <- data.frame(
    analysisId = 101,
    analysisName = "cond",
    domainId = "madeup",
    startDay = 0,
    endDay = 0,
    isBinary = "N",
    missingMeansZero = "N"
  )

  trainData$covariateData$covariateRef <- data.frame(
    covariateId = 12101,
    covariateName = "test",
    analysisId = 101,
    conceptId = 1
  )

  newData <- splineCovariates(
    trainData = trainData,
    featureEngineeringSettings = featureEngineeringSettings
  )

  testthat::expect_true(1 < nrow(as.data.frame(newData$covariateData$analysisRef)))
  testthat::expect_true((knots + 1) == nrow(as.data.frame(newData$covariateData$covariateRef)))
  testthat::expect_true((knots + 1) == length(table(as.data.frame(newData$covariateData$covariates)$covariateId)))
})


test_that("createStratifiedImputationSettings correct class", {
  ageSplits <- c(33, 38, 42)
  featureEngineeringSettings <- createStratifiedImputationSettings(
    covariateId = 12101,
    ageSplits = ageSplits
  )

  numSubjects <- nanoData$covariateData$covariates %>%
    dplyr::pull(.data$rowId) %>%
    dplyr::n_distinct()
  Andromeda::appendToTable(nanoData$covariateData$covariates, data.frame(
    rowId = sample(nanoData$cohorts$rowId, floor(numSubjects / 2)),
    covariateId = rep(12101, floor(numSubjects / 2)),
    covariateValue = sample(10, floor(numSubjects / 2), replace = TRUE)
  ))

  Andromeda::appendToTable(nanoData$covariateData$analysisRef, data.frame(
    analysisId = 101,
    analysisName = "cond",
    domainId = "madeup",
    startDay = 0,
    endDay = 0,
    isBinary = "N",
    missingMeansZero = "N"
  ))

  Andromeda::appendToTable(nanoData$covariateData$covariateRef, data.frame(
    covariateId = 12101,
    covariateName = "test",
    analysisId = 101,
    conceptId = 1
  ))

  stratifiedMeans <- calculateStratifiedMeans(
    trainData = nanoData,
    featureEngineeringSettings = featureEngineeringSettings
  )

  testthat::expect_true(nrow(stratifiedMeans) == 8)

  imputedData <- imputeMissingMeans(
    trainData = nanoData,
    covariateId = 12101,
    ageSplits = ageSplits,
    stratifiedMeans = stratifiedMeans
  )

  testthat::expect_equal(
    imputedData$covariateData$covariates %>%
      dplyr::filter(.data$covariateId == 12101) %>%
      dplyr::pull(.data$rowId) %>%
      dplyr::n_distinct(),
    numSubjects
  )
})

test_that("createNormalizer works", {
  normalizer <- createNormalizer(type = "minmax")
  expect_equal(normalizer$type, "minmax")
  expect_equal(attr(normalizer, "fun"), "minMaxNormalize")
  expect_s3_class(normalizer, "featureEngineeringSettings")

  normalizer <- createNormalizer(type = "robust")
  expect_equal(normalizer$type, "robust")

  expect_error(createNormalizer(type = "mean"))
  expect_error(createNormalizer(type = "median"))
  expect_error(createNormalizer(type = "zscore"))
  expect_error(createNormalizer(type = "none"))
})

test_that("normalization works", {
  normalizer <- createNormalizer(type = "minmax")
  addFeature <- function(data, covariateId, minValue, maxValue) {
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
    Andromeda::appendToTable(
      data$covariateData$covariateRef,
      data.frame(
        covariateId = covariateId,
        covariateName = "testCovariate",
        analysisId = 101,
        conceptId = 1
      )
    )
    Andromeda::appendToTable(
      data$covariateData$analysisRef,
      data.frame(
        analysisId = 101,
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
  data <- addFeature(tinyTrainData, 12101, -10, 10)
  normalizedData <- minMaxNormalize(data, normalizer)

  expect_equal(
    normalizedData$covariateData$covariates %>%
      dplyr::filter(.data$covariateId == 12101) %>%
      dplyr::pull(.data$covariateValue) %>%
      range(),
    c(0, 1)
  )

  normalizer <- createNormalizer(type = "robust")
  data <- addFeature(tinyTrainData, 12101, -10, 10)
  newTrainData <- robustNormalize(data, normalizer)
  feature <- newTrainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 12101) %>%
    dplyr::pull(.data$covariateValue)
  expect_true(all(feature >= -3) && all(feature <= 3))
})
