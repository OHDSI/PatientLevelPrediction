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

testFEFun <- function(type = "none") {
  result <- createFeatureEngineeringSettings(type = type)
  return(result)
}

test_that("createFeatureEngineeringSettings correct class", {
  featureEngineeringSettings <- testFEFun()

  expect_s3_class(featureEngineeringSettings, "featureEngineeringSettings")

  checkFun <- "sameData"
  expect_equal(attr(featureEngineeringSettings, "fun"), checkFun)
})

if (rlang::is_installed("reticulate")) {
  testUniFun <- function(k = 100) {
    result <- createUnivariateFeatureSelection(k = k)
    return(result)
  }
}

test_that("createUnivariateFeatureSelection correct class", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  k <- sample(1000, 1)
  featureEngineeringSettings <- testUniFun(k = k)

  expect_s3_class(featureEngineeringSettings, "featureEngineeringSettings")
  expect_equal(featureEngineeringSettings$k, k)
  expect_equal(attr(featureEngineeringSettings, "fun"), "univariateFeatureSelection")

  expect_error(testUniFun(k = "ffdff"))
  expect_error(testUniFun(k = NULL))
  expect_error(testUniFun(k = -1))
})


test_that("univariateFeatureSelection", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  skip_if_offline()
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

  expect_s3_class(featureEngineeringSettings, "featureEngineeringSettings")
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
  skip_if_offline()
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
  skip_if_offline()
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

  expect_s3_class(featureEngineeringSettings, "featureEngineeringSettings")
  expect_equal(featureEngineeringSettings$knots, 4)
  expect_equal(featureEngineeringSettings$continousCovariateId, 12)
  expect_equal(attr(featureEngineeringSettings, "fun"), "splineCovariates")

  expect_error(createSplineSettings(knots = "ffdff"))
  expect_error(createSplineSettings(knots = NULL))
})

test_that("splineCovariates works", {
  skip_if_offline()
  knots <- 4
  featureEngineeringSettings <- createSplineSettings(
    continousCovariateId = 12101,
    knots = knots
  )
  data(simulationProfile)
  trainData <- simulatePlpData(simulationProfile, n = 200)

  n <- 50
  trainData$covariateData$covariates <- data.frame(
    rowId = sample(trainData$cohorts$rowId, n),
    covariateId = rep(12101, n),
    covariateValue = sample(10, n, replace = TRUE)
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

  expect_true(1 < nrow(as.data.frame(newData$covariateData$analysisRef)))
  expect_true((knots + 1) == nrow(as.data.frame(newData$covariateData$covariateRef)))
  expect_true((knots + 1) == length(table(as.data.frame(newData$covariateData$covariates)$covariateId)))
})


test_that("createStratifiedImputationSettings correct class", {
  skip_if_offline()
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

  expect_true(nrow(stratifiedMeans) == 8)

  imputedData <- imputeMissingMeans(
    trainData = nanoData,
    covariateId = 12101,
    ageSplits = ageSplits,
    stratifiedMeans = stratifiedMeans
  )

  expect_equal(
    imputedData$covariateData$covariates %>%
      dplyr::filter(.data$covariateId == 12101) %>%
      dplyr::pull(.data$rowId) %>%
      dplyr::n_distinct(),
    numSubjects
  )
})

test_that("createRareFeatureRemover works", {
  remover <- createRareFeatureRemover(threshold = 0.1)
  expect_equal(remover$threshold, 0.1)
  expect_equal(attr(remover, "fun"), "removeRareFeatures")

  expect_error(createRareFeatureRemover(threshold = -1))
  expect_error(createRareFeatureRemover(threshold = "0.5"))
  expect_error(createRareFeatureRemover(threshold = 1))
})

test_that("Removing rare features works", {
  skip_if_offline()
  remover <- createRareFeatureRemover(threshold = 0.1)

  removedData <- removeRareFeatures(tinyTrainData, remover)
  expect_true(
    removedData$covariateData$covariates %>%
      dplyr::pull(.data$covariateId) %>%
      dplyr::n_distinct() <=
      tinyTrainData$covariateData$covariates %>%
        dplyr::pull(.data$covariateId) %>%
        dplyr::n_distinct()
  )
  metaData <- attr(removedData$covariateData, "metaData")
  testSettings <- metaData$featureEngineering$removeRare$settings$featureEngineeringSettings

  removedTestData <- removeRareFeatures(testData, remover, done = TRUE)
  expect_true(
    removedTestData$covariateData$covariates %>%
      dplyr::pull(.data$covariateId) %>%
      dplyr::n_distinct() <=
      testData$covariateData$covariates %>%
        dplyr::pull(.data$covariateId) %>%
        dplyr::n_distinct()
  )
})

test_that("two step FE and RClassifier works", {
  skip_if_offline()

  remover <- createRareFeatureRemover(threshold = 0.1)
  normalizer <- createNormalizer(type = "minmax")
  featureEngineeringSettings <- list(
    remover,
    normalizer
  )
  newTrainData <- copyTrainData(trainData)
  engineeredData <- featureEngineer(
    data = newTrainData,
    featureEngineeringSettings = featureEngineeringSettings
  )
  expect_true(
    engineeredData$covariateData$covariates %>%
      dplyr::pull(.data$covariateId) %>%
      dplyr::n_distinct() <=
      newTrainData$covariateData$covariates %>%
        dplyr::pull(.data$covariateId) %>%
        dplyr::n_distinct()
  )
  expect_true(
    engineeredData$covariateData$covariates %>%
      dplyr::filter(.data$covariateId == 1002) %>%
      dplyr::pull(.data$covariateValue) %>%
      max() <= 1.0
  )
  expect_true(
    engineeredData$covariateData$covariates %>%
      dplyr::filter(.data$covariateId == 1002) %>%
      dplyr::pull(.data$covariateValue) %>%
      min() >= 0.0
  )
  modelSettings <- setGradientBoostingMachine(
    ntrees = 10,
    maxDepth = 3,
    nthread = 2,
    learnRate = 0.1,
    seed = 42
  )
  plpModel <- fitPlp(engineeredData, modelSettings,
    analysisId = "FE",
    analysisPath = file.path(saveLoc, "FE2")
  )
  testEngineeredData <- applyFeatureEngineering(
    testData,
    plpModel$preprocessing$featureEngineering
  )
  expect_true(
    length(plpModel$preprocessing$featureEngineering) == 2
  )
  expect_true(
    testEngineeredData$covariateData$covariates %>%
      dplyr::pull(.data$covariateId) %>%
      dplyr::n_distinct() <=
      testData$covariateData$covariates %>%
        dplyr::pull(.data$covariateId) %>%
        dplyr::n_distinct()
  )
  minMaxs <- attr(plpModel$preprocessing$featureEngineering[["minMaxNormalize"]]$settings$featureEngineeringSettings, "minMaxs")
  minAge <- minMaxs$min[minMaxs$covariateId == 1002]
  maxAge <- minMaxs$max[minMaxs$covariateId == 1002]

  testAge <- testEngineeredData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 1002) %>%
    dplyr::pull(.data$covariateValue)
  originalTestAge <- testData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 1002) %>%
    dplyr::pull(.data$covariateValue)

  expect_equal(
    testAge,
    (originalTestAge - minAge) / (maxAge - minAge)
  )
})
