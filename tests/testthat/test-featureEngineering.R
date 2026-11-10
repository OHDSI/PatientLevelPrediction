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
  trainData <- simulatePlpData(simulationProfile, n = 200, seed = 42)

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
  skip_if_not_installed("xgboost")

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

createSplineData <- function(ageValues = c(20, 80, 40, 60), charlsonValues = c(4, 0, 1, 3)) {
  n <- length(ageValues)
  covariateData <- Andromeda::andromeda(
    covariates = data.frame(
      rowId = rep(seq_len(n), 2),
      covariateId = rep(c(1002, 1901), each = n),
      covariateValue = c(ageValues, charlsonValues)
    ),
    covariateRef = data.frame(
      covariateId = c(1002, 1901),
      covariateName = c("Age", "Charlson index"),
      conceptId = c(0, 0),
      analysisId = c(2, 901)
    ),
    analysisRef = data.frame(
      analysisId = c(2, 901),
      analysisName = c("Age", "Charlson index"),
      domainId = c("Demographics", "Condition"),
      startDay = c(0, 0),
      endDay = c(0, 0),
      isBinary = c("N", "N"),
      missingMeansZero = c("N", "N")
    )
  )
  class(covariateData) <- "CovariateData"
  attr(covariateData, "metaData") <- list(populationSize = n)
  structure(
    list(labels = data.frame(rowId = seq_len(n), outcomeCount = 0), covariateData = covariateData),
    class = "plpData"
  )
}

ageSplineSettings <- createSplineSettings(1002, knots = c(20, 50, 80))
charlsonSplineSettings <- createSplineSettings(1901, knots = c(0, 2, 4))

test_that("splines for covariates sharing an ID prefix keep separate columns", {
  data <- createSplineData()
  on.exit(Andromeda::close(data$covariateData), add = TRUE)

  engineered <- featureEngineer(data, list(ageSplineSettings, charlsonSplineSettings))
  ref <- as.data.frame(engineered$covariateData$covariateRef)
  sparse <- toSparseM(engineered, engineered$labels)

  expect_equal(nrow(ref), 8)
  expect_equal(dplyr::n_distinct(ref$covariateId), 8)
  expect_equal(ncol(sparse$dataMatrix), 8)
  age <- splines::bs(c(20, 80, 40, 60), knots = 50, Boundary.knots = c(20, 80))
  charlson <- splines::bs(c(4, 0, 1, 3), knots = 2, Boundary.knots = c(0, 4))
  expected <- cbind(age, charlson)
  actual <- as.matrix(sparse$dataMatrix)
  expect_equal(unname(actual[order(sparse$labels$originalRowId), ]), unname(expected), ignore_attr = TRUE)
})

test_that("persisted spline IDs are reused at prediction, including after a JSON round trip", {
  data <- createSplineData()
  on.exit(Andromeda::close(data$covariateData), add = TRUE)
  engineered <- featureEngineer(data, list(ageSplineSettings, charlsonSplineSettings))
  trainIds <- sort(as.data.frame(engineered$covariateData$covariateRef)$covariateId)
  saved <- attr(engineered$covariateData, "metaData")$featureEngineering

  jsonFile <- tempfile(fileext = ".json")
  on.exit(unlink(jsonFile), add = TRUE)
  ParallelLogger::saveSettingsToJson(saved, jsonFile)
  loaded <- ParallelLogger::loadSettingsFromJson(jsonFile)

  newData <- createSplineData(c(30, 70), c(2, 1))
  on.exit(Andromeda::close(newData$covariateData), add = TRUE)
  replayed <- applyFeatureEngineering(newData, loaded)

  expect_setequal(as.data.frame(replayed$covariateData$covariateRef)$covariateId, trainIds)
  expect_setequal(unique(as.data.frame(replayed$covariateData$covariates)$covariateId), trainIds)
})

test_that("a single spline keeps its established covariate IDs", {
  data <- createSplineData()
  on.exit(Andromeda::close(data$covariateData), add = TRUE)

  engineered <- featureEngineer(data, ageSplineSettings)

  expect_setequal(
    as.data.frame(engineered$covariateData$covariateRef)$covariateId,
    c(1901, 11683, 12683, 13683, 14683)
  )
})

test_that("models saved before spline IDs were persisted replay their original IDs", {
  data <- createSplineData()
  on.exit(Andromeda::close(data$covariateData), add = TRUE)
  legacy <- list(
    funct = "splineCovariates",
    settings = list(featureEngineeringSettings = ageSplineSettings, knots = c(20, 50, 80))
  )

  replayed <- applyFeatureEngineering(data, legacy)

  expect_setequal(
    as.data.frame(replayed$covariateData$covariateRef)$covariateId,
    c(1901, 11683, 12683, 13683, 14683)
  )
})

test_that("allocateSplineIds avoids collisions and stops when it cannot", {
  expect_equal(allocateSplineIds(1002, 683, 2, numeric()), c(11683, 12683))
  expect_equal(
    allocateSplineIds(1901, 683, 2, c(11683, 12683)),
    c(19011683, 19012683)
  )
  expect_error(
    allocateSplineIds(1901, 683, 2, c(11683, 12683, 19011683, 19012683)),
    "Cannot create unique"
  )
})

createRareData <- function(nPatients, featureRowIds, rowsPerPatient = 1) {
  covariateData <- Andromeda::andromeda(
    covariates = data.frame(
      rowId = c(1:100, rep(featureRowIds, each = rowsPerPatient)),
      covariateId = c(rep(1, 100), rep(2, length(featureRowIds) * rowsPerPatient)),
      covariateValue = 1
    ),
    covariateRef = data.frame(
      covariateId = c(1, 2),
      covariateName = c("background", "candidate"),
      analysisId = 1,
      conceptId = 0
    ),
    analysisRef = data.frame(analysisId = 1, analysisName = "binary features")
  )
  class(covariateData) <- "CovariateData"
  attr(covariateData, "metaData") <- list(populationSize = nPatients)
  structure(
    list(
      labels = data.frame(rowId = seq_len(nPatients), outcomeCount = 0),
      folds = data.frame(rowId = seq_len(nPatients), index = 1),
      covariateData = covariateData
    ),
    class = "plpData"
  )
}

remainingCovariates <- function(data) {
  sort(unique(as.data.frame(data$covariateData$covariates)$covariateId))
}

test_that("rare feature removal uses the whole training population as denominator", {
  data <- createRareData(nPatients = 200, featureRowIds = 1:15)
  on.exit(Andromeda::close(data$covariateData), add = TRUE)

  result <- removeRareFeatures(data, createRareFeatureRemover(threshold = 0.1))

  # 15 of 200 patients is 7.5%, below the 10% threshold
  expect_equal(remainingCovariates(result), 1)
  expect_equal(nrow(result$labels), 200)
})

test_that("rare feature removal counts patients, not rows", {
  data <- createRareData(nPatients = 100, featureRowIds = 1:5, rowsPerPatient = 2)
  on.exit(Andromeda::close(data$covariateData), add = TRUE)

  result <- removeRareFeatures(data, createRareFeatureRemover(threshold = 0.08))

  # 5 of 100 patients is 5%, even though the feature has 10 rows
  expect_equal(remainingCovariates(result), 1)
})

test_that("rare feature removal keeps features above the threshold", {
  data <- createRareData(nPatients = 100, featureRowIds = 1:15)
  on.exit(Andromeda::close(data$covariateData), add = TRUE)

  result <- removeRareFeatures(data, createRareFeatureRemover(threshold = 0.1))

  expect_equal(remainingCovariates(result), c(1, 2))
})

test_that("saved rare feature removal replays on plpData without labels", {
  data <- createRareData(nPatients = 100, featureRowIds = 1:15)
  on.exit(Andromeda::close(data$covariateData), add = TRUE)
  remover <- createRareFeatureRemover(threshold = 0.1)
  attr(remover, "rareFeatures") <- 2
  plpData <- structure(
    list(cohorts = data.frame(rowId = 1:100), outcomes = data.frame(), covariateData = data$covariateData),
    class = "plpData"
  )

  result <- removeRareFeatures(plpData, remover, done = TRUE)

  expect_equal(remainingCovariates(result), 1)
})
