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

  # appends to the covariates, so it works on a copy of the shared fixture
  stratData <- isolateCovariateData(nanoData)
  withr::defer(Andromeda::close(stratData$covariateData))
  numSubjects <- stratData$covariateData$covariates %>%
    dplyr::pull(.data$rowId) %>%
    dplyr::n_distinct()
  Andromeda::appendToTable(stratData$covariateData$covariates, data.frame(
    rowId = sample(stratData$cohorts$rowId, floor(numSubjects / 2)),
    covariateId = rep(12101, floor(numSubjects / 2)),
    covariateValue = sample(10, floor(numSubjects / 2), replace = TRUE)
  ))

  Andromeda::appendToTable(stratData$covariateData$analysisRef, data.frame(
    analysisId = 101,
    analysisName = "cond",
    domainId = "madeup",
    startDay = 0,
    endDay = 0,
    isBinary = "N",
    missingMeansZero = "N"
  ))

  Andromeda::appendToTable(stratData$covariateData$covariateRef, data.frame(
    covariateId = 12101,
    covariateName = "test",
    analysisId = 101,
    conceptId = 1
  ))

  stratifiedMeans <- calculateStratifiedMeans(
    trainData = stratData,
    featureEngineeringSettings = featureEngineeringSettings
  )

  expect_true(nrow(stratifiedMeans) == 8)

  imputedData <- imputeMissingMeans(
    trainData = stratData,
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
  distinctCovariates <- function(data) {
    data$covariateData$covariates %>%
      dplyr::pull(.data$covariateId) %>%
      dplyr::n_distinct()
  }

  # steps change the data they are given, so work on copies of the shared data
  removedData <- removeRareFeatures(copyTrainData(tinyTrainData), remover)
  expect_true(distinctCovariates(removedData) <= distinctCovariates(tinyTrainData))
  testSettings <- fittedFeatureEngineeringSettings(removedData, "removeRareFeatures")
  rareFeatures <- attr(testSettings, "rareFeatures")

  removedTestData <- removeRareFeatures(copyTrainData(testData), testSettings, done = TRUE)
  remaining <- removedTestData$covariateData$covariates %>%
    dplyr::pull(.data$covariateId)
  expect_false(any(remaining %in% rareFeatures))
  expect_true(distinctCovariates(removedTestData) <= distinctCovariates(testData))
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
  # featureEngineer() changes newTrainData itself, so compare with the shared data
  expect_true(
    engineeredData$covariateData$covariates %>%
      dplyr::pull(.data$covariateId) %>%
      dplyr::n_distinct() <=
      trainData$covariateData$covariates %>%
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
  minMaxSteps <- Filter(
    function(step) step$funct == "minMaxNormalize",
    plpModel$preprocessing$featureEngineering
  )
  minMaxs <- attr(minMaxSteps[[1]]$settings$featureEngineeringSettings, "minMaxs")
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

covariateValues <- function(data) {
  as.data.frame(data$covariateData$covariates) %>%
    dplyr::arrange(.data$rowId, .data$covariateId)
}

stepFunctions <- function(steps) {
  vapply(steps, function(step) step$funct, character(1))
}

test_that("training steps change the training data in place and record one entry per step", {
  data <- createSplineData()
  on.exit(Andromeda::close(data$covariateData), add = TRUE)

  engineered <- featureEngineer(
    data,
    list(ageSplineSettings, createNormalizer("minmax"), charlsonSplineSettings)
  )

  steps <- attr(engineered$covariateData, "metaData")$featureEngineering
  expect_null(names(steps))
  expect_equal(
    stepFunctions(steps),
    c("splineCovariates", "minMaxNormalize", "splineCovariates")
  )
  # training makes no copy: the training data itself was changed
  expect_equal(covariateValues(data), covariateValues(engineered))
})

test_that("a saved model with several kinds of steps replays all of them on a copy", {
  data <- createSplineData()
  on.exit(Andromeda::close(data$covariateData), add = TRUE)
  engineered <- featureEngineer(data, list(ageSplineSettings, createNormalizer("minmax")))
  saved <- attr(engineered$covariateData, "metaData")$featureEngineering

  jsonFile <- tempfile(fileext = ".json")
  on.exit(unlink(jsonFile), add = TRUE)
  saveSettingsToJsonFullPrecision(list(featureEngineering = saved), jsonFile)
  loaded <- ParallelLogger::loadSettingsFromJson(jsonFile)$featureEngineering

  newData <- createSplineData(c(30, 70), c(2, 1))
  on.exit(Andromeda::close(newData$covariateData), add = TRUE)
  before <- covariateValues(newData)
  replayed <- applyFeatureEngineering(newData, loaded)

  expect_equal(covariateValues(newData), before)
  values <- covariateValues(replayed)
  # the spline replaced age, and the normalizer scaled Charlson with the training range 0-4
  expect_false(1002 %in% values$covariateId)
  expect_equal(values$covariateValue[values$covariateId == 1901], c(2, 1) / 4)
})

test_that("saved steps are read from every stored format", {
  selection <- list(funct = "univariateFeatureSelection", settings = list(covariateIdsInclude = 1))
  spline <- list(funct = "splineCovariates", settings = list(knots = c(0, 1, 2)))
  imputer <- list(funct = "simpleImpute", settings = list(done = TRUE))

  canonical <- list(selection, spline, imputer)
  # listAppend() pairs, as saved before, and as renamed by the JSON round trip
  flat <- c(selection, spline)
  renamed <- stats::setNames(flat, c("funct", "settings", "funct.1", "settings.1"))
  # named entries, as the normalizers and imputers saved them
  mixed <- c(flat, list(simpleImputer = imputer))

  # a listAppend() pair after unnamed entries, as an old-style custom step leaves it
  appended <- c(list(selection), spline)
  names(appended) <- c("funct", "settings", NA)

  expect_equal(featureEngineeringSteps(canonical), canonical)
  expect_equal(featureEngineeringSteps(flat), list(selection, spline))
  expect_equal(featureEngineeringSteps(renamed), list(selection, spline))
  expect_equal(featureEngineeringSteps(mixed), canonical)
  expect_equal(featureEngineeringSteps(appended), list(selection, spline))
  expect_equal(featureEngineeringSteps(NULL), list())
  expect_warning(
    steps <- featureEngineeringSteps(list(selection, 42)),
    "entry 2 is not a step"
  )
  expect_equal(steps, list(selection))
})

test_that("recordFeatureEngineeringStep appends after steps saved in older formats", {
  covariateData <- Andromeda::andromeda()
  on.exit(Andromeda::close(covariateData), add = TRUE)
  data <- list(covariateData = covariateData)
  attr(data$covariateData, "metaData") <- list(featureEngineering = list(
    funct = "univariateFeatureSelection",
    settings = list(covariateIdsInclude = 1),
    simpleImputer = list(funct = "simpleImpute", settings = list(done = TRUE))
  ))

  data <- recordFeatureEngineeringStep(data, "removeRareFeatures", list(done = TRUE))
  data <- recordFeatureEngineeringStep(data, "removeRareFeatures", list(done = TRUE))

  steps <- attr(data$covariateData, "metaData")$featureEngineering
  # a step used twice keeps both entries
  expect_equal(
    stepFunctions(steps),
    c("univariateFeatureSelection", "simpleImpute", "removeRareFeatures", "removeRareFeatures")
  )
  expect_equal(steps[[1]]$settings, list(covariateIdsInclude = 1))
  expect_error(recordFeatureEngineeringStep(data, "removeRareFeatures", "not a list"))
})

test_that("a custom step written as in the vignette is applied at prediction", {
  # the implement function from vignette("AddingCustomFeatureEngineering")
  assign(
    "implementCapCovariate",
    function(trainData, featureEngineeringSettings, cap = NULL) {
      covariateId <- featureEngineeringSettings$covariateId
      if (is.null(cap)) {
        cap <- trainData$covariateData$covariates %>%
          dplyr::filter(.data$covariateId == !!covariateId) %>%
          dplyr::pull(.data$covariateValue) %>%
          stats::quantile(featureEngineeringSettings$quantile, names = FALSE)
      }
      trainData$covariateData$covariates <- trainData$covariateData$covariates %>%
        dplyr::mutate(covariateValue = dplyr::if_else(
          .data$covariateId == !!covariateId & .data$covariateValue > !!cap,
          !!cap,
          .data$covariateValue
        ))
      trainData <- recordFeatureEngineeringStep(
        trainData,
        funct = "implementCapCovariate",
        settings = list(featureEngineeringSettings = featureEngineeringSettings, cap = cap)
      )
      return(trainData)
    },
    envir = globalenv()
  )
  on.exit(rm("implementCapCovariate", envir = globalenv()), add = TRUE)
  capSettings <- structure(
    list(covariateId = 1002, quantile = 0.5),
    fun = "implementCapCovariate",
    class = "featureEngineeringSettings"
  )

  data <- createSplineData(ageValues = c(20, 80, 40, 60))
  on.exit(Andromeda::close(data$covariateData), add = TRUE)
  engineered <- featureEngineer(data, capSettings)
  ages <- covariateValues(engineered) %>% dplyr::filter(.data$covariateId == 1002)
  expect_equal(ages$covariateValue, c(20, 50, 40, 50))

  jsonFile <- tempfile(fileext = ".json")
  on.exit(unlink(jsonFile), add = TRUE)
  saveSettingsToJsonFullPrecision(
    list(featureEngineering = attr(engineered$covariateData, "metaData")$featureEngineering),
    jsonFile
  )
  loaded <- ParallelLogger::loadSettingsFromJson(jsonFile)$featureEngineering

  newData <- createSplineData(ageValues = c(30, 90), charlsonValues = c(1, 2))
  on.exit(Andromeda::close(newData$covariateData), add = TRUE)
  replayed <- applyFeatureEngineering(newData, loaded)
  newAges <- covariateValues(replayed) %>% dplyr::filter(.data$covariateId == 1002)
  expect_equal(newAges$covariateValue, c(30, 50))
  # the shared data is unchanged
  originalAges <- covariateValues(newData) %>% dplyr::filter(.data$covariateId == 1002)
  expect_equal(originalAges$covariateValue, c(30, 90))
})

test_that("an old-style custom step recorded with listAppend() is applied in any order", {
  # adds a constant covariate and records itself as the earlier vignette showed
  assign(
    "implementAddConstantOldStyle",
    function(trainData, featureEngineeringSettings, value = NULL) {
      value <- value %||% 1
      Andromeda::appendToTable(
        trainData$covariateData$covariates,
        data.frame(rowId = trainData$labels$rowId, covariateId = 9999, covariateValue = value)
      )
      attr(trainData$covariateData, "metaData")$featureEngineering <- listAppend(
        attr(trainData$covariateData, "metaData")$featureEngineering,
        list(funct = "implementAddConstantOldStyle", settings = list(value = value))
      )
      trainData
    },
    envir = globalenv()
  )
  on.exit(rm("implementAddConstantOldStyle", envir = globalenv()), add = TRUE)
  oldStyle <- structure(list(), fun = "implementAddConstantOldStyle", class = "featureEngineeringSettings")

  for (order in list(c("builtIn", "oldStyle"), c("oldStyle", "builtIn"))) {
    settings <- list(builtIn = createNormalizer("minmax"), oldStyle = oldStyle)[order]
    data <- createSplineData()
    engineered <- featureEngineer(data, unname(settings))
    saved <- attr(engineered$covariateData, "metaData")$featureEngineering
    jsonFile <- tempfile(fileext = ".json")
    saveSettingsToJsonFullPrecision(list(featureEngineering = saved), jsonFile)
    loaded <- ParallelLogger::loadSettingsFromJson(jsonFile)$featureEngineering

    expect_equal(
      stepFunctions(featureEngineeringSteps(loaded)),
      c(builtIn = "minMaxNormalize", oldStyle = "implementAddConstantOldStyle")[order],
      ignore_attr = TRUE
    )
    newData <- createSplineData(ageValues = c(30, 90), charlsonValues = c(1, 2))
    replayed <- covariateValues(applyFeatureEngineering(newData, loaded))
    # both steps ran: the constant was added and age was scaled with the training range 20-80
    expect_equal(replayed$rowId[replayed$covariateId == 9999], 1:2)
    expect_equal(replayed$covariateValue[replayed$covariateId == 1002], c(10, 70) / 60)
    Andromeda::close(data$covariateData)
    Andromeda::close(newData$covariateData)
    unlink(jsonFile)
  }
})

test_that("a step fitted twice is recorded and replayed twice", {
  data <- createRareData(nPatients = 200, featureRowIds = 1:15)
  on.exit(Andromeda::close(data$covariateData), add = TRUE)
  # covariate 2 is in 7.5% of patients and covariate 1 in 50%
  engineered <- featureEngineer(
    data,
    list(createRareFeatureRemover(threshold = 0.1), createRareFeatureRemover(threshold = 0.6))
  )
  saved <- attr(engineered$covariateData, "metaData")$featureEngineering
  expect_equal(stepFunctions(saved), c("removeRareFeatures", "removeRareFeatures"))

  newData <- createRareData(nPatients = 200, featureRowIds = 1:15)
  on.exit(Andromeda::close(newData$covariateData), add = TRUE)
  replayed <- applyFeatureEngineering(newData, saved)
  expect_equal(remainingCovariates(replayed), numeric(0))
  expect_equal(remainingCovariates(newData), c(1, 2))
})

test_that("covariateSummary replays steps with the population columns and the labels", {
  seen <- new.env()
  assign(
    "implementRecordLabelColumns",
    function(trainData, featureEngineeringSettings, done = FALSE) {
      if (done) {
        assign("labelColumns", colnames(trainData$labels), envir = seen)
      }
      recordFeatureEngineeringStep(
        trainData,
        funct = "implementRecordLabelColumns",
        settings = list(featureEngineeringSettings = featureEngineeringSettings, done = TRUE)
      )
    },
    envir = globalenv()
  )
  on.exit(rm("implementRecordLabelColumns", envir = globalenv()), add = TRUE)
  data <- createSplineData()
  on.exit(Andromeda::close(data$covariateData), add = TRUE)
  population <- data.frame(rowId = 1:4, ageYear = c(20, 80, 40, 60), gender = 8507, outcomeCount = c(0, 1, 0, 1))
  steps <- list(list(
    funct = "implementRecordLabelColumns",
    settings = list(featureEngineeringSettings = list(), done = TRUE)
  ))

  # the arguments runPlp() passes
  covariateSummary(
    covariateData = data$covariateData,
    cohort = population %>% dplyr::select(-"outcomeCount"),
    labels = population %>% dplyr::select("rowId", "outcomeCount"),
    featureEngineering = steps
  )

  expect_setequal(seen$labelColumns, c("rowId", "ageYear", "gender", "outcomeCount"))
})

createStratifiedData <- function(rowIds, ageYear, gender, values, labels = TRUE) {
  observed <- !is.na(values)
  covariateData <- Andromeda::andromeda(
    covariates = data.frame(
      rowId = rowIds[observed], covariateId = rep(12101, sum(observed)),
      covariateValue = as.numeric(values[observed])
    ),
    covariateRef = data.frame(
      covariateId = 12101, covariateName = "lab", analysisId = 101, conceptId = 0
    ),
    analysisRef = data.frame(
      analysisId = 101, analysisName = "lab", domainId = "Measurement",
      startDay = 0, endDay = 0, isBinary = "N", missingMeansZero = "N"
    )
  )
  class(covariateData) <- "CovariateData"
  attr(covariateData, "metaData") <- list(populationSize = length(rowIds))
  people <- data.frame(rowId = rowIds, ageYear = ageYear, gender = gender)
  data <- list(covariateData = covariateData)
  if (labels) {
    data$labels <- cbind(people, outcomeCount = 0)
  } else {
    data$cohorts <- people
  }
  structure(data, class = "plpData")
}

labValues <- function(data) {
  covariateValues(data) %>%
    dplyr::filter(.data$covariateId == 12101) %>%
    dplyr::select("rowId", "covariateValue")
}

test_that("stratified imputation runs in training and is applied at prediction", {
  settings <- createStratifiedImputationSettings(covariateId = 12101, ageSplits = 50)
  # training data has labels and no cohorts, as made by splitData()
  train <- createStratifiedData(
    rowIds = 1:6,
    ageYear = c(30, 30, 30, 60, 60, 60),
    gender = c(8507, 8507, 8507, 8507, 8507, 8532),
    values = c(10, 20, NA, 40, NA, 100)
  )
  on.exit(Andromeda::close(train$covariateData), add = TRUE)

  engineered <- featureEngineer(train, settings)
  # patients 3 and 5 get the mean of their age group and gender
  expect_equal(labValues(engineered)$covariateValue, c(10, 20, 15, 40, 40, 100))
  saved <- attr(engineered$covariateData, "metaData")$featureEngineering
  expect_equal(stepFunctions(saved), "stratifiedImputeCovariates")
  expect_false("cohorts" %in% names(engineered$covariateData))

  jsonFile <- tempfile(fileext = ".json")
  on.exit(unlink(jsonFile), add = TRUE)
  saveSettingsToJsonFullPrecision(list(featureEngineering = saved), jsonFile)
  loaded <- ParallelLogger::loadSettingsFromJson(jsonFile)$featureEngineering

  # a full plpData has cohorts and no labels
  newData <- createStratifiedData(
    rowIds = 11:14,
    ageYear = c(30, 60, 30, 60),
    gender = c(8507, 8532, 8532, 8507),
    values = c(NA, NA, NA, 5),
    labels = FALSE
  )
  on.exit(Andromeda::close(newData$covariateData), add = TRUE)
  replayed <- applyFeatureEngineering(newData, loaded)

  # training means by group; patient 13's group has no training values, so it
  # gets the overall training mean (10 + 20 + 40 + 100) / 4
  expect_equal(labValues(replayed)$rowId, 11:14)
  expect_equal(labValues(replayed)$covariateValue, c(15, 100, 42.5, 5))
  expect_equal(labValues(newData)$rowId, 14)
})

test_that("stratified imputation settings default to 5-year age groups", {
  settings <- createStratifiedImputationSettings(covariateId = 12101)
  expect_null(settings$ageSplits)
  train <- createStratifiedData(
    rowIds = 1:4,
    ageYear = c(31, 34, 36, 36),
    gender = 8507,
    values = c(10, NA, 30, NA)
  )
  on.exit(Andromeda::close(train$covariateData), add = TRUE)

  engineered <- featureEngineer(train, settings)
  # ages 31 and 34 are one group, 36 another
  expect_equal(labValues(engineered)$covariateValue, c(10, 10, 30, 30))
})

test_that("stratified imputation imputes stored NA values and uses the overall mean without age", {
  settings <- createStratifiedImputationSettings(covariateId = 12101, ageSplits = 50)
  train <- createStratifiedData(
    rowIds = 1:4,
    ageYear = c(30, 30, NA, 60),
    gender = 8507,
    values = c(10, 20, 30, 70)
  )
  on.exit(Andromeda::close(train$covariateData), add = TRUE)
  # a stored NA for patient 4
  train$covariateData$covariates <- train$covariateData$covariates %>%
    dplyr::mutate(covariateValue = dplyr::if_else(.data$rowId == 4, NA_real_, .data$covariateValue))

  engineered <- featureEngineer(train, settings)

  values <- labValues(engineered)
  expect_equal(values$rowId, 1:4)
  # patient 4's group (over 50) has no training value, so it gets the overall mean of 10, 20 and 30
  expect_equal(values$covariateValue, c(10, 20, 30, 20))
  # patient 3 has no age, so its value only counts towards the overall mean
  means <- featureEngineeringSteps(
    attr(engineered$covariateData, "metaData")$featureEngineering
  )[[1]]$settings$stratifiedMeans
  expect_false(anyNA(means$ageGroup))
})

test_that("stratified imputation without training values warns and replays after a JSON round trip", {
  settings <- createStratifiedImputationSettings(covariateId = 12101, ageSplits = 50)
  train <- createStratifiedData(rowIds = 1:3, ageYear = 30, gender = 8507, values = c(NA, NA, NA))
  on.exit(Andromeda::close(train$covariateData), add = TRUE)

  expect_warning(engineered <- featureEngineer(train, settings), "no values of covariate 12101")
  saved <- attr(engineered$covariateData, "metaData")$featureEngineering
  jsonFile <- tempfile(fileext = ".json")
  on.exit(unlink(jsonFile), add = TRUE)
  saveSettingsToJsonFullPrecision(list(featureEngineering = saved), jsonFile)
  loaded <- ParallelLogger::loadSettingsFromJson(jsonFile)$featureEngineering

  newData <- createStratifiedData(rowIds = 11:12, ageYear = 30, gender = 8507, values = c(NA, 5), labels = FALSE)
  on.exit(Andromeda::close(newData$covariateData), add = TRUE)
  replayed <- applyFeatureEngineering(newData, loaded)
  expect_equal(labValues(replayed)$rowId, 12)
})

test_that("runPlp with stratified imputation completes with the covariate summary", {
  skip_on_cran()
  data("simulationProfile", package = "PatientLevelPrediction")
  simulated <- simulatePlpData(simulationProfile, n = 800, seed = 42)
  on.exit(Andromeda::close(simulated$covariateData), add = TRUE)
  before <- simulated$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull()

  result <- runPlp(
    plpData = simulated,
    outcomeId = 3,
    analysisId = "stratifiedImputation",
    populationSettings = createStudyPopulationSettings(requireTimeAtRisk = FALSE),
    featureEngineeringSettings = createStratifiedImputationSettings(covariateId = 1002),
    modelSettings = setLassoLogisticRegression(seed = 42),
    logSettings = createLogSettings(verbosity = "ERROR"),
    executeSettings = createExecuteSettings(
      runSplitData = TRUE, runFeatureEngineering = TRUE, runPreprocessData = TRUE,
      runModelDevelopment = TRUE, runCovariateSummary = TRUE
    ),
    saveDirectory = file.path(saveLoc, "stratifiedImputation")
  )

  expect_false(is.null(result$covariateSummary))
  expect_equal(
    stepFunctions(result$model$preprocessing$featureEngineering),
    "stratifiedImputeCovariates"
  )
  expect_equal(simulated$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull(), before)
})
