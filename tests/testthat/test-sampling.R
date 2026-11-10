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
testType <- sample(c("none", "underSample", "overSample"), 1)
testNumberOutcomestoNonOutcomes <- 2
testSampleSeed <- sample(10000, 1)

sampleSettingFunc <- function(
    type = testType,
    numberOutcomestoNonOutcomes = testNumberOutcomestoNonOutcomes,
    sampleSeed = testSampleSeed) {
  result <- createSampleSettings(
    type = type,
    numberOutcomestoNonOutcomes = numberOutcomestoNonOutcomes,
    sampleSeed = sampleSeed
  )

  return(result)
}



test_that("createSampleSettings works", {
  sampleSettings <- sampleSettingFunc()
  expect_s3_class(sampleSettings, "sampleSettings")

  sampleFun <- "sameData"
  if (testType == "underSample") {
    sampleFun <- "underSampleData"
  }
  if (testType == "overSample") {
    sampleFun <- "overSampleData"
  }

  expect_equal(
    attr(sampleSettings, "fun"),
    sampleFun
  )

  expect_equal(
    sampleSettings$numberOutcomestoNonOutcomes,
    testNumberOutcomestoNonOutcomes
  )

  # the seed is ignored if sameData
  if (testType == "none") {
    testSampleSeed <- 1
  }
  expect_equal(
    sampleSettings$sampleSeed,
    testSampleSeed
  )
})


test_that("createSampleSettings expected errors", {
  expect_error(
    sampleSettingFunc(numberOutcomestoNonOutcomes = "fsfd")
  )
  expect_error(
    sampleSettingFunc(numberOutcomestoNonOutcomes = -1)
  )

  expect_error(
    sampleSettingFunc(sampleSeed = "fsfd")
  )

  expect_error(
    sampleSettingFunc(type = "fsfd")
  )
  expect_error(
    sampleSettingFunc(type = NULL)
  )
})


test_that("sampleData outputs are correct", {
  skip_if_not_installed("Eunomia")
  skip_if_offline()
  newTrainData <- trainData
  attr(newTrainData, "metaData")$sampleSettings <- NULL # remove for test

  sampleSettings <- sampleSettingFunc(type = "none")

  sampleData <- sampleData(newTrainData, sampleSettings)

  # make sure metaData captures
  expect_equal(
    length(attr(sampleData, "metaData")),
    length(attr(newTrainData, "metaData")) + 1
  )

  expect_equal(
    attr(sampleData, "metaData")$sampleSettings[[1]],
    sampleSettings
  )

  # check the data is the same:
  expect_equal(
    nrow(sampleData$labels),
    nrow(newTrainData$labels)
  )

  expect_equal(
    nrow(sampleData$folds),
    nrow(newTrainData$folds)
  )

  expect_equal(
    sampleData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull(),
    newTrainData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull()
  )
})

# specific functions for sampling


test_that("underSampleData works", {
  skip_if_not_installed("Eunomia")
  skip_if_offline()
  newTrainData <- trainData

  sampleSettings <- list(
    sampleSeed = 1,
    numberOutcomestoNonOutcomes = 1
  )

  underSampleData <- underSampleData(newTrainData, sampleSettings)

  expect_true(inherits(underSampleData, "plpData")) # add test based on github issue

  # the sampled data should be smaller...
  expect_true(nrow(underSampleData$labels) <= nrow(newTrainData$labels))

  expect_true(nrow(underSampleData$folds) <= nrow(newTrainData$folds))

  expect_true(
    underSampleData$covariateData$covariates %>%
      dplyr::tally() %>%
      dplyr::pull() <= newTrainData$covariateData$covariates %>%
      dplyr::tally() %>%
      dplyr::pull()
  )
  expect_true(
    underSampleData$covariateData$analysisRef %>%
      dplyr::pull(.data$analysisId) %>% 
      length() > 0
  )

  # perhaps add manual data test
})


test_that("overSampleData works", {
  skip_if_not_installed("Eunomia")
  skip_if_offline()
  newTrainData <- trainData

  sampleSettings <- list(
    sampleSeed = 1,
    numberOutcomestoNonOutcomes = 0.5
  )

  overSampleData <- overSampleData(newTrainData, sampleSettings)

  expect_true(inherits(overSampleData, "plpData")) # add test based on github issue

  # the sampled data should be smaller...
  expect_true(nrow(overSampleData$labels) >= nrow(newTrainData$labels))

  expect_true(nrow(overSampleData$folds) >= nrow(newTrainData$folds))

  expect_true(
    overSampleData$covariateData$covariates %>%
      dplyr::tally() %>%
      dplyr::pull() >= newTrainData$covariateData$covariates %>%
      dplyr::tally() %>%
      dplyr::pull()
  )

  expect_true(
    overSampleData$covariateData$analysisRef %>%
      dplyr::pull(.data$analysisId) %>% 
      length() > 0
  )
  # perhaps add manual data test
})

# synthetic train data for the oversampling and undersampling regressions
createSamplingFixture <- function(labels, folds, covariates) {
  covariateData <- Andromeda::andromeda(
    covariates = covariates,
    covariateRef = data.frame(
      covariateId = sort(unique(covariates$covariateId)),
      covariateName = "covariate",
      analysisId = 1,
      conceptId = 0
    ),
    analysisRef = data.frame(
      analysisId = 1,
      analysisName = "analysis",
      domainId = "domain",
      isBinary = "N",
      missingMeansZero = "Y"
    )
  )
  class(covariateData) <- "CovariateData"
  attr(covariateData, "metaData") <- list(populationSize = nrow(labels))
  fixture <- list(labels = labels, folds = folds, covariateData = covariateData)
  class(fixture) <- "plpData"
  return(fixture)
}

# subjectId is unique per original row, so it identifies the source of a copy
expectCompleteCopies <- function(sampled, original) {
  sampledCovariates <- sampled$covariateData$covariates %>%
    dplyr::collect() %>%
    as.data.frame()
  originalCovariates <- original$covariateData$covariates %>%
    dplyr::collect() %>%
    as.data.frame()

  expect_equal(anyDuplicated(sampled$labels$rowId), 0)
  expect_equal(nrow(sampled$folds), nrow(sampled$labels))
  expect_setequal(sampled$folds$rowId, sampled$labels$rowId)
  expect_true(all(sampledCovariates$rowId %in% sampled$labels$rowId))

  # original rows are untouched
  kept <- sampled$labels[sampled$labels$rowId %in% original$labels$rowId, ]
  expect_equal(nrow(kept), nrow(original$labels))
  expect_equal(kept$subjectId, original$labels$subjectId)

  copies <- sampled$labels[!sampled$labels$rowId %in% original$labels$rowId, ]
  expect_true(all(copies$outcomeCount > 0))
  for (i in seq_len(nrow(copies))) {
    source <- original$labels[original$labels$subjectId == copies$subjectId[i], ]
    expect_equal(copies$outcomeCount[i], source$outcomeCount)
    expect_equal(copies$survivalTime[i], source$survivalTime)
    expect_equal(
      sampled$folds$index[sampled$folds$rowId == copies$rowId[i]],
      original$folds$index[original$folds$rowId == source$rowId]
    )
    copyCovariates <- sampledCovariates[sampledCovariates$rowId == copies$rowId[i], ]
    sourceCovariates <- originalCovariates[originalCovariates$rowId == source$rowId, ]
    copyCovariates <- copyCovariates[order(copyCovariates$covariateId), ]
    sourceCovariates <- sourceCovariates[order(sourceCovariates$covariateId), ]
    expect_equal(copyCovariates$covariateId, sourceCovariates$covariateId)
    expect_equal(copyCovariates$covariateValue, sourceCovariates$covariateValue)
  }
  return(invisible(copies))
}

test_that("overSampleData copies labels, folds and covariates with unique rowIds", {
  labels <- data.frame(
    rowId = 1:6, subjectId = 101:106,
    outcomeCount = c(0, 0, 0, 0, 1, 1), survivalTime = 11:16
  )
  folds <- data.frame(rowId = 1:6, index = 1L)
  covariates <- data.frame(
    rowId = 1:6, covariateId = 10, covariateValue = c(1, 4, 2, 3, 2, 4)
  )
  original <- createSamplingFixture(labels, folds, covariates)
  withr::defer(Andromeda::close(original$covariateData))
  settings <- list(sampleSeed = 42, numberOutcomestoNonOutcomes = 1)

  sampled <- overSampleData(original, settings)
  withr::defer(Andromeda::close(sampled$covariateData))

  expect_equal(nrow(sampled$labels), 8)
  expect_equal(sum(sampled$labels$outcomeCount > 0), 4)
  copies <- expectCompleteCopies(sampled, original)
  expect_equal(nrow(copies), 2)
  expect_equal(
    attr(sampled$covariateData, "metaData")$populationSize,
    nrow(sampled$labels)
  )
})

test_that("overSampleData keeps rowIds unique across folds and repeated draws", {
  # gaps in rowId, as after a train/test split, and more copies than cases
  # so that a fold needs several draws
  caseIds <- c(2, 4, 6, 8)
  controlIds <- seq(10, 88, by = 2)
  rowIds <- c(caseIds, controlIds)
  labels <- data.frame(
    rowId = rowIds, subjectId = rowIds + 1000,
    outcomeCount = c(rep(1, 4), rep(0, length(controlIds))),
    survivalTime = 1
  )
  folds <- data.frame(rowId = rowIds, index = rep_len(1:2, length(rowIds)))
  # two covariates each, and case 8 has none
  covariates <- rbind(
    data.frame(rowId = rowIds[rowIds != 8], covariateId = 10, covariateValue = rowIds[rowIds != 8]),
    data.frame(rowId = rowIds[rowIds != 8], covariateId = 20, covariateValue = rowIds[rowIds != 8] * 2)
  )
  original <- createSamplingFixture(labels, folds, covariates)
  withr::defer(Andromeda::close(original$covariateData))

  sampled <- overSampleData(original, list(sampleSeed = 7, numberOutcomestoNonOutcomes = 1))
  withr::defer(Andromeda::close(sampled$covariateData))

  copies <- expectCompleteCopies(sampled, original)
  expect_gt(nrow(copies), length(caseIds))
  expect_gt(min(copies$rowId), max(rowIds))
  # the classes end up balanced in each fold
  merged <- merge(sampled$labels, sampled$folds, by = "rowId")
  byFold <- table(merged$index, merged$outcomeCount > 0)
  expect_equal(unname(byFold[, "FALSE"]), unname(byFold[, "TRUE"]))
})

test_that("overSampleData only copies the sole case of a fold", {
  # sample(100, n, replace = TRUE) would draw from 1:100
  labels <- data.frame(
    rowId = c(100, 1:10), subjectId = c(1100, 1001:1010),
    outcomeCount = c(1, rep(0, 10)), survivalTime = 1
  )
  folds <- data.frame(rowId = labels$rowId, index = 1L)
  covariates <- data.frame(rowId = labels$rowId, covariateId = 10, covariateValue = labels$rowId)
  original <- createSamplingFixture(labels, folds, covariates)
  withr::defer(Andromeda::close(original$covariateData))

  sampled <- overSampleData(original, list(sampleSeed = 3, numberOutcomestoNonOutcomes = 1))
  withr::defer(Andromeda::close(sampled$covariateData))

  copies <- expectCompleteCopies(sampled, original)
  expect_equal(nrow(copies), 9)
  expect_true(all(copies$subjectId == 1100))
})

test_that("Cyclops fits oversampled data as complete patient copies", {
  labels <- data.frame(
    rowId = 1:6, subjectId = 101:106,
    outcomeCount = c(0, 0, 0, 0, 1, 1), survivalTime = 1
  )
  covariates <- data.frame(
    rowId = 1:6, covariateId = 10, covariateValue = c(1, 4, 2, 3, 2, 4)
  )
  original <- createSamplingFixture(labels, data.frame(rowId = 1:6, index = 1L), covariates)
  withr::defer(Andromeda::close(original$covariateData))
  sampled <- overSampleData(original, list(sampleSeed = 42, numberOutcomestoNonOutcomes = 1))
  withr::defer(Andromeda::close(sampled$covariateData))

  fitData <- Andromeda::andromeda(
    outcomes = sampled$labels %>%
      dplyr::mutate(y = .data$outcomeCount, time = .data$survivalTime),
    covariates = sampled$covariateData$covariates %>% dplyr::collect()
  )
  withr::defer(Andromeda::close(fitData))
  cyclopsData <- Cyclops::convertToCyclopsData(
    outcomes = fitData$outcomes,
    covariates = fitData$covariates,
    addIntercept = TRUE,
    modelType = "lr",
    checkRowIds = FALSE,
    normalize = NULL,
    quiet = TRUE
  )
  fit <- Cyclops::fitCyclopsModel(
    cyclopsData,
    prior = Cyclops::createPrior("none"),
    control = Cyclops::createControl(tolerance = 1e-10, maxIterations = 10000)
  )

  # the reference model duplicates each selected case by subjectId
  sourceRows <- match(sampled$labels$subjectId, labels$subjectId)
  reference <- stats::glm(
    y ~ x,
    family = stats::binomial(),
    data = data.frame(
      y = labels$outcomeCount[sourceRows],
      x = covariates$covariateValue[sourceRows]
    )
  )
  expect_equal(unname(stats::coef(fit)), unname(stats::coef(reference)), tolerance = 1e-4)
})

test_that("toSparseM keeps oversampled rows aligned with their covariates", {
  labels <- data.frame(
    rowId = 1:6, subjectId = 101:106,
    outcomeCount = c(0, 0, 0, 0, 1, 1), survivalTime = 1
  )
  covariates <- data.frame(
    rowId = 1:6, covariateId = 10, covariateValue = c(1, 4, 2, 3, 2, 4)
  )
  original <- createSamplingFixture(labels, data.frame(rowId = 1:6, index = 1L), covariates)
  withr::defer(Andromeda::close(original$covariateData))
  sampled <- overSampleData(original, list(sampleSeed = 42, numberOutcomestoNonOutcomes = 1))
  withr::defer(Andromeda::close(sampled$covariateData))

  mapped <- toSparseM(sampled, sampled$labels)

  expect_equal(nrow(mapped$dataMatrix), 8)
  sourceValue <- covariates$covariateValue[match(mapped$labels$subjectId, labels$subjectId)]
  expect_equal(unname(as.numeric(mapped$dataMatrix[mapped$labels$rowId, 1])), sourceValue)
})

test_that("oversampled copies are not counted as test patients in the covariate summary", {
  data("simulationProfile", package = "PatientLevelPrediction", envir = environment())
  plpData <- simulatePlpData(simulationProfile, n = 500, seed = 42)
  outcomeId <- plpData$metaData$databaseDetails$outcomeIds[1]
  populationSettings <- createStudyPopulationSettings(
    requireTimeAtRisk = FALSE,
    riskWindowStart = 1,
    riskWindowEnd = 365
  )
  population <- createStudyPopulation(
    plpData,
    outcomeId = outcomeId,
    populationSettings = populationSettings
  )

  # every test rowId is above the largest train rowId, and few cases are kept
  # in train, so that the new rowIds of the copies equal test rowIds
  rowIds <- sort(population$rowId)
  trainCandidates <- rowIds[seq_len(floor(length(rowIds) * 0.75))]
  testIds <- setdiff(rowIds, trainCandidates)
  cases <- intersect(trainCandidates, population$rowId[population$outcomeCount > 0])
  trainIds <- setdiff(trainCandidates, cases[-(1:20)])
  splitIds <- rbind(
    data.frame(rowId = trainIds, index = rep_len(1:2, length(trainIds))),
    data.frame(rowId = testIds, index = -1)
  )

  result <- runPlp(
    plpData = plpData,
    outcomeId = outcomeId,
    populationSettings = populationSettings,
    splitSettings = createExistingSplitSettings(splitIds),
    sampleSettings = createSampleSettings(
      type = "overSample",
      numberOutcomestoNonOutcomes = 1,
      sampleSeed = 1
    ),
    executeSettings = createExecuteSettings(
      runSplitData = TRUE,
      runSampleData = TRUE,
      runCovariateSummary = TRUE
    ),
    saveDirectory = withr::local_tempdir()
  )

  summary <- as.data.frame(result$covariateSummary)
  observed <- function(column) {
    values <- summary[[column]]
    values[is.na(values)] <- 0
    return(values)
  }
  covariates <- plpData$covariateData$covariates %>% dplyr::collect()
  countIn <- function(ids) {
    vapply(summary$covariateId, function(covariateId) {
      sum(covariates$covariateId == covariateId & covariates$rowId %in% ids)
    }, numeric(1))
  }
  expect_equal(
    observed("TrainWithOutcome_CovariateCount") + observed("TrainWithNoOutcome_CovariateCount"),
    countIn(trainIds)
  )
  expect_equal(
    observed("TestWithOutcome_CovariateCount") + observed("TestWithNoOutcome_CovariateCount"),
    countIn(testIds)
  )
})

test_that("underSampleData keeps the sole control of a fold", {
  # sample(100, 1) would draw from 1:100
  labels <- data.frame(
    rowId = c(201:206, 100L, 301:306, 1:30),
    subjectId = 1:43,
    outcomeCount = c(rep(1, 6), 0, rep(1, 6), rep(0, 30)),
    survivalTime = 1
  )
  folds <- data.frame(rowId = labels$rowId, index = c(rep(1L, 7), rep(2L, 36)))
  covariates <- data.frame(rowId = labels$rowId, covariateId = 10, covariateValue = 1)
  original <- createSamplingFixture(labels, folds, covariates)
  withr::defer(Andromeda::close(original$covariateData))

  for (seed in 1:10) {
    sampled <- suppressWarnings(
      underSampleData(original, list(sampleSeed = seed, numberOutcomestoNonOutcomes = 1))
    )
    fold1 <- sampled$folds$rowId[sampled$folds$index == 1]
    fold2 <- sampled$folds$rowId[sampled$folds$index == 2]
    expect_true(100L %in% fold1)
    expect_setequal(fold1, c(201:206, 100L))
    expect_equal(length(setdiff(fold2, 301:306)), 6)
    Andromeda::close(sampled$covariateData)
  }
})

test_that("underSampleData draws controls as before when a fold has several", {
  labels <- data.frame(
    rowId = 1:40,
    subjectId = 1:40,
    outcomeCount = c(rep(1, 10), rep(0, 30)),
    survivalTime = 1
  )
  folds <- data.frame(rowId = 1:40, index = 1L)
  covariates <- data.frame(rowId = 1:40, covariateId = 10, covariateValue = 1)
  original <- createSamplingFixture(labels, folds, covariates)
  withr::defer(Andromeda::close(original$covariateData))

  sampled <- underSampleData(original, list(sampleSeed = 5, numberOutcomestoNonOutcomes = 1))
  withr::defer(Andromeda::close(sampled$covariateData))

  set.seed(5)
  expected <- sample(11:40, 10)
  expect_setequal(sampled$labels$rowId, c(1:10, expected))
  expect_setequal(sampled$folds$rowId, c(1:10, expected))
})

test_that("overSampleData stops on a fold without outcomes", {
  labels <- data.frame(
    rowId = 1:20, subjectId = 1001:1020,
    outcomeCount = c(1, 1, rep(0, 8), rep(0, 10)), survivalTime = 1
  )
  folds <- data.frame(rowId = labels$rowId, index = rep(1:2, each = 10))
  covariates <- data.frame(rowId = labels$rowId, covariateId = 10, covariateValue = 1)
  original <- createSamplingFixture(labels, folds, covariates)
  withr::defer(Andromeda::close(original$covariateData))

  expect_error(
    overSampleData(original, list(sampleSeed = 1, numberOutcomestoNonOutcomes = 1)),
    "Cannot oversample fold 2 because it has no outcomes"
  )
})
