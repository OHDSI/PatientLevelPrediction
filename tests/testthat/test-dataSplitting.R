# @file test_DataSplitting.R
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
# make sure pop is all plpData people
if (internet) {
  populationT <- plpData$cohorts
  populationT$outcomeCount <- sample(c(0, 1), nrow(populationT), replace = TRUE)
  attr(populationT, "metaData")$outcomeId <- outcomeId
  attr(populationT, "metaData")$populationSettings <- list(madeup = TRUE)
  attr(populationT, "metaData")$restrictPlpDataSettings <- list(madeup = TRUE)
  attr(populationT, "metaData")$attrition <- c(1, 2, 3)
}
# check correct inputs
testFraction1 <- sample(9, 1) / 10
trainFraction1 <- 1 - testFraction1
splitSeed1 <- sample(100000, 1)
nfold1 <- 1 + sample(10, 1)
type1 <- sample(c("stratified", "time", "subject"), 1)

defaultSetting <- function(
    testFraction = testFraction1,
    trainFraction = trainFraction1,
    splitSeed = splitSeed1,
    nfold = nfold1,
    type = type1) {
  result <- createDefaultSplitSetting(
    testFraction = testFraction,
    trainFraction = trainFraction,
    splitSeed = splitSeed,
    nfold = nfold,
    type = type
  )
  return(result)
}


test_that("createDefaultSplitSetting", {
  splitSettings <- defaultSetting()

  expect_s3_class(splitSettings, "splitSettings")

  expectFun <- "randomSplitter"
  if (type1 == "time") {
    expectFun <- "timeSplitter"
  }
  if (type1 == "subject") {
    expectFun <- "subjectSplitter"
  }

  expect_equal(attr(splitSettings, "fun"), expectFun)

  expect_equal(splitSettings$test, testFraction1)
  expect_equal(splitSettings$train, trainFraction1)
  expect_equal(splitSettings$seed, splitSeed1)
  expect_equal(splitSettings$nfold, nfold1)

  # check input errors for testFraction
  expect_error(
    defaultSetting(testFraction = "character")
  )

  expect_error(
    defaultSetting(testFraction = -0.1)
  )

  expect_error(
    defaultSetting(testFraction = 1.001)
  )

  # check input error for trainFraction
  expect_error(
    defaultSetting(trainFraction = "trainFraction")
  )

  expect_error(
    defaultSetting(trainFraction = 1.2)
  )

  expect_error(
    defaultSetting(trainFraction = -0.2)
  )

  # check error for splitSeed

  expect_error(
    defaultSetting(splitSeed = NULL)
  )

  expect_error(
    defaultSetting(splitSeed = "NULL")
  )

  # check error for nfold
  expect_error(
    defaultSetting(nfold = NULL)
  )
  expect_error(
    defaultSetting(nfold = "NULL")
  )

  # incorrect type
  expect_error(
    defaultSetting(type = "madeup")
  )
  expect_error(
    defaultSetting(type = NULL)
  )
  expect_error(
    defaultSetting(type = 1)
  )
})

test_that("createOutcomeLimitedSplitSettings", {
  splitSettings <- createOutcomeLimitedSplitSettings(
    maxTrainingOutcomes = 50,
    splitSeed = 123,
    nfold = 5,
    type = "subject"
  )

  expect_s3_class(splitSettings, "splitSettings")
  expect_equal(attr(splitSettings, "fun"), "outcomeLimitedSplitter")
  expect_equal(splitSettings$maxTrainingOutcomes, 50)
  expect_equal(splitSettings$seed, 123)
  expect_equal(splitSettings$nfold, 5)
  expect_equal(splitSettings$type, "subject")

  expect_error(createOutcomeLimitedSplitSettings(maxTrainingOutcomes = 0))
  expect_error(createOutcomeLimitedSplitSettings(maxTrainingOutcomes = "50"))
  expect_error(createOutcomeLimitedSplitSettings(maxTrainingOutcomes = 10.5))
  expect_error(createOutcomeLimitedSplitSettings(maxTrainingOutcomes = 50, splitSeed = NULL))
  expect_error(createOutcomeLimitedSplitSettings(maxTrainingOutcomes = 50, nfold = 1))
  expect_error(createOutcomeLimitedSplitSettings(maxTrainingOutcomes = 50, nfold = 2.5))
  expect_error(createOutcomeLimitedSplitSettings(maxTrainingOutcomes = 10, nfold = 3))
  expect_error(createOutcomeLimitedSplitSettings(maxTrainingOutcomes = 50, type = "time"))
})



test_that("Main split function: splitData", {
  skip_if_offline()
  # check default settings with test/train
  splitSettings <- defaultSetting()

  splitData <- splitData(
    plpData = plpData,
    population = populationT,
    splitSettings = splitSettings
  )

  # check class
  expect_s3_class(splitData, "splitData")

  # should have test/train
  expect_equal(names(splitData), c("Train", "Test"))

  # train and test are CovariateData
  expect_s4_class(splitData$Train$covariateData, "CovariateData")
  expect_s4_class(splitData$Test$covariateData, "CovariateData")

  # Train has labels/folds/covariateData
  expect_equal(names(splitData$Train), c("labels", "folds", "covariateData"))

  # Test has labels/covariateData
  expect_equal(names(splitData$Test), c("labels", "covariateData"))

  # check attributes for Train
  expect_equal(attr(splitData$Train, "metaData")$outcomeId, attr(populationT, "metaData")$outcomeId)
  expect_equal(attr(splitData$Train, "metaData")$targetId, plpData$metaData$databaseDetails$targetId)
  expect_equal(
    attr(splitData$Train, "metaData")$cdmDatabaseSchema,
    plpData$metaData$databaseDetails$cdmDatabaseSchema
  )

  expect_type(attr(splitData$Train, "metaData")$restrictPlpDataSettings, "list")
  expect_equal(
    attr(splitData$Train, "metaData")$covariateSettings,
    plpData$metaData$covariateSettings
  )
  expect_equal(
    attr(splitData$Train, "metaData")$populationSettings,
    attr(populationT, "metaData")$populationSettings
  )
  expect_equal(
    attr(splitData$Train, "metaData")$attrition,
    attr(populationT, "metaData")$attrition
  )

  expect_equal(
    attr(splitData$Train, "metaData")$splitSettings,
    splitSettings
  )

  # train+test should be full data as train+test = 1
  expect_equal(
    nrow(splitData$Train$labels) + nrow(splitData$Test$labels),
    nrow(populationT)
  )
  expect_equal(
    splitData$Train$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull() +
      splitData$Test$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull(),
    plpData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull()
  )

  # make sure nfolds is correct
  expect_equal(
    min(splitData$Train$folds$index),
    1
  )

  expect_equal(
    max(splitData$Train$folds$index),
    splitSettings$nfold
  )


  # check when test is 0
  splitSettings <- defaultSetting(
    testFraction = 0,
    trainFraction = 1
  )

  splitData <- splitData(
    plpData = plpData,
    population = populationT,
    splitSettings = splitSettings
  )

  # should just have train
  expect_equal(names(splitData), c("Train"))

  # train labels should be the same size at the population
  expect_equal(
    nrow(splitData$Train$labels),
    nrow(populationT)
  )
  expect_equal(
    splitData$Train$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull(),
    plpData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull()
  )
})

test_that("outcome-limited row splitting caps training outcomes", {
  dsPopulation <- data.frame(
    rowId = 1:400,
    outcomeCount = c(rep(1, 100), rep(0, 300))
  )
  splitSettings <- createOutcomeLimitedSplitSettings(
    maxTrainingOutcomes = 40,
    splitSeed = 1,
    nfold = 4,
    type = "stratified"
  )

  split <- outcomeLimitedSplitter(population = dsPopulation, splitSettings = splitSettings)
  splitRepeat <- outcomeLimitedSplitter(population = dsPopulation, splitSettings = splitSettings)
  test <- dsPopulation %>%
    dplyr::inner_join(split, by = "rowId")

  expect_equal(sum(test$index > 0 & test$outcomeCount == 1), 40)
  expect_equal(sum(test$index > 0 & test$outcomeCount == 0), 120)
  expect_equal(sum(test$index < 0), 240)
  expect_equal(sum(test$index == 0), 0)
  expect_equal(sort(unique(test$index[test$index > 0])), 1:4)
  expect_equal(as.integer(table(test$outcomeCount[test$index > 0])), c(120L, 40L))
  expect_equal(split, splitRepeat)
})

test_that("outcome-limited subject splitting keeps subjects together", {
  dsPopulation <- data.frame(
    rowId = 1:300,
    subjectId = rep(1:150, each = 2),
    outcomeCount = c(rep(c(1, 0), 50), rep(0, 200))
  )
  splitSettings <- createOutcomeLimitedSplitSettings(
    maxTrainingOutcomes = 20,
    splitSeed = 1,
    nfold = 4,
    type = "subject"
  )

  split <- outcomeLimitedSplitter(population = dsPopulation, splitSettings = splitSettings)
  test <- dsPopulation %>%
    dplyr::inner_join(split, by = "rowId")
  subjectIndexes <- test %>%
    dplyr::group_by(.data$subjectId) %>%
    dplyr::summarise(nIndexes = dplyr::n_distinct(.data$index), .groups = "drop")

  expect_equal(sum(test$index > 0 & test$outcomeCount == 1), 20)
  expect_equal(sum(test$index > 0 & test$outcomeCount == 0), 100)
  expect_true(all(subjectIndexes$nIndexes == 1))
  expect_equal(sum(test$index == 0), 0)
  expect_equal(sort(unique(test$index[test$index > 0])), 1:4)
})

test_that("outcome-limited subject splitting fails without test outcomes", {
  dsPopulation <- data.frame(
    rowId = 1:125,
    subjectId = c(rep(1, 25), 2:101),
    outcomeCount = c(rep(1, 25), rep(0, 100))
  )
  splitSettings <- createOutcomeLimitedSplitSettings(
    maxTrainingOutcomes = 20,
    splitSeed = 1,
    nfold = 4,
    type = "subject"
  )

  expect_error(
    outcomeLimitedSplitter(population = dsPopulation, splitSettings = splitSettings),
    "would leave no outcome-positive rows in the test set"
  )
})

test_that("outcome-limited subject splitting assigns folds by subject count", {
  dsPopulation <- data.frame(
    rowId = 1:330,
    subjectId = c(rep(1, 10), rep(2, 10), rep(3, 10), 4:303),
    outcomeCount = c(rep(1, 30), rep(0, 300))
  )
  splitSettings <- createOutcomeLimitedSplitSettings(
    maxTrainingOutcomes = 20,
    splitSeed = 1,
    nfold = 4,
    type = "subject"
  )

  split <- outcomeLimitedSplitter(population = dsPopulation, splitSettings = splitSettings)
  test <- dsPopulation %>%
    dplyr::inner_join(split, by = "rowId")
  outcomeSubjectFolds <- test %>%
    dplyr::filter(.data$index > 0, .data$outcomeCount > 0) %>%
    dplyr::distinct(.data$subjectId, .data$index)

  expect_equal(nrow(outcomeSubjectFolds), 2)
  expect_equal(sort(outcomeSubjectFolds$index), c(1, 2))
})

test_that("outcome-limited splitting falls back when outcome cap is not triggered", {
  dsPopulation <- data.frame(
    rowId = 1:200,
    outcomeCount = c(rep(1, 40), rep(0, 160))
  )
  splitSettings <- createOutcomeLimitedSplitSettings(
    maxTrainingOutcomes = 100,
    splitSeed = 1,
    nfold = 4,
    type = "stratified"
  )
  fallbackSettings <- createDefaultSplitSetting(
    testFraction = 0.25,
    trainFraction = 0.75,
    splitSeed = 1,
    nfold = 4,
    type = "stratified"
  )

  expect_equal(
    outcomeLimitedSplitter(population = dsPopulation, splitSettings = splitSettings),
    randomSplitter(population = dsPopulation, splitSettings = fallbackSettings)
  )
})

test_that("splitData supports outcome-limited split settings", {
  skip_if_offline()
  splitSettings <- createOutcomeLimitedSplitSettings(
    maxTrainingOutcomes = 10,
    splitSeed = 42,
    nfold = 2,
    type = "stratified"
  )

  splitData <- splitData(
    plpData = plpData,
    population = populationT,
    splitSettings = splitSettings
  )

  expect_s3_class(splitData, "splitData")
  expect_equal(names(splitData), c("Train", "Test"))
  expect_s4_class(splitData$Train$covariateData, "CovariateData")
  expect_s4_class(splitData$Test$covariateData, "CovariateData")
  expect_equal(sum(splitData$Train$labels$outcomeCount > 0), 10)
  expect_equal(attr(splitData$Train, "metaData")$splitSettings, splitSettings)
})

test_that("dataSummary works", {
  skip_if_offline()
  splitSettings <- defaultSetting(
    testFraction = 0,
    trainFraction = 1
  )

  splitData <- splitData(
    plpData = plpData,
    population = populationT,
    splitSettings = splitSettings
  )

  summaryPrint <- dataSummary(splitData)
  expect_equal(summaryPrint, TRUE)
})


test_that("Data stratified splitting", {
  splitSettings <- defaultSetting(
    test = 0.3,
    nfold = 3
  )

  # error due to insufficient outcomes
  dsPopulation1 <- data.frame(rowId = 1:20, outcomeCount = c(1, 1, 1, 1, rep(0, 16)))
  expect_error(randomSplitter(population = dsPopulation1, splitSettings = splitSettings))

  dsPopulation2 <- data.frame(rowId = 1:200, outcomeCount = c(rep(1, 42), rep(0, 158)))
  splitSettings <- defaultSetting(
    train = 0.7,
    test = 0.3,
    nfold = 4
  )
  # fold creation check 1 (fixed)
  test <- randomSplitter(population = dsPopulation2, splitSettings = splitSettings)
  test <- merge(dsPopulation2, test)
  test <- table(test$outcomeCount, test$index)
  testReturned <- paste(test, collapse = "-")
  testExpected <- paste(matrix(c(47, 28, 28, 28, 27, 12, 8, 8, 7, 7), ncol = 5, byrow = TRUE), collapse = "-")
  expect_identical(testReturned, testExpected)

  # fold creation check 2 (sum)
  size <- 500
  dsPopulation3 <- data.frame(rowId = 1:size, outcomeCount = c(rep(1, floor(size / 3)), rep(0, size - floor(size / 3))))
  splitSettings <- defaultSetting(
    train = 0.8,
    test = 0.2,
    nfold = 4
  )
  test <- randomSplitter(population = dsPopulation3, splitSettings = splitSettings)
  test <- merge(dsPopulation3, test)
  test <- table(test$outcomeCount, test$index)
  expect_equal(sum(test), size)

  # test the training fraction parameter for learning curves
  size <- 500
  dsPopulation4 <- data.frame(
    rowId = 1:size,
    outcomeCount = c(
      rep(1, floor(size / 3)),
      rep(0, size - floor(size / 3))
    )
  )
  splitSettings <- defaultSetting(
    train = 0.4,
    test = 0.2,
    nfold = 4
  )
  test <- randomSplitter(population = dsPopulation4, splitSettings = splitSettings)

  tolerance <- 5
  excludedPatients <- 200
  # test, if the number of patients in each fold are roughly the same
  expect_equal(length(test$index[test$index == 1]),
    length(test$index[test$index == 3]),
    tolerance = tolerance
  )
  expect_equal(length(test$index[test$index == 2]),
    length(test$index[test$index == 4]),
    tolerance = tolerance
  )
  expect_equal(length(test$index[test$index == 1]),
    length(test$index[test$index == 4]),
    tolerance = tolerance
  )
  # test, if patients were excluded according to the training fraction
  expect_equal(
    length(test$index[test$index == 0]),
    excludedPatients
  )
})

test_that("Data splitting by time", {
  # fold creation check (sum)
  size <- 500
  set.seed(1)
  dsPopulation2 <- data.frame(
    rowId = 1:size,
    outcomeCount = sample(0:1, size, replace = TRUE),
    cohortStartDate = as.Date("2010-01-01") + c(1:size)
  )
  splitSettings <- defaultSetting(
    train = 0.8,
    test = 0.2,
    nfold = 4
  )

  test <- timeSplitter(population = dsPopulation2, splitSettings = splitSettings)
  test <- merge(dsPopulation2, test)
  test <- table(test$outcomeCount, test$index)
  expect_equal(sum(test), size)

  # test the training fraction parameter for learning curves
  size <- 500
  set.seed(1)
  dsPopulation3 <- data.frame(
    rowId = 1:size,
    outcomeCount = sample(0:1, size, replace = TRUE),
    cohortStartDate = as.Date("2010-01-01") + c(1:size)
  )
  splitSettings <- defaultSetting(
    train = 0.4,
    test = 0.2,
    nfold = 4
  )
  test <- timeSplitter(population = dsPopulation3, splitSettings = splitSettings)

  tolerance <- 5
  excludedPatients <- 196
  # test, if the number of patients in each fold are roughly the same
  expect_equal(length(test$index[test$index == 1]),
    length(test$index[test$index == 3]),
    tolerance = tolerance
  )
  expect_equal(length(test$index[test$index == 2]),
    length(test$index[test$index == 4]),
    tolerance = tolerance
  )
  expect_equal(length(test$index[test$index == 1]),
    length(test$index[test$index == 4]),
    tolerance = tolerance
  )
  # test, if patients were excluded according to the training fraction
  expect_equal(
    length(test$index[test$index == 0]),
    excludedPatients
  )
})



test_that("Data splitting by subject", {
  # error message checks
  dsPopulation1 <- data.frame(rowId = 1:20, subjectId = 1:20, outcomeCount = c(1, 1, 1, 1, rep(0, 16)))
  splitSettings <- defaultSetting(
    train = 0.7,
    test = 0.3,
    nfold = 3
  )
  expect_error(subjectSplitter(population = dsPopulation1, splitSettings = splitSettings))

  dsPopulation2 <- data.frame(rowId = 1:200, subjectId = 1:200, outcomeCount = c(rep(1, 42), rep(0, 158)))
  splitSettings <- defaultSetting(
    train = 0.8,
    test = 0.2,
    nfold = 4
  )
  test <- subjectSplitter(population = dsPopulation2, splitSettings = splitSettings)
  test <- merge(dsPopulation2, test)
  test <- table(test$outcomeCount, test$index)
  testReturned <- paste(test, collapse = "-")
  testExpected <- paste(matrix(c(32, 32, 32, 31, 31, 8, 9, 9, 8, 8), ncol = 5, byrow = TRUE), collapse = "-")
  expect_identical(testReturned, testExpected)

  # test that people are not in multiple folds
  dsPopulation3 <- data.frame(rowId = 1:200, subjectId = rep(1:50, 4), outcomeCount = c(rep(1, 42), rep(0, 158)))
  splitSettings <- defaultSetting(
    train = 0.75,
    test = 0.25,
    nfold = 3
  )
  test <- subjectSplitter(population = dsPopulation3, splitSettings = splitSettings)
  test <- merge(dsPopulation3, test)

  expect_equal(unique(table(test$subjectId[test$index == -1])), 4)
  expect_equal(unique(table(test$subjectId[test$index == 2])), 4)
  expect_equal(unique(table(test$subjectId[test$index == 3])), 4)
  expect_equal(unique(table(test$subjectId[test$index == 1])), 4)

  # test that no subject is not assigned a fold
  expect_equal(sum(test$index == 0), 0)
})

test_that("Existing data splitter works", {
  skip_if_offline()
  # split by age
  age <- population$ageYear
  # create empty index same lengths as age
  index <- rep(0, length(age))
  index[age > 43] <- -1 # test set
  index[age <= 35] <- 1 # train fold 1
  index[age > 35 & age <= 43] <- 2 # train fold 2
  splitIds <- data.frame(rowId = population$rowId, index = index)
  splitSettings <- createExistingSplitSettings(splitIds)
  ageSplit <- splitData(
    plpData = plpData,
    population = population,
    splitSettings = splitSettings
  )

  # test only old people in test
  expect_equal(
    length(ageSplit$Test$labels$rowId),
    sum(age > 43)
  )
  # only young people in train
  expect_equal(
    length(ageSplit$Train$labels$rowId),
    sum(age <= 43)
  )
  # no overlap
  expect_equal(
    length(intersect(ageSplit$Test$labels$rowId, ageSplit$Train$labels$rowId)),
    0
  )
})

test_that("Outcome options works", {
  testPop <- data.frame(outcomeCount = sample(c(0, 1), 9, replace = TRUE))
  # regular plp outcome check
  expect_error(checkOutcomes(testPop, train = 0.75, nfold = 3))

  withr::with_options(list("plp.outcomes" = 100), {
    testPop <- data.frame(outcomeCount = sample(c(0, 1), 90, replace = TRUE))
    expect_error(checkOutcomes(testPop, train = 0.75, nfold = 3))
  })
})
