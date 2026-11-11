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

modelSettings <- setLassoLogisticRegression()

test_that("fitPlp", {
  skip_if_no_eunomia()
  plpModel <- fitPlp(
    trainData = trainData,
    modelSettings = modelSettings,
    analysisId = "fitting",
    analysisPath = tempdir()
  )

  expect_s3_class(plpModel, "plpModel")
})

test_that("fitPlp input errors", {
  skip_if_no_eunomia()
  expect_error(
    fitPlp(
      trainData = trainData,
      modelSettings = modelSettings,
      analysisPath = tempdir()
    ), 'argument "analysisId" is missing'
  )

  expect_error(
    fitPlp(
      trainData = list(covariateData = NULL),
      modelSettings = modelSettings,
      analysisId = "fitting",
      analysisPath = tempdir()
    ), "covariateData is NULL"
  )

  expect_error(
    fitPlp(
      trainData = trainData,
      modelSettings = NULL,
      analysisId = "fitting",
      analysisPath = tempdir()
    ), "modelSettings is wrong class"
  )

  expect_error(
    fitPlp(
      trainData = trainData,
      modelSettings = modelSettings,
      analysisId = "fitting"
    ), 'argument "analysisPath" is missing'
  )
})

createTuningFixture <- function(folds = c(1L, 1L, 2L, 2L)) {
  n <- length(folds)
  list(
    data = matrix(rep(c(0, 1), length.out = n), ncol = 1),
    labels = data.frame(
      rowId = seq_len(n), outcomeCount = rep(c(0, 1), length.out = n), index = folds
    ),
    settings = list(
      modelType = "binary",
      train = function(dataMatrix, labels, hyperParameters, settings) {
        list(risk = hyperParameters$risk)
      },
      predict = function(plpModel, data, cohort) {
        cohort$value <- plpModel$risk + (1 - 2 * plpModel$risk) * data[, 1]
        cohort
      }
    )
  )
}

cvValues <- function(result) {
  rows <- result$prediction[result$prediction$evaluationType == "CV", ]
  rows$value[order(rows$rowId)]
}

test_that("tied tuning scores attach the selected candidate's CV predictions", {
  fixture <- createTuningFixture()
  runGrid <- function(risks) {
    tuneHyperparameters(
      data = fixture$data, labels = fixture$labels,
      param = list(risk = as.list(risks)), settings = fixture$settings,
      hyperparamSettings = createHyperparameterSettings()
    )
  }

  ties <- runGrid(c(0.1, 0.3))
  expect_equal(ties$finalParam$risk, 0.1)
  expect_equal(ties$model$risk, 0.1)
  expect_equal(cvValues(ties), c(0.1, 0.9, 0.1, 0.9))

  reversed <- runGrid(c(0.3, 0.1))
  expect_equal(reversed$finalParam$risk, 0.3)
  expect_equal(cvValues(reversed), c(0.3, 0.7, 0.3, 0.7))
})

test_that("a strictly better later candidate still replaces the CV predictions", {
  fixture <- createTuningFixture()
  brier <- createTuningMetric(
    fun = function(prediction) mean((prediction$value - prediction$outcomeCount)^2),
    maximize = FALSE, name = "brier"
  )
  result <- tuneHyperparameters(
    data = fixture$data, labels = fixture$labels,
    param = list(risk = list(0.3, 0.1, 0.1)), settings = fixture$settings,
    hyperparamSettings = createHyperparameterSettings(tuningMetric = brier)
  )

  expect_equal(result$finalParam$risk, 0.1)
  expect_equal(cvValues(result), c(0.1, 0.9, 0.1, 0.9))
})

test_that("candidates without a usable score are not selected as best", {
  fixture <- createTuningFixture()
  metric <- list(
    name = "partial", maximize = TRUE,
    fun = function(prediction) if (all(prediction$value %in% c(0.3, 0.7))) NaN else 0.5
  )
  result <- tuneHyperparameters(
    data = fixture$data, labels = fixture$labels,
    param = list(risk = list(0.3, 0.1)), settings = fixture$settings,
    hyperparamSettings = createHyperparameterSettings(tuningMetric = metric)
  )

  expect_equal(result$finalParam$risk, 0.1)
  expect_equal(cvValues(result), c(0.1, 0.9, 0.1, 0.9))
})

test_that("per-fold tuning scores keep their real fold identifiers", {
  metric <- createTuningMetric(
    fun = function(prediction) unique(prediction$index) / 10,
    maximize = TRUE, name = "foldId"
  )
  scores <- function(folds) {
    fixture <- createTuningFixture(folds)
    result <- tuneHyperparameters(
      data = fixture$data, labels = fixture$labels,
      param = list(risk = list(0.1)), settings = fixture$settings,
      hyperparamSettings = createHyperparameterSettings(tuningMetric = metric)
    )
    result$paramSearch
  }

  sequential <- scores(rep(1:3, each = 2))
  summary <- sequential[[1]]$hyperSummary
  expect_equal(summary$fold, c("CV", "Fold1", "Fold2", "Fold3"))
  expect_equal(summary$value, c(0.2, 0.1, 0.2, 0.3))
  expect_equal(sequential[[1]]$cvPerformancePerFold, c(0.1, 0.2, 0.3))

  nonSequential <- scores(rep(c(3, 1, 7), each = 2))
  summary <- nonSequential[[1]]$hyperSummary
  expect_equal(summary$fold, c("CV", "Fold3", "Fold1", "Fold7"))
  expect_equal(summary$value[-1], c(0.3, 0.1, 0.7))
  expect_equal(summary$value[1], mean(c(0.3, 0.1, 0.7)))

  exported <- normalizeHyperParamSearchForDatabase(nonSequential)
  expect_equal(exported$fold, c("CV", "Fold3", "Fold1", "Fold7"))
  expect_equal(exported$value[-1], c(0.3, 0.1, 0.7))
})


test_that("fitClassifier trains on outcome presence and keeps outcome counts in predictions", {
  n <- 12
  # rowIds that are not the matrix rows, as after a split
  rowIds <- seq(5, by = 7, length.out = n)
  labels <- data.frame(rowId = rowIds, outcomeCount = rep(c(0, 1, 2), 4))
  covariates <- data.frame(rowId = rowIds, covariateId = 1001, covariateValue = 1)
  covariateData <- makeCovariateData(
    covariates,
    analysisRef = defaultAnalysisRef(defaultCovariateRef(covariates), isBinary = "Y")
  )
  withr::defer(Andromeda::close(covariateData))
  trainData <- list(
    labels = labels,
    folds = data.frame(rowId = rowIds, index = rep(1:2, each = 6)),
    covariateData = covariateData
  )
  attr(trainData, "metaData") <- list(targetId = 1, outcomeId = 2)

  # like toSparseM(): the labels are numbered by matrix row (in reverse here)
  # with the original rowId in originalRowId, and predictions map back to it
  trainedOn <- list()
  modelSettings <- list(
    param = list(risk = list(0.5)),
    settings = list(
      modelName = "toy", modelType = "binary", seed = 1,
      prepareData = function(trainData, map) {
        matrixLabels <- trainData$labels
        matrixLabels$originalRowId <- matrixLabels$rowId
        matrixLabels$rowId <- rev(seq_len(nrow(matrixLabels)))
        list(
          dataMatrix = matrix(1, nrow = nrow(matrixLabels), ncol = 1),
          labels = matrixLabels,
          covariateRef = data.frame(covariateId = 1001, columnId = 1)
        )
      },
      train = function(dataMatrix, labels, hyperParameters, settings) {
        trainedOn[[length(trainedOn) + 1]] <<- labels$outcomeCount
        list(risk = mean(labels$outcomeCount))
      },
      predict = function(plpModel, data, cohort) {
        cohort$value <- plpModel$risk
        cohort$rowId <- cohort$originalRowId
        cohort$originalRowId <- NULL
        cohort
      },
      variableImportance = function(model, covariateMap) NULL
    )
  )
  class(modelSettings) <- "modelSettings"

  model <- fitClassifier(trainData, modelSettings, analysisId = "binaryLabels")

  # every fit, in the CV folds and the final one, saw only 0 and 1
  expect_true(length(trainedOn) > 1)
  for (outcomes in trainedOn) {
    expect_true(all(outcomes %in% c(0, 1)))
  }
  expect_equal(model$model$risk, 8 / 12)
  # the predictions keep each patient's own original count for evaluation
  for (type in unique(model$prediction$evaluationType)) {
    rows <- model$prediction[model$prediction$evaluationType == type, ]
    expect_setequal(rows$rowId, labels$rowId)
    expect_equal(rows$outcomeCount, labels$outcomeCount[match(rows$rowId, labels$rowId)])
  }
  expect_true(2 %in% model$prediction$outcomeCount)

  # predictions with rowIds that are not in the labels stop
  modelSettings$settings$predict <- function(plpModel, data, cohort) {
    cohort$value <- plpModel$risk
    cohort
  }
  expect_error(
    fitClassifier(trainData, modelSettings, analysisId = "matrixRowIds"),
    "rowIds that are not in the training labels"
  )
})
