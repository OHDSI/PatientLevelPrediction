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

if (isTRUE(eunomiaAvailable)) {
# learningCurve
learningCurve <- PatientLevelPrediction::createLearningCurve(
  plpData = plpData,
  outcomeId = outcomeId, parallel = FALSE, cores = -1,
  modelSettings = setLassoLogisticRegression(),
  saveDirectory = file.path(saveLoc, "lcc"),
  splitSettings = createDefaultSplitSetting(testFraction = 0.2, nfold = 2),
  trainFractions = c(0.6, 0.7),
  trainEvents = NULL,
  preprocessSettings = createPreprocessSettings(
    minFraction = 0.001,
    normalize = TRUE
  )
)
}

test_that("learningCurve output correct", {
  skip_if_no_eunomia()  
  expect_true(is.data.frame(learningCurve))
  expect_equal(sum(colnames(learningCurve) %in% c(
    "trainFraction",
    "Train_AUROC",
    "nPredictors",
    "Train_populationSize",
    "Train_outcomeCount"
  )), 5)

  expect_equal(learningCurve$trainFraction, c(0.6, 0.7) * 100)
})

test_that("plotLearningCurve", {
  skip_if_not_installed("ggplot2")
  skip_on_cran()
  skip_if_no_eunomia()
  test <- plotLearningCurve(
    learningCurve = learningCurve,
    metric = "AUROC"
  )

  # test the plot works
  expect_s3_class(test, "ggplot")

  test <- plotLearningCurve(
    learningCurve = learningCurve,
    metric = "AUPRC"
  )
  expect_s3_class(test, "ggplot")

  test <- plotLearningCurve(
    learningCurve = learningCurve,
    metric = "sBrier"
  )
  expect_s3_class(test, "ggplot")
})

# 1000 patients with 100 outcomes, so 10 training events take 10% of them
learningCurvePopulation <- function() {
  data.frame(rowId = 1:1000, outcomeCount = rep(c(1, 0), c(100, 900)))
}

test_that("getTrainFractions turns training events into fractions that fit the training set", {
  local_mocked_bindings(createStudyPopulation = function(...) learningCurvePopulation())
  fractions <- function(trainEvents) {
    getTrainFractions(
      trainEvents,
      plpData = NULL, outcomeId = 3,
      populationSettings = createStudyPopulationSettings(),
      splitSettings = createDefaultSplitSetting(testFraction = 0.25)
    )
  }
  # 90 events need 90% of the patients, more than the 75% training set
  expect_equal(fractions(c(10, 50, 90)), c(0.1, 0.5))
  # when no number of events fits, the default fractions are used
  expect_equal(fractions(c(80, 90)), c(0.25, 0.5, 0.75))
})

test_that("createLearningCurve with trainEvents runs one model per derived fraction", {
  requested <- numeric(0)
  local_mocked_bindings(
    createStudyPopulation = function(...) learningCurvePopulation(),
    runPlp = function(...) {
      requested <<- c(requested, list(...)$splitSettings$train)
      list(
        executionSummary = list(TotalExecutionElapsedTime = 1),
        model = list(covariateImportance = data.frame(covariateValue = c(0, 0.5, -1))),
        performanceEvaluation = list(evaluationStatistics = data.frame(
          evaluation = "Train",
          metric = c("AUROC", "populationSize", "outcomeCount"),
          value = c(0.7, 750, 75)
        ))
      )
    }
  )
  learningCurve <- createLearningCurve(
    plpData = NULL, outcomeId = 3, parallel = FALSE,
    modelSettings = setLassoLogisticRegression(),
    saveDirectory = tempfile("learningCurve"),
    splitSettings = createDefaultSplitSetting(testFraction = 0.25),
    trainEvents = c(10, 50, 90)
  )

  expect_equal(requested, c(0.1, 0.5))
  expect_equal(learningCurve$trainFraction, c(10, 50))
  expect_true(all(c(
    "trainFraction", "Train_AUROC", "nPredictors", "Train_populationSize",
    "Train_outcomeCount", "executionTime"
  ) %in% colnames(learningCurve)))
  # covariates with a non-zero coefficient
  expect_equal(as.numeric(learningCurve$nPredictors), c(2, 2))
})
