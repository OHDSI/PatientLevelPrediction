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

if (internet) {
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
  skip_if_offline()  
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
  skip_if_offline()
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

test_that("getTrainFractions works", {
  skip_if_offline()
  learningCurve <- PatientLevelPrediction::createLearningCurve(
    plpData = tinyPlpData,
    outcomeId = outcomeId, parallel = FALSE, cores = -1,
    modelSettings = setLassoLogisticRegression(seed = 42),
    saveDirectory = file.path(saveLoc, "lcc"),
    splitSettings = createDefaultSplitSetting(
      testFraction = 0.33, nfold = 2,
      splitSeed = 42
    ),
    trainEvents = c(150, 200),
    preprocessSettings = createPreprocessSettings(
      minFraction = 0.001,
      normalize = TRUE
    )
  )
  expect_true(is.data.frame(learningCurve))
  expect_equal(sum(colnames(learningCurve) %in% c(
    "trainFraction",
    "Train_AUROC",
    "nPredictors",
    "Train_populationSize",
    "Train_outcomeCount"
  )), 5)
})
