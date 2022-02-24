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
context("LearningCurves")


# learningCurve 
learningCurve <- PatientLevelPrediction::createLearningCurve(
  plpData = plpData,
  outcomeId = 2, parallel = T, cores = 3,
  modelSettings = setLassoLogisticRegression(),
  saveDirectory =  file.path(saveLoc, 'lcc'),
  splitSettings = createDefaultSplitSetting(testFraction = 0.2), 
  trainFractions = c(0.6,0.7,0.8),
  trainEvents = NULL,
  preprocessSettings = createPreprocessSettings(
    minFraction = 0.001,
    normalize = T
  )
)

test_that("learningCurve output correct", {

  
  testthat::expect_true(is.data.frame(learningCurve))
  testthat::expect_equal(sum(colnames(learningCurve)%in%c(
    "trainFraction",
    "Train_AUROC",
    "nPredictors",
    "Train_populationSize",
    "Train_outcomeCount") ),5)
  
  testthat::expect_equal(learningCurve$trainFraction,  c(0.6,0.7,0.8)*100)
  
})

test_that("plotLearningCurve", {
  
  test <- plotLearningCurve(learningCurve = learningCurve, 
    metric = 'AUROC')
  
  # test the plot works
  testthat::expect_s3_class(test, 'ggplot')
  
  test <- plotLearningCurve(learningCurve = learningCurve,
    metric = "AUPRC")
  testthat::expect_s3_class(test, 'ggplot')
  
  test <- plotLearningCurve(learningCurve = learningCurve,
    metric = "sBrier")
  testthat::expect_s3_class(test, 'ggplot')
  
})


