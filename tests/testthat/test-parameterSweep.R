# Copyright 2015 Observational Health Data Sciences and Informatics
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

# This is a broad, shallow sweep of all functionality. It checks whether the code produces an output
# (and does not throw an error) under a wide range of parameter settings
set.seed(1234)
data(plpDataSimulationProfile)
sampleSize <- 2000
plpData <- simulatePlpData(plpDataSimulationProfile, n = sampleSize)

test_that("plpData functions", {
  s <- summary(plpData)
  expect_is(s, "summary.plpData")
  expect_equal(s$subjectCount, sampleSize)
})

test_that("modelfitting and prediction functions", {
  prior <- createPrior("laplace", 0.01, exclude = 0)

  lrModel <- fitPredictiveModel(plpData, modelType = "logistic", removeDropoutsForLr = FALSE, outcomeId = 2, prior = prior)
  expect_is(lrModel, "predictiveModel")
    
  lrModel <- fitPredictiveModel(plpData, modelType = "logistic", outcomeId = 2, prior = prior)
  expect_is(lrModel, "predictiveModel")
  
  prModel <- fitPredictiveModel(plpData, modelType = "poisson", outcomeId = 2, prior = prior)
  expect_is(prModel, "predictiveModel")
  
  survModel <- fitPredictiveModel(plpData, modelType = "survival", outcomeId = 2, prior = prior)
  expect_is(survModel, "predictiveModel")
  
  expect_false(identical(lrModel$coefficients, prModel$coefficients))
  expect_false(identical(lrModel$coefficients, survModel$coefficients))
  
  lrPrediction <- predictProbabilities(lrModel, plpData)
  expect_is(lrPrediction, "data.frame")
  
  prPrediction <- predictProbabilities(prModel, plpData)
  expect_is(prPrediction, "data.frame")
  
  survPrediction <- predictProbabilities(survModel, plpData)
  expect_is(survPrediction, "data.frame")
})

test_that("model evaluation functions", {
  prior <- createPrior("laplace", 0.01, exclude = 0)
  
  splits <- splitData(plpData, c(0.5,0.5))  
  lrModel <- fitPredictiveModel(splits[[1]], modelType = "logistic", outcomeId = 2, prior = prior)
  
  lrPrediction <- predictProbabilities(lrModel, splits[[2]])  
  auc <- computeAuc(lrPrediction, splits[[2]])
  expect_equal(auc > 0.7, TRUE)
  
  plot <- plotCalibration(lrPrediction, splits[[2]])
  expect_is(plot, "ggplot")
  
  plot <- plotRoc(lrPrediction, splits[[2]])
  expect_is(plot, "ggplot")
})  

test_that("covariate means functions", {
  means <- computeCovariateMeans(plpData, outcomeId = 2)
  expect_is(means, "data.frame")

  plot <- plotCovariateDifferenceOfTopVariables(means)
  expect_is(plot, "ggplot")
})

