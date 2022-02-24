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

library("testthat")

context("Plotting")
#TODO: add input checks and test these...
#options(fftempdir = getwd())

test_that("plots", {

  # test all the outputs are ggplots
  test <- plotSparseRoc(plpResult, typeColumn = 'evaluation')
  testthat::expect_s3_class(test, 'arrangelist')
  
  test <- plotPredictedPDF(plpResult, typeColumn = 'evaluation')
  testthat::expect_s3_class(test, 'arrangelist')
  
  test <- plotPreferencePDF(plpResult, typeColumn = 'evaluation')
  testthat::expect_s3_class(test, 'arrangelist')
  
  test <- plotPrecisionRecall(plpResult, typeColumn = 'evaluation')
  testthat::expect_s3_class(test, 'arrangelist')
  
  test <- plotF1Measure(plpResult, typeColumn = 'evaluation')
  testthat::expect_s3_class(test, 'arrangelist')
  
  if(!is.null(plpResult$performanceEvaluation$demographicSummary)){
    test <- plotDemographicSummary(plpResult, typeColumn = 'evaluation')
    testthat::expect_s3_class(test, 'arrangelist')
  }
  
  test <- plotSparseCalibration(plpResult, typeColumn = 'evaluation')
  testthat::expect_s3_class(test, 'arrangelist')
  
  test <- plotPredictionDistribution(plpResult, typeColumn = 'evaluation')
  testthat::expect_s3_class(test, 'arrangelist')
  
  test <- plotVariableScatterplot(plpResult$covariateSummary)
  testthat::expect_s3_class(test, 'ggplot')
  
  test <- plotGeneralizability(plpResult$covariateSummary, fileName=NULL)
  testthat::expect_s3_class(test, 'grob')

})


test_that("outcomeSurvivalPlot", {
  
  # test the plot works
  test <- outcomeSurvivalPlot(plpData = plpData, outcomeId = 2)
  testthat::expect_s3_class(test, 'ggsurvplot')
  
  testthat::expect_error(outcomeSurvivalPlot())
  testthat::expect_error(outcomeSurvivalPlot(plpData = NULL))
  testthat::expect_error(outcomeSurvivalPlot(outcomeId = 094954))
})


test_that("plotPlp", {
  
  # test the plot works
  test <- plotPlp(
    plpResult  = plpResult, 
    saveLocation = file.path(saveLoc, 'plots'),
    typeColumn = 'evaluation'
  )
  testthat::expect_equal(test, T)
  testthat::expect_equal(dir.exists(file.path(saveLoc,'plots')), T)
  
  # expect plots to be there
  expect_true(length(dir(file.path(saveLoc,'plots')))>0)
  
})

test_that("plotSmoothCalibration", {
  
  # test the plot works
  test <- plotSmoothCalibration(
    plpResult = plpResult, 
    smooth = "loess",
    span = 1,
    nKnots = 5,
    scatter = T,
    bins = 20,
    saveLocation = file.path(saveLoc, "plots")
  )
  testthat::expect_s3_class(test$test$smoothPlot, c("gg", "ggplot"))
  testthat::expect_s3_class(test$test$histPlot, c("gg", "ggplot"))
  testthat::expect_true(
    file.exists(
      file.path(saveLoc, "plots", "smoothCalibrationTest.pdf")
    )
  )
  
  pred <- plpResult$prediction
  plpResult$prediction <- NULL
  test2 <- plotSmoothCalibration(plpResult,
    smooth = "loess",
    span = 1,
    nKnots = 5,
    scatter = T,
    bins = 20,
    sample = T,
    saveLocation = NULL) 
  testthat::expect_s3_class(test2$test$smoothPlot, c("gg", "ggplot"))
  plpResult$prediction <- pred
  
  test3 <- plotSmoothCalibration(plpResult,
    smooth = "rcs",
    span = 1,
    nKnots = 5,
    scatter = F,
    bins = 20,
    fileName = NULL)
  testthat::expect_s3_class(test3$test$smoothPlot, c("gg", "ggplot"))
  testthat::expect_s3_class(test3$test$histPlot, c("gg", "ggplot"))
  testthat::expect_true( # is this tested needed again?
    file.exists(
      file.path(saveLoc, "plots", "smoothCalibrationTest.pdf")
    )
  )
  
})






