# Copyright 2020 Observational Health Data Sciences and Informatics
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
  test <- plotSparseRoc(plpResult$performanceEvaluation, typeColumn = 'evaluation')
  testthat::expect_s3_class(test, 'arrangelist')
  
  test <- plotPredictedPDF(plpResult$performanceEvaluation, typeColumn = 'evaluation')
  testthat::expect_s3_class(test, 'arrangelist')
  
  test <- plotPreferencePDF(plpResult$performanceEvaluation, typeColumn = 'evaluation')
  testthat::expect_s3_class(test, 'arrangelist')
  
  test <- plotPrecisionRecall(plpResult$performanceEvaluation, typeColumn = 'evaluation')
  testthat::expect_s3_class(test, 'arrangelist')
  
  test <- plotF1Measure(plpResult$performanceEvaluation, typeColumn = 'evaluation')
  testthat::expect_s3_class(test, 'arrangelist')
  
  if(!is.null(plpResult$performanceEvaluation$demographicSummary)){
    test <- plotDemographicSummary(plpResult$performanceEvaluation, typeColumn = 'evaluation')
    testthat::expect_s3_class(test, 'arrangelist')
  }
  
  test <- plotSparseCalibration(plpResult$performanceEvaluation, typeColumn = 'evaluation')
  testthat::expect_s3_class(test, 'arrangelist')
  
  test <- plotPredictionDistribution(plpResult$performanceEvaluation, typeColumn = 'evaluation')
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
    result = plpResultReal, 
    filename = filepath(saveLoc, 'plots'),
    typeColumn = 'evaluation')
  testthat::expect_equal(test, T)
  testthat::expect_equal(dir.exists(file.path(saveLoc,'plots')), T)
  
  # expect plots to be there
  expect_true(length(dir(file.path(saveLoc,'plots')))>0)
  
})

  
test_that("plotSmoothCalibration", {
  
  # test the plot works
  test <- plotSmoothCalibration(result = plpResultReal,
                                smooth = "loess",
                                span = 1,
                                nKnots = 5,
                                scatter = T,
                                type = "test",
                                bins = 20,
                                zoom = "none",
    saveLocation = NULL)
  testthat::expect_s3_class(test, c( "gtable", "gTree",  "grob",   "gDesc" ))
  
  pred <- plpResult$prediction
  plpResult$prediction <- NULL
  test2 <- plotSmoothCalibration(plpResult,
                        smooth = "loess",
                        span = 1,
                        nKnots = 5,
                        scatter = T,
                        type = "test",
                        bins = 20,
                        zoom = "data",
                        sample = T,
                        saveLocation = NULL) 
  testthat::expect_s3_class(test2, c( "gtable", "gTree",  "grob",   "gDesc" ))
  plpResult$prediction <- pred
  
  # this fails:
  #test2 <- plotSmoothCalibration(result = plpResultReal,
  #                              smooth = "rcs",
  #                              span = 1,
  #                              nKnots = 5,
  #                              scatter = F,
  #                              type = "test",
  #                              bins = 20,
  #                              zoom = "data",
  #                              fileName = NULL)
  #testthat::expect_s3_class(test2, 'ggplot')
  
})







