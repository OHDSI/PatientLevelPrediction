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
# TODO: add input checks and test these...

test_that("plots", {
  # test all the outputs are ggplots
  test <- plotSparseRoc(plpResult, typeColumn = "evaluation")
  testthat::expect_s3_class(test, "arrangelist")

  test <- plotPredictedPDF(plpResult, typeColumn = "evaluation")
  testthat::expect_s3_class(test, "arrangelist")

  test <- plotPreferencePDF(plpResult, typeColumn = "evaluation")
  testthat::expect_s3_class(test, "arrangelist")

  test <- plotPrecisionRecall(plpResult, typeColumn = "evaluation")
  testthat::expect_s3_class(test, "arrangelist")

  test <- plotF1Measure(plpResult, typeColumn = "evaluation")
  testthat::expect_s3_class(test, "arrangelist")

  if (!is.null(plpResult$performanceEvaluation$demographicSummary)) {
    test <- plotDemographicSummary(plpResult, typeColumn = "evaluation")
    testthat::expect_s3_class(test, "arrangelist")
  }

  test <- plotSparseCalibration(plpResult, typeColumn = "evaluation")
  testthat::expect_s3_class(test, "arrangelist")

  test <- plotPredictionDistribution(plpResult, typeColumn = "evaluation")
  testthat::expect_s3_class(test, "arrangelist")

  test <- plotVariableScatterplot(plpResult$covariateSummary)
  testthat::expect_s3_class(test, "ggplot")

  test <- plotGeneralizability(plpResult$covariateSummary, fileName = NULL)
  testthat::expect_s3_class(test, "grob")
})


test_that("outcomeSurvivalPlot", {
  # test the plot works
  test <- outcomeSurvivalPlot(plpData = plpData, outcomeId = outcomeId)
  testthat::expect_s3_class(test, "ggsurvplot")

  testthat::expect_error(outcomeSurvivalPlot())
  testthat::expect_error(outcomeSurvivalPlot(plpData = NULL))
  testthat::expect_error(outcomeSurvivalPlot(outcomeId = 094954))
})


test_that("plotPlp", {
  # test the plot works
  test <- plotPlp(
    plpResult = plpResult,
    saveLocation = file.path(saveLoc, "plots"),
    typeColumn = "evaluation"
  )
  testthat::expect_equal(test, TRUE)
  testthat::expect_equal(dir.exists(file.path(saveLoc, "plots")), TRUE)

  # expect plots to be there
  expect_true(length(dir(file.path(saveLoc, "plots"))) > 0)
})

test_that("plotSmoothCalibration", {
  # test the plot works
  test <- plotSmoothCalibration(
    plpResult = plpResult,
    smooth = "loess",
    scatter = TRUE,
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
    scatter = TRUE,
    bins = 20,
    sample = TRUE,
    saveLocation = NULL
  )
  testthat::expect_s3_class(test2$test$smoothPlot, c("gg", "ggplot"))
  plpResult$prediction <- pred

  test3 <- plotSmoothCalibration(plpResult,
    smooth = "rcs",
    span = 1,
    nKnots = 5,
    scatter = FALSE,
    bins = 20,
    fileName = NULL
  )
  testthat::expect_s3_class(test3$test$smoothPlot, c("gg", "ggplot"))
  testthat::expect_s3_class(test3$test$histPlot, c("gg", "ggplot"))
  testthat::expect_true( # is this tested needed again?
    file.exists(
      file.path(saveLoc, "plots", "smoothCalibrationTest.pdf")
    )
  )
})

nbData <- getNetBenefit(plpResult, evalType = "Test")
test_that("getNetBenefit returns the correct dataframe", {

  expect_s3_class(nbData, "data.frame")
  expectedColumns <- c("threshold", "TP", "FP", "netBenefit", "treatAll", "treatNone")
  expect_true(all(expectedColumns %in% colnames(nbData)))
})

test_that("getNetBenefit computes the net benefit correctly", {
  threshold <- nbData$threshold[[1]]
  truePositives <- nbData$TP[[1]]
  falsePositives <- nbData$FP[[1]]
  n <- nrow(plpResult$prediction %>% dplyr::filter(.data$evaluationType == "Test"))
  netBenefitCalculated <- (truePositives / n) - (falsePositives / n) * (threshold / (1 - threshold))
  expect_equal(nbData$netBenefit[[1]], netBenefitCalculated)
})

test_that("getNetBenefit handles invalid evalType gracefully", {
  expect_error(getNetBenefit(plpResult, evalType = "InvalidType"),
               "No prediction data found for evaluation type InvalidType")
})

test_that("plotNetBenefit returns a grob object", {
  plot <- plotNetBenefit(plpResult, evalType = "Test")
  expect_true(inherits(plot, "arrangelist"))
})

test_that("plotNetBenefit saves plot when saveLocation is specified", {
  tempDir <- tempdir()
  plotNetBenefit(plpResult, saveLocation = tempDir, fileName = "netBenefit.png", evalType = "Test")
  expect_true(file.exists(file.path(tempDir, "netBenefit.png")))
  #Clean up
  file.remove(file.path(tempDir, "netBenefit.png"))
})

test_that("plotNetBenefit handles NULL evalType", {
  plot <- plotNetBenefit(plpResult, evalType = NULL)
  expect_true(inherits(plot, "arrangelist"))
})


test_that("plotNetBenefit creates correct number of plots when evalType is NULL", {
  plot <- plotNetBenefit(plpResult, evalType = NULL)
  # Since evalType is NULL, it should plot for all unique evaluation types
  evalTypes <- unique(plpResult$performanceEvaluation$thresholdSummary$evaluation)
  expect_equal(length(plot[[1]]$grobs) - 1 , length(evalTypes)) # -1 for text grob
})
