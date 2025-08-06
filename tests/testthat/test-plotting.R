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

# TODO: add input checks and test these...
pdf(file = NULL)
test_that("plots", {
  skip_if_not_installed("ggplot2")
  skip_on_cran()
  skip_if_offline()
  # test all the outputs are ggplots
  test <- plotSparseRoc(plpResult, typeColumn = "evaluation")
  expect_s3_class(test, "arrangelist")

  test <- plotPredictedPDF(plpResult, typeColumn = "evaluation")
  expect_s3_class(test, "arrangelist")

  test <- plotPreferencePDF(plpResult, typeColumn = "evaluation")
  expect_s3_class(test, "arrangelist")

  test <- plotPrecisionRecall(plpResult, typeColumn = "evaluation")
  expect_s3_class(test, "arrangelist")

  test <- plotF1Measure(plpResult, typeColumn = "evaluation")
  expect_s3_class(test, "arrangelist")

  if (!is.null(plpResult$performanceEvaluation$demographicSummary)) {
    test <- plotDemographicSummary(plpResult, typeColumn = "evaluation")
    expect_s3_class(test, "arrangelist")
  }

  test <- plotSparseCalibration(plpResult, typeColumn = "evaluation")
  expect_s3_class(test, "arrangelist")

  test <- plotPredictionDistribution(plpResult, typeColumn = "evaluation")
  expect_s3_class(test, "arrangelist")

  test <- plotVariableScatterplot(plpResult$covariateSummary)
  expect_s3_class(test, "ggplot")

  test <- plotGeneralizability(plpResult$covariateSummary, fileName = NULL)
  expect_s3_class(test, "grob")
})


test_that("outcomeSurvivalPlot", {
  skip_if_not_installed("survminer")
  skip_on_cran()
  skip_if_offline()
  # test the plot works
  test <- outcomeSurvivalPlot(plpData = plpData, outcomeId = outcomeId)
  expect_s3_class(test, "ggsurvplot")

  expect_error(outcomeSurvivalPlot())
  expect_error(outcomeSurvivalPlot(plpData = NULL))
  expect_error(outcomeSurvivalPlot(outcomeId = 094954))
})


test_that("plotPlp", {
  skip_if_not_installed(c("ggplot2", "gridExtra"))
  skip_on_cran()
  skip_if_offline()
  # test the plot works
  test <- plotPlp(
    plpResult = plpResult,
    saveLocation = file.path(saveLoc, "plots"),
    typeColumn = "evaluation"
  )
  expect_equal(test, TRUE)
  expect_equal(dir.exists(file.path(saveLoc, "plots")), TRUE)

  # expect plots to be there
  expect_true(length(dir(file.path(saveLoc, "plots"))) > 0)
})

test_that("plotSmoothCalibration", {
  skip_if_not_installed(c("ggplot2", "mgcv"))
  skip_on_cran()
  skip_if_offline()
  # test the plot works
  test <- plotSmoothCalibration(
    plpResult = plpResult,
    smooth = "loess",
    scatter = TRUE,
    bins = 20,
    saveLocation = file.path(saveLoc, "plots")
  )
  expect_s3_class(test$test$smoothPlot, c("gg", "ggplot"))
  expect_s3_class(test$test$histPlot, c("gg", "ggplot"))
  expect_true(
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
  expect_s3_class(test2$test$smoothPlot, c("gg", "ggplot"))
})

test_that("Smooth calibration plot works with rcs", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("mgcv")
  skip_on_cran()
  test3 <- plotSmoothCalibration(plpResult,
    smooth = "rcs",
    span = 1,
    nKnots = 5,
    scatter = FALSE,
    bins = 20,
    fileName = NULL
  )
  expect_s3_class(test3$test$smoothPlot, c("gg", "ggplot"))
  expect_s3_class(test3$test$histPlot, c("gg", "ggplot"))
  expect_true( # is this tested needed again?
    file.exists(
      file.path(saveLoc, "plots", "smoothCalibrationTest.pdf")
    )
  )
})

if (internet && rlang::is_installed("Eunomia")) {
  nbData <- getNetBenefit(plpResult, evalType = "Test")
}
test_that("getNetBenefit returns the correct dataframe", {
  skip_if_offline()
  expect_s3_class(nbData, "data.frame")
  expectedColumns <- c("threshold", "TP", "FP", "netBenefit", "treatAll", "treatNone")
  expect_true(all(expectedColumns %in% colnames(nbData)))
})

test_that("getNetBenefit computes the net benefit correctly", {
  skip_if_offline()
  threshold <- nbData$threshold[[1]]
  truePositives <- nbData$TP[[1]]
  falsePositives <- nbData$FP[[1]]
  n <- nrow(plpResult$prediction %>% dplyr::filter(.data$evaluationType == "Test"))
  netBenefitCalculated <- (truePositives / n) - (falsePositives / n) * (threshold / (1 - threshold))
  expect_equal(nbData$netBenefit[[1]], netBenefitCalculated)
})

test_that("getNetBenefit handles invalid evalType gracefully", {
  skip_if_offline()
  expect_error(
    getNetBenefit(plpResult, evalType = "InvalidType"),
    "No prediction data found for evaluation type InvalidType"
  )
})

test_that("plotNetBenefit returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  skip_on_cran()
  skip_if_offline()
  plot <- plotNetBenefit(plpResult, evalType = "Test", showPlot = FALSE)
  expect_true(inherits(plot, "ggplot"))
})

test_that("plotNetBenefit saves plot when saveLocation is specified", {
  skip_if_not_installed("ggplot2")
  skip_on_cran()
  skip_if_offline()
  tempDir <- tempfile()
  plotNetBenefit(
    plpResult,
    saveLocation = tempDir, 
    fileName = "netBenefit.png", 
    evalType = "Test",
    showPlot = FALSE
  )
  expect_true(file.exists(file.path(tempDir, "netBenefit.png")))
  # Clean up
  file.remove(file.path(tempDir, "netBenefit.png"))
})

test_that("plotNetBenefit handles NULL evalType", {
  skip_if_not_installed("ggplot2")
  skip_on_cran()
  skip_if_offline()
  plot <- plotNetBenefit(plpResult, evalType = NULL)
  expect_true(inherits(plot, "ggplot"))
})


test_that("plotNetBenefit creates correct number of plots when evalType is NULL", {
  skip_if_not_installed("ggplot2")
  skip_on_cran()
  skip_if_offline()
  plot <- plotNetBenefit(plpResult, evalType = NULL)
  # Since evalType is NULL, it should plot for all unique evaluation types
  evalTypes <- unique(plpResult$performanceEvaluation$thresholdSummary$evaluation)
  expect_equal(length(plot$facet), length(evalTypes)) # -1 for text grob
})
dev.off()
