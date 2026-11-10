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


test_that("calibration plots tolerate groups that are not estimable at the horizon", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gridExtra")
  summary <- data.frame(
    evaluation = "Test",
    averagePredictedProbability = c(0.1, 0.2, 0.3, 0.4, 0.5),
    observedIncidence = c(0.1, NA, 0.3, 0.35, 0.5),
    PersonCountAtRisk = c(50, 50, 50, 50, 50)
  )
  result <- list(performanceEvaluation = list(calibrationSummary = summary))
  expect_s3_class(plotSparseCalibration(result), "arrangelist")
  expect_s3_class(plotSparseCalibration2(result), "arrangelist")
})

test_that("demographic plot keeps predicted risk for groups without an estimable outcome", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gridExtra")
  summary <- data.frame(
    evaluation = "Test",
    ageGroup = c("Age group: 50-54", "Age group: 55-59"),
    genGroup = "Male",
    averagePredictedProbability = c(0.1, 0.2),
    StDevPredictedProbability = c(0.02, 0.03),
    PersonCountAtRisk = c(10, 10),
    PersonCountWithOutcome = c(2, NA)
  )
  result <- list(performanceEvaluation = list(demographicSummary = summary))
  expect_s3_class(plotDemographicSummary(result), "arrangelist")

  local_mocked_bindings(
    marrangeGrob = function(grobs, ...) grobs,
    .package = "gridExtra"
  )
  plotted <- plotDemographicSummary(result)[[1]]$data
  predicted <- plotted[plotted$variable == "averagePredictedProbability", ]
  expect_setequal(trimws(as.character(predicted$age)), c("50-54", "55-59"))
  expect_equal(nrow(plotted[plotted$variable == "observed", ]), 1)
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

test_that("Plot net benefit errors", {
  skip_if_not_installed("ggplot2")
  skip_on_cran()
  skip_if_offline()
  expect_error(plotNetBenefit(plpResults, modelNames = c("Model1", "Model2")))
})

test_that("getNetBenefit works with thresholdSummary", { 
  skip_if_not_installed("ggplot2")
  skip_on_cran()
  skip_if_offline()

  result <- plpResult
  result$prediction <- NULL
  netBenefit <- getNetBenefit(result, evalType = "Test")

  expect_true(!is.null(netBenefit$netBenefit))
  expect_true(is.numeric(netBenefit$netBenefit))
})
dev.off()

# Keeps the ggplot objects that plot functions would hand to marrangeGrob.
capturePlots <- function(expr, env = parent.frame()) {
  testthat::local_mocked_bindings(marrangeGrob = function(grobs, ...) grobs, .package = "gridExtra", .env = env)
  expr
}

makeSurvivalPlotData <- function() {
  cohorts <- data.frame(
    rowId = 1:2, subjectId = 1:2, targetId = 1, cohortStartDate = as.Date("2020-01-01"),
    daysFromObsStart = 400, daysToCohortEnd = 0, daysToObsEnd = c(400, 100),
    ageYear = 60, gender = 8507
  )
  outcomes <- data.frame(rowId = 1, outcomeId = 1, outcomeCount = 1, daysToEvent = 100)
  plp <- list(cohorts = cohorts, outcomes = outcomes, metaData = list())
  class(plp) <- "plpData"
  plp
}

survivalPlotSettings <- function(start, end = 3650, startAnchor = "cohort start", endAnchor = "cohort start") {
  createStudyPopulationSettings(
    requireTimeAtRisk = FALSE, removeSubjectsWithPriorOutcome = FALSE,
    riskWindowStart = start, startAnchor = startAnchor,
    riskWindowEnd = end, endAnchor = endAnchor
  )
}

test_that("outcomeSurvivalPlot uses one time origin with a delayed risk window", {
  skip_if_not_installed("survminer")
  skip_if_not_installed("ggplot2")
  result <- suppressWarnings(outcomeSurvivalPlot(
    makeSurvivalPlotData(), 1,
    populationSettings = survivalPlotSettings(30),
    riskTable = FALSE, confInt = FALSE
  ))
  survival <- result$data.survplot
  expect_equal(min(survival$surv[survival$n.event > 0]), 0.5)
  expect_equal(survival$time[survival$n.event > 0], 71)
  # the x axis follows the risk-window origin: days 30..3650 become 1..3621
  expect_equal(result$plot$coordinates$limits$x, c(0, 3621))
  expect_equal(result$plot$labels$x, "Days since start of the time-at-risk window")
})

test_that("outcomeSurvivalPlot is unchanged for the default risk window", {
  skip_if_not_installed("survminer")
  skip_if_not_installed("ggplot2")
  result <- suppressWarnings(outcomeSurvivalPlot(
    makeSurvivalPlotData(), 1,
    populationSettings = survivalPlotSettings(1),
    riskTable = FALSE, confInt = FALSE
  ))
  survival <- result$data.survplot
  # with the default window daysToEvent equals survivalTime, and the tie at day 100 keeps survival 0.5
  expect_equal(min(survival$surv[survival$n.event > 0]), 0.5)
  expect_equal(survival$time[survival$n.event > 0], 100)
  expect_equal(result$plot$coordinates$limits$x, c(0, 3650))
})

test_that("outcomeSurvivalPlot x axis is the window length unless the window ends have different anchors", {
  skip_if_not_installed("survminer")
  skip_if_not_installed("ggplot2")
  plp <- makeSurvivalPlotData()
  plp$cohorts$daysToCohortEnd <- 5
  bothCohortEnd <- survivalPlotSettings(1, end = 365, startAnchor = "cohort end", endAnchor = "cohort end")
  result <- suppressWarnings(outcomeSurvivalPlot(
    plp, 1,
    populationSettings = bothCohortEnd, riskTable = FALSE, confInt = FALSE
  ))
  expect_equal(result$plot$coordinates$limits$x, c(0, 365))

  mixed <- survivalPlotSettings(1, end = 365, endAnchor = "cohort end")
  result <- suppressWarnings(outcomeSurvivalPlot(
    plp, 1,
    populationSettings = mixed, riskTable = FALSE, confInt = FALSE
  ))
  population <- suppressWarnings(createStudyPopulation(plp, 1, populationSettings = mixed))
  expect_equal(result$plot$coordinates$limits$x, c(0, max(population$survivalTime)))
  expect_false(max(population$survivalTime) == 365)
})

test_that("plotSparseCalibration ribbon is the pointwise CI of the fitted line", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gridExtra")
  summary <- data.frame(
    evaluation = "Test",
    averagePredictedProbability = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5),
    observedIncidence = c(0.06, 0.08, 0.17, 0.18, 0.27, 0.28, 0.37, 0.38, 0.47, 0.48)
  )
  plots <- capturePlots(plotSparseCalibration(list(performanceEvaluation = list(calibrationSummary = summary))))
  data <- plots[[1]]$data
  model <- stats::lm(observedIncidence ~ averagePredictedProbability, data = summary)
  expected <- stats::predict(
    model,
    newdata = data.frame(averagePredictedProbability = data$x),
    interval = "confidence"
  )
  expect_equal(data$lci, unname(expected[, "lwr"]))
  expect_equal(data$uci, unname(expected[, "upr"]))
  # the band is narrowest around the mean of the points, unlike joined marginal limits
  width <- data$uci - data$lci
  expect_equal(which.min(width), which.min(abs(data$x - mean(summary$averagePredictedProbability))))
})

test_that("plotSparseCalibration2 plots stored Kaplan-Meier bounds for survival summaries", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gridExtra")
  skip_if_not_installed("survival")
  prediction <- data.frame(
    rowId = 1:100, value = 0.1,
    outcomeCount = c(rep(0, 90), 1, rep(0, 9)),
    survivalTime = c(rep(10, 90), 20, rep(30, 9)),
    evaluation = "Test"
  )
  summary <- suppressWarnings(getCalibrationSummary(
    prediction, "survival",
    typeColumn = "evaluation", numberOfStrata = 10, truncateFraction = 0.01
  ))
  plots <- capturePlots(plotSparseCalibration2(list(performanceEvaluation = list(calibrationSummary = summary))))
  data <- plots[[1]]$data
  expect_equal(data$lci, summary$observedIncidenceLB)
  expect_equal(data$uci, summary$observedIncidenceUB)
  # not the narrower binomial interval from the fabricated event count
  binom <- stats::binom.test(round(summary$observedIncidence * summary$PersonCountAtRisk), summary$PersonCountAtRisk)$conf.int
  expect_gt(data$uci - data$lci, binom[2] - binom[1])
})

test_that("plotSparseCalibration2 keeps binomial intervals for binary summaries", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gridExtra")
  summary <- data.frame(
    evaluation = "Test",
    averagePredictedProbability = c(0.1, 0.2),
    observedIncidence = c(0.1, 0.25),
    PersonCountAtRisk = c(100, 40)
  )
  plots <- capturePlots(plotSparseCalibration2(list(performanceEvaluation = list(calibrationSummary = summary))))
  data <- plots[[1]]$data
  expected <- stats::binom.test(10, 100)$conf.int
  expect_equal(data$lci[1], expected[1])
  expect_equal(data$uci[1], expected[2])
})

makeSurvivalNetBenefitResult <- function(censorTime, timepoint = 365, addModelType = TRUE) {
  prediction <- data.frame(
    rowId = 1:4,
    value = c(0.1, 0.2, 0.3, 0.4),
    outcomeCount = c(1, 0, 0, 0),
    survivalTime = c(100, censorTime, 365, 365),
    evaluationType = "Test",
    evaluation = "Test"
  )
  attr(prediction, "metaData") <- list(modelType = if (addModelType) "survival" else NULL, timepoint = timepoint)
  list(
    prediction = prediction,
    performanceEvaluation = list(
      calibrationSummary = data.frame(evaluation = "Test", averagePredictedProbability = 0.25, observedIncidence = 0.33)
    )
  )
}
