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
test_that("evaluatePlp", {
  skip_if_offline()
  eval <- evaluatePlp(
    prediction = plpResult$prediction,
    typeColumn = "evaluationType"
  )
  expect_equal(class(eval), "plpEvaluation")
  expect_equal(names(eval), c("evaluationStatistics", "thresholdSummary", "demographicSummary", "calibrationSummary", "predictionDistribution"))
})

test_that("evaluatePlp uses All when type column is absent", {
  prediction <- data.frame(
    rowId = 1:100,
    ageYear = sample(18:90, 100, replace = TRUE),
    gender = sample(c(8507, 8532), 100, replace = TRUE),
    outcomeCount = rep(c(0, 1), 50),
    value = seq(0.01, 0.99, length.out = 100)
  )
  attr(prediction, "metaData") <- list(modelType = "binary")

  eval <- evaluatePlp(prediction)

  expect_equal(unique(as.character(eval$evaluationStatistics$evaluation)), "All")
  expect_equal(unique(eval$thresholdSummary$evaluation), "All")
  expect_equal(unique(eval$demographicSummary$evaluation), "All")
  expect_equal(unique(eval$calibrationSummary$evaluation), "All")
  expect_equal(unique(eval$predictionDistribution$evaluation), "All")
})

test_that("modelBasedConcordance", {
  skip_if_offline()
  concordance <- PatientLevelPrediction::modelBasedConcordance(prediction = plpResult$prediction)
  expect_type(concordance, "double")
})

test_that("evaluatePlp_survival", {
  skip_if_not_installed("survival")
  skip_if_not_installed("polspline")
  n <- 100
  plpResultSurvivalPred <- data.frame(
    rowId = 1:n,
    ageYear = sample(100, n, replace = TRUE),
    gender = sample(c("8507", "8532"), n, replace = TRUE),
    outcomeCount = c(rep(1, n * 0.1), rep(0, n * 0.9)),
    value = runif(n, max = 0.1),
    evaluationType = rep("Train", n),
    survivalTime = sample(2000, n, replace = TRUE)
  )
  attr(plpResultSurvivalPred, "metaData")$modelType <- "survival"
  attr(plpResultSurvivalPred, "metaData")$timepoint <- 365

  eval <- evaluatePlp(
    prediction = plpResultSurvivalPred,
    typeColumn = "evaluationType"
  )
  expect_equal(class(eval), "plpEvaluation")
  expect_true(5 == sum(names(eval) %in% c("evaluationStatistics", "demographicSummary", "calibrationSummary", "thresholdSummary", "predictionDistribution")))
})

test_that("AUROC", {
  ePrediction <- data.frame(value = runif(100), outcomeCount = round(runif(100)))
  attr(ePrediction, "metaData") <- list(modelType = "binary")
  procAuc <- pROC::roc(ePrediction$outcomeCount, ePrediction$value,
    direction = "<"
  )
  tolerance <- 0.001

  plpAUC <- computeAuc(ePrediction, confidenceInterval = FALSE)
  expect_equal(as.numeric(procAuc$auc), plpAUC, tolerance = tolerance)
})

test_that("computeAuprc without rawValue matches PRROC", {
  ePrediction <- data.frame(value = runif(100, min = 0.0001, max = 0.9999), outcomeCount = round(runif(100)))

  positive <- ePrediction$value[ePrediction$outcomeCount == 1]
  negative <- ePrediction$value[ePrediction$outcomeCount == 0]
  pr <- PRROC::pr.curve(scores.class0 = positive, scores.class1 = negative)

  expect_equal(computeAuprc(ePrediction), as.double(pr$auc.integral))
})

test_that("computeAuprc uses rawValue when available", {
  rawScores <- stats::rnorm(100)
  ePrediction <- data.frame(
    value = rep(0.5, 100),
    rawValue = rawScores,
    outcomeCount = c(rep(1, 50), rep(0, 50))
  )
  pr <- PRROC::pr.curve(
    scores.class0 = rawScores[ePrediction$outcomeCount == 1],
    scores.class1 = rawScores[ePrediction$outcomeCount == 0]
  )

  expect_equal(computeAuprc(ePrediction), as.double(pr$auc.integral))
})

test_that("Brierscore", {
  skip_if_not_installed("scoring")
  ePrediction <- data.frame(value = runif(100), outcomeCount = round(runif(100)))

  ePrediction$dummy <- 1
  brierScoring <- scoring::brierscore(outcomeCount ~ value, data = ePrediction, group = "dummy")$brieravg
  brierPlp <- brierScore(ePrediction)$brier
  expect_equal(as.double(brierScoring), (brierPlp))
})

test_that("scaled Brier score uses observed prevalence", {
  prediction <- data.frame(
    value = rep(c(0.19, 0.21), each = 10),
    outcomeCount = rep(c(1, rep(0, 9)), 2)
  )
  # by hand: reference loss 0.1*0.9 = 0.09, model loss
  # ((1-.19)^2 + 9*.19^2 + (1-.21)^2 + 9*.21^2)/20 = 0.1001
  result <- brierScore(prediction)
  expect_equal(result$brier, 0.1001)
  expect_equal(result$brierScaled, 1 - 0.1001 / 0.09)
  expect_lt(result$brierScaled, 0)

  # predicting prevalence gives zero skill
  calibrated <- data.frame(value = rep(0.1, 20), outcomeCount = prediction$outcomeCount)
  expect_equal(brierScore(calibrated)$brierScaled, 0)
  # perfect separation gives skill of one
  perfect <- data.frame(value = c(0, 0, 1, 1), outcomeCount = c(0, 0, 1, 1))
  expect_equal(brierScore(perfect)$brierScaled, 1)
  # counts above one are still one outcome per person in the reference
  counted <- data.frame(value = c(0.5, 0.5), outcomeCount = c(0, 2))
  expect_equal(brierScore(counted)$brierScaled, 1 - brierScore(counted)$brier / 0.25)

  # one class only: there is no reference loss to scale by
  noEvents <- data.frame(value = c(0.1, 0.2), outcomeCount = c(0, 0))
  allEvents <- data.frame(value = c(0.1, 0.2), outcomeCount = c(1, 1))
  expect_true(is.na(brierScore(noEvents)$brierScaled))
  expect_true(is.na(brierScore(allEvents)$brierScaled))
})

test_that("evaluation statistics report the observed-prevalence scaled Brier score", {
  prediction <- data.frame(
    rowId = 1:20,
    value = rep(c(0.19, 0.21), each = 10),
    outcomeCount = rep(c(1, rep(0, 9)), 2),
    evaluation = "Test"
  )
  attr(prediction, "metaData") <- list(modelType = "binary")
  stats <- getEvaluationStatistics(prediction, "binary", typeColumn = "evaluation")
  scaled <- as.numeric(stats$value[stats$metric == "brier score scaled"])
  expect_equal(scaled, 1 - 0.1001 / 0.09)
})
test_that("Average precision", {
  skip_if_not_installed("Metrics")
  ePrediction <- data.frame(value = runif(100), outcomeCount = round(runif(100)))

  avePMetrics <- Metrics::apk(
    nrow(ePrediction),
    which(ePrediction$outcomeCount == 1), (1:nrow(ePrediction))[order(-ePrediction$value)]
  )
  avePPlp <- averagePrecision(ePrediction)
  expect_equal(as.double(avePMetrics), avePPlp)
})

test_that("Evaluation metrics handle no outcome patients", {
  ePrediction <- data.frame(
    rowId = 1:100,
    ageYear = sample(18:90, 100, replace = TRUE),
    gender = sample(c(8507, 8532), 100, replace = TRUE),
    outcomeCount = rep(0, 100),
    value = runif(100, min = 0.0001, max = 0.9999),
    evaluationType = rep("Validation", 100)
  )
  attr(ePrediction, "metaData") <- list(modelType = "binary")

  expect_true(is.na(computeAuc(ePrediction, confidenceInterval = FALSE)))
  aucCi <- computeAuc(ePrediction, confidenceInterval = TRUE)
  expect_true(is.data.frame(aucCi))
  expect_true(all(is.na(aucCi)))
  expect_true(is.na(computeAuprc(ePrediction)))
  expect_true(is.na(averagePrecision(ePrediction)))

  eval <- evaluatePlp(ePrediction, typeColumn = "evaluationType")
  expect_equal(class(eval), "plpEvaluation")
  aurocRow <- eval$evaluationStatistics[eval$evaluationStatistics$metric == "AUROC", , drop = FALSE]
  expect_true(nrow(aurocRow) == 1)
  expect_true(is.na(as.numeric(aurocRow$value[1])))
})


test_that("Calibration metrics", {
  skip_if_not_installed("ResourceSelection")
  ePrediction <- data.frame(
    rowId = 1:100,
    value = c(rep(0, 50), rep(1, 50)),
    outcomeCount = c(rep(0, 50), rep(1, 50))
  )
  # test the output
  calibrationTest1 <- calibrationLine(ePrediction, numberOfStrata = 2)
  expect_equal(calibrationTest1$lm["Intercept"][[1]], 0)
  expect_equal(calibrationTest1$lm["Gradient"][[1]], 1)
  expect_true(nrow(calibrationTest1$aggregateLmData) == 2, TRUE)

  # should return - need to test all three
  ## lm # has the 'Intercept' and 'Gradient'
  ## aggregateLmData # obs vs pred for groups
  ## hosmerlemeshow # hosmerlemeshow value
  ePrediction2 <- data.frame(
    rowId = 1:100,
    value = c(0.1 + runif(50) * 0.9, runif(50) * 0.6),
    outcomeCount = c(rep(1, 50), rep(0, 50))
  )

  hsExist2 <- ResourceSelection::hoslem.test(ePrediction2$outcomeCount,
    ePrediction2$value,
    g = 10
  )
  calibrationTest2 <- calibrationLine(ePrediction2, numberOfStrata = 10)
  #  test plp values vs ResourceSelection::hoslem.test
  expect_equal(
    calibrationTest2$hosmerlemeshow["Xsquared"][[1]],
    hsExist2$statistic[[1]])
  expect_equal(
    calibrationTest2$hosmerlemeshow["df"][[1]],
    hsExist2$parameter[[1]])
  expect_equal(
    calibrationTest2$hosmerlemeshow["pvalue"][[1]],
    hsExist2$p.value)
})


test_that("calibration errors include predictions of exactly 0 and 1", {
  prediction <- data.frame(
    rowId = 1:20,
    value = rep(c(0, 1), each = 10),
    outcomeCount = rep(c(1, 0), each = 10),
    evaluation = "Test"
  )
  attr(prediction, "metaData") <- list(modelType = "binary")
  stats <- getEvaluationStatistics(prediction, "binary", typeColumn = "evaluation")
  values <- as.numeric(stats$value[match(c("Eavg", "E90", "Emax"), stats$metric)])
  # by hand: every observed risk is 1 - p, so every absolute calibration error is 1
  expect_equal(values, c(1, 1, 1))
})

test_that("calibration errors match endpoints-included values for mixed predictions", {
  prediction <- data.frame(
    rowId = 1:6,
    value = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    outcomeCount = c(0, 0, 0, 1, 1, 1),
    evaluation = "Test"
  )
  attr(prediction, "metaData") <- list(modelType = "binary")
  stats <- getEvaluationStatistics(prediction, "binary", typeColumn = "evaluation")
  values <- as.numeric(stats$value[match(c("Eavg", "E90", "Emax"), stats$metric)])
  # LOWESS (iter = 0) over all six points, and over only the interior four
  fit <- stats::lowess(prediction$value, prediction$outcomeCount, iter = 0)
  distance <- abs(prediction$value - stats::approx(fit, xout = prediction$value, ties = mean)$y)
  expect_equal(values, c(mean(distance), unname(stats::quantile(distance, 0.9)), max(distance)))
  interior <- prediction[prediction$value > 0 & prediction$value < 1, ]
  excludedFit <- stats::lowess(interior$value, interior$outcomeCount, iter = 0)
  excludedDistance <- abs(interior$value - stats::approx(excludedFit, xout = interior$value, ties = mean)$y)
  expect_false(isTRUE(all.equal(values[1], mean(excludedDistance))))
})

test_that("calibration errors are NA rather than zero when they cannot be estimated", {
  prediction <- data.frame(
    rowId = 1:20,
    value = 0.5,
    outcomeCount = c(rep(1, 10), rep(0, 10)),
    evaluation = "Test"
  )
  attr(prediction, "metaData") <- list(modelType = "binary")
  stats <- suppressWarnings(
    getEvaluationStatistics(prediction, "binary", typeColumn = "evaluation")
  )
  values <- stats$value[match(c("Eavg", "E90", "Emax"), stats$metric)]
  expect_true(all(is.na(values)))
})

test_that("E statistics binary", {
  prediction <- data.frame(
    value = c(seq(.1, .5, length.out = 5), NA, .2),
    outcomeCount = c(0, 0, 0, 1, 1, 0, NA)
  )
  eStatsBinary <- PatientLevelPrediction:::calculateEStatisticsBinary(prediction)
  expect_equal(
    eStatsBinary,
    c(Eavg = .34, E90 = .56, Emax = .6)
  )
})

# TODO: test pref scores
# test computePreferenceScore(prediction)

#############################################################################
