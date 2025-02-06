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

test_that("modelBasedConcordance", {
  skip_if_offline()
  concordance <- PatientLevelPrediction::modelBasedConcordance(prediction = plpResult$prediction)
  expect_type(concordance, "double")
})

test_that("evaluatePlp_survival", {
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
    algorithm = 3,
    direction = "<"
  )
  tolerance <- 0.001

  plpAUC <- computeAuc(ePrediction, confidenceInterval = FALSE)
  expect_equal(as.numeric(procAuc$auc), plpAUC, tolerance = tolerance)
})

test_that("AUPRC", {
  ePrediction <- data.frame(value = runif(100), outcomeCount = round(runif(100)))

  positive <- ePrediction$value[ePrediction$outcomeCount == 1]
  negative <- ePrediction$value[ePrediction$outcomeCount == 0]
  pr <- PRROC::pr.curve(scores.class0 = positive, scores.class1 = negative)
  auprc <- pr$auc.integral

  # area under precision-recall curve must be between 0 and 1
  expect_gte(auprc, 0)
  expect_lte(auprc, 1)
})

test_that("Brierscore", {
  skip_if_not_installed("scoring")
  ePrediction <- data.frame(value = runif(100), outcomeCount = round(runif(100)))

  ePrediction$dummy <- 1
  brierScoring <- scoring::brierscore(outcomeCount ~ value, data = ePrediction, group = "dummy")$brieravg
  brierPlp <- brierScore(ePrediction)$brier
  expect_equal(as.double(brierScoring), (brierPlp))
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
