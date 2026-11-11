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
test_that("getThresholdSummary binary", {
  ePrediction <- data.frame(
    value = runif(100),
    outcomeCount = round(runif(100)),
    evaluation = rep("Test", 100)
  )

  thresSum <- getThresholdSummary(
    prediction = ePrediction,
    predictionType = "binary",
    typeColumn = "evaluation"
  )

  expect_true("evaluation" %in% colnames(thresSum))
  expect_equal(nrow(thresSum), length(unique(ePrediction$value)))
  expect_equal(ncol(thresSum), 24)

  expect_equal(
    thresSum$truePositiveCount + thresSum$falseNegativeCount,
    rep(sum(ePrediction$outcomeCount), length(thresSum$truePositiveCount))
  )

  expect_equal(
    thresSum$truePositiveCount + thresSum$falsePositiveCount +
      thresSum$trueNegativeCount + thresSum$falseNegativeCount,
    rep(nrow(ePrediction), length(thresSum$truePositiveCount))
  )

  thresSumBin <- getThresholdSummary_binary(
    prediction = ePrediction,
    evalColumn = "evaluation"
  )

  expect_equal(thresSumBin, thresSum)
})

test_that("thresholdSummary reports F1 of zero when no true positives are captured", {
  prediction <- data.frame(
    rowId = 1:4,
    value = c(0.9, 0.8, 0.7, 0.6),
    outcomeCount = c(0, 0, 1, 1),
    evaluation = "Test"
  )
  result <- getThresholdSummary(prediction)
  f1 <- result$f1Score[order(-result$predictionThreshold)]
  # by hand: (TP,FP,FN) = (0,1,2), (0,2,2), (1,2,1), (2,2,0); F1 = 2TP/(2TP+FP+FN)
  expect_equal(f1, c(0, 0, 0.4, 2 / 3))
})

test_that("getThresholdSummary survival", {
  ePrediction <- data.frame(
    value = c(
      (100 + sample(10, 50, replace = TRUE)),
      (105 + sample(10, 150, replace = TRUE))
    ) / 250,
    outcomeCount = c(rep(1, 50), rep(0, 150)),
    evaluation = rep("Test", 200),
    survivalTime = 50 + sample(365 * 2, 200, replace = TRUE)
  )

  thresSum <- getThresholdSummary_survival(
    prediction = ePrediction,
    evalColumn = "evaluation",
    timepoint = 365
  )

  expect_true("evaluation" %in% colnames(thresSum))
  expect_true(nrow(thresSum) > 0)
  expect_equal(ncol(thresSum), 5)
})


test_that("survival net benefit uses absolute probabilities", {
  prediction <- data.frame(
    rowId = 1:20,
    value = seq(0.01, 0.19, length.out = 20),
    outcomeCount = as.integer(1:20 %in% c(5, 15)),
    survivalTime = ifelse(1:20 %in% c(5, 15), 100, 730),
    evaluation = "Test"
  )
  attr(prediction, "metaData") <- list(timepoint = 365, modelType = "survival")

  result <- getThresholdSummary(prediction, predictionType = "survival")

  # all thresholds stay on the probability scale of the input
  expect_equal(max(result$threshold), 0.19)
  expect_equal(min(result$threshold), 0.01)
  # with complete follow-up net benefit is TP/N - FP/N * t/(1-t)
  handNetBenefit <- sapply(result$threshold, function(t) {
    selected <- prediction$value > t
    (sum(selected & prediction$outcomeCount == 1) -
      sum(selected & prediction$outcomeCount == 0) * t / (1 - t)) / 20
  })
  hasValue <- !is.na(result$p)
  expect_equal(result$p[hasValue], handNetBenefit[hasValue])
  # worked example: 19 selected at t = 0.01, both cases among them
  expect_equal(result$p[1], 2 / 20 - 17 / 20 * 0.01 / 0.99)
})

test_that("survival net benefit is not calculated for values that are not probabilities", {
  prediction <- data.frame(
    rowId = 1:20,
    value = seq(1.5, 20, length.out = 20),
    outcomeCount = as.integer(1:20 %in% c(5, 15)),
    survivalTime = ifelse(1:20 %in% c(5, 15), 100, 730),
    evaluation = "Test"
  )
  expect_null(getThresholdSummary_survival(prediction, "evaluation", timepoint = 365))
})

test_that("f1Score", {
  expect_equal(f1Score(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  # no true positives with positives present: precision and recall are 0, F1 is 0
  expect_equal(f1Score(TP = 0, TN = 5, FN = 3, FP = 2), 0)
  expect_equal(f1Score(TP = c(0, 10), TN = c(5, 3), FN = c(3, 5), FP = c(2, 5)), c(0, 2 / 3))
  expect_equal(f1Score(TP = 2, TN = 0, FN = 0, FP = 0), 1)
  expect_equal(f1Score(TP = 10, TN = 3, FN = 5, FP = 5), 0.6666667,
  tolerance = 1e-4)
})

test_that("accuracy", {
  expect_equal(accuracy(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_equal(accuracy(TP = 10, TN = 3, FN = 5, FP = 5), 13 / 23)
})

test_that("sensitivity", {
  expect_equal(sensitivity(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_equal(sensitivity(TP = 10, TN = 3, FN = 5, FP = 5), (10 / (10 + 5)))
})

test_that("falseNegativeRate", {
  expect_equal(falseNegativeRate(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_equal(falseNegativeRate(TP = 10, TN = 3, FN = 5, FP = 5), 5 / (10 + 5))
})

test_that("falsePositiveRate", {
  expect_equal(falsePositiveRate(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_equal(falsePositiveRate(TP = 10, TN = 3, FN = 5, FP = 5), 5 / (5 + 3))
})

test_that("specificity", {
  expect_equal(specificity(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_equal(specificity(TP = 10, TN = 3, FN = 5, FP = 5), 3 / (5 + 3))
})

test_that("positivePredictiveValue", {
  expect_equal(positivePredictiveValue(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_equal(positivePredictiveValue(TP = 10, TN = 3, FN = 5, FP = 5), 10 / (10 + 5))
})


test_that("falseDiscoveryRate", {
  expect_equal(falseDiscoveryRate(TP = 0, TN = 0, FN = 0, FP = 0),NaN)
  expect_equal(falseDiscoveryRate(TP = 10, TN = 3, FN = 5, FP = 5), 5 / (10 + 5))
})

test_that("negativePredictiveValue", {
  expect_equal(negativePredictiveValue(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_equal(negativePredictiveValue(TP = 10, TN = 3, FN = 5, FP = 5), 3 / (5 + 3))
})

test_that("falseOmissionRate", {
  expect_equal(falseOmissionRate(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_equal(falseOmissionRate(TP = 10, TN = 3, FN = 5, FP = 5), 5 / (5 + 3))
})

test_that("negativeLikelihoodRatio", {
  expect_equal(negativeLikelihoodRatio(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_equal(negativeLikelihoodRatio(TP = 10, TN = 3, FN = 5, FP = 5), (5 / (10 + 5)) / (3 / (5 + 3)))
})

test_that("positiveLikelihoodRatio", {
  expect_equal(positiveLikelihoodRatio(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_equal(positiveLikelihoodRatio(TP = 10, TN = 3, FN = 5, FP = 5), (10 / (10 + 5)) / (5 / (5 + 3)))
})


test_that("diagnosticOddsRatio", {
  expect_equal(diagnosticOddsRatio(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_equal(diagnosticOddsRatio(TP = 10, TN = 3, FN = 5, FP = 5), ((10 / (10 + 5)) / (5 / (5 + 3))) / ((5 / (10 + 5)) / (3 / (5 + 3))))
})

test_that("confusion matrix metrics reject negative and missing counts", {
  metrics <- list(
    f1Score = f1Score, accuracy = accuracy, sensitivity = sensitivity,
    falseNegativeRate = falseNegativeRate, falsePositiveRate = falsePositiveRate,
    specificity = specificity, positivePredictiveValue = positivePredictiveValue,
    falseDiscoveryRate = falseDiscoveryRate, negativePredictiveValue = negativePredictiveValue,
    falseOmissionRate = falseOmissionRate, negativeLikelihoodRatio = negativeLikelihoodRatio,
    positiveLikelihoodRatio = positiveLikelihoodRatio, diagnosticOddsRatio = diagnosticOddsRatio
  )
  valid <- list(TP = 1, TN = 1, FN = 1, FP = 1)
  for (name in names(metrics)) {
    for (count in names(valid)) {
      negative <- valid
      negative[[count]] <- -1
      expect_error(do.call(metrics[[name]], negative), paste(count, "< 0"), fixed = TRUE, info = name)
      missing <- valid
      missing[count] <- list(NULL)
      expect_error(do.call(metrics[[name]], missing), paste("Incorrect", count, "class"), info = name)
    }
  }
})
