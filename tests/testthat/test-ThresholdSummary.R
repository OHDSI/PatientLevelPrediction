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

test_that("getThresholdSummary survival", {
  ePrediction <- data.frame(
    value = c(
      (100 + sample(10, 50, replace = TRUE)),
      (105 + sample(10, 150, replace = TRUE))
    ),
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


test_that("f1Score", {
  expect_equal(f1Score(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_error(f1Score(TP = -1, TN = 0, FN = 0, FP = 0))
  expect_error(f1Score(TP = 1, TN = -1, FN = 0, FP = 0))
  expect_error(f1Score(TP = 1, TN = 3, FN = -1, FP = 0))
  expect_error(f1Score(TP = 1, TN = 1, FN = 5, FP = -1))
  expect_error(f1Score(TP = NULL, TN = 0, FN = 0, FP = 0))
  expect_error(f1Score(TP = 1, TN = NULL, FN = 0, FP = 0))
  expect_error(f1Score(TP = 1, TN = 3, FN = NULL, FP = 0))
  expect_error(f1Score(TP = 1, TN = 1, FN = 5, FP = NULL))
  expect_equal(f1Score(TP = 10, TN = 3, FN = 5, FP = 5), 0.6666667,
  tolerance = 1e-4)
})

test_that("accuracy", {
  expect_equal(accuracy(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_error(accuracy(TP = -1, TN = 0, FN = 0, FP = 0))
  expect_error(accuracy(TP = 1, TN = -1, FN = 0, FP = 0))
  expect_error(accuracy(TP = 1, TN = 3, FN = -1, FP = 0))
  expect_error(accuracy(TP = 1, TN = 1, FN = 5, FP = -1))
  expect_error(accuracy(TP = NULL, TN = 0, FN = 0, FP = 0))
  expect_error(accuracy(TP = 1, TN = NULL, FN = 0, FP = 0))
  expect_error(accuracy(TP = 1, TN = 3, FN = NULL, FP = 0))
  expect_error(accuracy(TP = 1, TN = 1, FN = 5, FP = NULL))
  expect_equal(accuracy(TP = 10, TN = 3, FN = 5, FP = 5), 13 / 23)
})

test_that("sensitivity", {
  expect_equal(sensitivity(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_error(sensitivity(TP = -1, TN = 0, FN = 0, FP = 0))
  expect_error(sensitivity(TP = 1, TN = -1, FN = 0, FP = 0))
  expect_error(sensitivity(TP = 1, TN = 3, FN = -1, FP = 0))
  expect_error(sensitivity(TP = 1, TN = 1, FN = 5, FP = -1))
  expect_error(sensitivity(TP = NULL, TN = 0, FN = 0, FP = 0))
  expect_error(sensitivity(TP = 1, TN = NULL, FN = 0, FP = 0))
  expect_error(sensitivity(TP = 1, TN = 3, FN = NULL, FP = 0))
  expect_error(sensitivity(TP = 1, TN = 1, FN = 5, FP = NULL))
  expect_equal(sensitivity(TP = 10, TN = 3, FN = 5, FP = 5), (10 / (10 + 5)))
})

test_that("falseNegativeRate", {
  expect_equal(falseNegativeRate(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_error(falseNegativeRate(TP = -1, TN = 0, FN = 0, FP = 0))
  expect_error(falseNegativeRate(TP = 1, TN = -1, FN = 0, FP = 0))
  expect_error(falseNegativeRate(TP = 1, TN = 3, FN = -1, FP = 0))
  expect_error(falseNegativeRate(TP = 1, TN = 1, FN = 5, FP = -1))
  expect_error(falseNegativeRate(TP = NULL, TN = 0, FN = 0, FP = 0))
  expect_error(falseNegativeRate(TP = 1, TN = NULL, FN = 0, FP = 0))
  expect_error(falseNegativeRate(TP = 1, TN = 3, FN = NULL, FP = 0))
  expect_error(falseNegativeRate(TP = 1, TN = 1, FN = 5, FP = NULL))
  expect_equal(falseNegativeRate(TP = 10, TN = 3, FN = 5, FP = 5), 5 / (10 + 5))
})

test_that("falsePositiveRate", {
  expect_equal(falsePositiveRate(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_error(falsePositiveRate(TP = -1, TN = 0, FN = 0, FP = 0))
  expect_error(falsePositiveRate(TP = 1, TN = -1, FN = 0, FP = 0))
  expect_error(falsePositiveRate(TP = 1, TN = 3, FN = -1, FP = 0))
  expect_error(falsePositiveRate(TP = 1, TN = 1, FN = 5, FP = -1))
  expect_error(falsePositiveRate(TP = NULL, TN = 0, FN = 0, FP = 0))
  expect_error(falsePositiveRate(TP = 1, TN = NULL, FN = 0, FP = 0))
  expect_error(falsePositiveRate(TP = 1, TN = 3, FN = NULL, FP = 0))
  expect_error(falsePositiveRate(TP = 1, TN = 1, FN = 5, FP = NULL))
  expect_equal(falsePositiveRate(TP = 10, TN = 3, FN = 5, FP = 5), 5 / (5 + 3))
})

test_that("specificity", {
  expect_equal(specificity(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_error(specificity(TP = -1, TN = 0, FN = 0, FP = 0))
  expect_error(specificity(TP = 1, TN = -1, FN = 0, FP = 0))
  expect_error(specificity(TP = 1, TN = 3, FN = -1, FP = 0))
  expect_error(specificity(TP = 1, TN = 1, FN = 5, FP = -1))
  expect_error(specificity(TP = NULL, TN = 0, FN = 0, FP = 0))
  expect_error(specificity(TP = 1, TN = NULL, FN = 0, FP = 0))
  expect_error(specificity(TP = 1, TN = 3, FN = NULL, FP = 0))
  expect_error(specificity(TP = 1, TN = 1, FN = 5, FP = NULL))
  expect_equal(specificity(TP = 10, TN = 3, FN = 5, FP = 5), 3 / (5 + 3))
})

test_that("positivePredictiveValue", {
  expect_equal(positivePredictiveValue(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_error(positivePredictiveValue(TP = -1, TN = 0, FN = 0, FP = 0))
  expect_error(positivePredictiveValue(TP = 1, TN = -1, FN = 0, FP = 0))
  expect_error(positivePredictiveValue(TP = 1, TN = 3, FN = -1, FP = 0))
  expect_error(positivePredictiveValue(TP = 1, TN = 1, FN = 5, FP = -1))
  expect_error(positivePredictiveValue(TP = NULL, TN = 0, FN = 0, FP = 0))
  expect_error(positivePredictiveValue(TP = 1, TN = NULL, FN = 0, FP = 0))
  expect_error(positivePredictiveValue(TP = 1, TN = 3, FN = NULL, FP = 0))
  expect_error(positivePredictiveValue(TP = 1, TN = 1, FN = 5, FP = NULL))
  expect_equal(positivePredictiveValue(TP = 10, TN = 3, FN = 5, FP = 5), 10 / (10 + 5))
})


test_that("falseDiscoveryRate", {
  expect_equal(falseDiscoveryRate(TP = 0, TN = 0, FN = 0, FP = 0),NaN)
  expect_error(falseDiscoveryRate(TP = -1, TN = 0, FN = 0, FP = 0))
  expect_error(falseDiscoveryRate(TP = 1, TN = -1, FN = 0, FP = 0))
  expect_error(falseDiscoveryRate(TP = 1, TN = 3, FN = -1, FP = 0))
  expect_error(falseDiscoveryRate(TP = 1, TN = 1, FN = 5, FP = -1))
  expect_error(falseDiscoveryRate(TP = NULL, TN = 0, FN = 0, FP = 0))
  expect_error(falseDiscoveryRate(TP = 1, TN = NULL, FN = 0, FP = 0))
  expect_error(falseDiscoveryRate(TP = 1, TN = 3, FN = NULL, FP = 0))
  expect_error(falseDiscoveryRate(TP = 1, TN = 1, FN = 5, FP = NULL))
  expect_equal(falseDiscoveryRate(TP = 10, TN = 3, FN = 5, FP = 5), 5 / (10 + 5))
})

test_that("negativePredictiveValue", {
  expect_equal(negativePredictiveValue(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_error(negativePredictiveValue(TP = -1, TN = 0, FN = 0, FP = 0))
  expect_error(negativePredictiveValue(TP = 1, TN = -1, FN = 0, FP = 0))
  expect_error(negativePredictiveValue(TP = 1, TN = 3, FN = -1, FP = 0))
  expect_error(negativePredictiveValue(TP = 1, TN = 1, FN = 5, FP = -1))
  expect_error(negativePredictiveValue(TP = NULL, TN = 0, FN = 0, FP = 0))
  expect_error(negativePredictiveValue(TP = 1, TN = NULL, FN = 0, FP = 0))
  expect_error(negativePredictiveValue(TP = 1, TN = 3, FN = NULL, FP = 0))
  expect_error(negativePredictiveValue(TP = 1, TN = 1, FN = 5, FP = NULL))
  expect_equal(negativePredictiveValue(TP = 10, TN = 3, FN = 5, FP = 5), 3 / (5 + 3))
})

test_that("falseOmissionRate", {
  expect_equal(falseOmissionRate(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_error(falseOmissionRate(TP = -1, TN = 0, FN = 0, FP = 0))
  expect_error(falseOmissionRate(TP = 1, TN = -1, FN = 0, FP = 0))
  expect_error(falseOmissionRate(TP = 1, TN = 3, FN = -1, FP = 0))
  expect_error(falseOmissionRate(TP = 1, TN = 1, FN = 5, FP = -1))
  expect_error(falseOmissionRate(TP = NULL, TN = 0, FN = 0, FP = 0))
  expect_error(falseOmissionRate(TP = 1, TN = NULL, FN = 0, FP = 0))
  expect_error(falseOmissionRate(TP = 1, TN = 3, FN = NULL, FP = 0))
  expect_error(falseOmissionRate(TP = 1, TN = 1, FN = 5, FP = NULL))
  expect_equal(falseOmissionRate(TP = 10, TN = 3, FN = 5, FP = 5), 5 / (5 + 3))
})

test_that("negativeLikelihoodRatio", {
  expect_equal(negativeLikelihoodRatio(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_error(negativeLikelihoodRatio(TP = -1, TN = 0, FN = 0, FP = 0))
  expect_error(negativeLikelihoodRatio(TP = 1, TN = -1, FN = 0, FP = 0))
  expect_error(negativeLikelihoodRatio(TP = 1, TN = 3, FN = -1, FP = 0))
  expect_error(negativeLikelihoodRatio(TP = 1, TN = 1, FN = 5, FP = -1))
  expect_error(negativeLikelihoodRatio(TP = NULL, TN = 0, FN = 0, FP = 0))
  expect_error(negativeLikelihoodRatio(TP = 1, TN = NULL, FN = 0, FP = 0))
  expect_error(negativeLikelihoodRatio(TP = 1, TN = 3, FN = NULL, FP = 0))
  expect_error(negativeLikelihoodRatio(TP = 1, TN = 1, FN = 5, FP = NULL))
  expect_equal(negativeLikelihoodRatio(TP = 10, TN = 3, FN = 5, FP = 5), (5 / (10 + 5)) / (3 / (5 + 3)))
})

test_that("positiveLikelihoodRatio", {
  expect_equal(positiveLikelihoodRatio(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_error(positiveLikelihoodRatio(TP = -1, TN = 0, FN = 0, FP = 0))
  expect_error(positiveLikelihoodRatio(TP = 1, TN = -1, FN = 0, FP = 0))
  expect_error(positiveLikelihoodRatio(TP = 1, TN = 3, FN = -1, FP = 0))
  expect_error(positiveLikelihoodRatio(TP = 1, TN = 1, FN = 5, FP = -1))
  expect_error(positiveLikelihoodRatio(TP = NULL, TN = 0, FN = 0, FP = 0))
  expect_error(positiveLikelihoodRatio(TP = 1, TN = NULL, FN = 0, FP = 0))
  expect_error(positiveLikelihoodRatio(TP = 1, TN = 3, FN = NULL, FP = 0))
  expect_error(positiveLikelihoodRatio(TP = 1, TN = 1, FN = 5, FP = NULL))
  expect_equal(positiveLikelihoodRatio(TP = 10, TN = 3, FN = 5, FP = 5), (10 / (10 + 5)) / (5 / (5 + 3)))
})


test_that("diagnosticOddsRatio", {
  expect_equal(diagnosticOddsRatio(TP = 0, TN = 0, FN = 0, FP = 0), NaN)
  expect_error(diagnosticOddsRatio(TP = -1, TN = 0, FN = 0, FP = 0))
  expect_error(diagnosticOddsRatio(TP = 1, TN = -1, FN = 0, FP = 0))
  expect_error(diagnosticOddsRatio(TP = 1, TN = 3, FN = -1, FP = 0))
  expect_error(diagnosticOddsRatio(TP = 1, TN = 1, FN = 5, FP = -1))
  expect_error(diagnosticOddsRatio(TP = NULL, TN = 0, FN = 0, FP = 0))
  expect_error(diagnosticOddsRatio(TP = 1, TN = NULL, FN = 0, FP = 0))
  expect_error(diagnosticOddsRatio(TP = 1, TN = 3, FN = NULL, FP = 0))
  expect_error(diagnosticOddsRatio(TP = 1, TN = 1, FN = 5, FP = NULL))
  expect_equal(diagnosticOddsRatio(TP = 10, TN = 3, FN = 5, FP = 5), ((10 / (10 + 5)) / (5 / (5 + 3))) / ((5 / (10 + 5)) / (3 / (5 + 3))))
})
