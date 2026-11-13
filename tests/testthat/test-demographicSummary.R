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

test_that("getDemographicSummary for binary predictions", {
  prediction <- data.frame(
    rowId = 1:100,
    ageYear = sample(100, 100, replace = TRUE),
    gender = sample(c(8507, "female"), 100, replace = TRUE),
    value = runif(100),
    outcomeCount = round(runif(100)),
    evaluation = rep("Test", 100)
  )

  demoSum <- getDemographicSummary(
    prediction = prediction,
    predictionType = "binary",
    typeColumn = "evaluation"
  )

  expect_equal(ncol(demoSum), 12)
  expect_true("evaluation" %in% colnames(demoSum))

  # check correct gender length
  expect_equal(length(unique(prediction$gender)), length(unique(demoSum$genGroup)))


  demoSumBin <- getDemographicSummary_binary(
    prediction = prediction,
    evalColumn = "evaluation"
  )
  expect_equal(demoSum, demoSumBin)
})


test_that("getDemographicSummary for survival predictions", {
  prediction <- data.frame(
    rowId = 1:100,
    ageYear = sample(100, 100, replace = TRUE),
    gender = sample(c(8507, "female"), 100, replace = TRUE),
    value = runif(100),
    outcomeCount = round(runif(100)),
    evaluation = rep("Test", 100),
    survivalTime = 50 + sample(730, 100, replace = TRUE)
  )

  demoSumSurv <- getDemographicSummary_survival(
    prediction = prediction,
    evalColumn = "evaluation",
    timepoint = 365
  )

  expect_s3_class(demoSumSurv, "data.frame")
  expect_equal(ncol(demoSumSurv), 8)
  expect_true("evaluation" %in% colnames(demoSumSurv))

  # check correct gender length
  expect_equal(length(unique(prediction$gender)), length(unique(demoSumSurv$genGroup)))
})

test_that("binary demographic summary does not count unknown genders as female", {
  prediction <- data.frame(
    rowId = 1:5,
    ageYear = 52,
    gender = c(8507, 8532, 0, 8551, NA),
    value = c(0.2, 0.4, 0.6, 0.8, 0.5),
    outcomeCount = c(0, 1, 0, 1, 0),
    evaluation = "Test"
  )
  result <- getDemographicSummary(prediction, predictionType = "binary")
  female <- result[which(result$genGroup == "Female"), ]
  unknown <- result[which(result$genGroup == "Unknown"), ]
  expect_equal(female$PersonCountAtRisk, 1)
  expect_equal(female$PersonCountWithOutcome, 1)
  expect_equal(female$averagePredictedProbability, 0.4)
  expect_equal(unknown$PersonCountAtRisk, 2)
  expect_equal(unknown$PersonCountWithOutcome, 1)
  expect_equal(unknown$averagePredictedProbability, 0.7)
  expect_equal(result$PersonCountAtRisk[is.na(result$genGroup)], 1)
  expect_equal(result$PersonCountAtRisk[which(result$genGroup == "Male")], 1)
})

test_that("survival demographic summary does not count unknown genders as female", {
  prediction <- data.frame(
    rowId = 1:4,
    ageYear = 52,
    gender = c("8532", "8532", "0", "8551"),
    value = c(0.2, 0.4, 0.6, 0.8),
    outcomeCount = c(1, 0, 1, 0),
    survivalTime = c(100, 500, 100, 500),
    evaluation = "Test"
  )
  result <- getDemographicSummary_survival(prediction, "evaluation", timepoint = 365)
  expect_setequal(result$genGroup, c("Female", "Unknown"))
  expect_equal(result$PersonCountAtRisk[result$genGroup == "Female"], 2)
  expect_equal(result$PersonCountAtRisk[result$genGroup == "Unknown"], 2)
  expect_equal(as.numeric(result$observedRisk[result$genGroup == "Female"]), 1 / 2)
})

test_that("survival demographic summary keeps groups without estimable events", {
  prediction <- data.frame(
    rowId = 1:10,
    ageYear = c(rep(52, 4), rep(72, 3), 72, rep(52, 2)),
    gender = c(rep(8507, 4), rep(8532, 3), 8507, rep(8532, 2)),
    value = c(0.1, 0.2, 0.3, 0.4, 0.05, 0.06, 0.07, 0.9, 0.15, 0.25),
    # group A (M, 50-54): 2 events, 2 censored after the horizon
    # group B (F, 70-74): no events, followed past the horizon
    # group C (M, 70-74): one person with an event before the horizon
    # group D (F, 50-54): no events, follow-up ends before the horizon
    outcomeCount = c(1, 1, 0, 0, 0, 0, 0, 1, 0, 0),
    survivalTime = c(100, 200, 500, 500, 500, 500, 500, 100, 100, 200),
    evaluation = "Test"
  )
  result <- getDemographicSummary_survival(prediction, "evaluation", timepoint = 365)
  lookup <- function(age, gen) {
    result[result$ageGroup == paste0("Age group: ", age) & result$genGroup == gen, ]
  }
  expect_equal(nrow(result), 4)
  groupA <- lookup("50-54", "Male")
  expect_equal(as.numeric(groupA$observedRisk), 1 / 2) # S(365) = 3/4 * 2/3
  expect_equal(groupA$PersonCountWithOutcome, 2)
  groupB <- lookup("70-74", "Female")
  expect_equal(as.numeric(groupB$observedRisk), 0)
  expect_equal(groupB$PersonCountAtRisk, 3)
  expect_equal(groupB$PersonCountWithOutcome, 0)
  groupC <- lookup("70-74", "Male")
  expect_equal(as.numeric(groupC$observedRisk), 1)
  expect_equal(groupC$PersonCountAtRisk, 1)
  groupD <- lookup("50-54", "Female")
  expect_true(is.na(as.numeric(groupD$observedRisk)))
  expect_true(is.na(groupD$PersonCountWithOutcome))
  expect_equal(groupD$PersonCountAtRisk, 2)
})

