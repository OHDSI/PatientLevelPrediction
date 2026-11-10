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

testCovariateData <- Andromeda::andromeda()
testCovariateData$covariates <- data.frame(
  rowId = c(1, 2, 5, 7, 10),
  covariateId = rep(1001, 5),
  covariateValue = rep(1, 5)
)

testCovariateData$covariateRef <- data.frame(
  covariateId = 1001,
  covariateName = "Made up covariate"
)

labels <- data.frame(
  rowId = 1:20,
  outcomeCount = c(rep(1, 5), rep(0, 15))
)


test_that("covariateSummary works with no strata or labels", {
  res <- covariateSummary(
    covariateData = testCovariateData,
    cohort = data.frame(rowId = 1:10),
    labels = NULL,
    strata = NULL,
    variableImportance = NULL,
    featureEngineering = NULL
  )

  expect_equal(res$CovariateCount, 5)
  expect_equal(res$CovariateMean, 0.5)
  expect_equal(res$CovariateStDev, 0.5)


  res <- covariateSummary(
    covariateData = testCovariateData,
    cohort = data.frame(rowId = 1:20),
    labels = NULL,
    strata = NULL,
    variableImportance = NULL,
    featureEngineering = NULL
  )

  expect_equal(res$CovariateCount, 5)
  expect_equal(res$CovariateMean, 0.25)
  expect_equal(res$CovariateStDev, sqrt((5 * (0.25 - 1)^2 + 15 * (0.25 - 0)^2) / 20))
})

test_that("covariateSummary works with labels", {
  res <- covariateSummary(
    covariateData = testCovariateData,
    cohort = data.frame(rowId = 1:20),
    labels = labels,
    strata = NULL,
    variableImportance = NULL,
    featureEngineering = NULL
  )

  expect_equal(res$WithNoOutcome_CovariateCount, 2)
  expect_equal(res$WithOutcome_CovariateCount, 3)

  expect_equal(res$WithNoOutcome_CovariateMean, 2 / 15)
  expect_equal(res$WithOutcome_CovariateMean, 3 / 5)

  expect_equal(res$WithNoOutcome_CovariateStDev, sqrt((2 * (2 / 15 - 1)^2 + 13 * (2 / 15)^2) / 15))
  expect_equal(res$WithOutcome_CovariateStDev, sqrt((3 * (3 / 5 - 1)^2 + 2 * (3 / 5)^2) / 5))

  expect_equal(abs(res$StandardizedMeanDiff), abs((2 / 15 - 3 / 5) / sqrt(((2 * (2 / 15 - 1)^2 + 13 * (2 / 15)^2) / 15 + (3 * (3 / 5 - 1)^2 + 2 * (3 / 5)^2) / 5) / 2)))
})

test_that("covariateSummarySubset works with temporal data", {
  covs <- Andromeda::andromeda(
    covariates = data.frame(
      covariateId = c(1, 2, 1, 2, 2),
      rowId = c(1, 1, 2, 2, 2),
      covariateValue = c(1, 1, 1, 1, 1),
      timeId = c(1, 1, 1, 1, 2)
    ),
    covariateRef = data.frame(
      covariateId = c(1, 2),
      covariateName = c("cov1", "cov2"),
      analysisId = c(1, 2)
    ),
    analysisRef = data.frame(
      analysisId = c(1, 2),
      analysisName = c("analysis1", "analysis2")
    ),
    timeRef = data.frame(
      timePart = "day",
      timeInterval = 1,
      sequenceStartDay = 0,
      sequenceEndDay = 1
    )
  )
  class(covs) <- "CovariateData"
  rowIds <- c(2)
  restrictToSubset <- covariateSummarySubset(covs, rowIds)
  expect_equal(nrow(restrictToSubset), 2)
})

test_that("pooled covariate summaries keep the population of zero-only subgroups", {
  sparseData <- Andromeda::andromeda()
  # covariate 1001 only occurs for rowIds 1:10, all in Train with an outcome (N = 50)
  sparseData$covariates <- data.frame(
    rowId = c(1:10, 1:200),
    covariateId = c(rep(1001, 10), rep(1002, 200)),
    covariateValue = 1
  )
  sparseData$covariateRef <- data.frame(
    covariateId = c(1001, 1002),
    covariateName = c("rare", "common")
  )
  cohort <- data.frame(rowId = 1:200)
  sparseLabels <- data.frame(rowId = 1:200, outcomeCount = c(rep(1, 100), rep(0, 100)))
  strata <- data.frame(
    rowId = 1:200,
    strataName = c(rep("Train", 50), rep("Test", 50), rep("Train", 50), rep("Test", 50))
  )
  rare <- function(x) x[x$covariateId == 1001, ]

  # by hand, across 200 people 10 positive
  pooledMean <- 10 / 200
  pooledSd <- sqrt(0.05 * 0.95)

  unstratified <- rare(covariateSummary(sparseData, cohort))
  expect_equal(unstratified$CovariateMean, pooledMean)
  expect_equal(unstratified$CovariateStDev, pooledSd)

  withLabels <- rare(covariateSummary(sparseData, cohort, labels = sparseLabels))
  expect_equal(withLabels$CovariateMean, pooledMean)
  expect_equal(withLabels$CovariateStDev, pooledSd)
  expect_equal(withLabels$CovariateCount, 10)
  expect_equal(withLabels$WithOutcome_CovariateMean, 0.1)
  expect_equal(withLabels$WithNoOutcome_CovariateMean, 0)

  withBoth <- rare(covariateSummary(
    sparseData, cohort,
    labels = sparseLabels, strata = strata
  ))
  expect_equal(withBoth$CovariateMean, pooledMean)
  expect_equal(withBoth$CovariateStDev, pooledSd)
  expect_equal(withBoth$CovariateCount, 10)
  expect_equal(withBoth$WithOutcome_CovariateMean, 10 / 100)
  expect_equal(withBoth$WithOutcome_CovariateStDev, sqrt(0.1 * 0.9))
  expect_equal(withBoth$WithNoOutcome_CovariateMean, 0)
  expect_equal(withBoth$StandardizedMeanDiff, 0.1 / sqrt((0.1 * 0.9 + 0) / 2))
  # per-subgroup columns still describe their own subgroup
  expect_equal(withBoth$TrainWithOutcome_CovariateMean, 10 / 50)
  expect_equal(withBoth$TestWithOutcome_CovariateMean, 0)

  common <- covariateSummary(sparseData, cohort, labels = sparseLabels, strata = strata)
  common <- common[common$covariateId == 1002, ]
  expect_equal(common$CovariateMean, 1)
  expect_equal(common$CovariateStDev, 0)
})

test_that("covariateSummary works when a label group has no covariate rows", {
  sparseData <- Andromeda::andromeda()
  sparseData$covariates <- data.frame(
    rowId = 1:5,
    covariateId = 1001,
    covariateValue = 1
  )
  sparseData$covariateRef <- data.frame(covariateId = 1001, covariateName = "only cases")
  sparseLabels <- data.frame(rowId = 1:10, outcomeCount = c(rep(1, 5), rep(0, 5)))
  result <- covariateSummary(sparseData, data.frame(rowId = 1:10), labels = sparseLabels)
  # by hand: 5 of 10 people, all in the outcome group
  expect_equal(result$CovariateMean, 0.5)
  expect_equal(result$WithOutcome_CovariateMean, 1)
  expect_equal(result$WithNoOutcome_CovariateMean, 0)
})

test_that("covariateSummary works with strata but no labels", {
  sparseData <- Andromeda::andromeda()
  sparseData$covariates <- data.frame(
    rowId = c(1:4, 1:10),
    covariateId = c(rep(1001, 4), rep(1002, 10)),
    covariateValue = 1
  )
  sparseData$covariateRef <- data.frame(covariateId = c(1001, 1002), covariateName = c("rare", "common"))
  strata <- data.frame(rowId = 1:10, strataName = rep(c("Train", "Test"), each = 5))
  result <- covariateSummary(sparseData, data.frame(rowId = 1:10), strata = strata)
  rare <- result[result$covariateId == 1001, ]
  # by hand: 4 of 10 people, all in Train (5 people)
  expect_equal(rare$CovariateMean, 0.4)
  expect_equal(rare$Train_CovariateMean, 4 / 5)
  expect_equal(rare$Test_CovariateMean, 0)
  # no outcome groups, so no standardized mean difference
  expect_false("StandardizedMeanDiff" %in% colnames(result))
})
