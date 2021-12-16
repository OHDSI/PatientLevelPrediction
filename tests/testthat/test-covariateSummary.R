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

context("covariateSummary")

testCovariateData <- Andromeda::andromeda()
testCovariateData$covariates <- data.frame(
  rowId = c(1,2,5,7,10),
  covariateId = rep(1001, 5),
  covariateValue = rep(1,5)
)

testCovariateData$covariateRef <- data.frame(
  covariateId = 1001,
  covariateName = 'Made up covariate'
)

labels <- data.frame(
  rowId = 1:20, 
  outcomeCount = c(rep(1,5), rep(0,15))
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
expect_equal(res$CovariateStDev, sqrt((5*(0.25-1)^2+15*(0.25-0)^2)/20))

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
  
  expect_equal(res$WithNoOutcome_CovariateMean, 2/15)
  expect_equal(res$WithOutcome_CovariateMean, 3/5)
  
  expect_equal(res$WithNoOutcome_CovariateStDev, sqrt((2*(2/15-1)^2+13*(2/15)^2)/15))
  expect_equal(res$WithOutcome_CovariateStDev, sqrt((3*(3/5-1)^2+2*(3/5)^2)/5))
  
  expect_equal(abs(res$StandardizedMeanDiff),abs((2/15-3/5)/sqrt(((2*(2/15-1)^2+13*(2/15)^2)/15+(3*(3/5-1)^2+2*(3/5)^2)/5)/2)))
  
})
