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

# add limitCovariatesToPopulation(covariateData, rowIds) test
test_that("batchRestrict", {
  skip_if_offline()
  metaData <- attr(plpData$covariateData, "metaData")
  covariateData <-
    PatientLevelPrediction:::batchRestrict(
      plpData$covariateData,
      population,
      sizeN = 1000000
    )
  expect_s4_class(covariateData, "CovariateData")

  expect_equal(
    names(metaData),
    names(attr(covariateData, "metaData"))
  )
})

test_that("limitPop with timeRef", {
  covs <- Andromeda::andromeda(
    covariates = data.frame(
      covariateId = c(1, 2, 1, 2, 2),
      rowId = c(1, 1, 2, 2, 2),
      value = c(1, 1, 1, 1, 1),
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
  limitedCovs <- limitCovariatesToPopulation(covs, rowIds)
  expect_equal(
    as.data.frame(limitedCovs$timeRef),
    as.data.frame(covs$timeRef)
  )
})
