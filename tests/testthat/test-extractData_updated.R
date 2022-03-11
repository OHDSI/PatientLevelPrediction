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

context("extractPlp")

test_that("summary.plpData", {
  attr(plpData$outcomes, "metaData")$outcomeIds <- c(2,3)
  sum <- summary.plpData(plpData)
  testthat::expect_equal(class(sum),'summary.plpData')
})

test_that("getPlpData errors", {
  testthat::expect_error(getPlpData(cohortId = NULL))
  testthat::expect_error(getPlpData(cohortId = c(1,2)))
  testthat::expect_error(getPlpData(cohortId = 1, outcomeIds = NULL))
})


test_that("getPlpData works", {
  testthat::expect_true(is(plpData, "plpData"))
})

test_that("getCovariateData", {
  testthat::expect_error(getCovariateData())
})

