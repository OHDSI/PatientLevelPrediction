# Copyright 2020 Observational Health Data Sciences and Informatics
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

library("testthat")
context("AndromedaHelperFunctions")

# add limitCovariatesToPopulation(covariateData, rowIds) test 

test_that("calculatePrevs", {
  THpop <- data.frame(rowId = 1:10,
                      outcomeCount = c(rep(0,5), rep(1,5)))
  N <- sample(9,1)
  covariates <- data.frame(rowId = sample(10, N),
                           covariateId = c(rep(101, N)))
  THcovariates <- Andromeda::andromeda()
  THcovariates$covariates <- covariates
  THdata <- list(covariateData = THcovariates)
  THres <- PatientLevelPrediction:::calculatePrevs(plpData = THdata, population= THpop)
  
  testthat::expect_equal(THres$covariateId, 101)
  testthat::expect_equal(THres$prev.out, sum(merge(THpop, as.data.frame(THcovariates$covariates), by='rowId')[,2]==1)/5)
  testthat::expect_equal(THres$prev.noout, sum(merge(THpop, as.data.frame(THcovariates$covariates), by='rowId')[,2]==0)/5)
  
})

# batcheRestrict test 
test_that("batchRestrict", {
  
  covariateData <- PatientLevelPrediction:::batchRestrict(plpData4$covariateData, population4, sizeN = 10000000)
  expect_is(covariateData, 'CovariateData')
  
})


# can't test clearffTempDir or checkffFolder?
