# Copyright 2019 Observational Health Data Sciences and Informatics
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
context("RcppExports")


ffValues <- ff::as.ff(rep(1, 10))
ffBins <- ff::as.ff(rep(c(1:5), 2))

test_that("bySum", {
  
  testthat::expect_error(bySum(c(1,2), ffBins))
  
  testthat::expect_equal(dim(bySum(ffValues, ffBins)), c(5,2))
  testthat::expect_equal(bySum(ffValues, ffBins)[1,2], 2)
  
  
})


test_that("byMax", {
  
  testthat::expect_error(byMax(c(1,2), ffBins))
  
  testthat::expect_equal(dim(byMax(ffValues, ffBins)), c(5,2))
  testthat::expect_equal(byMax(ffValues, ffBins)[1,2], 1)
  
  
})