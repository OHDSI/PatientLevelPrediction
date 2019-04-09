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
context("ParamChecks")

# Test unit for the creation of the study externalValidatePlp


test_that("checkBoolean", {
  
  testthat::expect_error(checkBoolean(1))
  testthat::expect_error(checkBoolean('tertet'))
  
  testthat::expect_equal(checkBoolean(T), T)
  testthat::expect_equal(checkBoolean(F), T)
  
  
})

test_that("checkHigherEqual", {
  
  testthat::expect_error(checkHigherEqual(1,2))
  testthat::expect_error(checkHigherEqual('tertet',1))
  
  testthat::expect_equal(checkHigherEqual(1,0), T)
  testthat::expect_equal(checkHigherEqual(0,0), T)
  
  
})



test_that("checkLowerEqual", {
  
  testthat::expect_error(checkLowerEqual(2,1))
  testthat::expect_error(checkLowerEqual('tertet',1))
  
  testthat::expect_equal(checkLowerEqual(0,1), T)
  testthat::expect_equal(checkLowerEqual(0,0), T)

})


test_that("checkHigher", {
  
  testthat::expect_error(checkHigher(1,2))
  testthat::expect_error(checkHigher(0,0))
  testthat::expect_error(checkHigher('tertet',1))
  
  testthat::expect_equal(checkHigher(1,0), T)
  
})


test_that("checkLower", {
  
  testthat::expect_error(checkLower(2,1))
  testthat::expect_error(checkLower(0,0))
  testthat::expect_error(checkLower('tertet',1))
  
  testthat::expect_equal(checkLower(0,1), T)
  
})

test_that("checkNotNull", {
  
  testthat::expect_error(checkNotNull(NULL))
  
  testthat::expect_equal(checkNotNull(0), T)
  testthat::expect_equal(checkNotNull(T), T)
  testthat::expect_equal(checkNotNull('yeryey'), T)
  
})


test_that("checkIsClass", {
  
  testthat::expect_error(checkIsClass('dsdsds', 'double'))
  
  testthat::expect_equal(checkIsClass('dsdsds', "character"), T)

})

test_that("checkInStringVector", {
  
  testthat::expect_error(checkInStringVector('dsdsds', c('dsds','double')))
  
  testthat::expect_equal(checkInStringVector('dsdsds', c('dsdsds','double')), T)
  
})