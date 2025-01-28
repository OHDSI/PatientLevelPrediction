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

library("testthat")
context("ParamChecks")

test_that("checkBoolean", {
  testthat::expect_error(checkBoolean(1))
  testthat::expect_error(checkBoolean("tertet"))

  testthat::expect_equal(checkBoolean(TRUE), TRUE)
  testthat::expect_equal(checkBoolean(FALSE), TRUE)
})

test_that("checkHigherEqual", {
  testthat::expect_error(checkHigherEqual(1, 2))
  testthat::expect_error(checkHigherEqual("tertet", 1))

  testthat::expect_equal(checkHigherEqual(1, 0), TRUE)
  testthat::expect_equal(checkHigherEqual(0, 0), TRUE)
})



test_that("checkLowerEqual", {
  testthat::expect_error(checkLowerEqual(2, 1))
  testthat::expect_error(checkLowerEqual("tertet", 1))

  testthat::expect_equal(checkLowerEqual(0, 1), TRUE)
  testthat::expect_equal(checkLowerEqual(0, 0), TRUE)
})


test_that("checkHigher", {
  testthat::expect_error(checkHigher(1, 2))
  testthat::expect_error(checkHigher(0, 0))
  testthat::expect_error(checkHigher("tertet", 1))

  testthat::expect_equal(checkHigher(1, 0), TRUE)
})


test_that("checkLower", {
  testthat::expect_error(checkLower(2, 1))
  testthat::expect_error(checkLower(0, 0))
  testthat::expect_error(checkLower("tertet", 1))

  testthat::expect_equal(checkLower(0, 1), TRUE)
})

test_that("checkNotNull", {
  testthat::expect_error(checkNotNull(NULL))

  testthat::expect_equal(checkNotNull(0), TRUE)
  testthat::expect_equal(checkNotNull(TRUE), TRUE)
  testthat::expect_equal(checkNotNull("yeryey"), TRUE)
})


test_that("checkIsClass", {
  testthat::expect_error(checkIsClass("dsdsds", "double"))

  testthat::expect_equal(checkIsClass("dsdsds", "character"), TRUE)
})

test_that("checkInStringVector", {
  testthat::expect_error(checkInStringVector("dsdsds", c("dsds", "double")))

  testthat::expect_equal(checkInStringVector("dsdsds", c("dsdsds", "double")), TRUE)
})

test_that("createDir", {
  dir <- tempfile()
  createDir(dir)
  testthat::expect_equal(file.exists(dir), TRUE)
  unlink(dir)
})

test_that("checkFileExists", {
  file <- tempfile()
  testthat::expect_error(checkFileExists(file))
  file.create(file)
  testthat::expect_equal(checkFileExists(file), TRUE)
  unlink(file)
})

test_that("checkDataframe", {
  expect_error(checkDataframe(
    data.frame(a = 1:2, b = 1:2), c("a", "c"),
    c("numeric", "numeric")
  ))
  expect_error(checkDataframe(
    data.frame(a = 1:2, b = 1:2),
    c("a", "b"), c("numeric", "character")
  ))
  expect_error(checkDataframe(
    data.frame(a = 1:2, b = 1:2),
    c("a", "b"), c("numeric", "numeric", "numeric")
  ))
  expect_true(checkDataframe(
    data.frame(a = 1:2, b = 1:2),
    c("a", "b"), c("integer", "integer")
  ))
  # allow both numeric and integer in a
  expect_true(checkDataframe(
    data.frame(a = as.numeric(1:2), b = 1:2),
    c("a", "b"), list(c("numeric", "integer"), "integer")
  ))
  expect_true(checkDataframe(
    data.frame(a = 1:2, b = 1:2),
    c("a", "b"), list(c("numeric", "integer"), "integer")
  ))
})
