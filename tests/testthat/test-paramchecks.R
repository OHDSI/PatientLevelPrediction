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
test_that("checkBoolean", {
  expect_error(checkBoolean(1), "1 not defined correctly")
  expect_error(checkBoolean("tertet"), "\"tertet\" not defined correctly")

  expect_equal(checkBoolean(TRUE), TRUE)
  expect_equal(checkBoolean(FALSE), TRUE)
})

test_that("checkHigherEqual", {
  expect_error(checkHigherEqual(1, 2), "1 needs to be >= 2")
  expect_error(checkHigherEqual("tertet", 1), "\"tertet\" needs to be >= 1")

  expect_equal(checkHigherEqual(1, 0), TRUE)
  expect_equal(checkHigherEqual(0, 0), TRUE)
})



test_that("checkLowerEqual", {
  expect_error(checkLowerEqual(2, 1), "2 needs to be <= 1")
  expect_error(checkLowerEqual("tertet", 1), "\"tertet\" needs to be <= 1")

  expect_equal(checkLowerEqual(0, 1), TRUE)
  expect_equal(checkLowerEqual(0, 0), TRUE)
})


test_that("checkHigher", {
  expect_error(checkHigher(1, 2), "1 needs to be > 2")
  expect_error(checkHigher(0, 0), "0 needs to be > 0")
  expect_error(checkHigher("tertet", 1), "\"tertet\" needs to be > 1")

  expect_equal(checkHigher(1, 0), TRUE)
})


test_that("checkLower", {
  expect_error(checkLower(2, 1), "2 needs to be < 1")
  expect_error(checkLower(0, 0), "0 needs to be < 0")
  expect_error(checkLower("tertet", 1), "\"tertet\" needs to be < 1")

  expect_equal(checkLower(0, 1), TRUE)
})

test_that("checkNotNull", {
  expect_error(checkNotNull(NULL), "NULL cannot be empty")

  expect_equal(checkNotNull(0), TRUE)
  expect_equal(checkNotNull(TRUE), TRUE)
  expect_equal(checkNotNull("yeryey"), TRUE)
})


test_that("checkIsClass", {
  expect_error(checkIsClass("dsdsds", "double"), "\"dsdsds\" is wrong class")

  expect_equal(checkIsClass("dsdsds", "character"), TRUE)
})

test_that("checkIsWholeNumber", {
  expect_error(checkIsWholeNumber(1.5), "1\\.5 must be a single whole number")
  expect_error(checkIsWholeNumber(c(1, 2)), "c\\(1, 2\\) must be a single whole number")
  expect_error(checkIsWholeNumber("1"), "\"1\" must be a single whole number")
  expect_error(checkIsWholeNumber(NA_real_), "NA_real_ must be a single whole number")
  expect_error(checkIsWholeNumber(Inf), "Inf must be a single whole number")

  expect_equal(checkIsWholeNumber(1), TRUE)
  expect_equal(checkIsWholeNumber(1L), TRUE)
})

test_that("checkSingleFiniteNumeric", {
  expect_error(checkSingleFiniteNumeric(c(1, 2)), "c\\(1, 2\\) must be a single finite numeric value")
  expect_error(checkSingleFiniteNumeric("1"), "\"1\" must be a single finite numeric value")
  expect_error(checkSingleFiniteNumeric(NA_real_), "NA_real_ must be a single finite numeric value")
  expect_error(checkSingleFiniteNumeric(Inf), "Inf must be a single finite numeric value")

  expect_equal(checkSingleFiniteNumeric(1), TRUE)
  expect_equal(checkSingleFiniteNumeric(1L), TRUE)
})

test_that("checkInStringVector", {
  expect_error(checkInStringVector("dsdsds", c("dsds", "double")), "\"dsdsds\" has incorrect value")
  expect_error(checkInStringVector(c("dsdsds", "double"), c("dsdsds", "double")), "c\\(\"dsdsds\", \"double\"\\) has incorrect value")
  expect_error(checkInStringVector(NA_character_, c("dsdsds", "double")), "NA_character_ has incorrect value")

  expect_equal(checkInStringVector("dsdsds", c("dsdsds", "double")), TRUE)
})

test_that("createDir", {
  dir <- tempfile()
  createDir(dir)
  expect_equal(file.exists(dir), TRUE)
  unlink(dir)
})

test_that("checkFileExists", {
  file <- tempfile()
  expect_error(checkFileExists(file), "file does not exist")
  file.create(file)
  expect_equal(checkFileExists(file), TRUE)
  unlink(file)
})

test_that("checkDataframe", {
  expect_error(checkDataframe(
    data.frame(a = 1:2, b = 1:2), c("a", "c"),
    c("numeric", "numeric")
  ), "Column names of data\\.frame\\(a = 1:2, b = 1:2\\) are not correct")
  expect_error(checkDataframe(
    data.frame(a = 1:2, b = 1:2),
    c("a", "b"), c("numeric", "character")
  ), "Column types of data\\.frame\\(a = 1:2, b = 1:2\\) are not correct\\.")
  expect_error(checkDataframe(
    data.frame(a = 1:2, b = 1:2),
    c("a", "b"), c("numeric", "numeric", "numeric")
  ), "The length of 'columnTypes' must be equal to the length of 'columns'")
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
