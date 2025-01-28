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
# how to test checkPlpInstallation?


test_that("createTempModelLoc", {
  expect_equal(class(PatientLevelPrediction:::createTempModelLoc()), "character")
})

list1 <- list(a = 1:2, b = 5:6)
list2 <- list(c = 1:5)
test_that("listAppend", {
  expect_equal(length(listAppend(list1, list2)), 3)
})

# how to test configurePython?

test_that("setPythonEnvironment", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  expect_error(setPythonEnvironment(envname = "madeup34343"))
  expect_equal(class(setPythonEnvironment(envname = "madeup34343", envtype = "conda")), "character")
})

test_that("Borrowed cut2", {
  x <- c(1, rep(2, 2), rep(4, 4), rep(5, 5), rep(6, 6))
  groups <- PatientLevelPrediction:::cut2(x, g = 3)
  expect_true(
    all(levels(groups) == c("[1,5)", "5", "6"))
  )
})

# getOs test?
