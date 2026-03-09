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
})

test_that("Borrowed cut2", {
  x <- c(1, rep(2, 2), rep(4, 4), rep(5, 5), rep(6, 6))
  groups <- PatientLevelPrediction:::cut2(x, g = 3)
  expect_true(
    all(levels(groups) == c("[1,5)", "5", "6"))
  )
})

test_that("getModelIntercept", {
  getIntercept <- PatientLevelPrediction:::getModelIntercept

  # missing model
  expect_equal(getIntercept(NULL), 0)
  expect_equal(getIntercept(list()), 0)
  expect_equal(getIntercept(list(model = NULL), default = 7), 7)

  # Cyclops-style intercept present
  cyclopsModel <- list(
    model = list(
      coefficients = list(
        covariateIds = c("(Intercept)", "1002"),
        betas = c(1.23, 0.4)
      )
    )
  )
  expect_equal(getIntercept(cyclopsModel), 1.23)

  # Cyclops-style intercept absent
  cyclopsNoIntercept <- list(
    model = list(
      coefficients = list(
        covariateIds = c("1002", "1003"),
        betas = c(1.23, 0.4)
      )
    )
  )
  expect_equal(getIntercept(cyclopsNoIntercept), 0)

  # Cyclops-style malformed returns default
  cyclopsMalformed <- list(
    model = list(
      coefficients = list(
        covariateIds = c("(Intercept)", "1002")
      )
    )
  )
  expect_equal(getIntercept(cyclopsMalformed, default = 9), 9)

  # GLM-style intercept present
  glmModel <- list(model = list(intercept = -2.5))
  expect_equal(getIntercept(glmModel), -2.5)

  # GLM-style intercept invalid returns default
  expect_equal(getIntercept(list(model = list(intercept = c(1, 2))), default = 11), 11)
  expect_equal(getIntercept(list(model = list(intercept = "x")), default = 11), 11)
  expect_equal(getIntercept(list(model = list(intercept = NA_real_)), default = 11), 11)
  expect_equal(getIntercept(list(model = list(intercept = Inf)), default = 11), 11)

  # Prefer Cyclops intercept when both are present
  both <- list(
    model = list(
      intercept = -2.5,
      coefficients = list(
        covariateIds = c("(Intercept)", "1002"),
        betas = c(9.9, 0.4)
      )
    )
  )
  expect_equal(getIntercept(both), 9.9)

  # Non-linear model objects default
  expect_equal(getIntercept(list(model = structure(1, class = "xgb.Booster")), default = 3), 3)
})

# getOs test?
