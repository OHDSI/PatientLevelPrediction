# Copyright 2026 Observational Health Data Sciences and Informatics
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

test_that("sanitizeSklearnAdaBoostParams drops deprecated keys", {
  params <- list(
    algorithm = "SAMME.R",
    n_estimators = 10L
  )

  sanitized <- PatientLevelPrediction:::sanitizeSklearnAdaBoostParams(params)

  expect_null(sanitized$algorithm)
  expect_equal(sanitized$n_estimators, 10L)
})

test_that("sanitizeSklearnAdaBoostParams maps base_estimator to estimator", {
  params <- list(
    base_estimator = "tree",
    n_estimators = 10L
  )

  sanitized <- PatientLevelPrediction:::sanitizeSklearnAdaBoostParams(params)

  expect_null(sanitized$base_estimator)
  expect_equal(sanitized$estimator, "tree")
})


test_that("restoreAdaBoostAlgorithm keeps a supported saved algorithm", {
  params <- list(algorithm = "SAMME", n_estimators = 10L, base_estimator = "tree")

  restored <- PatientLevelPrediction:::restoreAdaBoostAlgorithm(
    params,
    supportsSammeR = TRUE,
    acceptsAlgorithm = TRUE
  )

  expect_equal(restored$algorithm, "SAMME")
  expect_equal(restored$estimator, "tree")
  expect_null(restored$base_estimator)
})

test_that("restoreAdaBoostAlgorithm drops the argument when the runtime removed it", {
  restored <- PatientLevelPrediction:::restoreAdaBoostAlgorithm(
    list(algorithm = "SAMME", n_estimators = 10L),
    supportsSammeR = FALSE,
    acceptsAlgorithm = FALSE
  )

  expect_null(restored$algorithm)
  expect_equal(restored$n_estimators, 10L)
})

test_that("restoreAdaBoostAlgorithm rejects SAMME.R on runtimes without it", {
  expect_error(
    PatientLevelPrediction:::restoreAdaBoostAlgorithm(
      list(algorithm = "SAMME.R"),
      supportsSammeR = FALSE,
      acceptsAlgorithm = FALSE
    ),
    "SAMME.R"
  )
})

test_that("restoreAdaBoostAlgorithm leaves models without a saved algorithm alone", {
  restored <- PatientLevelPrediction:::restoreAdaBoostAlgorithm(
    list(n_estimators = 10L),
    supportsSammeR = FALSE,
    acceptsAlgorithm = FALSE
  )
  expect_null(restored$algorithm)
  expect_equal(restored$n_estimators, 10L)
})

test_that("AdaBoostClassifierInputs asks for SAMME only where SAMME.R is the default", {
  captured <- NULL
  fakeClassifier <- function(...) {
    captured <<- list(...)
    "model"
  }
  param <- list(nEstimators = 10L, learningRate = 0.5, seed = 1L)

  local_mocked_bindings(adaBoostDefaultsToSammeR = function(classifier) TRUE)
  PatientLevelPrediction:::AdaBoostClassifierInputs(fakeClassifier, param)
  expect_equal(captured$algorithm, "SAMME")
  expect_equal(captured$n_estimators, 10L)
  expect_equal(captured$learning_rate, 0.5)
  expect_equal(captured$random_state, 1L)

  # scikit-learn 1.6 and later only implement SAMME and deprecate the argument
  local_mocked_bindings(adaBoostDefaultsToSammeR = function(classifier) FALSE)
  PatientLevelPrediction:::AdaBoostClassifierInputs(fakeClassifier, param)
  expect_false("algorithm" %in% names(captured))
})

test_that("AdaBoost models are fitted with SAMME on the installed scikit-learn", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  sklearn <- reticulate::import("sklearn", convert = FALSE)
  model <- PatientLevelPrediction:::AdaBoostClassifierInputs(
    sklearn$ensemble$AdaBoostClassifier,
    list(nEstimators = 5L, learningRate = 1, seed = 1L)
  )
  algorithm <- reticulate::py_to_r(model$get_params())$algorithm
  # NULL where the argument was removed; "deprecated" where it defaults to SAMME
  expect_false(identical(algorithm, "SAMME.R"))
})
