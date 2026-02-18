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

