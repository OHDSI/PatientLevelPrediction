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

if (rlang::is_installed("reticulate") && identical(Sys.getenv("NOT_CRAN"), "true")) {
  reticulate::py_require("scikit-learn")
  sklearn <- reticulate::import("sklearn", convert = FALSE)
  np <- reticulate::import("numpy", convert = FALSE)

  data <- sklearn$datasets$make_classification(
    n_samples = 500L, n_features = 3L,
    n_classes = 2L, n_informative = 3L,
    n_redundant = 0L, random_state = 0L,
    shuffle = FALSE
  )

  xUnseen <- sklearn$datasets$make_classification(
    n_samples = 100L, n_features = 3L,
    n_classes = 2L, n_informative = 3L,
    n_redundant = 0L, random_state = 42L,
    shuffle = FALSE
  )[[0]]
  X <- data[[0]]
  y <- data[[1]]
}
test_that("Decision tree to json is correct", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  classifier <- sklearn$tree$DecisionTreeClassifier(max_depth = 3L)

  model <- classifier$fit(X, y)
  predictions <- reticulate::py_to_r(model$predict_proba(xUnseen))
  path <- file.path(tempdir(), "model.json")

  sklearnToJson(model, path)

  loadedModel <- sklearnFromJson(path)

  loadedPredictions <- reticulate::py_to_r(loadedModel$predict_proba(xUnseen))

  expect_true(all.equal(predictions, loadedPredictions))
})

test_that("Random forest to json is correct", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  classifier <- sklearn$ensemble$RandomForestClassifier(n_estimators = 10L)

  model <- classifier$fit(X, y)
  predictions <- reticulate::py_to_r(model$predict_proba(xUnseen))
  path <- file.path(tempdir(), "model.json")

  sklearnToJson(model, path)

  loadedModel <- sklearnFromJson(path)

  loadedPredictions <- reticulate::py_to_r(loadedModel$predict_proba(xUnseen))

  expect_true(all.equal(predictions, loadedPredictions))
})

test_that("Adaboost to json is correct", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  classifier <- sklearn$ensemble$AdaBoostClassifier(n_estimators = 10L)

  model <- classifier$fit(X, y)
  predictions <- reticulate::py_to_r(model$predict_proba(xUnseen))
  path <- file.path(tempdir(), "model.json")

  sklearnToJson(model, path)

  loadedModel <- sklearnFromJson(path)

  loadedPredictions <- reticulate::py_to_r(loadedModel$predict_proba(xUnseen))

  expect_true(all.equal(predictions, loadedPredictions))
})

test_that("Naive Bayes to json is correct", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  classifier <- sklearn$naive_bayes$GaussianNB()

  model <- classifier$fit(X, y)
  predictions <- reticulate::py_to_r(model$predict_proba(xUnseen))
  path <- file.path(tempdir(), "model.json")

  sklearnToJson(model, path)

  loadedModel <- sklearnFromJson(path)

  loadedPredictions <- reticulate::py_to_r(loadedModel$predict_proba(xUnseen))

  expect_true(all.equal(predictions, loadedPredictions))
})

test_that("MLP to json is correct", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  # lower tolerance to not get convergence warning
  classifier <- sklearn$neural_network$MLPClassifier(tol = 1e-2)

  model <- classifier$fit(X, y)
  predictions <- reticulate::py_to_r(model$predict_proba(xUnseen))
  path <- file.path(tempdir(), "model.json")

  sklearnToJson(model, path)

  loadedModel <- sklearnFromJson(path)

  loadedPredictions <- reticulate::py_to_r(loadedModel$predict_proba(xUnseen))

  expect_true(all.equal(predictions, loadedPredictions))
})

test_that("SVM to json is correct", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  classifier <- sklearn$svm$SVC(probability = TRUE)

  # create sparse data because then some of the internal fields in the
  # SVM will be sparse
  featureHasher <- sklearn$feature_extraction$FeatureHasher(n_features = 3L)
  random <- reticulate::import("random", convert = FALSE)
  features <- list()
  ySparse <- np$empty(100L)
  for (i in 1:100) {
    row <- reticulate::dict(
      a = random$randint(0, 2),
      b = random$randint(3, 5),
      c = random$randint(6, 8)
    )
    features <- c(features, row)
    reticulate::py_set_item(ySparse, i - 1L, random$randint(0, 2))
  }
  xSparse <- featureHasher$transform(features)

  model <- classifier$fit(xSparse, ySparse)
  predictions <- reticulate::py_to_r(model$predict_proba(xUnseen))
  path <- file.path(tempdir(), "model.json")

  sklearnToJson(model, path)

  loadedModel <- sklearnFromJson(path)

  loadedPredictions <- reticulate::py_to_r(loadedModel$predict_proba(xUnseen))

  expect_true(all.equal(predictions, loadedPredictions))
})
