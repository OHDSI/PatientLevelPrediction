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

  expect_true(reticulate::py_has_attr(loadedModel$estimators_[0], "n_features_in_"))
  expect_no_error(sklearnToJson(loadedModel, path))

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

  expect_true(reticulate::py_has_attr(loadedModel$estimators_[0], "n_features_in_"))
  expect_no_error(sklearnToJson(loadedModel, path))

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

for (inputFormat in c("dense", "sparse")) {
  test_that(paste(inputFormat, "SVM predictions survive current and legacy JSON round trips"), {
    skip_if_not_installed("reticulate")
    skip_on_cran()
    classifier <- sklearn$svm$SVC(probability = TRUE, random_state = 42L)
    xTrain <- X
    if (inputFormat == "sparse") {
      sp <- reticulate::import("scipy.sparse", convert = FALSE)
      xTrain <- sp$csr_matrix(X)
    }

    model <- classifier$fit(xTrain, y)
    predictions <- reticulate::py_to_r(model$predict_proba(xUnseen))
    decisions <- reticulate::py_to_r(model$decision_function(xUnseen))
    # Uninformative data can produce constant probabilities that hide coefficient errors.
    expect_gt(diff(range(predictions[, 2])), 0.5)
    path <- tempfile(fileext = ".json")
    sklearnToJson(model, path)

    py <- reticulate::import_builtins(convert = FALSE)
    json <- reticulate::import("json", convert = FALSE)
    with(py$open(path, "r"), as = file, {
      legacyModelDict <- json$load(fp = file)
    })
    for (key in c(
      "n_features_in_",
      "_effective_probability",
      "fit_status_",
      "_num_iter",
      "n_iter_"
    )) {
      if (reticulate::py_bool(legacyModelDict$`__contains__`(key))) {
        invisible(legacyModelDict$pop(key))
      }
    }
    legacyPath <- tempfile(fileext = ".json")
    with(py$open(legacyPath, "w"), as = file, {
      json$dump(legacyModelDict, fp = file)
    })

    for (modelPath in c(path, legacyPath)) {
      loadedModel <- sklearnFromJson(modelPath)
      expect_true(reticulate::py_has_attr(loadedModel, "_effective_probability"))
      expect_equal(reticulate::py_to_r(loadedModel$n_features_in_), ncol(reticulate::py_to_r(X)))
      expect_equal(reticulate::py_to_r(loadedModel$predict_proba(xUnseen)), predictions)
      expect_equal(reticulate::py_to_r(loadedModel$decision_function(xUnseen)), decisions)
    }
  })
}

test_that("CSR serialization preserves trailing zero columns and all-zero matrices", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  sp <- reticulate::import("scipy.sparse", convert = FALSE)
  json <- reticulate::import("json", convert = FALSE)

  for (values in list(cbind(diag(2), 0), matrix(0, nrow = 2, ncol = 3))) {
    csrMatrix <- sp$csr_matrix(values)
    serializedMatrix <- json$loads(json$dumps(serializeCsrMatrix(csrMatrix)))
    loadedMatrix <- deSerializeCsrMatrix(serializedMatrix)
    expect_equal(reticulate::py_to_r(loadedMatrix$toarray()), values)
  }
})
