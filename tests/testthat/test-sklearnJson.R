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

# rewrites the node values of a saved tree model the way older scikit-learn stored them
toLegacyCounts <- function(path) {
  helper <- reticulate::py_run_string("
def toCounts(path):
    import json
    with open(path) as f:
        model = json.load(f)
    def scale(treeDict):
        for i, node in enumerate(treeDict['values']):
            weight = treeDict['nodes'][i][6]
            treeDict['values'][i] = [[x * weight for x in row] for row in node]
    if 'tree_' in model:
        scale(model['tree_'])
    else:
        for estimator in model['estimators_']:
            scale(estimator['tree_'])
    with open(path, 'w') as f:
        json.dump(model, f)
", local = TRUE, convert = TRUE)
  helper$toCounts(path)
}

test_that("Decision trees saved with class counts load as probabilities", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  model <- sklearn$tree$DecisionTreeClassifier(max_depth = 3L, random_state = 1L)$fit(X, y)
  predictions <- reticulate::py_to_r(model$predict_proba(xUnseen))
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  sklearnToJson(model, path)
  toLegacyCounts(path)

  loaded <- sklearnFromJson(path)
  expect_equal(reticulate::py_to_r(loaded$predict_proba(xUnseen)), predictions)
})

test_that("Random forests saved with class counts load as probabilities", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  model <- sklearn$ensemble$RandomForestClassifier(
    n_estimators = 5L, max_depth = 3L, random_state = 1L
  )$fit(X, y)
  predictions <- reticulate::py_to_r(model$predict_proba(xUnseen))
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  sklearnToJson(model, path)
  toLegacyCounts(path)

  loaded <- sklearnFromJson(path)
  expect_equal(reticulate::py_to_r(loaded$predict_proba(xUnseen)), predictions)
})

test_that("Current tree JSON is unchanged by the legacy count handling", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  model <- sklearn$tree$DecisionTreeClassifier(max_depth = 2L, random_state = 1L)$fit(X, y)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  sklearnToJson(model, path)
  loaded <- sklearnFromJson(path)
  expect_equal(
    reticulate::py_to_r(loaded$tree_$value),
    reticulate::py_to_r(model$tree_$value)
  )
})

# edits the saved params of an AdaBoost model so that algorithm states can be exercised
setSavedAdaBoostAlgorithm <- function(path, algorithm) {
  helper <- reticulate::py_run_string("
def setAlgorithm(path, algorithm):
    import json
    with open(path) as f:
        model = json.load(f)
    model['params']['algorithm'] = algorithm
    with open(path, 'w') as f:
        json.dump(model, f)
", local = TRUE, convert = TRUE)
  helper$setAlgorithm(path, algorithm)
}

test_that("AdaBoost models saved with SAMME keep their predictions", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  params <- list(n_estimators = 10L, random_state = 1L)
  if ("algorithm" %in% names(reticulate::py_to_r(sklearn$ensemble$AdaBoostClassifier()$get_params()))) {
    params$algorithm <- "SAMME"
  }
  model <- do.call(sklearn$ensemble$AdaBoostClassifier, params)$fit(X, y)
  predictions <- reticulate::py_to_r(model$predict_proba(xUnseen))
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  sklearnToJson(model, path)
  setSavedAdaBoostAlgorithm(path, "SAMME")

  loaded <- sklearnFromJson(path)
  expect_equal(reticulate::py_to_r(loaded$predict_proba(xUnseen)), predictions)
})

test_that("AdaBoost models saved with SAMME.R are rejected when unsupported", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  skip_if(reticulate::py_has_attr(sklearn$ensemble$AdaBoostClassifier, "_boost_real"))
  model <- sklearn$ensemble$AdaBoostClassifier(n_estimators = 5L, random_state = 1L)$fit(X, y)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  sklearnToJson(model, path)
  setSavedAdaBoostAlgorithm(path, "SAMME.R")

  expect_error(sklearnFromJson(path), "SAMME.R")
})
