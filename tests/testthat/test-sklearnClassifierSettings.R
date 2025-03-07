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

test_that("setAdaBoost settings work checks", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  adset <- setAdaBoost(
    nEstimators = list(10, 50, 200),
    learningRate = list(1, 0.5, 0.1),
    algorithm = list("SAMME"),
    seed = sample(1000000, 1)
  )

  expect_equal(adset$fitFunction, "fitSklearn")

  expect_equal(length(adset$param), 3 * 3 * 1)

  expect_equal(unique(unlist(lapply(adset$param, function(x) x[[1]]))), c(10, 50, 200))
  expect_equal(unique(unlist(lapply(adset$param, function(x) x[[2]]))), c(1, 0.5, 0.1))
  expect_equal(unique(lapply(adset$param, function(x) x[[3]])), list("SAMME"))

  expect_false(attr(adset$param, "settings")$requiresDenseMatrix)
  expect_equal(attr(adset$param, "settings")$name, "AdaBoost")
  expect_equal(attr(adset$param, "settings")$pythonModule, "sklearn.ensemble")
  expect_equal(attr(adset$param, "settings")$pythonClass, "AdaBoostClassifier")


  inputs <- AdaBoostClassifierInputs(list, adset$param[[1]])
  expect_equal(
    names(inputs),
    c("n_estimators", "learning_rate", "algorithm", "random_state")
  )
})


test_that("setAdaBoost errors as expected", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  expect_error(setAdaBoost(nEstimators = list(-1)))
  expect_error(setAdaBoost(learningRate = list(-1)))
  expect_error(setAdaBoost(algorithm = list(-1)))
  expect_error(setAdaBoost(seed = list("seed")))
})


test_that("setMLP settings work checks", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  mlpset <- setMLP(
    hiddenLayerSizes = list(c(100), c(20, 4)), # must be integers
    activation = list("relu"),
    solver = list("adam"),
    alpha = list(0.3, 0.01, 0.0001, 0.000001),
    batchSize = list("auto"),
    learningRate = list("constant"),
    learningRateInit = list(0.001),
    powerT = list(0.5),
    maxIter = list(200, 100),
    shuffle = list(TRUE),
    tol = list(0.0001),
    warmStart = list(TRUE),
    momentum = list(0.9),
    nesterovsMomentum = list(TRUE),
    earlyStopping = list(FALSE),
    validationFraction = list(0.1),
    beta1 = list(0.9),
    beta2 = list(0.999),
    epsilon = list(1, 0.1, 0.00000001),
    nIterNoChange = list(10),
    seed = sample(100000, 1)
  )

  expect_equal(mlpset$fitFunction, "fitSklearn")

  expect_equal(length(mlpset$param), 2 * 4 * 2 * 3)

  expect_equal(unique(lapply(mlpset$param, function(x) x[[1]])), list(c(100), c(20, 4)))
  expect_equal(unique(unlist(lapply(mlpset$param, function(x) x[[2]]))), "relu")
  expect_equal(unique(unlist(lapply(mlpset$param, function(x) x[[4]]))), c(0.3, 0.01, 0.0001, 0.000001))
  expect_equal(unique(lapply(mlpset$param, function(x) x[[9]])), list(200, 100))

  expect_false(attr(mlpset$param, "settings")$requiresDenseMatrix)
  expect_equal(attr(mlpset$param, "settings")$name, "Neural Network")
  expect_equal(attr(mlpset$param, "settings")$pythonModule, "sklearn.neural_network")
  expect_equal(attr(mlpset$param, "settings")$pythonClass, "MLPClassifier")

  inputs <- MLPClassifierInputs(list, mlpset$param[[1]])
  expect_equal(
    names(inputs),
    c(
      "hidden_layer_sizes", "activation", "solver", "alpha", "batch_size",
      "learning_rate", "learning_rate_init", "power_t", "max_iter", "shuffle",
      "random_state", "tol", "verbose", "warm_start", "momentum", "nesterovs_momentum",
      "early_stopping", "validation_fraction", "beta_1", "beta_2", "epsilon",
      "n_iter_no_change"
    )
  )
})


test_that("setNaiveBayes settings work checks", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  nbset <- setNaiveBayes()

  expect_equal(nbset$fitFunction, "fitSklearn")

  expect_equal(length(nbset$param), 1)

  expect_true(attr(nbset$param, "settings")$requiresDenseMatrix)
  expect_equal(attr(nbset$param, "settings")$name, "Naive Bayes")
  expect_equal(attr(nbset$param, "settings")$pythonModule, "sklearn.naive_bayes")
  expect_equal(attr(nbset$param, "settings")$pythonClass, "GaussianNB")

  inputs <- GaussianNBInputs(list, nbset$param[[1]])
  expect_equal(names(inputs), NULL)
})


test_that("setRandomForest settings work checks", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  rfset <- setRandomForest(
    ntrees = list(100, 500),
    criterion = list("gini"),
    maxDepth = list(4, 10, 17),
    minSamplesSplit = list(2, 5),
    minSamplesLeaf = list(1, 10),
    minWeightFractionLeaf = list(0),
    mtries = list("sqrt", "log2"),
    maxLeafNodes = list(NULL),
    minImpurityDecrease = list(0),
    bootstrap = list(TRUE),
    maxSamples = list(NULL, 0.9),
    oobScore = list(FALSE),
    nJobs = list(NULL),
    classWeight = list(NULL),
    seed = sample(100000, 1)
  )

  expect_equal(rfset$fitFunction, "fitSklearn")

  expect_equal(length(rfset$param), 2 * 3 * 2 * 2 * 2 * 2 * 1)

  expect_equal(unique(lapply(rfset$param, function(x) x[[1]])), list(100, 500))
  expect_equal(unique(unlist(lapply(rfset$param, function(x) x[[3]]))), c(4, 10, 17))

  expect_false(attr(rfset$param, "settings")$requiresDenseMatrix)
  expect_equal(attr(rfset$param, "settings")$name, "Random forest")
  expect_equal(attr(rfset$param, "settings")$pythonModule, "sklearn.ensemble")
  expect_equal(attr(rfset$param, "settings")$pythonClass, "RandomForestClassifier")

  inputs <- RandomForestClassifierInputs(list, rfset$param[[1]])
  expect_equal(
    names(inputs),
    c(
      "n_estimators", "criterion", "max_depth", "min_samples_split", "min_samples_leaf",
      "min_weight_fraction_leaf", "max_features", "max_leaf_nodes", "min_impurity_decrease",
      "bootstrap", "max_samples", "oob_score", "n_jobs", "random_state", "verbose",
      "warm_start", "class_weight"
    )
  )
})


test_that("setSVM  settings work checks", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  svmset <- setSVM(
    C = list(1, 0.9, 2, 0.1),
    kernel = list("rbf"),
    degree = list(1, 3, 5),
    gamma = list("scale", 1e-04, 3e-05, 0.001, 0.01, 0.25),
    coef0 = list(0.0),
    shrinking = list(TRUE),
    tol = list(0.001),
    classWeight = list(NULL),
    cacheSize = 500,
    seed = sample(100000, 1)
  )

  expect_equal(svmset$fitFunction, "fitSklearn")

  expect_equal(length(svmset$param), 4 * 3 * 6 * 1)

  expect_equal(unique(lapply(svmset$param, function(x) x[[4]])), list("scale", 1e-04, 3e-05, 0.001, 0.01, 0.25))
  expect_equal(unique(unlist(lapply(svmset$param, function(x) x[[1]]))), c(1, 0.9, 2, 0.1))

  expect_false(attr(svmset$param, "settings")$requiresDenseMatrix)
  expect_equal(attr(svmset$param, "settings")$name, "Support Vector Machine")
  expect_equal(attr(svmset$param, "settings")$pythonModule, "sklearn.svm")
  expect_equal(attr(svmset$param, "settings")$pythonClass, "SVC")

  inputs <- SVCInputs(list, svmset$param[[1]])
  expect_equal(
    names(inputs),
    c(
      "C", "kernel", "degree", "gamma", "coef0",
      "shrinking", "probability", "tol", "cache_size",
      "class_weight", "verbose", "max_iter", "decision_function_shape",
      "break_ties", "random_state"
    )
  )
})
