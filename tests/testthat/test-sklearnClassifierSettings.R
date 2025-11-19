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

  expect_false(adset$settings$requiresDenseMatrix)
  expect_equal(adset$settings$modelName, "adaboost")
  expect_equal(adset$settings$pythonModule, "sklearn.ensemble")
  expect_equal(adset$settings$pythonClass, "AdaBoostClassifier")
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

  expect_false(mlpset$settings$requiresDenseMatrix)
  expect_equal(mlpset$settings$modelName, "mlp")
  expect_equal(mlpset$settings$pythonModule, "sklearn.neural_network")
  expect_equal(mlpset$settings$pythonClass, "MLPClassifier")
})


test_that("setNaiveBayes settings work checks", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  nbset <- setNaiveBayes()

  expect_true(nbset$settings$requiresDenseMatrix)
  expect_equal(nbset$settings$modelName, "naiveBayes")
  expect_equal(nbset$settings$pythonModule, "sklearn.naive_bayes")
  expect_equal(nbset$settings$pythonClass, "GaussianNB")
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
  expect_false(rfset$settings$requiresDenseMatrix)
  expect_equal(rfset$settings$modelName, "randomForest")
  expect_equal(rfset$settings$pythonModule, "sklearn.ensemble")
  expect_equal(rfset$settings$pythonClass, "RandomForestClassifier")
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
  expect_false(svmset$settings$requiresDenseMatrix)
  expect_equal(svmset$settings$modelName, "svm")
  expect_equal(svmset$settings$pythonModule, "sklearn.svm")
  expect_equal(svmset$settings$pythonClass, "SVC")
})
