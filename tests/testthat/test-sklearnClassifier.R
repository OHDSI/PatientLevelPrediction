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
  if (Sys.getenv("GITHUB_ACTIONS") == "true") {
    reticulate::py_require("scikit-learn", python_version = Sys.getenv("RETICULATE_PY_VERSION"))
    # force instantiation of python with correct version
    sklearn <- reticulate::import("sklearn")
  } else {
    reticulate::py_require("scikit-learn")
  }
}
test_that("DecisionTree settings work checks", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  dtset <- setDecisionTree(
    criterion = list("gini"),
    splitter = list("best"),
    maxDepth = list(4, 10, NULL),
    minSamplesSplit = list(2, 10),
    minSamplesLeaf = list(10, 50),
    minWeightFractionLeaf = list(0),
    maxFeatures = list(100, "sqrt", NULL),
    maxLeafNodes = list(NULL),
    minImpurityDecrease = list(10^-7),
    classWeight = list(NULL),
    seed = sample(1000000, 1)
  )

  expect_equal(dtset$fitFunction, "fitSklearn")

  expect_equal(length(dtset$param), 3 * 2 * 2 * 3 * 1)

  expect_equal(unique(unlist(lapply(dtset$param, function(x) x[[1]]))), "gini")
  expect_equal(unique(unlist(lapply(dtset$param, function(x) x[[2]]))), "best")
  expect_equal(length(unique(lapply(dtset$param, function(x) x[[3]]))), 3)

  expect_false(attr(dtset$param, "settings")$requiresDenseMatrix)
  expect_equal(attr(dtset$param, "settings")$name, "Decision Tree")
  expect_equal(attr(dtset$param, "settings")$pythonModule, "sklearn.tree")
  expect_equal(attr(dtset$param, "settings")$pythonClass, "DecisionTreeClassifier")
})


test_that("DecisionTree errors as expected", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  expect_error(setDecisionTree(criterion = list("madeup")))

  expect_error(setDecisionTree(maxDepth = list(-1)))
  expect_error(setDecisionTree(minSamplesSplit = list(-1)))
  expect_error(setDecisionTree(minSamplesLeaf = list(-1)))
})


test_that("check fit of DecisionTree", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  modelSettings <- setDecisionTree(
    criterion = list("gini"),
    splitter = list("best"),
    maxDepth = list(as.integer(4)),
    minSamplesSplit = list(2),
    minSamplesLeaf = list(10),
    minWeightFractionLeaf = list(0),
    maxFeatures = list("sqrt"),
    maxLeafNodes = list(NULL),
    minImpurityDecrease = list(10^-7),
    classWeight = list(NULL),
    seed = sample(1000000, 1)
  )

  plpModel <- fitPlp(
    trainData = tinyTrainData,
    modelSettings = modelSettings,
    analysisId = "DecisionTree",
    analysisPath = tempdir()
  )

  predictions <- predictPlp(plpModel, tinyTrainData, tinyTrainData$labels)
  trainPredictions <- plpModel$prediction %>% 
    dplyr::filter(.data$evaluationType == "Train") %>%
    dplyr::pull(.data$value)
  expect_equal(mean(predictions$value), mean(trainPredictions))
  expect_correct_fitPlp(plpModel, trainData)
  # add check for other model design settings
  # test with one feature
  oneModel <- fitPlp(
    trainData = oneTrainData,
    modelSettings = modelSettings,
    analysisId = "DecisionTreeOne",
    analysisPath = tempdir()
  )
  onePredictions <- predictPlp(oneModel, oneTrainData, oneTrainData$labels)
  oneTrainPredictions <- oneModel$prediction %>% 
    dplyr::filter(.data$evaluationType == "Train") %>%
    dplyr::pull(.data$value)
  expect_equal(mean(onePredictions$value), mean(oneTrainPredictions))
  expect_correct_fitPlp(oneModel, oneTrainData)

})

test_that("fitSklearn errors with wrong covariateData", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  newTrainData <- copyTrainData(trainData)
  class(newTrainData$covariateData) <- "notCovariateData"
  modelSettings <- setAdaBoost()
  analysisId <- 42

  expect_error(fitSklearn(newTrainData,
    modelSettings,
    search = "grid",
    analysisId
  ))
})


test_that("AdaBoost fit works", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  modelSettings <- setAdaBoost(
    nEstimators = list(10),
    learningRate = list(0.1),
  )

  plpModel <- fitPlp(
    trainData = tinyTrainData,
    modelSettings = modelSettings,
    analysisId = "Adaboost",
    analysisPath = tempdir()
  )

  expect_correct_fitPlp(plpModel, trainData)
  expect_equal(dir(plpModel$model), "model.json")

  oneModel <- fitPlp(
    trainData = oneTrainData,
    modelSettings = modelSettings,
    analysisId = "AdaBoostOne",
    analysisPath = tempdir()
  )
  onePredictions <- predictPlp(oneModel, oneTrainData, oneTrainData$labels)
  oneTrainPredictions <- oneModel$prediction %>% 
    dplyr::filter(.data$evaluationType == "Train") %>%
    dplyr::pull(.data$value)
  expect_equal(mean(onePredictions$value), mean(oneTrainPredictions))
  expect_correct_fitPlp(oneModel, oneTrainData)
})


test_that("RandomForest fit works", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  modelSettings <- setRandomForest(
    ntrees = list(10),
    maxDepth = list(4),
    minSamplesSplit = list(2),
    minSamplesLeaf = list(10),
    mtries = list("sqrt"),
    maxSamples = list(0.9),
    classWeight = list(NULL)
  )

  plpModel <- fitPlp(
    trainData = tinyTrainData,
    modelSettings = modelSettings,
    analysisId = "RandomForest",
    analysisPath = tempdir()
  )

  expect_correct_fitPlp(plpModel, trainData)
  expect_equal(dir(plpModel$model), "model.json")

  oneModel <- fitPlp(
    trainData = oneTrainData,
    modelSettings = modelSettings,
    analysisId = "RFOne",
    analysisPath = tempdir()
  )
  onePredictions <- predictPlp(oneModel, oneTrainData, oneTrainData$labels)
  oneTrainPredictions <- oneModel$prediction %>% 
    dplyr::filter(.data$evaluationType == "Train") %>%
    dplyr::pull(.data$value)
  expect_equal(mean(onePredictions$value), mean(oneTrainPredictions))
  expect_correct_fitPlp(oneModel, oneTrainData)
})


test_that("MLP fit works", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  modelSettings <- setMLP(
    hiddenLayerSizes = list(c(20)),
    alpha = list(1e-6),
    maxIter = list(50),
    epsilon = list(1e-08),
    learningRateInit = list(0.01),
    tol = list(1e-2) # reduce tol so I don't get convergence warnings
  )

  plpModel <- fitPlp(
    trainData = tinyTrainData,
    modelSettings = modelSettings,
    analysisId = "MLP",
    analysisPath = tempdir()
  )

  expect_correct_fitPlp(plpModel, trainData)
  expect_equal(dir(plpModel$model), "model.json")
  
  oneModel <- fitPlp(
    trainData = oneTrainData,
    modelSettings = modelSettings,
    analysisId = "MLPOne",
    analysisPath = tempdir()
  )
  onePredictions <- predictPlp(oneModel, oneTrainData, oneTrainData$labels)
  oneTrainPredictions <- oneModel$prediction %>% 
    dplyr::filter(.data$evaluationType == "Train") %>%
    dplyr::pull(.data$value)
  expect_equal(mean(onePredictions$value), mean(oneTrainPredictions))
  expect_correct_fitPlp(oneModel, oneTrainData)
})


test_that("Naive bayes fit works", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  modelSettings <- setNaiveBayes()

  plpModel <- fitPlp(
    trainData = tinyTrainData,
    modelSettings = modelSettings,
    analysisId = "Naive bayes",
    analysisPath = tempdir()
  )

  expect_correct_fitPlp(plpModel, trainData)
  expect_equal(dir(plpModel$model), "model.json")
  
  oneModel <- fitPlp(
    trainData = oneTrainData,
    modelSettings = modelSettings,
    analysisId = "NaiveBayesOne",
    analysisPath = tempdir()
  )
  onePredictions <- predictPlp(oneModel, oneTrainData, oneTrainData$labels)
  oneTrainPredictions <- oneModel$prediction %>% 
    dplyr::filter(.data$evaluationType == "Train") %>%
    dplyr::pull(.data$value)
  expect_equal(mean(onePredictions$value), mean(oneTrainPredictions))
  expect_correct_fitPlp(oneModel, oneTrainData)
})


test_that("Support vector machine fit works", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  modelSettings <- setSVM(
    C = list(1),
    degree = list(1),
    gamma = list("scale"),
    classWeight = list(NULL)
  )

  plpModel <- fitPlp(
    trainData = tinyTrainData,
    modelSettings = modelSettings,
    analysisId = "SVM",
    analysisPath = tempdir()
  )

  expect_correct_fitPlp(plpModel, trainData)
  expect_equal(dir(plpModel$model), "model.json")
})

test_that("Sklearn predict works", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  modelSettings <- setAdaBoost(
    nEstimators = list(10),
    learningRate = list(0.1),
  )

  plpModel <- fitPlp(
    trainData = tinyTrainData,
    modelSettings = modelSettings,
    analysisId = "Adaboost",
    analysisPath = tempdir()
  )

  predictions <- predictPythonSklearn(
    plpModel,
    testData,
    population
  )
  expect_correct_predictions(predictions, testData)
})
