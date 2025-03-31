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
test_that("Create existing sklearn works", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  expect_error(createSklearnModel("existing"))
  # create a file model.pkl for testing
  file.create("model.pkl")
  covariateSettings <-
    FeatureExtraction::createCovariateSettings(useDemographicsAge = TRUE)
  populationSettings <- createStudyPopulationSettings()
  # dataframe wrong type
  expect_error(createSklearnModel(
    modelLocation = "model.pkl",
    covariateMap = list(
      columnId = "columnId",
      covariateId = c(1)
    ),
    covariateSettings = covariateSettings,
    populationSettings = populationSettings
  ))
  # dataframe wrong column names
  expect_error(createSklearnModel(
    modelLocation = "model.pkl",
    covariateMap = data.frame(
      columnId = c(1),
      notCovariateId = c(1002),
    ),
    covariateSettings = covariateSettings,
    populationSettings = populationSettings
  ))
  # dataframe wrong column types
  expect_error(createSklearnModel(
    modelLocation = "model.pkl",
    covariateMap = data.frame(
      columnId = 1,
      covariateId = "2"
    ),
    covariateSettings = covariateSettings,
    populationSettings = populationSettings
  ))

  model <- createSklearnModel(
    modelLocation = "model.pkl",
    covariateMap = data.frame(
      columnId = c(1, 2),
      covariateId = c(1002, 1003)
    ),
    covariateSettings = covariateSettings,
    populationSettings = populationSettings
  )
  expect_equal(attr(model, "modelType"), "binary")
  expect_equal(attr(model, "saveType"), "file")
  expect_equal(attr(model, "predictionFunction"), "predictPythonSklearn")
  expect_equal(attr(model, "saveToJson"), FALSE)
  expect_equal(class(model), "plpModel")
  unlink("model.pkl")
})

test_that("existing sklearn model works", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  skip_if_offline()
  # fit a simple sklearn model with plp
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

  # load model json and save as pickle with joblib
  model <- sklearnFromJson(file.path(plpModel$model, "model.json"))
  joblib <- reticulate::import("joblib")
  joblib$dump(model, file.path(plpModel$model, "model.pkl"))

  # extract covariateMap from plpModel
  covariateMap <- plpModel$covariateImportance %>% dplyr::select(columnId, covariateId)

  existingModel <- createSklearnModel(
    modelLocation = file.path(plpModel$model),
    covariateMap = covariateMap,
    covariateSettings = plpModel$modelDesign$covariateSettings,
    populationSettings = plpModel$modelDesign$populationSettings
  )

  prediction <- predictPlp(plpModel, testData, testData$labels)
  predictionNew <- predictPlp(existingModel, testData, testData$labels)

  expect_correct_predictions(prediction, testData)
  expect_equal(prediction$value, predictionNew$value)
})

test_that("Externally trained sklearn model works", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  skip_if_offline()
  # change map to be some random order
  covariateIds <- tinyTrainData$covariateData$covariates %>%
    dplyr::pull(.data$covariateId) %>%
    unique()
  map <- data.frame(
    columnId = sample(1:20, length(covariateIds)),
    covariateId = sample(covariateIds, length(covariateIds))
  )
  matrixData <- toSparseM(tinyTrainData, map = map)
  matrix <- matrixData$dataMatrix %>%
    Matrix::as.matrix()

  # fit with sklearn
  xMatrix <- reticulate::r_to_py(matrix)
  y <- reticulate::r_to_py(tinyTrainData$labels$outcomeCount)

  sklearn <- reticulate::import("sklearn")
  classifier <- sklearn$tree$DecisionTreeClassifier()
  classifier <- classifier$fit(xMatrix, y)

  testMatrix <- toSparseM(testData, map = matrixData$covariateMap)
  xTest <- reticulate::r_to_py(testMatrix$dataMatrix %>% Matrix::as.matrix())
  yTest <- reticulate::r_to_py(testData$labels$outcomeCount)
  externalPredictions <- classifier$predict_proba(xTest)[, 2]
  auc <- sklearn$metrics$roc_auc_score(yTest, externalPredictions)

  joblib <- reticulate::import("joblib")
  path <- tempfile()
  createDir(path)
  joblib$dump(classifier, file.path(path, "model.pkl"))
  plpModel <- createSklearnModel(
    model = path,
    covariateMap = matrixData$covariateMap,
    covariateSettings = FeatureExtraction::createCovariateSettings(
      useDemographicsAge = TRUE
    ),
    populationSettings = populationSettings
  )
  prediction <- predictPlp(plpModel, testData, testData$labels)

  expect_equal(mean(prediction$value), mean(externalPredictions))
  expect_correct_predictions(prediction, testData)
})

test_that("Create existing GLM model works", {
  expect_error(createGlmModel(coefficients = data.frame(
    weights = c(1, 2),
    covariateId = c(1, 2)
  )))
  expect_error(createGlmModel(coefficients = data.frame(
    coefficient = c("1", "2"),
    covariateId = c(1, 2)
  )))
  expect_error(createGlmModel(coefficients = data.frame(
    coefficient = c(1, 2),
    covariateId = c("1", "2")
  )))
  expect_error(createGlmModel(coefficients = data.frame(
    coefficient = c(1, 2),
    covariateId = c(1, 2)
  ), intercept = "2"))
  expect_error(createGlmModel(coefficients = data.frame(
    coefficient = c(1, 2),
    covariateId = c(1, 2)
  ), mapping = 1))
  
  
  expect_error(createGlmModel(coefficients = data.frame(
    coefficient = c(1, 2),
    covariateId = c(1, 2)
  ), mapping = "logistic", targetId = "char"))
  expect_error(createGlmModel(coefficients = data.frame(
    coefficient = c(1, 2),
    covariateId = c(1, 2)
  ), mapping = "logistic", outcomeId = "char"))
  expect_error(createGlmModel(coefficients = data.frame(
    coefficient = c(1, 2),
    covariateId = c(1, 2)
  ), mapping = "logistic", populationSettings = "char"))
  expect_error(createGlmModel(coefficients = data.frame(
    coefficient = c(1, 2),
    covariateId = c(1, 2)
  ), mapping = "logistic", restrictPlpDataSettings = "char"))
  expect_error(createGlmModel(coefficients = data.frame(
    coefficient = c(1, 2),
    covariateId = c(1, 2)
  ), mapping = "logistic", covariateSettings = "char"))
  expect_error(createGlmModel(coefficients = data.frame(
    coefficient = c(1, 2),
    covariateId = c(1, 2)
  ), mapping = "logistic", requireDenseMatrix = 4))
  
  model <- createGlmModel(
    coefficients = data.frame(
      coefficient = c(1, 2),
      covariateId = c(1, 2)
    ),
    intercept = 2,
    mapping = "logistic"
  )
  expect_equal(model$model$intercept, 2)
  expect_equal(model$model$mapping, "logistic")
  expect_equal(attr(model, "modelType"), "binary")
  expect_equal(attr(model, "saveType"), "RtoJson")
  expect_equal(attr(model, "predictionFunction"), "PatientLevelPrediction::predictGlm")
  
  model <- createGlmModel(
    coefficients = data.frame(
      coefficient = c(1, 2),
      covariateId = c(1, 2)
    ),
    intercept = 2,
    mapping = "logistic", 
    targetId = 33, 
    outcomeId = 1, 
    covariateSettings = FeatureExtraction::createCovariateSettings(useDemographicsAge = TRUE), 
    populationSettings = createStudyPopulationSettings(), 
    restrictPlpDataSettings = createRestrictPlpDataSettings(),
    requireDenseMatrix = TRUE
  )
  expect_equal(model$modelDesign$targetId, 33)
  expect_equal(model$modelDesign$outcomeId, 1)
  expect_equal(model$preprocessing$requireDenseMatrix, TRUE)
  expect_equal(model$modelDesign$populationSettings, createStudyPopulationSettings())
  expect_equal(model$modelDesign$restrictPlpDataSettings, createRestrictPlpDataSettings())
  expect_equal(model$modelDesign$covariateSettings, FeatureExtraction::createCovariateSettings(useDemographicsAge = TRUE))
  
  madeupFunc <- function(x){return(x)}
  model <- createGlmModel(
    coefficients = data.frame(
      coefficient = c(1, 2),
      covariateId = c(1, 2)
    ),
    intercept = 2,
    mapping = "madeupFunc"
  )
  expect_equal(model$model$mapping, "madeupFunc")
  expect_equal(attr(model, "modelType"), "binary")
  expect_equal(attr(model, "saveType"), "RtoJson")
  expect_equal(attr(model, "predictionFunction"), "PatientLevelPrediction::predictGlm")
  
  model <- createGlmModel(
    coefficients = data.frame(
      coefficient = c(1, 2),
      covariateId = c(1, 2)
    ),
    intercept = 2,
    mapping = madeupFunc
  )
  expect_equal(model$model$mapping, madeupFunc)
  expect_equal(attr(model, "modelType"), "binary")
  expect_equal(attr(model, "saveType"), "RtoJson")
  expect_equal(attr(model, "predictionFunction"), "PatientLevelPrediction::predictGlm")
})

test_that("Existing glm model works", {
  model <- createGlmModel(coefficients = data.frame(
    coefficient = c(0.05),
    covariateId = c(1002)
  ), intercept = -2.5)
  prediction <- predictPlp(model, testData, testData$labels)
  expect_correct_predictions(prediction, testData)
})
