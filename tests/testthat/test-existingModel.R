test_that("Create existing sklearn works", {
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

  expect_correct_predictions(prediction, testData)
})
