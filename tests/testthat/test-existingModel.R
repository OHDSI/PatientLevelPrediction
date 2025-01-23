test_that("Create existing sklearn works", {
  expect_error(createSciKitLearnModel("existing"))
  # create a file model.pkl for testing
  file.create("model.pkl")
  covariateSettings <-
    FeatureExtraction::createCovariateSettings(useDemographicsAge = TRUE)
  populationSettings <- createStudyPopulationSettings()
  # dataframe wrong type
  expect_error(createSciKitLearnModel(
    modelLocation = "model.pkl",
    covariateMap = list(
      columnId = "columnId",
      modelCovariateIdName = "modelCovariateIdName"
    ),
    covariateSettings = covariateSettings,
    populationSettings = populationSettings
  ))
  # dataframe wrong column names
  expect_error(createSciKitLearnModel(
    modelLocation = "model.pkl",
    covariateMap = data.frame(
      columnId = "columnId",
      modelCovariateIdName = "modelCovariateIdName"
    ),
    covariateSettings = covariateSettings,
    populationSettings = populationSettings
  ))
  # dataframe wrong column types
  expect_error(createSciKitLearnModel(
    modelLocation = "model.pkl",
    covariateMap = data.frame(
      columnId = 1,
      modelCovariateIdName = 2
    ),
    covariateSettings = covariateSettings,
    populationSettings = populationSettings
  ))

  model <- createSciKitLearnModel(
    modelLocation = "model.pkl",
    covariateMap = data.frame(
      columnId = c(1, 2),
      covariateId = c(1002, 1003),
      modelCovariateIdName = c("feature1", "feature2")
    ),
    covariateSettings = covariateSettings,
    populationSettings = populationSettings
  )
  expect_equal(attr(model, "modelType"), "binary")
  expect_equal(attr(model, "saveType"), "file")
  expect_equal(attr(model, "predictionFunction"), "predictPythonSklearn")
  expect_equal(attr(model, "saveToJson"), FALSE)
  expect_equal(class(model), "plpModel")
  expect_equal(model$preprocessing$featureEngineering$funct, "mapColumns")
  expect_equal(model$preprocessing$featureEngineering$settings$featureEngineeringSettings$columnMap$columnId, c(1, 2))
  expect_equal(model$preprocessing$featureEngineering$settings$featureEngineeringSettings$columnMap$modelCovariateIdName, c("feature1", "feature2"))
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

  # extract covariatMap from plpModel
  covariateMap <- plpModel$covariateImportance %>% dplyr::select(columnId, covariateId)
  covariateMap$modelCovariateIdName <- as.character(covariateMap$covariateId)
  
  existingModel <- createSciKitLearnModel(
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
