# add a test numerical feature with missing values of certain percentage
createMissingData <- function(trainData, missingness, test = FALSE) {
  missingData <- list(
    labels = trainData$labels
  )
  if (!test) {
    missingData$folds <- trainData$folds
  }
  missingData$covariateData <- Andromeda::copyAndromeda(trainData$covariateData)
  rowIds <- missingData$labels$rowId
  nData <- floor(length(rowIds) * (1 - missingness))
  covariateId <- rep(666, nData)
  withr::with_seed(
    1234,
    covariateValue <- runif(n = nData)
  )
  Andromeda::appendToTable(
    missingData$covariateData$covariates,
    data.frame(
      rowId = rowIds[1:nData],
      covariateId = covariateId,
      covariateValue = covariateValue
    )
  )
  Andromeda::appendToTable(
    (missingData$covariateData$covariateRef),
    data.frame(
      covariateId = 666,
      covariateName = "fakeMissingVariable",
      analysisId = 666,
      conceptId = 666
    )
  )
  Andromeda::appendToTable(
    missingData$covariateData$analysisRef,
    data.frame(
      analysisId = 666,
      analysisName = "missing",
      domainId = "missing",
      startDay = NA,
      endDay = NA,
      isBinary = "N",
      missingMeansZero = "N"
    )
  )
  missingData
}

test_that("createSimpleImputer works", {
  imputer <- createSimpleImputer()

  expect_equal(imputer$method, "mean")
  expect_equal(imputer$missingThreshold, 0.3)
  expect_false(imputer$addMissingIndicator)
  expect_equal(attr(imputer, "fun"), "simpleImpute")
  expect_s3_class(imputer, "featureEngineeringSettings")

  imputer <- createSimpleImputer(
    method = "median",
    missingThreshold = 0.5,
    addMissingIndicator = TRUE
  )
  expect_equal(imputer$method, "median")
  expect_equal(imputer$missingThreshold, 0.5)
  expect_true(imputer$addMissingIndicator)
  expect_s3_class(imputer, "featureEngineeringSettings")

  expect_s3_class(imputer, "featureEngineeringSettings")
  expect_error(createSimpleImputer(method = "mean", missingThreshold = -1))
  expect_error(createSimpleImputer(method = "mean", missingThreshold = "0.5"))
  expect_error(createSimpleImputer(method = "mean", missingThreshold = 1))
  expect_error(createSimpleImputer(method = "notMean"))
  expect_error(createSimpleImputer(addMissingIndicator = "true"))
})

test_that("createIterativeImputer works", {
  skip_if_not_installed("glmnet")
  imputer <- createIterativeImputer()

  expect_equal(imputer$method, "pmm")
  expect_false(imputer$addMissingIndicator)
  expect_error(createIterativeImputer(method = "notPmm"))
  expect_equal(attr(imputer, "fun"), "iterativeImpute")
  expect_s3_class(imputer, "featureEngineeringSettings")
  expect_error(createIterativeImputer(method = "pmm", missingThreshold = -1))
  expect_error(createIterativeImputer(method = "pmm", missingThreshold = "0.5"))
  expect_error(createIterativeImputer(method = "pmm", missingThreshold = 1))

  imputer <- createIterativeImputer(
    method = "pmm",
    missingThreshold = 0.5,
    addMissingIndicator = TRUE
  )
  expect_equal(imputer$missingThreshold, 0.5)
  expect_true(imputer$addMissingIndicator)
  expect_s3_class(imputer, "featureEngineeringSettings")
  expect_error(createIterativeImputer(addMissingIndicator = "true"))
})

test_that("simpleImpute works", {
  skip_if_offline()
  missingData <- createMissingData(tinyTrainData, 0.2)

  imputer <- createSimpleImputer(method = "mean", missingThreshold = 0.3)

  imputedData <- simpleImpute(missingData, imputer, done = FALSE)

  newFeature <- imputedData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 666) %>%
    dplyr::pull(.data$covariateValue)

  originalFeature <- missingData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 666)

  imputedFeature <- imputedData$covariateData$covariates %>%
    dplyr::filter(
      .data$covariateId == 666,
      !.data$rowId %in% !!(originalFeature %>%
        dplyr::pull(.data$rowId))
    ) %>%
    dplyr::pull(.data$covariateValue)
  originalFeature <- originalFeature %>%
    dplyr::pull(.data$covariateValue)


  expect_true(length(newFeature) > length(originalFeature))
  expect_equal(length(newFeature), nrow(imputedData$labels))
  expect_equal(mean(originalFeature), unique(imputedFeature))

  missingTestData <- createMissingData(testData, 0.4, test = TRUE)
  # extract featureEngineeringSettings from imputedData
  metaData <- attr(imputedData$covariateData, "metaData")
  testSettings <- metaData$featureEngineering$simpleImputer$settings$featureEngineeringSettings

  imputedTestData <- simpleImpute(missingTestData, testSettings, done = TRUE)

  newFeatureTest <- imputedTestData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 666) %>%
    dplyr::pull(.data$covariateValue)
  originalFeatureTest <- missingTestData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 666)
  imputedFeatureTest <- imputedTestData$covariateData$covariates %>%
    dplyr::filter(
      .data$covariateId == 666,
      !.data$rowId %in% !!(originalFeatureTest %>%
        dplyr::pull(.data$rowId))
    ) %>%
    dplyr::pull(.data$covariateValue)
  originalFeatureTest <- originalFeatureTest %>%
    dplyr::pull(.data$covariateValue)

  expect_true(length(newFeatureTest) > length(originalFeatureTest))
  expect_equal(length(newFeatureTest), nrow(imputedTestData$labels))
  # should use mean from training data
  expect_equal(mean(originalFeature), unique(imputedFeatureTest))

  imputer <- createSimpleImputer(method = "median", missingThreshold = 0.3)

  imputedData <- simpleImpute(missingData, imputer, done = FALSE)

  newFeature <- imputedData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 666) %>%
    dplyr::pull(.data$covariateValue)

  originalFeature <- missingData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 666)

  imputedFeature <- imputedData$covariateData$covariates %>%
    dplyr::filter(
      .data$covariateId == 666,
      !.data$rowId %in% !!(originalFeature %>%
        dplyr::pull(.data$rowId))
    ) %>%
    dplyr::pull(.data$covariateValue)
  originalFeature <- originalFeature %>%
    dplyr::pull(.data$covariateValue)

  expect_true(length(newFeature) > length(originalFeature))
  expect_equal(length(newFeature), nrow(imputedData$labels))
  expect_equal(median(originalFeature), unique(imputedFeature))

  imputer <- createSimpleImputer(method = "mean", missingThreshold = 0.1)
  imputedData <- simpleImpute(missingData, imputer, done = FALSE)
  newFeature <- imputedData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 666) %>%
    dplyr::pull(.data$covariateValue)
  expect_true(length(newFeature) == 0)
})

test_that("IterativeImputer works", {
  skip_if_offline()
  skip_if_not_installed("glmnet")
  missingData <- createMissingData(tinyTrainData, 0.2)
  imputer <- createIterativeImputer(
    method = "pmm", missingThreshold = 0.3,
    methodSettings = list(
      pmm = list(
      k = 1,
      iterations = 1
    ))
  )
  imputedData <- iterativeImpute(missingData, imputer, done = FALSE)

  newFeature <- imputedData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 666) %>%
    dplyr::pull(.data$covariateValue)

  originalFeature <- missingData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 666)

  imputedFeature <- imputedData$covariateData$covariates %>%
    dplyr::filter(
      .data$covariateId == 666,
      !.data$rowId %in% !!(originalFeature %>%
        dplyr::pull(.data$rowId))
    ) %>%
    dplyr::pull(.data$covariateValue)
  originalFeature <- originalFeature %>%
    dplyr::pull(.data$covariateValue)

  expect_true(length(newFeature) > length(originalFeature))
  expect_equal(length(newFeature), nrow(imputedData$labels))

  missingTestData <- createMissingData(testData, 0.4, test = TRUE)
  # extract featureEngineeringSettings from imputedData
  metaData <- attr(imputedData$covariateData, "metaData")
  testSettings <- metaData$featureEngineering$iterativeImputer$settings$featureEngineeringSettings

  imputedTestData <- iterativeImpute(missingTestData, testSettings, done = TRUE)

  newFeatureTest <- imputedTestData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 666) %>%
    dplyr::pull(.data$covariateValue)
  originalFeatureTest <- missingTestData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 666)
  imputedFeatureTest <- imputedTestData$covariateData$covariates %>%
    dplyr::filter(
      .data$covariateId == 666,
      !.data$rowId %in% !!(originalFeatureTest %>%
        dplyr::pull(.data$rowId))
    ) %>%
    dplyr::pull(.data$covariateValue)
  originalFeatureTest <- originalFeatureTest %>%
    dplyr::pull(.data$covariateValue)

  expect_true(length(newFeatureTest) > length(originalFeatureTest))
  expect_equal(length(newFeatureTest), nrow(imputedTestData$labels))
})

test_that("SimpleImputer can add missing indicators for imputed features", {
  skip_if_offline()
  missingData <- createMissingData(tinyTrainData, 0.2)
  imputer <- createSimpleImputer(
    method = "mean",
    missingThreshold = 0.3,
    addMissingIndicator = TRUE
  )
  imputedData <- simpleImpute(missingData, imputer, done = FALSE)

  metaData <- attr(imputedData$covariateData, "metaData")
  testSettings <- metaData$featureEngineering$simpleImputer$settings$featureEngineeringSettings
  indicatorInfo <- attr(testSettings, "missingIndicatorInfo")
  expect_true(nrow(indicatorInfo$map) > 0)

  fakeVariableMap <- indicatorInfo$map %>%
    dplyr::filter(.data$sourceCovariateId == 666)
  expect_equal(nrow(fakeVariableMap), 1)
  indicatorId <- fakeVariableMap$indicatorCovariateId

  expect_true(indicatorId %in% (imputedData$covariateData$covariateRef %>%
    dplyr::pull(.data$covariateId)))
  expect_true(any(imputedData$covariateData$analysisRef %>%
    dplyr::pull(.data$isBinary) == "Y"))

  observedRows <- missingData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 666) %>%
    dplyr::pull(.data$rowId)
  expectedMissingRows <- setdiff(missingData$labels$rowId, observedRows)
  indicatorRows <- imputedData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == !!indicatorId) %>%
    dplyr::pull(.data$rowId)
  indicatorValues <- imputedData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == !!indicatorId) %>%
    dplyr::pull(.data$covariateValue)
  expect_setequal(indicatorRows, expectedMissingRows)
  expect_true(all(indicatorValues == 1))

  missingTestData <- createMissingData(testData, 0.4, test = TRUE)
  imputedTestData <- simpleImpute(missingTestData, testSettings, done = TRUE)
  indicatorRowsTest <- imputedTestData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == !!indicatorId) %>%
    dplyr::pull(.data$rowId)
  observedRowsTest <- missingTestData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 666) %>%
    dplyr::pull(.data$rowId)
  expectedMissingRowsTest <- setdiff(missingTestData$labels$rowId, observedRowsTest)
  expect_setequal(indicatorRowsTest, expectedMissingRowsTest)
})

test_that("IterativeImputer can add missing indicators for imputed features", {
  skip_if_offline()
  skip_if_not_installed("glmnet")
  missingData <- createMissingData(tinyTrainData, 0.2)
  imputer <- createIterativeImputer(
    method = "pmm",
    missingThreshold = 0.3,
    methodSettings = list(pmm = list(k = 1, iterations = 1)),
    addMissingIndicator = TRUE
  )
  imputedData <- iterativeImpute(missingData, imputer, done = FALSE)

  metaData <- attr(imputedData$covariateData, "metaData")
  testSettings <- metaData$featureEngineering$iterativeImputer$settings$featureEngineeringSettings
  indicatorInfo <- attr(testSettings, "missingIndicatorInfo")
  expect_true(nrow(indicatorInfo$map) > 0)

  fakeVariableMap <- indicatorInfo$map %>%
    dplyr::filter(.data$sourceCovariateId == 666)
  expect_equal(nrow(fakeVariableMap), 1)
  indicatorId <- fakeVariableMap$indicatorCovariateId
  indicatorRows <- imputedData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == !!indicatorId) %>%
    dplyr::pull(.data$rowId)
  expect_true(length(indicatorRows) > 0)
})
