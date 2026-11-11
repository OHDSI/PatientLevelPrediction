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

test_that("simpleImpute works", {
  trainData <- makeToyImputationData(n = 80, seed = 5)
  testDataSmall <- makeToyImputationData(n = 80, seed = 6)
  missingData <- createMissingData(trainData, 0.2)

  imputer <- createSimpleImputer(
    method = "mean",
    missingThreshold = 0.3,
    addMissingIndicator = TRUE
  )

  # the imputer changes its input, so take the snapshot first
  missingCovariates <- missingData$covariateData$covariates %>%
    dplyr::collect()
  imputedData <- simpleImpute(missingData, imputer, done = FALSE)

  imputedCovariates <- imputedData$covariateData$covariates %>%
    dplyr::collect()

  newFeature <- imputedCovariates %>%
    dplyr::filter(.data$covariateId == 666) %>%
    dplyr::pull(.data$covariateValue)

  originalFeature <- missingCovariates %>%
    dplyr::filter(.data$covariateId == 666)

  imputedFeature <- imputedCovariates %>%
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

  missingTestData <- createMissingData(testDataSmall, 0.4, test = TRUE)
  # extract featureEngineeringSettings from imputedData
  testSettings <- fittedFeatureEngineeringSettings(imputedData, "simpleImpute")

  indicatorInfo <- attr(testSettings, "missingIndicatorInfo")
  expect_true(!is.null(indicatorInfo))
  expect_true(!is.null(indicatorInfo$map))
  expect_true(nrow(indicatorInfo$map) > 0)
  fakeVariableMap <- indicatorInfo$map %>%
    dplyr::filter(.data$sourceCovariateId == 666)
  expect_equal(nrow(fakeVariableMap), 1)
  indicatorId <- fakeVariableMap$indicatorCovariateId

  observedRows <- missingCovariates %>%
    dplyr::filter(.data$covariateId == 666) %>%
    dplyr::pull(.data$rowId)
  expectedMissingRows <- setdiff(missingData$labels$rowId, observedRows)
  indicatorRows <- imputedCovariates %>%
    dplyr::filter(.data$covariateId == !!indicatorId) %>%
    dplyr::pull(.data$rowId)
  indicatorValues <- imputedCovariates %>%
    dplyr::filter(.data$covariateId == !!indicatorId) %>%
    dplyr::pull(.data$covariateValue)
  expect_setequal(indicatorRows, expectedMissingRows)
  expect_true(all(indicatorValues == 1))

  # the imputer changes its input, so take the snapshot first
  missingTestCovariates <- missingTestData$covariateData$covariates %>%
    dplyr::collect()
  imputedTestData <- simpleImpute(missingTestData, testSettings, done = TRUE)

  imputedTestCovariates <- imputedTestData$covariateData$covariates %>%
    dplyr::collect()

  newFeatureTest <- imputedTestCovariates %>%
    dplyr::filter(.data$covariateId == 666) %>%
    dplyr::pull(.data$covariateValue)
  originalFeatureTest <- missingTestCovariates %>%
    dplyr::filter(.data$covariateId == 666)
  imputedFeatureTest <- imputedTestCovariates %>%
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

  indicatorRowsTest <- imputedTestCovariates %>%
    dplyr::filter(.data$covariateId == !!indicatorId) %>%
    dplyr::pull(.data$rowId)
  observedRowsTest <- missingTestCovariates %>%
    dplyr::filter(.data$covariateId == 666) %>%
    dplyr::pull(.data$rowId)
  expectedMissingRowsTest <- setdiff(missingTestData$labels$rowId, observedRowsTest)
  expect_setequal(indicatorRowsTest, expectedMissingRowsTest)

  missingInfo <- attr(testSettings, "missingInfo")
  expect_true(any(missingInfo$covariateId == 666 & missingInfo$missing > 0.1))

  filteredCovariates <- missingCovariates %>%
    dplyr::left_join(missingInfo, by = "covariateId") %>%
    dplyr::filter(is.na(.data$missing) | .data$missing <= 0.1) %>%
    dplyr::filter(.data$covariateId == 666) %>%
    dplyr::collect()
  expect_equal(nrow(filteredCovariates), 0)
})

test_that("simpleImpute fills missing values across many continuous features", {
  trainData <- makeWideMissingImputationData(
    n = 80,
    nFeatures = 30,
    missingness = 0.8,
    seed = 11,
    test = FALSE
  )
  testData <- makeWideMissingImputationData(
    n = 80,
    nFeatures = 30,
    missingness = 0.8,
    seed = 12,
    test = TRUE
  )
  imputer <- createSimpleImputer(
    method = "mean",
    missingThreshold = 0.95,
    addMissingIndicator = FALSE
  )

  imputedTrain <- simpleImpute(trainData, imputer, done = FALSE)
  settings <- fittedFeatureEngineeringSettings(imputedTrain, "simpleImpute")
  imputedTest <- simpleImpute(testData, settings, done = TRUE)

  featureIds <- trainData$covariateData$covariateRef %>%
    dplyr::collect() %>%
    dplyr::pull(.data$covariateId)
  trainObserved <- trainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId %in% !!featureIds) %>%
    dplyr::collect()
  trainMeans <- trainObserved %>%
    dplyr::group_by(.data$covariateId) %>%
    dplyr::summarise(expectedValue = mean(.data$covariateValue), .groups = "drop")

  trainCompleted <- imputedTrain$covariateData$covariates %>%
    dplyr::filter(.data$covariateId %in% !!featureIds) %>%
    dplyr::collect() %>%
    dplyr::count(.data$covariateId, name = "nRows")
  expect_true(all(trainCompleted$nRows == nrow(trainData$labels)))

  trainMissingPairs <- expand.grid(
    rowId = trainData$labels$rowId,
    covariateId = featureIds,
    KEEP.OUT.ATTRS = FALSE
  ) %>%
    dplyr::anti_join(
      trainObserved %>% dplyr::select("rowId", "covariateId"),
      by = c("rowId", "covariateId")
    )
  trainImputedValues <- imputedTrain$covariateData$covariates %>%
    dplyr::collect() %>%
    dplyr::semi_join(trainMissingPairs, by = c("rowId", "covariateId")) %>%
    dplyr::left_join(trainMeans, by = "covariateId")
  expect_equal(trainImputedValues$covariateValue, trainImputedValues$expectedValue)

  testObserved <- testData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId %in% !!featureIds) %>%
    dplyr::collect()
  testCompleted <- imputedTest$covariateData$covariates %>%
    dplyr::filter(.data$covariateId %in% !!featureIds) %>%
    dplyr::collect() %>%
    dplyr::count(.data$covariateId, name = "nRows")
  expect_true(all(testCompleted$nRows == nrow(testData$labels)))

  testMissingPairs <- expand.grid(
    rowId = testData$labels$rowId,
    covariateId = featureIds,
    KEEP.OUT.ATTRS = FALSE
  ) %>%
    dplyr::anti_join(
      testObserved %>% dplyr::select("rowId", "covariateId"),
      by = c("rowId", "covariateId")
    )
  testImputedValues <- imputedTest$covariateData$covariates %>%
    dplyr::collect() %>%
    dplyr::semi_join(testMissingPairs, by = c("rowId", "covariateId")) %>%
    dplyr::left_join(trainMeans, by = "covariateId")
  expect_equal(testImputedValues$covariateValue, testImputedValues$expectedValue)
})

test_that("simpleImpute stores serializable imputer metadata", {
  trainData <- makeToyImputationData(n = 80, seed = 13)
  missingData <- createMissingData(trainData, 0.2)
  imputer <- createSimpleImputer(
    method = "mean",
    missingThreshold = 0.3,
    addMissingIndicator = FALSE
  )

  imputedData <- simpleImpute(missingData, imputer, done = FALSE)
  settings <- fittedFeatureEngineeringSettings(imputedData, "simpleImpute")
  imputedValues <- attr(settings, "imputer")

  expect_false(inherits(imputedValues, "tbl_Andromeda"))
  expect_true(is.data.frame(imputedValues))
  expect_true(all(c("covariateId", "imputedValues") %in% colnames(imputedValues)))

  serializablePreprocessing <- list(
    featureEngineering = list(
      simpleImputer = list(
        funct = "simpleImpute",
        settings = list(featureEngineeringSettings = settings, done = TRUE)
      )
    )
  )
  jsonFile <- tempfile(fileext = ".json")
  expect_no_error(ParallelLogger::saveSettingsToJson(serializablePreprocessing, jsonFile))
})

test_that("simpleImpute apply path imputes patients with no observed numeric values", {
  for (method in c("mean", "median")) {
    train <- makeImputationFixture(
      rowIds = 1:6,
      covariates = rbind(
        data.frame(rowId = 1:4, covariateId = 10, covariateValue = c(10, 20, 30, 60)),
        data.frame(rowId = 1:6, covariateId = 20, covariateValue = 1)
      ),
      numericIds = 10,
      binaryIds = 20,
      folds = TRUE
    )
    withr::defer(Andromeda::close(train$covariateData))
    settings <- createSimpleImputer(
      method = method,
      missingThreshold = 0.5,
      addMissingIndicator = TRUE
    )
    fitted <- PatientLevelPrediction:::simpleImpute(train, settings, done = FALSE)
    withr::defer(Andromeda::close(fitted$covariateData))
    saved <- fittedFeatureEngineeringSettings(fitted, "simpleImpute")
    indicatorId <- attr(saved, "missingIndicatorInfo")$map$indicatorCovariateId

    test <- makeImputationFixture(
      rowIds = c(101, 102),
      covariates = rbind(
        data.frame(rowId = 101, covariateId = 10, covariateValue = 5),
        data.frame(rowId = c(101, 102), covariateId = 20, covariateValue = 1)
      ),
      numericIds = 10,
      binaryIds = 20
    )
    withr::defer(Andromeda::close(test$covariateData))
    applied <- PatientLevelPrediction:::simpleImpute(test, saved, done = TRUE)
    withr::defer(Andromeda::close(applied$covariateData))

    expected <- if (method == "mean") 30 else 25
    labRows <- covariateRowsOf(applied, 10)
    expect_equal(labRows$rowId, c(101, 102))
    expect_equal(labRows$covariateValue, c(5, expected))
    indicatorRows <- covariateRowsOf(applied, indicatorId)
    expect_equal(indicatorRows$rowId, 102)
    expect_equal(indicatorRows$covariateValue, 1)
  }
})

test_that("extractMissingInfo uses the labeled population as denominator", {
  labelsOnly <- makeImputationFixture(
    rowIds = 1:100,
    covariates = data.frame(rowId = 1:60, covariateId = 10, covariateValue = 1),
    numericIds = 10,
    folds = TRUE
  )
  withr::defer(Andromeda::close(labelsOnly$covariateData))
  expect_equal(
    PatientLevelPrediction:::extractMissingInfo(labelsOnly)$missingInfo$missing,
    0.4
  )

  withBinaryFeature <- makeImputationFixture(
    rowIds = 1:100,
    covariates = rbind(
      data.frame(rowId = 1:60, covariateId = 10, covariateValue = 1),
      data.frame(rowId = 1:100, covariateId = 20, covariateValue = 1)
    ),
    numericIds = 10,
    binaryIds = 20,
    folds = TRUE
  )
  withr::defer(Andromeda::close(withBinaryFeature$covariateData))
  expect_equal(
    PatientLevelPrediction:::extractMissingInfo(withBinaryFeature)$missingInfo$missing,
    0.4
  )

  outsideLabels <- makeImputationFixture(
    rowIds = 1:50,
    covariates = data.frame(
      rowId = c(1:40, 51:60),
      covariateId = 10,
      covariateValue = 1
    ),
    numericIds = 10,
    folds = TRUE
  )
  withr::defer(Andromeda::close(outsideLabels$covariateData))
  expect_equal(
    PatientLevelPrediction:::extractMissingInfo(outsideLabels)$missingInfo$missing,
    0.2
  )
  expect_false("tmpMissingInfoRowIds" %in% names(outsideLabels$covariateData))
})

test_that("imputers remove features above the threshold when patients have no covariates", {
  makeLabOnlyData <- function() {
    makeImputationFixture(
      rowIds = 1:10,
      covariates = data.frame(rowId = 1:6, covariateId = 10, covariateValue = c(1, 2, 3, 4, 5, 6)),
      numericIds = 10,
      folds = TRUE
    )
  }

  simpleData <- makeLabOnlyData()
  withr::defer(Andromeda::close(simpleData$covariateData))
  simple <- PatientLevelPrediction:::simpleImpute(
    simpleData,
    createSimpleImputer(missingThreshold = 0.3),
    done = FALSE
  )
  withr::defer(Andromeda::close(simple$covariateData))
  expect_equal(nrow(covariateRowsOf(simple, 10)), 0)

  skip_if_not_installed("glmnet")
  iterativeData <- makeLabOnlyData()
  withr::defer(Andromeda::close(iterativeData$covariateData))
  iterative <- PatientLevelPrediction:::iterativeImpute(
    iterativeData,
    createIterativeImputer(
      missingThreshold = 0.3,
      methodSettings = list(pmm = list(k = 1, iterations = 1, alpha = 1))
    ),
    done = FALSE
  )
  withr::defer(Andromeda::close(iterative$covariateData))
  expect_equal(nrow(covariateRowsOf(iterative, 10)), 0)
})
