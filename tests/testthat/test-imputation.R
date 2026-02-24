# add a test numerical feature with missing values of certain percentage
subsetSplitData <- function(data, n = 250, seed = 1) {
  stopifnot(is.list(data), !is.null(data$labels), !is.null(data$covariateData))

  labels <- data$labels
  covariateRowIds <- dplyr::collect(
    dplyr::distinct(
      dplyr::select(data$covariateData$covariates, "rowId")
    )
  )$rowId
  rowIds <- intersect(labels$rowId, covariateRowIds)
  if (length(rowIds) > n) {
    rowIds <- withr::with_seed(seed, sample(rowIds, size = n))
  }

  out <- list(
    labels = dplyr::filter(labels, .data$rowId %in% !!rowIds),
    covariateData = Andromeda::andromeda(
      analysisRef = data$covariateData$analysisRef
    )
  )

  if (!is.null(data$folds)) {
    out$folds <- dplyr::filter(data$folds, .data$rowId %in% !!rowIds)
  }

  out$covariateData$covariates <- dplyr::filter(data$covariateData$covariates, .data$rowId %in% !!rowIds)
  out$covariateData$covariateRef <- data$covariateData$covariateRef

  attributes(out$covariateData)$metaData <- attributes(data$covariateData)$metaData
  class(out$covariateData) <- class(data$covariateData)

  attributes(out)$metaData <- attributes(data)$metaData
  class(out) <- class(data)
  out
}

makeToyImputationData <- function(n = 150, seed = 1) {
  labels <- data.frame(rowId = seq_len(n))
  folds <- data.frame(
    rowId = labels$rowId,
    index = 1L
  )
  covariates <- withr::with_seed(seed, {
    data.frame(
      rowId = rep(seq_len(n), each = 2),
      covariateId = rep(c(10, 20), times = n),
      covariateValue = c(
        runif(n),
        stats::rbinom(n, size = 1, prob = 0.3)
      )
    )
  })
  covariateRef <- data.frame(
    covariateId = c(10, 20),
    covariateName = c("contFeature", "binFeature"),
    analysisId = c(10, 20),
    conceptId = c(10, 20)
  )
  analysisRef <- data.frame(
    analysisId = c(10, 20),
    analysisName = c("continuous", "binary"),
    domainId = c("measurement", "feature engineering"),
    startDay = c(NA_real_, NA_real_),
    endDay = c(NA_real_, NA_real_),
    isBinary = c("N", "Y"),
    missingMeansZero = c("N", "Y")
  )
  covariateData <- Andromeda::andromeda(
    covariates = covariates,
    covariateRef = covariateRef,
    analysisRef = analysisRef
  )
  class(covariateData) <- "CovariateData"
  attr(covariateData, "metaData") <- list()

  trainData <- list(
    labels = labels,
    folds = folds,
    covariateData = covariateData
  )
  class(trainData) <- "plpData"
  attr(trainData, "metaData") <- list()

  trainData
}

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

makeWideMissingImputationData <- function(
  n = 80,
  nFeatures = 25,
  missingness = 0.75,
  seed = 1,
  test = FALSE
) {
  stopifnot(nFeatures > 0, missingness >= 0, missingness < 1)
  out <- makeToyImputationData(n = n, seed = seed)
  if (test) {
    out$folds <- NULL
  }

  labels <- out$labels
  covariateIds <- seq_len(nFeatures) + 1000L
  nObserved <- max(1L, floor(n * (1 - missingness)))

  covariates <- withr::with_seed(seed, {
    lapply(covariateIds, function(covariateId) {
      observedRows <- sort(sample(labels$rowId, nObserved, replace = FALSE))
      data.frame(
        rowId = observedRows,
        covariateId = covariateId,
        covariateValue = runif(nObserved)
      )
    }) %>%
      dplyr::bind_rows()
  })

  covariateRef <- data.frame(
    covariateId = covariateIds,
    covariateName = paste0("contFeature", covariateIds),
    analysisId = covariateIds,
    conceptId = covariateIds
  )
  analysisRef <- data.frame(
    analysisId = covariateIds,
    analysisName = rep("continuous", length(covariateIds)),
    domainId = rep("measurement", length(covariateIds)),
    startDay = rep(NA_real_, length(covariateIds)),
    endDay = rep(NA_real_, length(covariateIds)),
    isBinary = rep("N", length(covariateIds)),
    missingMeansZero = rep("N", length(covariateIds))
  )
  Andromeda::appendToTable(
    out$covariateData$covariates,
    covariates
  )
  Andromeda::appendToTable(
    out$covariateData$covariateRef,
    covariateRef
  )
  Andromeda::appendToTable(
    out$covariateData$analysisRef,
    analysisRef
  )
  out
}

skip_if_no_sklearn_iterative <- function() {
  skip_if_not_installed("reticulate")
  available <- tryCatch(
    {
      reticulate::import("sklearn.experimental.enable_iterative_imputer")
      reticulate::import("sklearn")
      TRUE
    },
    error = function(e) FALSE
  )
  if (!available) {
    skip("scikit-learn IterativeImputer not available")
  }
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

test_that("createSklearnIterativeImputer works", {
  skip_on_cran()
  skip_if_no_sklearn_iterative()
  imputer <- createSklearnIterativeImputer()

  expect_equal(imputer$method, "sklearnIterative")
  expect_equal(imputer$missingThreshold, 0.3)
  expect_false(imputer$addMissingIndicator)
  expect_equal(attr(imputer, "fun"), "sklearnIterativeImpute")
  expect_s3_class(imputer, "featureEngineeringSettings")

  expect_error(createSklearnIterativeImputer(missingThreshold = -1))
  expect_error(createSklearnIterativeImputer(addMissingIndicator = "true"))
  expect_error(createSklearnIterativeImputer(methodSettings = "bad"))
  expect_error(createSklearnIterativeImputer(methodSettings = list(maxIter = 0)))
  expect_error(createSklearnIterativeImputer(methodSettings = list(tol = 0)))
  expect_error(createSklearnIterativeImputer(methodSettings = list(nNearestFeatures = 0)))
  expect_error(createSklearnIterativeImputer(methodSettings = list(initialStrategy = "bad")))
  expect_error(createSklearnIterativeImputer(methodSettings = list(imputationOrder = "bad")))
})

test_that("sklearnIterativeImpute works and reuses binary predictors", {
  skip_if_offline()
  skip_on_cran()
  skip_if_no_sklearn_iterative()
  trainData <- subsetSplitData(tinyTrainData, n = 150, seed = 1)
  testDataSmall <- subsetSplitData(testData, n = 150, seed = 2)
  missingData <- createMissingData(trainData, 0.2)
  imputer <- createSklearnIterativeImputer(
    missingThreshold = 0.3,
    methodSettings = list(
      maxIter = 2,
      nNearestFeatures = 20,
      randomState = 11
    ),
    addMissingIndicator = TRUE
  )

  imputedData <- sklearnIterativeImpute(missingData, imputer, done = FALSE)

  imputedCovariates <- imputedData$covariateData$covariates %>%
    dplyr::collect()
  newFeature <- imputedCovariates %>%
    dplyr::filter(.data$covariateId == 666) %>%
    dplyr::pull(.data$covariateValue)
  expect_equal(length(newFeature), nrow(imputedData$labels))

  metaData <- attr(imputedData$covariateData, "metaData")
  testSettings <- metaData$featureEngineering$sklearnIterativeImputer$settings$featureEngineeringSettings
  predictorIds <- attr(testSettings, "sklearnPredictorCovariateIds")
  predictorInfo <- imputedData$covariateData$covariateRef %>%
    dplyr::filter(.data$covariateId %in% !!predictorIds) %>%
    dplyr::select("covariateId", "analysisId") %>%
    dplyr::inner_join(
      imputedData$covariateData$analysisRef %>%
        dplyr::select("analysisId", "missingMeansZero"),
      by = "analysisId"
    ) %>%
    dplyr::collect()
  expect_true(any(predictorInfo$missingMeansZero == "Y"))

  missingTestData <- createMissingData(testDataSmall, 0.4, test = TRUE)
  imputedTestData <- sklearnIterativeImpute(missingTestData, testSettings, done = TRUE)
  imputedTestCovariates <- imputedTestData$covariateData$covariates %>%
    dplyr::collect()
  newFeatureTest <- imputedTestCovariates %>%
    dplyr::filter(.data$covariateId == 666) %>%
    dplyr::pull(.data$covariateValue)
  expect_equal(length(newFeatureTest), nrow(imputedTestData$labels))
  expect_no_error(Andromeda::copyAndromeda(imputedTestData$covariateData))
})

test_that("sklearnIterativeImpute errors when runtime imputer state is missing", {
  skip_if_offline()
  skip_on_cran()
  skip_if_no_sklearn_iterative()
  trainData <- subsetSplitData(tinyTrainData, n = 150, seed = 3)
  testDataSmall <- subsetSplitData(testData, n = 150, seed = 4)
  missingData <- createMissingData(trainData, 0.2)
  imputer <- createSklearnIterativeImputer(
    missingThreshold = 0.3,
    methodSettings = list(maxIter = 1, randomState = 123)
  )
  imputedData <- sklearnIterativeImpute(missingData, imputer, done = FALSE)
  settings <- attr(imputedData$covariateData, "metaData")$featureEngineering$sklearnIterativeImputer$settings$featureEngineeringSettings
  settings$sklearnImputerKey <- "missing-key"

  missingTestData <- createMissingData(testDataSmall, 0.4, test = TRUE)
  expect_error(
    sklearnIterativeImpute(missingTestData, settings, done = TRUE),
    "runtime state not available"
  )
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

  imputedData <- simpleImpute(missingData, imputer, done = FALSE)

  missingCovariates <- missingData$covariateData$covariates %>%
    dplyr::collect()
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
  metaData <- attr(imputedData$covariateData, "metaData")
  testSettings <- metaData$featureEngineering$simpleImputer$settings$featureEngineeringSettings

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

  imputedTestData <- simpleImpute(missingTestData, testSettings, done = TRUE)

  missingTestCovariates <- missingTestData$covariateData$covariates %>%
    dplyr::collect()
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

  filteredCovariates <- missingData$covariateData$covariates %>%
    dplyr::left_join(missingInfo, by = "covariateId", copy = TRUE) %>%
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
  settings <- attr(imputedTrain$covariateData, "metaData")$featureEngineering$simpleImputer$settings$featureEngineeringSettings
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
  settings <- attr(imputedData$covariateData, "metaData")$featureEngineering$simpleImputer$settings$featureEngineeringSettings
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

test_that("IterativeImputer works", {
  skip_if_not_installed("glmnet")
  trainData <- makeToyImputationData(n = 80, seed = 7)
  testDataSmall <- makeToyImputationData(n = 80, seed = 8)
  missingData <- createMissingData(trainData, 0.2)
  imputer <- createIterativeImputer(
    method = "pmm", missingThreshold = 0.3,
    methodSettings = list(
      pmm = list(
      k = 1,
      iterations = 1
    )),
    addMissingIndicator = TRUE
  )

  testthat::local_mocked_bindings(
    pmmFit = function(data, k = 5, alpha = 1) {
      rows <- data$xMiss %>%
        dplyr::pull(.data$rowId) %>%
        unique()
      yObs <- data$yObs %>%
        dplyr::collect()
      donorValues <- yObs$y
      intercept <- mean(donorValues, na.rm = TRUE)
      list(
        imputedValues = data.frame(
          rowId = rows,
          imputedValue = rep(intercept, length(rows))
        ),
        model = list(
          intercept = intercept,
          coefficients = data.frame(covariateId = integer(0), values = numeric(0)),
          predictions = data.frame(
            rowId = yObs$rowId,
            prediction = yObs$y,
            observedValue = yObs$y
          )
        )
      )
    },
    .package = "PatientLevelPrediction"
  )

  imputedData <- iterativeImpute(missingData, imputer, done = FALSE)

  missingCovariates <- missingData$covariateData$covariates %>%
    dplyr::collect()
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

  metaData <- attr(imputedData$covariateData, "metaData")
  testSettings <- metaData$featureEngineering$iterativeImputer$settings$featureEngineeringSettings
  indicatorInfo <- attr(testSettings, "missingIndicatorInfo")
  expect_true(!is.null(indicatorInfo))
  expect_true(!is.null(indicatorInfo$map))
  expect_true(nrow(indicatorInfo$map) > 0)
  fakeVariableMap <- indicatorInfo$map %>%
    dplyr::filter(.data$sourceCovariateId == 666)
  expect_equal(nrow(fakeVariableMap), 1)

  missingTestData <- createMissingData(testDataSmall, 0.4, test = TRUE)
  imputedTestData <- iterativeImpute(missingTestData, testSettings, done = TRUE)

  missingTestCovariates <- missingTestData$covariateData$covariates %>%
    dplyr::collect()
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
})

makePmmData <- function(nObs = 6, nMiss = 3) {
  stopifnot(nObs >= 3)
  covariateId <- 101
  xObs <- data.frame(
    rowId = rep(seq_len(nObs), each = 2),
    covariateId = rep(c(covariateId, covariateId + 1), times = nObs),
    covariateValue = c(
      seq_len(nObs),
      seq(from = 10, by = 2, length.out = nObs)
    )
  )
  xMiss <- data.frame(
    rowId = nObs + seq_len(nMiss),
    covariateId = covariateId,
    covariateValue = seq(from = 2, by = 1, length.out = nMiss)
  )
  yObs <- data.frame(
    rowId = seq_len(nObs),
    y = seq(from = 0.1, by = 0.2, length.out = nObs)
  )
  Andromeda::andromeda(
    xObs = xObs,
    xMiss = xMiss,
    yObs = yObs
  )
}

makeApplyImputeDataForKTest <- function() {
  covariates <- data.frame(
    rowId = c(1, 3, 1, 2, 3, 4),
    covariateId = c(100, 100, 200, 200, 200, 200),
    covariateValue = c(10, 30, 1, 0, 1, 0)
  )
  covariateRef <- data.frame(
    covariateId = c(100, 200),
    covariateName = c("bmi", "binary feature"),
    analysisId = c(10, 20),
    conceptId = c(1, 2)
  )
  analysisRef <- data.frame(
    analysisId = c(10, 20),
    analysisName = c("continuous", "binary"),
    domainId = c("measurement", "feature engineering"),
    startDay = c(NA_real_, NA_real_),
    endDay = c(NA_real_, NA_real_),
    isBinary = c("N", "Y"),
    missingMeansZero = c("N", "Y")
  )
  covariateData <- Andromeda::andromeda(
    covariates = covariates,
    covariateRef = covariateRef,
    analysisRef = analysisRef
  )
  class(covariateData) <- "CovariateData"
  attr(covariateData, "metaData") <- list()

  trainData <- list(
    labels = data.frame(rowId = 1:4),
    covariateData = covariateData
  )
  class(trainData) <- "plpData"
  attr(trainData, "metaData") <- list()

  trainData
}

makeApplyImputeDataForParityTest <- function() {
  covariates <- data.frame(
    rowId = c(1, 2, 3, 4, 1, 2, 3, 4),
    covariateId = c(300, 300, 300, 300, 200, 200, 200, 200),
    covariateValue = c(10, 20, 30, 40, 1, 0, 1, 0)
  )
  covariateRef <- data.frame(
    covariateId = c(300, 200),
    covariateName = c("continuous missingMeansZero", "binary feature"),
    analysisId = c(30, 20),
    conceptId = c(3, 2)
  )
  analysisRef <- data.frame(
    analysisId = c(30, 20),
    analysisName = c("continuous", "binary"),
    domainId = c("measurement", "feature engineering"),
    startDay = c(NA_real_, NA_real_),
    endDay = c(NA_real_, NA_real_),
    isBinary = c("N", "Y"),
    missingMeansZero = c("Y", "Y")
  )
  covariateData <- Andromeda::andromeda(
    covariates = covariates,
    covariateRef = covariateRef,
    analysisRef = analysisRef
  )
  class(covariateData) <- "CovariateData"
  attr(covariateData, "metaData") <- list()

  trainData <- list(
    labels = data.frame(rowId = 1:4),
    covariateData = covariateData
  )
  class(trainData) <- "plpData"
  attr(trainData, "metaData") <- list()

  trainData
}

makeApplyImputerSettingsForKTest <- function(k = 3) {
  settings <- createIterativeImputer(
    missingThreshold = 0.8,
    method = "pmm",
    methodSettings = list(
      pmm = list(
        k = k,
        iterations = 1
      )
    )
  )
  attr(settings, "missingInfo") <- data.frame(covariateId = 100, missing = 0.5)
  attr(settings, "imputer") <- list(
    "100" = list(
      intercept = 0,
      coefficients = data.frame(covariateId = 200, values = 1),
      predictions = data.frame(rowId = c(1, 3), prediction = c(0.2, 0.4))
    )
  )
  settings
}

test_that("iterativeImpute apply path preserves non-imputed continuous covariates", {
  skip_if_not_installed("glmnet")
  trainData <- makeApplyImputeDataForParityTest()
  settings <- createIterativeImputer(
    missingThreshold = 0.3,
    method = "pmm",
    methodSettings = list(
      pmm = list(
        k = 1,
        iterations = 1
      )
    )
  )
  attr(settings, "missingInfo") <- data.frame(
    covariateId = integer(0),
    missing = numeric(0)
  )

  testthat::local_mocked_bindings(
    copyAndromeda = function(x) x,
    .package = "Andromeda"
  )

  out <- PatientLevelPrediction:::iterativeImpute(
    trainData = trainData,
    featureEngineeringSettings = settings,
    done = TRUE
  )

  preservedRows <- out$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 300) %>%
    dplyr::select("rowId") %>%
    dplyr::distinct() %>%
    dplyr::collect()
  expect_equal(nrow(preservedRows), nrow(trainData$labels))
})

test_that("iterativeImpute apply path uses configured PMM k and keeps covariateData copyable", {
  skip_if_not_installed("glmnet")
  trainData <- makeApplyImputeDataForKTest()
  settings <- makeApplyImputerSettingsForKTest(k = 3)

  observed <- new.env(parent = emptyenv())
  observed$k <- NULL
  originalPmmPredict <- PatientLevelPrediction:::pmmPredict

  testthat::local_mocked_bindings(
    pmmPredict = function(data, k = 5, imputer) {
      observed$k <- k
      originalPmmPredict(data = data, k = k, imputer = imputer)
    },
    .package = "PatientLevelPrediction"
  )

  out <- PatientLevelPrediction:::iterativeImpute(
    trainData = trainData,
    featureEngineeringSettings = settings,
    done = TRUE
  )

  expect_equal(observed$k, 3)
  expect_false(any(grepl("^dbplyr_", names(out$covariateData))))
  expect_no_error(Andromeda::copyAndromeda(out$covariateData))
})

test_that("findNearestSortedIndices matches full-distance ranking", {
  predsObs <- c(-3.2, -0.6, 0.4, 2.3, 5.9, 8.1)
  targets <- c(-1.7, 0.9, 4.8)
  sortedOrder <- order(predsObs)
  sortedPreds <- predsObs[sortedOrder]

  for (target in targets) {
    for (k in c(1, 2, 3, 4)) {
      nearestSorted <- PatientLevelPrediction:::findNearestSortedIndices(
        sortedPredictions = sortedPreds,
        prediction = target,
        k = k
      )
      nearest <- sortedOrder[nearestSorted]
      baseline <- order(abs(predsObs - target))[seq_len(k)]
      expect_setequal(nearest, baseline)
    }
  }
})

test_that("samplePmmDonors with k=1 matches nearest donor deterministically", {
  predsObs <- c(0.1, 1.4, 2.2, 4.9)
  donorMapping <- c(10, 20, 30, 40)
  predsTarget <- c(0.2, 2.6, 4.2)

  expected <- vapply(predsTarget, function(target) {
    donorMapping[order(abs(predsObs - target))[1]]
  }, numeric(1))

  actual <- PatientLevelPrediction:::samplePmmDonors(
    predsObs = predsObs,
    donorMapping = donorMapping,
    predsTarget = predsTarget,
    k = 1
  )

  expect_equal(actual, expected)
})

test_that("pmmFit runs on a minimal valid dataset", {
  skip_if_not_installed("glmnet")
  pmmData <- makePmmData(nObs = 12, nMiss = 4)

  expect_no_error(
    PatientLevelPrediction:::pmmFit(pmmData, k = 1)
  )
})

test_that("scalePmmCovariates normalizes continuous values", {
  xMiss <- dplyr::tibble(
    rowId = c(1, 2, 3, 4, 5, 6),
    covariateId = c(11, 11, 11, 22, 22, 22),
    covariateValue = c(2, 4, 6, 0, 1, 0)
  )

  scaled <- PatientLevelPrediction:::scalePmmCovariates(xMiss) %>%
    dplyr::collect()

  continuous <- scaled %>%
    dplyr::filter(.data$covariateId == 11) %>%
    dplyr::pull(.data$covariateValue)
  binary <- scaled %>%
    dplyr::filter(.data$covariateId == 22) %>%
    dplyr::pull(.data$covariateValue)

  expect_equal(min(continuous), 0)
  expect_equal(max(continuous), 1)
  expect_equal(binary, c(0, 1, 0))
})

test_that("pmmFit clamps k to available donors", {
  skip_if_not_installed("glmnet")
  pmmData <- makePmmData(nObs = 12, nMiss = 6)

  withr::with_seed(
    42,
    results <- PatientLevelPrediction:::pmmFit(pmmData, k = 50)
  )

  expect_equal(nrow(results$imputedValues), 6)
  expect_false(anyNA(results$imputedValues$imputedValue))
})

test_that("pmmFit handles empty xMiss without failing", {
  skip_if_not_installed("glmnet")
  pmmData <- makePmmData(nObs = 12, nMiss = 1)
  pmmData$xMiss <- pmmData$xMiss %>%
    dplyr::filter(FALSE)

  results <- PatientLevelPrediction:::pmmFit(pmmData, k = 1)

  expect_equal(nrow(results$imputedValues), 0)
  expect_true(all(c("intercept", "coefficients", "predictions") %in% names(results$model)))
})

test_that("PMM train/test imputations stay in observed range and preserve variance", {
  skip_if_not_installed("glmnet")
  pmmData <- withr::with_seed(1, {
    nObs <- 120
    nMissTrain <- 50

    x1 <- runif(nObs, min = -2, max = 2)
    x2 <- rnorm(nObs)
    y <- 1 + 2 * x1 - 1.5 * x2 + rnorm(nObs, sd = 0.5)

    xObs <- data.frame(
      rowId = rep(seq_len(nObs), each = 2),
      covariateId = rep(c(101, 102), times = nObs),
      covariateValue = c(x1, x2)
    )
    yObs <- data.frame(rowId = seq_len(nObs), y = y)

    xMissTrain <- data.frame(
      rowId = rep(1000 + seq_len(nMissTrain), each = 2),
      covariateId = rep(c(101, 102), times = nMissTrain),
      covariateValue = c(
        runif(nMissTrain, min = -2, max = 2),
        rnorm(nMissTrain)
      )
    )
    Andromeda::andromeda(xObs = xObs, xMiss = xMissTrain, yObs = yObs)
  })

  fitResults <- withr::with_seed(111, PatientLevelPrediction:::pmmFit(pmmData, k = 5))

  nMissTest <- 60
  testXMiss <- withr::with_seed(222, data.frame(
    rowId = rep(2000 + seq_len(nMissTest), each = 2),
    covariateId = rep(c(101, 102), times = nMissTest),
    covariateValue = c(
      runif(nMissTest, min = -2, max = 2),
      rnorm(nMissTest)
    )
  ))

  testResults <- withr::with_seed(333, {
    PatientLevelPrediction:::pmmPredict(
      data = list(xMiss = testXMiss),
      k = 5,
      imputer = fitResults$model
    )
  })

  observedY <- pmmData$yObs %>% dplyr::pull(.data$y)
  observedRange <- range(observedY)
  trainImputed <- fitResults$imputedValues$imputedValue
  testImputed <- testResults$imputedValues$imputedValue

  # PMM draws donor values from observed y-values.
  expect_true(all(trainImputed %in% observedY))
  expect_true(all(testImputed %in% observedY))

  # Imputed values should remain within plausible observed range.
  expect_gte(min(testImputed), observedRange[1])
  expect_lte(max(testImputed), observedRange[2])

  # Train vs test imputed distributions should both retain non-trivial variance.
  trainVar <- stats::var(trainImputed)
  testVar <- stats::var(testImputed)
  expect_gt(trainVar, 0)
  expect_gt(testVar, 0)

  # Keep a loose variance-ratio bound to catch severe shrinkage/explosion.
  varRatio <- testVar / trainVar
  expect_gt(varRatio, 0.5)
  expect_lt(varRatio, 2)
})

test_that("pmmFit and pmmPredict validate k", {
  skip_if_not_installed("glmnet")
  pmmData <- makePmmData(nObs = 12, nMiss = 2)

  invalidK <- list(0, -1, 2.5, NA_real_, c(1, 2))
  for (k in invalidK) {
    expect_error(
      PatientLevelPrediction:::pmmFit(pmmData, k = k),
      "k must be a single positive integer"
    )
  }

  predictData <- list(
    xMiss = dplyr::tibble(
      rowId = c(1001, 1002),
      covariateId = c(11, 11),
      covariateValue = c(0.1, 0.2)
    )
  )
  imputer <- list(
    intercept = 0,
    coefficients = data.frame(covariateId = 11, values = 1),
    predictions = data.frame(rowId = 1:3, prediction = c(0.2, 0.5, 0.8))
  )
  for (k in invalidK) {
    expect_error(
      PatientLevelPrediction:::pmmPredict(predictData, k = k, imputer = imputer),
      "k must be a single positive integer"
    )
  }
})

test_that("appendMissingIndicatorMetadata normalizes IDs before duplicate checks", {
  outputData <- list(covariateData = Andromeda::andromeda())

  outputData$covariateData$analysisRef <- dplyr::tibble(
    analysisId = "1666 ",
    analysisName = "existing",
    domainId = "feature engineering",
    startDay = NA_real_,
    endDay = NA_real_,
    isBinary = "Y",
    missingMeansZero = "Y"
  )
  outputData$covariateData$covariateRef <- dplyr::tibble(
    covariateId = "2666 ",
    covariateName = "existing covariate",
    analysisId = "1666 ",
    conceptId = 1
  )

  indicatorInfo <- list(
    map = data.frame(sourceCovariateId = 666, indicatorCovariateId = 2666),
    analysisRef = data.frame(
      analysisId = 1666,
      analysisName = "existing",
      domainId = "feature engineering",
      startDay = NA_real_,
      endDay = NA_real_,
      isBinary = "Y",
      missingMeansZero = "Y"
    ),
    covariateRef = data.frame(
      covariateId = 2666,
      covariateName = "existing covariate",
      analysisId = 1666,
      conceptId = 1
    )
  )

  updated <- PatientLevelPrediction:::appendMissingIndicatorMetadata(outputData, indicatorInfo)

  expect_equal(nrow(dplyr::collect(updated$covariateData$analysisRef)), 1)
  expect_equal(nrow(dplyr::collect(updated$covariateData$covariateRef)), 1)
})
