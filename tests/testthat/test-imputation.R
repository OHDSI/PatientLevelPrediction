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

  testSettings <- fittedFeatureEngineeringSettings(imputedData, "sklearnIterativeImpute")
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
  settings <- fittedFeatureEngineeringSettings(imputedData, "sklearnIterativeImpute")
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

  # the imputer changes its input, so take the snapshot first
  missingCovariates <- missingData$covariateData$covariates %>%
    dplyr::collect()
  imputedData <- iterativeImpute(missingData, imputer, done = FALSE)

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

  testSettings <- fittedFeatureEngineeringSettings(imputedData, "iterativeImpute")
  indicatorInfo <- attr(testSettings, "missingIndicatorInfo")
  expect_true(!is.null(indicatorInfo))
  expect_true(!is.null(indicatorInfo$map))
  expect_true(nrow(indicatorInfo$map) > 0)
  fakeVariableMap <- indicatorInfo$map %>%
    dplyr::filter(.data$sourceCovariateId == 666)
  expect_equal(nrow(fakeVariableMap), 1)

  missingTestData <- createMissingData(testDataSmall, 0.4, test = TRUE)
  # the imputer changes its input, so take the snapshot first
  missingTestCovariates <- missingTestData$covariateData$covariates %>%
    dplyr::collect()
  imputedTestData <- iterativeImpute(missingTestData, testSettings, done = TRUE)

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

makeImputationFixture <- function(rowIds,
                                  covariates,
                                  numericIds,
                                  binaryIds = numeric(0),
                                  analysisIds = c(numericIds, binaryIds),
                                  zeroMeansMissingIds = numeric(0),
                                  folds = FALSE) {
  covariateIds <- c(numericIds, binaryIds)
  analysisRef <- data.frame(
    analysisId = analysisIds,
    analysisName = paste0("analysis", analysisIds),
    domainId = "measurement",
    startDay = NA_real_,
    endDay = NA_real_,
    isBinary = ifelse(covariateIds %in% numericIds, "N", "Y"),
    missingMeansZero = ifelse(
      covariateIds %in% c(binaryIds, zeroMeansMissingIds),
      "Y",
      "N"
    )
  ) %>% dplyr::distinct()
  covariateData <- Andromeda::andromeda(
    covariates = covariates,
    covariateRef = data.frame(
      covariateId = covariateIds,
      covariateName = paste0("feature", covariateIds),
      analysisId = analysisIds,
      conceptId = covariateIds
    ),
    analysisRef = analysisRef
  )
  class(covariateData) <- "CovariateData"
  attr(covariateData, "metaData") <- list()
  data <- list(labels = data.frame(rowId = rowIds, outcomeCount = 0))
  if (folds) {
    data$folds <- data.frame(rowId = rowIds, index = 1L)
  }
  data$covariateData <- covariateData
  class(data) <- "plpData"
  attr(data, "metaData") <- list()
  data
}

covariateRows <- function(data, covariateId) {
  data$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == !!covariateId) %>%
    dplyr::collect() %>%
    dplyr::arrange(.data$rowId)
}


makeSavedPmmSettings <- function() {
  settings <- createIterativeImputer(
    missingThreshold = 0.8,
    methodSettings = list(pmm = list(k = 1, iterations = 1, alpha = 1))
  )
  attr(settings, "missingInfo") <- data.frame(
    covariateId = c(100, 200),
    missing = c(0.25, 0)
  )
  attr(settings, "initialValues") <- data.frame(
    covariateId = c(100, 200),
    initialValue = c(20, 20)
  )
  attr(settings, "imputer") <- list(
    "100" = list(
      intercept = 0,
      coefficients = data.frame(covariateId = 200, values = 1),
      predictions = data.frame(
        rowId = c(101, 102),
        prediction = c(10, 30),
        observedValue = c(10, 30)
      )
    )
  )
  settings
}

test_that("createMissingIndicatorInfo avoids existing and newly allocated ids", {
  data <- makeImputationFixture(
    rowIds = 1:2,
    covariates = data.frame(
      rowId = 1:2,
      covariateId = c(10010, 11010),
      covariateValue = 1
    ),
    numericIds = c(10010, 11010),
    analysisIds = c(10, 10)
  )
  withr::defer(Andromeda::close(data$covariateData))

  info <- PatientLevelPrediction:::createMissingIndicatorInfo(data, c(10010, 11010))

  expect_equal(anyDuplicated(info$map$indicatorCovariateId), 0)
  expect_false(any(info$map$indicatorCovariateId %in% c(10010, 11010)))
  expect_equal(nrow(info$covariateRef), 2)
})

test_that("createMissingIndicatorInfo keeps the default ids when nothing collides", {
  data <- makeImputationFixture(
    rowIds = 1:2,
    covariates = data.frame(
      rowId = 1:2,
      covariateId = c(1001010, 3005010),
      covariateValue = 1
    ),
    numericIds = c(1001010, 3005010),
    analysisIds = c(10, 10)
  )
  withr::defer(Andromeda::close(data$covariateData))

  info <- PatientLevelPrediction:::createMissingIndicatorInfo(data, c(1001010, 3005010))

  expect_equal(
    info$map$indicatorCovariateId[match(c(1001010, 3005010), info$map$sourceCovariateId)],
    c(1002010, 3006010)
  )
})

test_that("allocateIndicatorCovariateIds resolves chained collisions", {
  allocated <- PatientLevelPrediction:::allocateIndicatorCovariateIds(
    candidateIds = c(2000, 2000, 3000),
    usedIds = c(2000, 4000)
  )
  expect_equal(anyDuplicated(allocated), 0)
  expect_false(any(allocated %in% c(2000, 4000)))
  expect_equal(allocated[3], 3000)
})

test_that("simpleImpute keeps missing indicators separate from colliding covariates", {
  train <- makeImputationFixture(
    rowIds = 1:3,
    covariates = data.frame(
      rowId = c(1, 3, 1, 2, 3),
      covariateId = c(10010, 10010, 11010, 11010, 11010),
      covariateValue = c(2, 4, 7, 8, 9)
    ),
    numericIds = c(10010, 11010),
    analysisIds = c(10, 10),
    folds = TRUE
  )
  withr::defer(Andromeda::close(train$covariateData))
  settings <- createSimpleImputer(
    method = "mean",
    missingThreshold = 0.5,
    addMissingIndicator = TRUE
  )

  fitted <- PatientLevelPrediction:::simpleImpute(train, settings, done = FALSE)
  withr::defer(Andromeda::close(fitted$covariateData))

  expect_equal(covariateRows(fitted, 11010)$covariateValue, c(7, 8, 9))
  sparse <- toSparseM(fitted, fitted$labels)
  expect_equal(ncol(sparse$dataMatrix), 3)
})

test_that("iterativeImpute keeps missing indicators separate from colliding covariates", {
  skip_if_not_installed("glmnet")
  x <- 1:20
  train <- makeImputationFixture(
    rowIds = x,
    covariates = rbind(
      data.frame(rowId = 1:18, covariateId = 10010, covariateValue = 2 * x[1:18] + 1),
      data.frame(rowId = x, covariateId = 11010, covariateValue = x),
      data.frame(rowId = x, covariateId = 20, covariateValue = x %% 2)
    ),
    numericIds = c(10010, 11010),
    binaryIds = 20,
    analysisIds = c(10, 10, 20),
    folds = TRUE
  )
  withr::defer(Andromeda::close(train$covariateData))
  settings <- createIterativeImputer(
    missingThreshold = 0.5,
    methodSettings = list(pmm = list(k = 1, iterations = 1, alpha = 1)),
    addMissingIndicator = TRUE
  )

  fitted <- withr::with_seed(42, {
    PatientLevelPrediction:::iterativeImpute(train, settings, done = FALSE)
  })
  withr::defer(Andromeda::close(fitted$covariateData))

  saved <- fittedFeatureEngineeringSettings(fitted, "iterativeImpute")
  indicatorIds <- attr(saved, "missingIndicatorInfo")$map$indicatorCovariateId
  expect_false(any(indicatorIds %in% c(10010, 11010)))
  expect_equal(covariateRows(fitted, 11010)$covariateValue, x)
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
    labRows <- covariateRows(applied, 10)
    expect_equal(labRows$rowId, c(101, 102))
    expect_equal(labRows$covariateValue, c(5, expected))
    indicatorRows <- covariateRows(applied, indicatorId)
    expect_equal(indicatorRows$rowId, 102)
    expect_equal(indicatorRows$covariateValue, 1)
  }
})

test_that("iterativeImpute apply path imputes a feature absent from the whole batch", {
  skip_if_not_installed("glmnet")
  settings <- makeSavedPmmSettings()
  settings$addMissingIndicator <- TRUE
  attr(settings, "missingIndicatorInfo") <- list(
    map = data.frame(sourceCovariateId = 100, indicatorCovariateId = 1100),
    covariateRef = data.frame(
      covariateId = 1100,
      covariateName = "feature100 missing indicator",
      analysisId = 1100,
      conceptId = 100
    ),
    analysisRef = data.frame(
      analysisId = 1100,
      analysisName = "indicator",
      domainId = "feature engineering",
      startDay = NA_real_,
      endDay = NA_real_,
      isBinary = "Y",
      missingMeansZero = "Y"
    )
  )
  test <- makeImputationFixture(
    rowIds = 1:3,
    covariates = data.frame(rowId = 1:3, covariateId = 200, covariateValue = c(10, 30, 30)),
    numericIds = c(100, 200)
  )
  withr::defer(Andromeda::close(test$covariateData))

  applied <- withr::with_seed(42, {
    PatientLevelPrediction:::iterativeImpute(test, settings, done = TRUE)
  })
  withr::defer(Andromeda::close(applied$covariateData))

  imputed <- covariateRows(applied, 100)
  expect_equal(imputed$rowId, 1:3)
  expect_equal(imputed$covariateValue, c(10, 30, 30))
  expect_equal(covariateRows(applied, 1100)$rowId, 1:3)
  expect_equal(covariateRows(applied, 200)$covariateValue, c(10, 30, 30))
})

test_that("iterativeImpute apply path keeps training mean for features without a PMM model", {
  skip_if_not_installed("glmnet")
  settings <- makeSavedPmmSettings()

  partiallyAbsent <- makeImputationFixture(
    rowIds = 1:3,
    covariates = rbind(
      data.frame(rowId = 1:3, covariateId = 100, covariateValue = c(10, 30, 30)),
      data.frame(rowId = 1:2, covariateId = 200, covariateValue = c(10, 30))
    ),
    numericIds = c(100, 200)
  )
  withr::defer(Andromeda::close(partiallyAbsent$covariateData))
  applied <- PatientLevelPrediction:::iterativeImpute(partiallyAbsent, settings, done = TRUE)
  withr::defer(Andromeda::close(applied$covariateData))
  expect_equal(covariateRows(applied, 200)$covariateValue, c(10, 30, 20))

  absent <- makeImputationFixture(
    rowIds = 1:3,
    covariates = data.frame(rowId = 1:3, covariateId = 100, covariateValue = c(10, 30, 30)),
    numericIds = c(100, 200)
  )
  withr::defer(Andromeda::close(absent$covariateData))
  appliedAbsent <- PatientLevelPrediction:::iterativeImpute(absent, settings, done = TRUE)
  withr::defer(Andromeda::close(appliedAbsent$covariateData))
  expect_equal(covariateRows(appliedAbsent, 200)$covariateValue, c(20, 20, 20))
})

test_that("iterativeImpute apply path leaves untrained sparse numeric covariates alone", {
  skip_if_not_installed("glmnet")
  settings <- makeSavedPmmSettings()
  attr(settings, "imputer")[["100"]]$coefficients <- data.frame(covariateId = 300, values = 1)
  test <- makeImputationFixture(
    rowIds = 1:3,
    covariates = rbind(
      data.frame(rowId = c(1, 3), covariateId = 100, covariateValue = c(10, 30)),
      data.frame(rowId = 1:3, covariateId = 200, covariateValue = 1),
      data.frame(rowId = 2, covariateId = 300, covariateValue = 30)
    ),
    numericIds = c(100, 200, 300),
    zeroMeansMissingIds = 300
  )
  withr::defer(Andromeda::close(test$covariateData))

  applied <- withr::with_seed(42, {
    PatientLevelPrediction:::iterativeImpute(test, settings, done = TRUE)
  })
  withr::defer(Andromeda::close(applied$covariateData))

  expect_equal(covariateRows(applied, 300)$rowId, 2)
  # the sparse covariate still informs the PMM prediction for row 2
  expect_equal(covariateRows(applied, 100)$covariateValue, c(10, 30, 30))
  allValues <- applied$covariateData$covariates %>% dplyr::collect()
  expect_false(anyNA(allValues$covariateValue))
})

test_that("iterativeImpute apply path selects missing rows by rowId, not label order", {
  skip_if_not_installed("glmnet")
  settings <- makeSavedPmmSettings()
  imputeWithLabels <- function(labelOrder) {
    test <- makeImputationFixture(
      rowIds = labelOrder,
      covariates = data.frame(
        rowId = c(1, 3, 1, 2, 3),
        covariateId = c(100, 100, 200, 200, 200),
        covariateValue = c(10, 30, 10, 30, 30)
      ),
      numericIds = c(100, 200)
    )
    withr::defer(Andromeda::close(test$covariateData))
    applied <- withr::with_seed(42, {
      PatientLevelPrediction:::iterativeImpute(test, settings, done = TRUE)
    })
    withr::defer(Andromeda::close(applied$covariateData))
    covariateRows(applied, 100)
  }

  sortedRows <- imputeWithLabels(c(1, 2, 3))
  permutedRows <- imputeWithLabels(c(3, 1, 2))

  expect_equal(sortedRows$covariateValue, c(10, 30, 30))
  expect_equal(permutedRows, sortedRows)
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
  expect_equal(nrow(covariateRows(simple, 10)), 0)

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
  expect_equal(nrow(covariateRows(iterative, 10)), 0)
})

test_that("sklearnIterativeImpute removes features above the threshold when patients have no covariates", {
  skip_on_cran()
  skip_if_no_sklearn_iterative()
  data <- makeImputationFixture(
    rowIds = 1:10,
    covariates = data.frame(rowId = 1:6, covariateId = 10, covariateValue = c(1, 2, 3, 4, 5, 6)),
    numericIds = 10,
    folds = TRUE
  )
  withr::defer(Andromeda::close(data$covariateData))
  settings <- createSklearnIterativeImputer(missingThreshold = 0.3)

  imputed <- PatientLevelPrediction:::sklearnIterativeImpute(data, settings, done = FALSE)
  withr::defer(Andromeda::close(imputed$covariateData))

  expect_equal(nrow(covariateRows(imputed, 10)), 0)
})

test_that("predictPlp imputes the requested population, including patients outside labels", {
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
  settings <- createSimpleImputer(method = "mean", missingThreshold = 0.5)
  fitted <- PatientLevelPrediction:::simpleImpute(train, settings, done = FALSE)
  withr::defer(Andromeda::close(fitted$covariateData))
  featureEngineering <- attr(fitted$covariateData, "metaData")$featureEngineering

  model <- createGlmModel(
    coefficients = data.frame(covariateId = 10, coefficient = 1),
    intercept = 0,
    mapping = "linear",
    featureEngineering = featureEngineering
  )
  test <- makeImputationFixture(
    rowIds = 102,
    covariates = rbind(
      data.frame(rowId = 101, covariateId = 10, covariateValue = 5),
      data.frame(rowId = c(101, 102, 103), covariateId = 20, covariateValue = 1)
    ),
    numericIds = 10,
    binaryIds = 20
  )
  withr::defer(Andromeda::close(test$covariateData))
  population <- data.frame(rowId = c(101, 102, 103), outcomeCount = 0)

  prediction <- predictPlp(model, test, population)
  prediction <- prediction[order(prediction$rowId), ]

  expect_equal(prediction$rowId, c(101, 102, 103))
  expect_equal(prediction$value, c(5, 30, 30))
})

test_that("iterativeImpute leaves sparse missing-means-zero covariates alone at prediction", {
  addSparseCount <- function(data, rowIds) {
    Andromeda::appendToTable(
      data$covariateData$covariates,
      data.frame(rowId = rowIds, covariateId = 30, covariateValue = rep(c(1, 2, 3), length.out = length(rowIds)))
    )
    Andromeda::appendToTable(
      data$covariateData$covariateRef,
      data.frame(covariateId = 30, covariateName = "cohortCount", analysisId = 30, conceptId = -1)
    )
    Andromeda::appendToTable(
      data$covariateData$analysisRef,
      data.frame(
        analysisId = 30, analysisName = "cohort covariate", domainId = "cohort covariate",
        startDay = NA, endDay = NA, isBinary = "N", missingMeansZero = "Y"
      )
    )
    data
  }
  countRows <- function(data) {
    data$covariateData$covariates %>%
      dplyr::filter(.data$covariateId == 30) %>%
      dplyr::arrange(.data$rowId) %>%
      dplyr::collect() %>%
      as.data.frame()
  }

  trainData <- addSparseCount(
    createMissingData(makeToyImputationData(seed = 1), missingness = 0.2),
    rowIds = 1:40
  )
  class(trainData) <- "plpData"
  attr(trainData, "metaData") <- list()
  testData <- addSparseCount(
    createMissingData(makeToyImputationData(seed = 2), missingness = 0.2, test = TRUE),
    rowIds = 11:50
  )
  class(testData) <- "plpData"
  attr(testData, "metaData") <- list()
  expectedTest <- countRows(testData)

  imputer <- createIterativeImputer(
    missingThreshold = 0.9,
    method = "pmm",
    methodSettings = list(pmm = list(k = 1, iterations = 1))
  )
  imputedTrain <- iterativeImpute(trainData, imputer, done = FALSE)
  expect_equal(nrow(countRows(imputedTrain)), 40)

  fitted <- fittedFeatureEngineeringSettings(imputedTrain, "iterativeImpute")
  imputedTest <- iterativeImpute(testData, fitted, done = TRUE)
  expect_equal(countRows(imputedTest)$rowId, expectedTest$rowId)
  expect_equal(countRows(imputedTest)$covariateValue, expectedTest$covariateValue)

  # Features fitted by the imputer are still imputed at prediction.
  imputedFake <- imputedTest$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 666) %>%
    dplyr::collect()
  expect_equal(nrow(imputedFake), nrow(testData$labels))
})

test_that("imputers apply to prediction data with more than labels and covariateData", {
  fixture <- function(folds) {
    x <- 1:20
    makeImputationFixture(
      rowIds = x,
      covariates = rbind(
        data.frame(rowId = 1:18, covariateId = 10, covariateValue = 2 * x[1:18] + 1),
        data.frame(rowId = x, covariateId = 11, covariateValue = x),
        data.frame(rowId = x, covariateId = 12, covariateValue = x %% 5)
      ),
      numericIds = c(10, 11, 12),
      analysisIds = c(10, 11, 12),
      folds = folds
    )
  }
  plainPlpData <- function(data) {
    data$cohorts <- data.frame(rowId = data$labels$rowId)
    data$outcomes <- data.frame(rowId = integer(0))
    data$metaData <- list()
    data
  }
  checkImputer <- function(imputer, settings) {
    train <- fixture(folds = TRUE)
    withr::defer(Andromeda::close(train$covariateData))
    fitted <- imputer(train, settings, done = FALSE)

    test <- plainPlpData(fixture(folds = FALSE))
    withr::defer(Andromeda::close(test$covariateData))
    applied <- applyFeatureEngineering(
      test,
      attr(fitted$covariateData, "metaData")$featureEngineering
    )
    withr::defer(Andromeda::close(applied$covariateData))

    expect_equal(nrow(covariateRows(applied, 10)), 20)
    expect_s3_class(applied, "plpData")
    expect_equal(colnames(applied$cohorts), "rowId")
    # the caller's data is left as it was
    expect_equal(nrow(covariateRows(test, 10)), 18)
  }

  checkImputer(
    PatientLevelPrediction:::simpleImpute,
    createSimpleImputer(method = "mean", missingThreshold = 0.5)
  )
  skip_if_not_installed("glmnet")
  checkImputer(
    PatientLevelPrediction:::iterativeImpute,
    createIterativeImputer(
      method = "pmm",
      methodSettings = list(pmm = list(k = 2, iterations = 1)),
      missingThreshold = 0.5
    )
  )
})
