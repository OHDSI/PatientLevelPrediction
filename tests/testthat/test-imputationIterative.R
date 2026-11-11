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

  imputed <- covariateRowsOf(applied, 100)
  expect_equal(imputed$rowId, 1:3)
  expect_equal(imputed$covariateValue, c(10, 30, 30))
  expect_equal(covariateRowsOf(applied, 1100)$rowId, 1:3)
  expect_equal(covariateRowsOf(applied, 200)$covariateValue, c(10, 30, 30))
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
  expect_equal(covariateRowsOf(applied, 200)$covariateValue, c(10, 30, 20))

  absent <- makeImputationFixture(
    rowIds = 1:3,
    covariates = data.frame(rowId = 1:3, covariateId = 100, covariateValue = c(10, 30, 30)),
    numericIds = c(100, 200)
  )
  withr::defer(Andromeda::close(absent$covariateData))
  appliedAbsent <- PatientLevelPrediction:::iterativeImpute(absent, settings, done = TRUE)
  withr::defer(Andromeda::close(appliedAbsent$covariateData))
  expect_equal(covariateRowsOf(appliedAbsent, 200)$covariateValue, c(20, 20, 20))
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

  expect_equal(covariateRowsOf(applied, 300)$rowId, 2)
  # the sparse covariate still informs the PMM prediction for row 2
  expect_equal(covariateRowsOf(applied, 100)$covariateValue, c(10, 30, 30))
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
    covariateRowsOf(applied, 100)
  }

  sortedRows <- imputeWithLabels(c(1, 2, 3))
  permutedRows <- imputeWithLabels(c(3, 1, 2))

  expect_equal(sortedRows$covariateValue, c(10, 30, 30))
  expect_equal(permutedRows, sortedRows)
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
