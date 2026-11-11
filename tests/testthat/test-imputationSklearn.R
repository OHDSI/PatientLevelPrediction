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

test_that("sklearnIterativeImpute applies its saved steps without the session's Python object", {
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
  expect_false(is.null(attr(settings, "sklearnImputerSteps")))

  inSession <- sklearnIterativeImpute(createMissingData(testDataSmall, 0.4, test = TRUE), settings, done = TRUE)
  # as after loading the model in a new session
  settings$sklearnImputerKey <- "missing-key"
  withoutPython <- sklearnIterativeImpute(createMissingData(testDataSmall, 0.4, test = TRUE), settings, done = TRUE)

  rows <- function(data) {
    as.data.frame(data$covariateData$covariates) %>% dplyr::arrange(.data$rowId, .data$covariateId)
  }
  expect_equal(rows(withoutPython), rows(inSession), tolerance = 0)
})

test_that("sklearn imputer steps applied in R match sklearn's transform", {
  skip_on_cran()
  skip_if_no_sklearn_iterative()
  X <- withr::with_seed(3, {
    n <- 200
    x <- cbind(stats::rnorm(n), stats::rnorm(n, 5), stats::runif(n), stats::rbinom(n, 1, 0.3), stats::rnorm(n, -2, 3))
    x[, 2] <- x[, 2] + 2 * x[, 1]
    for (j in c(1, 2, 3, 5)) x[sample(n, 50), j] <- NA
    x
  })
  newX <- withr::with_seed(4, {
    x <- X[sample(nrow(X), 80), ]
    x[sample(length(x), 60)] <- NA
    x
  })
  variants <- list(
    default = list(),
    oneNearestFeature = list(nNearestFeatures = 1),
    clipped = list(minValue = -1, maxValue = 4),
    skipComplete = list(skipComplete = TRUE),
    median = list(initialStrategy = "median"),
    randomOrder = list(imputationOrder = "random"),
    oneIteration = list(maxIter = 1)
  )
  for (name in names(variants)) {
    methodSettings <- normalizeSklearnIterativeMethodSettings(variants[[name]])
    imputer <- createSklearnIterativeImputerObject(methodSettings)
    invisible(suppressWarnings(imputer$fit_transform(X)))
    steps <- extractSklearnImputerSteps(imputer, covariateIds = 1:5, methodSettings = methodSettings)

    jsonFile <- tempfile(fileext = ".json")
    saveSettingsToJsonFullPrecision(list(steps = steps), jsonFile)
    loaded <- ParallelLogger::loadSettingsFromJson(jsonFile)$steps
    unlink(jsonFile)

    expected <- reticulate::py_to_r(imputer$transform(newX))
    expect_equal(applySklearnImputerSteps(newX, steps), expected, tolerance = 1e-12, info = name)
    expect_equal(applySklearnImputerSteps(newX, loaded), applySklearnImputerSteps(newX, steps),
      tolerance = 0, info = name
    )
  }
})

test_that("sklearn imputers drawing from the posterior can only be applied in their session", {
  skip_on_cran()
  skip_if_no_sklearn_iterative()
  train <- createMissingData(makeToyImputationData(n = 120, seed = 1), 0.3)
  on.exit(Andromeda::close(train$covariateData), add = TRUE)
  imputer <- createSklearnIterativeImputer(
    missingThreshold = 0.5,
    methodSettings = list(samplePosterior = TRUE, maxIter = 2)
  )
  fitted <- suppressWarnings(sklearnIterativeImpute(train, imputer, done = FALSE))
  settings <- fittedFeatureEngineeringSettings(fitted, "sklearnIterativeImpute")
  expect_null(attr(settings, "sklearnImputerSteps"))

  settings$sklearnImputerKey <- "missing-key"
  newData <- createMissingData(makeToyImputationData(n = 60, seed = 2), 0.4, test = TRUE)
  on.exit(Andromeda::close(newData$covariateData), add = TRUE)
  expect_error(
    sklearnIterativeImpute(newData, settings, done = TRUE),
    "samplePosterior = TRUE"
  )
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

  expect_equal(nrow(covariateRowsOf(imputed, 10)), 0)
})
