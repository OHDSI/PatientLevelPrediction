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

  expect_equal(covariateRowsOf(fitted, 11010)$covariateValue, c(7, 8, 9))
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
  expect_equal(covariateRowsOf(fitted, 11010)$covariateValue, x)
})
