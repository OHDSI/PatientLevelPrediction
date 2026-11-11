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

    expect_equal(nrow(covariateRowsOf(applied, 10)), 20)
    expect_s3_class(applied, "plpData")
    expect_equal(colnames(applied$cohorts), "rowId")
    # the caller's data is left as it was
    expect_equal(nrow(covariateRowsOf(test, 10)), 18)
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
