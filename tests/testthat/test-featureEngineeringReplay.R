replayedRows <- function(data) {
  as.data.frame(data$covariateData$covariates) %>%
    dplyr::arrange(.data$rowId, .data$covariateId)
}

# Fits one step on toy training data, saves its fitted state with the writer
# savePlpModel() uses and with the four-decimal writer of earlier versions,
# loads both back, and returns the rows replayed on the same new data from
# memory and from each loaded file. applyFeatureEngineering() copies its input,
# so one new dataset serves all three replays.
replayInMemoryAndReloaded <- function(settings) {
  # the iterative imputer's glmnet fit uses random folds
  withr::local_seed(42)
  train <- createMissingData(makeToyImputationData(n = 120, seed = 1), 0.3)
  withr::defer(Andromeda::close(train$covariateData))
  engineered <- suppressWarnings(featureEngineer(train, settings))
  withr::defer(Andromeda::close(engineered$covariateData))
  saved <- attr(engineered$covariateData, "metaData")$featureEngineering

  reload <- function(writer) {
    jsonFile <- tempfile(fileext = ".json")
    on.exit(unlink(jsonFile))
    writer(list(featureEngineering = saved), jsonFile)
    ParallelLogger::loadSettingsFromJson(jsonFile)$featureEngineering
  }
  newData <- createMissingData(makeToyImputationData(n = 60, seed = 2), 0.4, test = TRUE)
  withr::defer(Andromeda::close(newData$covariateData))
  replay <- function(steps) {
    replayed <- applyFeatureEngineering(newData, steps)
    on.exit(Andromeda::close(replayed$covariateData))
    replayedRows(replayed)
  }
  list(
    inMemory = replay(saved),
    reloaded = replay(reload(saveSettingsToJsonFullPrecision)),
    # earlier versions saved preprocessing with ParallelLogger::saveSettingsToJson(),
    # which wrote the learned tables row by row with four decimals
    legacy = replay(reload(ParallelLogger::saveSettingsToJson))
  )
}

fittedStepSettings <- function() {
  list(
    simpleMean = createSimpleImputer(method = "mean", missingThreshold = 0.5, addMissingIndicator = TRUE),
    simpleMedian = createSimpleImputer(method = "median", missingThreshold = 0.5),
    iterative = createIterativeImputer(
      method = "pmm", missingThreshold = 0.5, addMissingIndicator = TRUE,
      methodSettings = list(pmm = list(k = 1, iterations = 1))
    ),
    minMax = createNormalizer("minmax"),
    robust = createNormalizer("robust", settings = list(clip = TRUE)),
    rareNone = createRareFeatureRemover(threshold = 0.01)
  )
}

test_that("fitted feature engineering replays after saving and loading, now and from earlier versions", {
  skip_if_not_installed("glmnet")
  for (name in names(fittedStepSettings())) {
    result <- replayInMemoryAndReloaded(fittedStepSettings()[[name]])
    # saved at full precision: exactly the same rows
    expect_equal(result$reloaded, result$inMemory, tolerance = 0, info = name)
    # saved by earlier versions: same rows, values within the four-decimal rounding
    expect_equal(result$legacy$rowId, result$inMemory$rowId, info = name)
    expect_equal(result$legacy$covariateId, result$inMemory$covariateId, info = name)
    # predictive mean matching picks the donor with the nearest predicted value,
    # so rounded predictions can pick another donor; the other steps only lose precision
    if (name != "iterative") {
      expect_equal(result$legacy$covariateValue, result$inMemory$covariateValue,
        tolerance = 1e-3, info = name
      )
    }
  }
})

test_that("steps whose learned tables are empty replay after saving and loading", {
  binaryOnly <- function(n) {
    covariateData <- Andromeda::andromeda(
      covariates = data.frame(rowId = seq_len(n), covariateId = 20, covariateValue = 1),
      covariateRef = data.frame(covariateId = 20, covariateName = "b", analysisId = 20, conceptId = 0),
      analysisRef = data.frame(
        analysisId = 20, analysisName = "b", domainId = "x", startDay = 0, endDay = 0,
        isBinary = "Y", missingMeansZero = "Y"
      )
    )
    class(covariateData) <- "CovariateData"
    attr(covariateData, "metaData") <- list()
    structure(
      list(
        labels = data.frame(rowId = seq_len(n), outcomeCount = 0),
        folds = data.frame(rowId = seq_len(n), index = 1),
        covariateData = covariateData
      ),
      class = "plpData"
    )
  }
  for (settings in list(
    createNormalizer("minmax"),
    createNormalizer("robust", settings = list(clip = FALSE)),
    createSimpleImputer(method = "mean", missingThreshold = 0.5)
  )) {
    train <- binaryOnly(40)
    engineered <- suppressWarnings(featureEngineer(train, settings))
    jsonFile <- tempfile(fileext = ".json")
    saveSettingsToJsonFullPrecision(
      list(featureEngineering = attr(engineered$covariateData, "metaData")$featureEngineering),
      jsonFile
    )
    loaded <- ParallelLogger::loadSettingsFromJson(jsonFile)$featureEngineering
    newData <- binaryOnly(20)
    replayed <- applyFeatureEngineering(newData, loaded)
    expect_equal(nrow(replayedRows(replayed)), 20, info = attr(settings, "fun"))
    Andromeda::close(train$covariateData)
    Andromeda::close(newData$covariateData)
    Andromeda::close(replayed$covariateData)
    unlink(jsonFile)
  }
})

test_that("PMM models without predictors replay from files saved by earlier versions", {
  skip_if_not_installed("glmnet")
  train <- createMissingData(makeToyImputationData(n = 120, seed = 1), 0.3)
  withr::defer(Andromeda::close(train$covariateData))
  engineered <- withr::with_seed(1, suppressWarnings(featureEngineer(
    train,
    createIterativeImputer(
      method = "pmm", missingThreshold = 0.5,
      methodSettings = list(pmm = list(k = 1, iterations = 1))
    )
  )))
  withr::defer(Andromeda::close(engineered$covariateData))
  saved <- attr(engineered$covariateData, "metaData")$featureEngineering
  # glmnet can select no predictors; the imputer then has an empty coefficient table
  imputer <- attr(saved[[1]]$settings$featureEngineeringSettings, "imputer")
  for (variable in names(imputer)) {
    imputer[[variable]]$coefficients <- imputer[[variable]]$coefficients[0, , drop = FALSE]
  }
  attr(saved[[1]]$settings$featureEngineeringSettings, "imputer") <- imputer

  jsonFile <- tempfile(fileext = ".json")
  withr::defer(unlink(jsonFile))
  ParallelLogger::saveSettingsToJson(list(featureEngineering = saved), jsonFile)
  legacy <- ParallelLogger::loadSettingsFromJson(jsonFile)$featureEngineering

  newData <- createMissingData(makeToyImputationData(n = 60, seed = 2), 0.4, test = TRUE)
  withr::defer(Andromeda::close(newData$covariateData))
  replay <- function(steps) {
    replayed <- withr::with_seed(2, suppressWarnings(applyFeatureEngineering(newData, steps)))
    on.exit(Andromeda::close(replayed$covariateData))
    as.data.frame(replayed$covariateData$covariates) %>% dplyr::arrange(.data$rowId, .data$covariateId)
  }
  inMemory <- replay(saved)
  fromLegacy <- replay(legacy)
  expect_equal(fromLegacy$rowId, inMemory$rowId)
  expect_equal(fromLegacy$covariateId, inMemory$covariateId)
  # every patient of the new data has the imputed feature
  expect_setequal(fromLegacy$rowId[fromLegacy$covariateId == 666], newData$labels$rowId)
})
