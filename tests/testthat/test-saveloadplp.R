# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

test_that("savePlpDataError", {
  expect_error(savePlpData())
  expect_error(savePlpData(plpData = 1))
  expect_error(savePlpData(plpData = 1, file = "testing"))
})

if (internet && rlang::is_installed("Eunomia")) {
  oldCohorts <- plpData$cohorts
  oldOutcomes <- plpData$outcomes
  oldCovariates <- as.data.frame(plpData$covariateData$covariates)
  oldCovariateRef <- as.data.frame(plpData$covariateData$covariateRef)
}
test_that("savePlpData", {
  skip_if_not_installed("Eunomia")
  skip_if_offline()
  savePlpData(
    plpData = plpData,
    file = file.path(saveLoc, "saveDataTest"), overwrite = TRUE
  )
  testExist <- dir.exists(file.path(saveLoc, "saveDataTest"))
  expect_equal(testExist, TRUE)
})

test_that("loadPlpDataError", {
  expect_error(loadPlpData(file = "madeup/dffdf/testing"))
})

test_that("loadPlpData", {
  skip_if_not_installed("Eunomia")
  skip_if_offline()
  loadedData <- loadPlpData(file = file.path(saveLoc, "saveDataTest"))
  expect_identical(loadedData$cohorts, oldCohorts)
  expect_identical(loadedData$outcomes, oldOutcomes)
  expect_equal(
    as.data.frame(loadedData$covariateData$covariates),
    oldCovariates
  )
  expect_equal(
    as.data.frame(loadedData$covariateData$covariateRef),
    oldCovariateRef
  )
})

# add tests using simualted data...
test_that("print.plpData", {
  expect_equal(print.plpData(NULL), NULL)
})

test_that("summary.plpData", {
  expect_error(summary.plpData(NULL))
})

test_that("print.summary.plpData", {
  expect_error(print.summary.plpData(NULL))
})


test_that("savePlpModelError", {
  expect_error(savePlpModel(dirPath = NULL))
  expect_error(savePlpModel(plpModel = NULL))
  expect_error(savePlpModel(plpModel = NULL, dirPath = NULL))
})

test_that("loadPlpModelError", {
  expect_error(loadPlpModel(dirPath = NULL))
  expect_error(loadPlpModel(dirPath = "madeup.txt"))
})

test_that("move model files when saveType is file", {
  skip_if_not_installed("Eunomia")
  skip_if_offline()
# make an sklearn model to test file transport
  dir.create(file.path(saveLoc, "testMoveStart"))
  write.csv(data.frame(a = 1, b = 2), file = file.path(saveLoc, "testMoveStart", "file.csv"), row.names = FALSE)
  plpModelTemp <- plpResult$model
  plpModelTemp$model <- file.path(saveLoc, "testMoveStart")
  plpModelTemp$modelDesign$modelSettings$settings$saveType <- "file"

  savePlpModel(plpModel = plpModelTemp, dirPath = file.path(saveLoc, "testMoveEnd"))

  expect_equal(dir(file.path(saveLoc, "testMoveStart")), dir(file.path(saveLoc, "testMoveEnd", "model")))
})

test_that("savePrediction", {
  predLoc <- savePrediction(
    prediction = data.frame(rowId = 1:10, value = 1:10),
    dirPath = saveLoc, fileName = "pred.json"
  )
  expect_equal(file.exists(predLoc), TRUE)
})

test_that("loadPrediction", {
  pred <- loadPrediction(file.path(saveLoc, "pred.json"))
  expect_identical(data.frame(rowId = 1:10, value = 1:10), pred)
})


test_that("savePlpResultError", {
  expect_error(savePlpResult(dirPath = NULL))
  expect_error(savePlpResult(result = NULL))
})

test_that("savePlpResult", {
  emptyModel <- list()
  emptyModel$modelDesign$modelSettings$settings$predict <- "none"
  emptyModel$modelDesign$modelSettings$settings$saveType <- "RtoJson"
  class(emptyModel) <- "plpModel"
  emptyResult <- list(
    model = emptyModel,
    prediction = data.frame(rowId = 1:5, value = 1:5),
    performanceEvaluation = data.frame(),
    covariateSummary = NULL,
    executionSettings = NULL
  )
  class(emptyResult) <- "runPlp"

  savePlpResult(result = emptyResult, dirPath = file.path(saveLoc, "plpResultTest"))
  expect_equal(dir.exists(file.path(saveLoc, "plpResultTest")), TRUE)
  expect_equal(dir(file.path(saveLoc, "plpResultTest")), c("model", "runPlp.rds"))
})


test_that("loadPlpResultError", {
  expect_error(loadPlpResult(dirPath = NULL))
  expect_error(loadPlpResult(dirPath = "madeup/dfdfd/j"))
  write.csv(c(0), file.path(saveLoc, "file2.csv"))
  expect_error(loadPlpResult(dirPath = file.path(saveLoc, "file1.csv")))
})

test_that("loadPlpResult", {
  emptyModel <- list()
  emptyModel$modelDesign$modelSettings$settings$predict <- "none"
  emptyModel$modelDesign$modelSettings$settings$saveType <- "RtoJson"
  class(emptyModel) <- "plpModel"
  emptyResult <- list(
    model = emptyModel,
    prediction = data.frame(rowId = 1:5, value = 1:5),
    performanceEvaluation = data.frame(),
    covariateSummary = NULL,
    executionSettings = NULL
  )
  class(emptyResult) <- "runPlp"

  plpResultLoaded <- loadPlpResult(file.path(saveLoc, "plpResultTest"))

  expect_identical(plpResultLoaded$covariateSummary, emptyResult$covariateSummary)
  expect_identical(plpResultLoaded$executionSummary, emptyResult$executionSummary)
  expect_identical(plpResultLoaded$performanceEvaluation, emptyResult$performanceEvaluation)
  expect_identical(plpResultLoaded$prediction, emptyResult$prediction)
})


test_that("savePlpShareable works", {
  skip_if_not_installed("Eunomia")
  skip_if_offline()
  # check it works
  savePlpShareable(plpResult, file.path(saveLoc, "plpFriendly"), minCellCount = 0)
  shareableLoad <- loadPlpShareable(file.path(saveLoc, "plpFriendly"))

  # check covariateSummary
  expect_equal(nrow(shareableLoad$covariateSummary), nrow(plpResult$covariateSummary))

  # check performanceEvaluation
  expect_equal(
    dim(shareableLoad$performanceEvaluation$evaluationStatistics),
    dim(plpResult$performanceEvaluation$evaluationStatistics)
  )
  expect_equal(
    dim(shareableLoad$performanceEvaluation$thresholdSummary),
    dim(plpResult$performanceEvaluation$thresholdSummary)
  )
  expect_equal(
    dim(shareableLoad$performanceEvaluation$demographicSummary),
    dim(plpResult$performanceEvaluation$demographicSummary)
  )
  expect_equal(
    dim(shareableLoad$performanceEvaluation$calibrationSummary),
    dim(plpResult$performanceEvaluation$calibrationSummary)
  )
  expect_equal(
    dim(shareableLoad$performanceEvaluation$predictionDistribution),
    dim(plpResult$performanceEvaluation$predictionDistribution)
  )
})

# Note: saving from database to csv is in the database upload test file



test_that("applyMinCellCount works", {
  result <- data.frame(
    performance_id = 1:2,
    covariate_id = 1:2,
    covariate_name = paste0("name", 1:2),
    concept_id = 1:2,
    covariate_value = runif(2),
    covariate_count = c(100, 50),
    covariate_mean = runif(2),
    covariate_st_dev = runif(2),
    with_no_outcome_covariate_count = c(10, 5),
    with_no_outcome_covariate_mean = runif(2),
    with_no_outcome_covariate_st_dev = runif(2),
    with_outcome_covariate_count = c(90, 45),
    with_outcome_covariate_mean = runif(2),
    with_outcome_covariate_st_dev = runif(2),
    standardized_mean_diff = runif(2)
  )

  minCellResult <- applyMinCellCount(
    tableName = "covariate_summary",
    sensitiveColumns = getPlpSensitiveColumns(),
    result = result,
    minCellCount = 5
  )
  # check nothing removed
  expect_equal(2, sum(minCellResult$covariate_count != -1))
  expect_equal(2, sum(minCellResult$with_no_outcome_covariate_count != -1))
  expect_equal(2, sum(minCellResult$with_outcome_covariate_count != -1))

  # now check values are removed
  minCellResult <- applyMinCellCount(
    tableName = "covariate_summary",
    sensitiveColumns = getPlpSensitiveColumns(),
    result = result,
    minCellCount = 10
  )
  expect_equal(0, sum(minCellResult$covariate_count == -1))
  expect_equal(minCellResult$with_no_outcome_covariate_count[2], -1)
  expect_equal(1, sum(minCellResult$with_no_outcome_covariate_count == -1))
  expect_equal(1, sum(minCellResult$with_outcome_covariate_count == -1))
})

test_that("applyMinCellCount leaves missing counts missing", {
  result <- data.frame(
    performance_id = 1,
    person_count_at_risk = c(20, 2, 30, 3),
    person_count_with_outcome = c(12, NA, NA, 1)
  )
  minCellResult <- applyMinCellCount(
    tableName = "demographic_summary",
    sensitiveColumns = getPlpSensitiveColumns(),
    result = result,
    minCellCount = 5
  )
  expect_equal(minCellResult$person_count_at_risk, c(20, -1, 30, -1))
  expect_equal(minCellResult$person_count_with_outcome, c(12, NA, NA, -1))

  # columns censored together: a small count hides its NA partner too
  calibration <- result
  minCellResult <- applyMinCellCount(
    tableName = "calibration_summary",
    sensitiveColumns = getPlpSensitiveColumns(),
    result = calibration,
    minCellCount = 5
  )
  expect_equal(minCellResult$person_count_at_risk, c(20, -1, 30, -1))
  expect_equal(minCellResult$person_count_with_outcome, c(12, -1, NA, -1))
})

test_that("savePlpData round-trips a custom population with its attributes", {
  data("simulationProfile", package = "PatientLevelPrediction", envir = environment())
  simulated <- simulatePlpData(simulationProfile, n = 200, seed = 17)
  population <- simulated$cohorts[1:20, ]
  population$outcomeCount <- rep(c(0, 1), 10)
  attr(population, "metaData") <- list(outcomeId = 3, marker = "custom")
  simulated$population <- population

  path <- tempfile("savePopulation")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  savePlpData(simulated, path)
  expect_true(file.exists(file.path(path, "population.rds")))

  loaded <- loadPlpData(path)
  on.exit(Andromeda::close(loaded$covariateData), add = TRUE, after = FALSE)
  expect_equal(loaded$population, population)
  expect_equal(attr(loaded$population, "metaData"), attr(population, "metaData"))
})

test_that("savePlpData leaves plpData without a population unchanged", {
  data("simulationProfile", package = "PatientLevelPrediction", envir = environment())
  simulated <- simulatePlpData(simulationProfile, n = 200, seed = 17)
  path <- tempfile("saveNoPopulation")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  savePlpData(simulated, path)
  expect_false(file.exists(file.path(path, "population.rds")))
  loaded <- loadPlpData(path)
  on.exit(Andromeda::close(loaded$covariateData), add = TRUE, after = FALSE)
  expect_null(loaded$population)
})

test_that("savePlpData removes optional files left by an earlier save into the same folder", {
  data("simulationProfile", package = "PatientLevelPrediction", envir = environment())
  withPopulation <- simulatePlpData(simulationProfile, n = 200, seed = 17)
  population <- withPopulation$cohorts[1:20, ]
  population$outcomeCount <- 0
  withPopulation$population <- population
  withPopulation$simulationTruth <- data.frame(rowId = 1:2, trueRisk = 0.1)
  path <- tempfile("saveTwice")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  savePlpData(withPopulation, path)
  expect_true(file.exists(file.path(path, "population.rds")))
  expect_true(file.exists(file.path(path, "simulationTruth.rds")))

  withoutPopulation <- simulatePlpData(simulationProfile, n = 100, seed = 18)
  withoutPopulation$simulationTruth <- NULL
  savePlpData(withoutPopulation, path)
  expect_false(file.exists(file.path(path, "population.rds")))
  expect_false(file.exists(file.path(path, "simulationTruth.rds")))

  loaded <- loadPlpData(path)
  on.exit(Andromeda::close(loaded$covariateData), add = TRUE, after = FALSE)
  expect_null(loaded$population)
  expect_null(loaded$simulationTruth)
  expect_equal(nrow(loaded$cohorts), 100)
})

test_that("Saved GLM models keep coefficient precision", {
  model <- createGlmModel(
    coefficients = data.frame(
      covariateId = c(1002, 2002),
      coefficient = c(4e-05, 0.1)
    ),
    intercept = -2,
    mapping = "logistic"
  )
  path <- tempfile("saveGlmPrecision")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  savePlpModel(model, path)
  loaded <- loadPlpModel(path)
  expect_identical(loaded$model$coefficients$coefficient, c(4e-05, 0.1))
  expect_equal(loaded$model$intercept, -2)

  # the file stays plain numeric JSON readable by the standard loader
  raw <- ParallelLogger::loadSettingsFromJson(file.path(path, "model.json"))
  expect_true(is.numeric(raw$coefficients$coefficient))
})

test_that("Full precision JSON writer keeps data frames held in attributes", {
  # feature engineering steps keep the values they learned in training as attributes
  settings <- structure(
    list(type = "minmax"),
    class = "featureEngineeringSettings",
    fun = "minMaxNormalize",
    minMaxs = dplyr::tibble(covariateId = c(1002, 1003), max = c(80.123456789, 1 / 3), min = c(20, NA)),
    nested = list(map = data.frame(sourceCovariateId = 1, indicatorCovariateId = 2))
  )
  object <- list(step = list(funct = "minMaxNormalize", settings = list(featureEngineeringSettings = settings)))
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  PatientLevelPrediction:::saveSettingsToJsonFullPrecision(object, path)
  loaded <- ParallelLogger::loadSettingsFromJson(path)$step$settings$featureEngineeringSettings

  expect_s3_class(loaded, "featureEngineeringSettings")
  expect_equal(attr(loaded, "fun"), "minMaxNormalize")
  minMaxs <- attr(loaded, "minMaxs")
  expect_s3_class(minMaxs, "data.frame")
  expect_identical(minMaxs$max, c(80.123456789, 1 / 3))
  expect_equal(minMaxs$min, c(20, NA))
  expect_s3_class(attr(loaded, "nested")$map, "data.frame")
  expect_equal(attr(loaded, "nested")$map$indicatorCovariateId, 2)
})

test_that("Full precision JSON writer keeps missing values in tables with one row", {
  # an imputer saves the missing indicator analysis it adds; for a single
  # measurement analysis without a time window that table has one row, and
  # prediction appends it to the analysisRef of the new data
  analysisRef <- data.frame(
    analysisId = 1010, analysisName = "measurement missing indicator",
    domainId = "measurement", startDay = NA_real_, endDay = NA_real_,
    isBinary = "Y", missingMeansZero = "Y"
  )
  settings <- structure(list(method = "mean"), analysisRef = analysisRef)
  jsonFile <- tempfile(fileext = ".json")
  on.exit(unlink(jsonFile), add = TRUE)
  saveSettingsToJsonFullPrecision(list(settings = settings), jsonFile)
  reloaded <- attr(ParallelLogger::loadSettingsFromJson(jsonFile)$settings, "analysisRef")
  expect_true(is.na(reloaded$startDay))
  expect_false(is.character(reloaded$startDay))

  covariateData <- Andromeda::andromeda(analysisRef = data.frame(
    analysisId = 10, analysisName = "measurement", domainId = "measurement",
    startDay = -365, endDay = 0, isBinary = "N", missingMeansZero = "N"
  ))
  on.exit(Andromeda::close(covariateData), add = TRUE)
  Andromeda::appendToTable(covariateData$analysisRef, reloaded)
  expect_setequal(dplyr::pull(covariateData$analysisRef, "analysisId"), c(10, 1010))
})

test_that("only the package's own feature engineering steps get saved tables repaired", {
  # a list of records, as a custom step might save it
  records <- list(list(id = 1, name = "a"), list(id = 2, name = "b"))
  steps <- featureEngineeringSteps(list(
    list(funct = "implementCustomStep", settings = list(records = records)),
    list(funct = "minMaxNormalize", settings = list(records = records))
  ))

  expect_identical(steps[[1]]$settings$records, records)
  expect_s3_class(steps[[2]]$settings$records, "data.frame")
  expect_equal(steps[[2]]$settings$records$id, c(1, 2))
})

test_that("Full precision JSON writer handles the settings shapes saved with models", {
  object <- list(
    tiny = 1.5e-12,
    third = 1 / 3,
    large = 123456789.123456,
    negative = -7.25e-05,
    vector = c(0.1, 2e-300),
    missing = c(1.5, NA),
    infinite = c(-Inf, Inf),
    integers = 1:3,
    flag = TRUE,
    text = "__not a marker",
    nothing = NULL,
    frame = data.frame(covariateId = c(1, 2), maxValue = c(0.000012345, 987654.321)),
    date = as.Date("2026-01-02"),
    nested = list(inner = 5e-06)
  )
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  PatientLevelPrediction:::saveSettingsToJsonFullPrecision(object, path)
  loaded <- ParallelLogger::loadSettingsFromJson(path)

  expect_identical(loaded$tiny, 1.5e-12)
  expect_identical(loaded$third, 1 / 3)
  expect_identical(loaded$large, 123456789.123456)
  expect_identical(loaded$negative, -7.25e-05)
  expect_identical(loaded$vector[1], 0.1)
  # tiny values must survive instead of being rounded to zero; reading a number
  # with an exponent this large back is not exact on every platform (R's
  # conversion from text on macOS arm64), so this one is checked to tolerance
  expect_equal(loaded$vector[2], 2e-300, tolerance = 1e-12)
  expect_equal(loaded$missing, c(1.5, NA))
  expect_true(is.nan(loaded$missing[2]) || is.na(loaded$missing[2]))
  expect_equal(loaded$infinite, c(-Inf, Inf))
  expect_equal(loaded$integers, 1:3)
  expect_true(loaded$flag)
  expect_equal(loaded$text, "__not a marker")
  expect_true("nothing" %in% names(loaded))
  expect_null(loaded$nothing)
  expect_identical(loaded$frame$maxValue, c(0.000012345, 987654.321))
  expect_equal(loaded$date, "2026-01-02")
  expect_identical(loaded$nested$inner, 5e-06)
})

test_that("a reloaded model with feature engineering predicts exactly as the fitted model", {
  skip_on_cran()
  data("simulationProfile", package = "PatientLevelPrediction")
  simulated <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
  on.exit(Andromeda::close(simulated$covariateData), add = TRUE)
  # a continuous measurement that is missing for 30% of patients
  measured <- withr::with_seed(7, sample(simulated$cohorts$rowId, 700))
  Andromeda::appendToTable(
    simulated$covariateData$covariates,
    data.frame(rowId = measured, covariateId = 9101, covariateValue = withr::with_seed(7, stats::rnorm(700, 5, 2)))
  )
  Andromeda::appendToTable(
    simulated$covariateData$covariateRef,
    data.frame(
      covariateId = 9101, covariateName = "measurement", analysisId = 101, conceptId = 0,
      valueAsConceptId = NA_real_, collisions = NA_real_
    )
  )
  # the simulated analysisRef only has analysisId; age (analysis 2) and the
  # measurement are continuous
  analysisIds <- c(dplyr::pull(simulated$covariateData$analysisRef, .data$analysisId), 101)
  simulated$covariateData$analysisRef <- data.frame(
    analysisId = analysisIds, analysisName = paste("analysis", analysisIds), domainId = "x",
    startDay = 0, endDay = 0,
    isBinary = ifelse(analysisIds %in% c(2, 101), "N", "Y"),
    missingMeansZero = ifelse(analysisIds %in% c(2, 101), "N", "Y")
  )
  populationSettings <- createStudyPopulationSettings(requireTimeAtRisk = FALSE)
  saveDirectory <- file.path(saveLoc, "reloadedFeatureEngineering")

  result <- runPlp(
    plpData = simulated,
    outcomeId = 3,
    analysisId = "fe",
    populationSettings = populationSettings,
    featureEngineeringSettings = list(
      createSimpleImputer(method = "mean", missingThreshold = 0.5, addMissingIndicator = TRUE),
      createNormalizer("minmax"),
      createRareFeatureRemover(threshold = 0.01)
    ),
    modelSettings = setLassoLogisticRegression(seed = 42),
    logSettings = createLogSettings(verbosity = "ERROR"),
    executeSettings = createExecuteSettings(
      runSplitData = TRUE, runFeatureEngineering = TRUE, runPreprocessData = TRUE,
      runModelDevelopment = TRUE
    ),
    saveDirectory = saveDirectory
  )
  loaded <- loadPlpModel(file.path(saveDirectory, "fe", "plpResult", "model"))
  population <- createStudyPopulation(simulated, outcomeId = 3, populationSettings = populationSettings)

  fitted <- predictPlp(result$model, simulated, population)
  reloaded <- predictPlp(loaded, simulated, population)

  expect_equal(reloaded$rowId, fitted$rowId)
  expect_equal(reloaded$value, fitted$value, tolerance = 0)
})
