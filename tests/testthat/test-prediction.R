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
test_that("prediction inputs", {
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  # =====================================
  # check prediction
  # =====================================
  expect_error(predictPlp(
    model = NULL, population = population,
    plpData = plpData
  ))
  expect_error(predictPlp(
    model = list(), population = NULL,
    plpData = plpData
  ))
  expect_error(predictPlp(
    model = list(), population = population,
    plpData = NULL
  ))
})


test_that("prediction works", {
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  # =====================================
  # check prediction
  # =====================================
  pred <- predictPlp(
    plpModel = plpResult$model,
    population = population,
    plpData = plpData
  )
  pred <- pred[order(pred$rowId), ]
  tempPrediction <- plpResult$prediction[order(plpResult$prediction$rowId), ]
  expect_equal(
    nrow(pred),
    nrow(population)
  )

  rowId <- tempPrediction$rowId[tempPrediction$evaluationType == "Test"][1]

  expect_equal(
    pred$value[pred$rowId == rowId],
    tempPrediction$value[
      tempPrediction$evaluationType == "Test" &
        tempPrediction$rowId == rowId
    ]
  )

  # check metaData
  expect_equal(length(names(attr(pred, "metaData"))), 6) # 8 if survivial

  # add single person pred and compare with manual cal

  # add prediction of other models
})

# predict.*


test_that("applyTidyCovariateData", {
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  covariateIds <- plpData$covariateData$covariateRef %>%
    dplyr::select("covariateId") %>%
    dplyr::pull()
  remove <- sample(covariateIds, 10)
  deletedRedundantCovariateIds <- remove[1:5]
  deletedInfrequentCovariateIds <- remove[6:10]

  prepocessSettings <- list(
    normFactors = data.frame(
      covariateId = covariateIds,
      maxValue = rep(0.1, length(covariateIds))
    ),
    deletedRedundantCovariateIds = deletedRedundantCovariateIds,
    deletedInfrequentCovariateIds = deletedInfrequentCovariateIds
  )

  # get covariateSize before
  covariateCount <- plpData$covariateData$covariates %>%
    dplyr::tally() %>%
    dplyr::pull()

  newCovariateData <- applyTidyCovariateData(
    covariateData = plpData$covariateData,
    preprocessSettings = prepocessSettings
  )

  # some covariates removed
  expect_true(newCovariateData$covariates %>% dplyr::tally() %>% dplyr::pull() < covariateCount)

  newCovs <- newCovariateData$covariateRef %>%
    dplyr::select("covariateId") %>%
    dplyr::pull()

  expect_equal(sum(covariateIds[!covariateIds %in% newCovs] %in% remove), 10)
})

survivalWindow <- function(start, end, startAnchor = "cohort start", endAnchor = "cohort start") {
  createStudyPopulationSettings(
    riskWindowStart = start, startAnchor = startAnchor,
    riskWindowEnd = end, endAnchor = endAnchor,
    requireTimeAtRisk = FALSE
  )
}

test_that("survival horizon is the length of a time-at-risk window with one anchor", {
  expect_equal(getSurvivalHorizon(survivalWindow(1, 365)), 365)
  expect_equal(getSurvivalHorizon(survivalWindow(0, 365)), 366)
  expect_equal(getSurvivalHorizon(survivalWindow(30, 365)), 336)
  expect_equal(getSurvivalHorizon(survivalWindow(0, 30, "cohort end", "cohort end")), 31)
  expect_null(getSurvivalHorizon(survivalWindow(1, 365, endAnchor = "cohort end")))
  expect_null(getSurvivalHorizon(NULL))
})

test_that("survival timepoint defaults to the window length and must lie within the window", {
  delayed <- survivalWindow(30, 365)
  mixed <- survivalWindow(1, 365, endAnchor = "cohort end")
  expect_equal(getSurvivalTimepoint(delayed), 336)
  expect_equal(getSurvivalTimepoint(delayed, 180), 180)
  expect_equal(getSurvivalTimepoint(delayed, 336), 336)
  expect_error(getSurvivalTimepoint(delayed, 337), "after the end of the time-at-risk window")
  expect_error(getSurvivalTimepoint(delayed, 0), "positive number")
  expect_error(getSurvivalTimepoint(mixed), "length differs between patients")
  expect_equal(getSurvivalTimepoint(mixed, 180), 180)
  expect_error(getSurvivalTimepoint(NULL), "no populationSettings")
})

test_that("Cox baseline survival is read as a step function at the timepoint", {
  baseline <- list(time = c(10, 20, 30), surv = c(0.9, 0.8, 0.7))
  # the nearest baseline time to 28 is 30, but survival at 28 is still that of day 20
  expect_equal(baselineSurvivalAt(baseline, 28), 0.8)
  expect_equal(baselineSurvivalAt(baseline, 30), 0.7)
  expect_equal(baselineSurvivalAt(baseline, 100), 0.7)
  expect_equal(baselineSurvivalAt(baseline, 5), 1)
})

test_that("a Cox model with a delayed risk window is predicted and evaluated at the window length", {
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  skip_if_not_installed("survival")
  skip_if_not_installed("polspline")
  skip_on_cran()
  populationSettings <- survivalWindow(30, 365)
  result <- runPlp(
    plpData = plpData,
    outcomeId = outcomeId,
    analysisId = "coxDelayedWindow",
    populationSettings = populationSettings,
    splitSettings = createDefaultSplitSetting(splitSeed = 12),
    modelSettings = setCoxModel(seed = 42),
    logSettings = createLogSettings(verbosity = "ERROR"),
    executeSettings = createExecuteSettings(
      runSplitData = TRUE, runSampleData = FALSE, runFeatureEngineering = FALSE,
      runPreprocessData = TRUE, runModelDevelopment = TRUE, runCovariateSummary = FALSE
    ),
    saveDirectory = file.path(saveLoc, "coxDelayedWindow")
  )
  expect_equal(max(result$prediction$survivalTime), 336)
  expect_equal(attr(result$prediction, "metaData")$timepoint, 336)

  # at the old horizon of 365 days these were not numbers, all NA, or an error
  statistics <- result$performanceEvaluation$evaluationStatistics
  survival <- statistics[statistics$metric == "Survival", ]
  expect_setequal(survival$evaluation, c("Test", "Train"))
  expect_true(all(survival$timepoint == 336))
  expect_false(anyNA(suppressWarnings(as.numeric(survival$value))))
  expect_false(anyNA(result$performanceEvaluation$calibrationSummary$observedIncidence))
  expect_gt(nrow(result$performanceEvaluation$demographicSummary), 0)

  population <- createStudyPopulation(plpData, outcomeId, populationSettings)
  atWindowEnd <- predictPlp(result$model, plpData, population)
  atDay30 <- predictPlp(result$model, plpData, population, timepoint = 30)
  expect_equal(attr(atDay30, "metaData")$timepoint, 30)
  expect_equal(atDay30$rowId, atWindowEnd$rowId)
  expect_true(all(atDay30$value <= atWindowEnd$value))
  expect_true(any(atDay30$value < atWindowEnd$value))
  expect_error(
    predictPlp(result$model, plpData, population, timepoint = 400),
    "after the end of the time-at-risk window"
  )

  mixed <- createStudyPopulation(plpData, outcomeId, survivalWindow(1, 365, endAnchor = "cohort end"))
  expect_error(predictPlp(result$model, plpData, mixed), "length differs between patients")
})

test_that("runPlp stops a survival model when the window ends have different anchors", {
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  mixed <- survivalWindow(1, 365, endAnchor = "cohort end")
  expect_error(
    checkInputs(list(
      plpData = plpData, outcomeId = outcomeId,
      populationSettings = mixed, modelSettings = setCoxModel(seed = 42)
    )),
    "length differs between patients"
  )
  expect_error(
    runPlp(
      plpData = plpData,
      outcomeId = outcomeId,
      analysisId = "coxMixedAnchors",
      populationSettings = mixed,
      modelSettings = setCoxModel(seed = 42),
      logSettings = createLogSettings(verbosity = "FATAL"),
      saveDirectory = file.path(saveLoc, "coxMixedAnchors")
    ),
    "Settings are invalid"
  )
  # binary models are unaffected
  expect_true(checkInputs(list(
    plpData = plpData, outcomeId = outcomeId,
    populationSettings = mixed, modelSettings = setLassoLogisticRegression(seed = 42)
  )))
})

createSharedPlpData <- function() {
  covariateData <- Andromeda::andromeda(
    covariates = data.frame(
      rowId = c(1, 2, 1, 2),
      covariateId = c(101, 101, 202, 202),
      covariateValue = c(1, 1, 1, 2)
    ),
    covariateRef = data.frame(
      covariateId = c(101, 202),
      covariateName = c("A feature", "B feature"),
      analysisId = c(1, 1),
      conceptId = c(0, 0)
    ),
    analysisRef = data.frame(
      analysisId = 1,
      analysisName = "analysis",
      domainId = "domain",
      startDay = 0,
      endDay = 0,
      isBinary = "N",
      missingMeansZero = "N"
    )
  )
  class(covariateData) <- "CovariateData"
  attr(covariateData, "metaData") <- list(populationSize = 2)
  structure(
    list(cohorts = data.frame(rowId = c(1, 2)), outcomes = data.frame(), covariateData = covariateData),
    class = "plpData"
  )
}

covariateRows <- function(plpData) {
  rows <- as.data.frame(plpData$covariateData$covariates)
  rows[order(rows$rowId, rows$covariateId), c("rowId", "covariateId", "covariateValue")]
}

test_that("predictPlp with saved feature selection does not change data used by later models", {
  for (selector in c("univariateFeatureSelection", "randomForestFeatureSelection")) {
    selection <- list(
      funct = selector,
      settings = list(
        featureEngineeringSettings = list(k = 1L),
        covariateIdsInclude = 101
      )
    )
    modelA <- createGlmModel(
      data.frame(covariateId = 101, coefficient = 1),
      intercept = 0,
      featureEngineering = selection
    )
    modelB <- createGlmModel(data.frame(covariateId = 202, coefficient = 1), intercept = 0)
    shared <- createSharedPlpData()
    on.exit(Andromeda::close(shared$covariateData), add = TRUE)
    before <- covariateRows(shared)

    predictionA <- predictPlp(modelA, shared, shared$cohorts)
    expect_equal(predictionA$value[order(predictionA$rowId)], stats::plogis(c(1, 1)))
    predictionB <- predictPlp(modelB, shared, shared$cohorts)

    expect_equal(predictionB$value[order(predictionB$rowId)], stats::plogis(c(1, 2)))
    expect_equal(covariateRows(shared), before)
    expect_setequal(
      as.data.frame(shared$covariateData$covariateRef)$covariateId,
      c(101, 202)
    )
  }
})

test_that("predictPlp with saved splines does not remove the source covariate from shared data", {
  splineKnots <- c(0, 1.5, 3)
  splines <- list(
    funct = "splineCovariates",
    settings = list(
      featureEngineeringSettings = createSplineSettings(
        continousCovariateId = 202,
        knots = splineKnots
      ),
      knots = splineKnots
    )
  )
  splineModel <- createGlmModel(
    data.frame(covariateId = 101, coefficient = 1),
    intercept = 0,
    featureEngineering = splines
  )
  modelB <- createGlmModel(data.frame(covariateId = 202, coefficient = 1), intercept = 0)
  shared <- createSharedPlpData()
  on.exit(Andromeda::close(shared$covariateData), add = TRUE)
  before <- covariateRows(shared)

  predictPlp(splineModel, shared, shared$cohorts)
  prediction <- predictPlp(modelB, shared, shared$cohorts)

  expect_equal(prediction$value[order(prediction$rowId)], stats::plogis(c(1, 2)))
  expect_equal(covariateRows(shared), before)
})

test_that("applyFeatureEngineering protects shared data from user-written transforms", {
  # Follows the template in the custom feature engineering vignette
  assign(
    "auditCustomFe",
    function(trainData, featureEngineeringSettings) {
      trainData$covariateData$covariates <- trainData$covariateData$covariates %>%
        dplyr::filter(.data$covariateId == 101)
      trainData
    },
    envir = globalenv()
  )
  on.exit(rm("auditCustomFe", envir = globalenv()), add = TRUE)
  shared <- createSharedPlpData()
  on.exit(Andromeda::close(shared$covariateData), add = TRUE)

  result <- applyFeatureEngineering(
    shared,
    list(funct = "auditCustomFe", settings = list(featureEngineeringSettings = list()))
  )

  expect_equal(nrow(covariateRows(shared)), 4)
  expect_equal(nrow(covariateRows(result)), 2)
})

test_that("applyFeatureEngineering keeps class, metaData and plpData shape", {
  shared <- createSharedPlpData()
  on.exit(Andromeda::close(shared$covariateData), add = TRUE)

  result <- applyFeatureEngineering(
    shared,
    list(
      funct = "univariateFeatureSelection",
      settings = list(
        featureEngineeringSettings = list(k = 1L),
        covariateIdsInclude = 101
      )
    )
  )

  expect_s3_class(result, "plpData")
  expect_equal(names(result), names(shared))
  expect_true(FeatureExtraction::isCovariateData(result$covariateData))
  expect_equal(attr(result$covariateData, "metaData")$populationSize, 2)
  expect_equal(unique(covariateRows(result)$covariateId), 101)
})

test_that("a model trained without normalization predicts after saving and loading", {
  # tidyCovariateData(normalize = FALSE) leaves no normalization factors
  model <- createGlmModel(
    data.frame(covariateId = c(101, 202), coefficient = c(0.5, -0.25)),
    intercept = -1,
    tidyCovariates = list(
      deletedInfrequentCovariateIds = c(),
      deletedRedundantCovariateIds = c(),
      normFactors = NULL
    )
  )
  path <- tempfile("withoutNormalization")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  savePlpModel(model, path)
  loaded <- loadPlpModel(path)
  expect_null(loaded$preprocessing$tidyCovariates$normFactors)

  shared <- createSharedPlpData()
  on.exit(Andromeda::close(shared$covariateData), add = TRUE)
  fitted <- predictPlp(model, shared, shared$cohorts)
  reloaded <- predictPlp(loaded, shared, shared$cohorts)
  expect_equal(reloaded$value[order(reloaded$rowId)], stats::plogis(c(-0.75, -1)))
  expect_equal(reloaded$value[order(reloaded$rowId)], fitted$value[order(fitted$rowId)])

  # models saved by earlier versions have a data frame without columns instead
  oldStyle <- model
  oldStyle$preprocessing$tidyCovariates$normFactors <- data.frame()
  oldPrediction <- predictPlp(oldStyle, shared, shared$cohorts)
  expect_equal(oldPrediction$value[order(oldPrediction$rowId)], stats::plogis(c(-0.75, -1)))
})
