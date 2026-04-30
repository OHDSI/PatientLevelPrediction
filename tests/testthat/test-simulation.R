# @file test-simulation.R
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
test_that("create simulation profile works", {
  cohorts <- data.frame(rowId = 1:10, subjectId = 1:10, targetId = rep(1, 10))
  covariates <- data.frame(
    rowId = c(rep(1:10, each = 2), 1:10),
    covariateId = c(rep(c(1002, 14310024102), 10), rep(8532001, 10)),
    covariateValue = c(stats::runif(20), stats::rbinom(10, 1, 0.5))
  )
  covariateRef <- data.frame(
    covariateId = c(1002, 14310024102, 8532001),
    covariateName = c("cont1", "cont2", "bin1"),
    analysisId = c(1, 1, 2)
  )
  analysisRef <- data.frame(
    analysisId = c(1, 2),
    analysisName = c("continuous", "binary"),
    isBinary = c("N", "Y")
  )
  outcomes <- data.frame(
    rowId = c(1, 2, 3, 4, 5),
    daysToEvent = c(80, 90, 100, 110, 120)
  )
  metaData <- list(
    databaseDetails = list(outcomeIds = c(1))
  )
  simData <- list(
    cohorts = cohorts,
    outcomes = outcomes,
    metaData = metaData,
    covariateData = list(
      covariates = covariates,
      covariateRef = covariateRef,
      analysisRef = analysisRef
    )
  )
  expect_warning(
    simProfile <- createSimulationProfile(simData),
    "Default outcome model omitted"
  )
  expect_type(simProfile, "list")
  expect_s3_class(simProfile, "plpDataSimulationProfile")
  expect_true(all(c(
    "covariateInfo", "timeMax", "outcomeRate",
    "outcomeModels", "metaData", "covariateRef"
  ) %in% names(simProfile)))

  prevalence <- simProfile$covariateInfo$covariatePrevalence
  expect_type(prevalence, "double")
  expect_equal(as.numeric(prevalence["1002"]), 1)
  expect_equal(as.numeric(prevalence["8532001"]), 1)
  expect_equal(as.numeric(prevalence["14310024102"]), 1)

  continuousCovs <- simProfile$covariateInfo$continuousCovariates
  expect_equal(nrow(continuousCovs), 2)
  expect_equal(continuousCovs$covariateId, c(1002, 14310024102))
  expect_equal(simProfile$outcomeRate, 0.5)
  expect_equal(simProfile$timeMax, max(outcomes$daysToEvent))
  expect_equal(
    length(simProfile$outcomeModels),
    length(metaData$databaseDetails$outcomeIds)
  )
  expect_equal(simProfile$outcomeModels[[1]][["(Intercept)"]], -2)
  expect_equal(simProfile$outcomeModels[[1]][["1002"]], 0.04)
  expect_equal(simProfile$outcomeModels[[1]][["8532001"]], 0.5)
  expect_false("8532" %in% names(simProfile$outcomeModels[[1]]))
  expect_true(all(
    setdiff(names(simProfile$outcomeModels[[1]]), "(Intercept)") %in% names(prevalence)
  ))
  expect_equal(simProfile$metaData, metaData)
  expect_equal(simProfile$covariateRef, as.data.frame(covariateRef))

  customOutcomeModel <- c("(Intercept)" = -1, "1002" = 0.1, "8532001" = 0.3)
  customProfile <- createSimulationProfile(simData, outcomeModels = customOutcomeModel)
  expect_equal(customProfile$outcomeModels[[1]], customOutcomeModel)

  cyclopsCoefficients <- data.frame(
    betas = c(-1, 0.1, 0.3),
    covariateIds = c("(Intercept)", "1002", "8532001"),
    stringsAsFactors = FALSE
  )
  cyclopsModel <- list(coefficients = cyclopsCoefficients)
  plpModel <- list(model = cyclopsModel)
  class(plpModel) <- "plpModel"
  plpResult <- list(model = plpModel)
  class(plpResult) <- "runPlp"

  cyclopsCoefficientProfile <- createSimulationProfile(simData, outcomeModels = cyclopsCoefficients)
  expect_equal(cyclopsCoefficientProfile$outcomeModels[[1]], customOutcomeModel)

  cyclopsProfile <- createSimulationProfile(simData, outcomeModels = cyclopsModel)
  expect_equal(cyclopsProfile$outcomeModels[[1]], customOutcomeModel)

  plpModelProfile <- createSimulationProfile(simData, outcomeModels = plpModel)
  expect_equal(plpModelProfile$outcomeModels[[1]], customOutcomeModel)

  plpResultProfile <- createSimulationProfile(simData, outcomeModels = plpResult)
  expect_equal(plpResultProfile$outcomeModels[[1]], customOutcomeModel)

  glmModel <- list(
    intercept = -1,
    coefficients = data.frame(
      covariateId = c(1002, 8532001),
      coefficient = c(0.1, 0.3)
    )
  )
  glmProfile <- createSimulationProfile(simData, outcomeModels = glmModel)
  expect_equal(glmProfile$outcomeModels[[1]], customOutcomeModel)

  glmModelWithInterceptCoefficient <- list(
    intercept = 0,
    coefficients = data.frame(
      covariateId = c("(Intercept)", "1002", "8532001"),
      coefficient = c(-1, 0.1, 0.3)
    )
  )
  glmWithInterceptProfile <- createSimulationProfile(
    simData,
    outcomeModels = glmModelWithInterceptCoefficient
  )
  expect_equal(glmWithInterceptProfile$outcomeModels[[1]], customOutcomeModel)

  twoOutcomeSimData <- simData
  twoOutcomeSimData$metaData$databaseDetails$outcomeIds <- c(1, 2)
  repeatedOutcomeModelProfile <- createSimulationProfile(
    twoOutcomeSimData,
    outcomeModels = cyclopsModel
  )
  expect_equal(repeatedOutcomeModelProfile$outcomeModels, list(customOutcomeModel, customOutcomeModel))

  secondOutcomeModel <- c("(Intercept)" = -2, "1002" = 0.2, "8532001" = 0.4)
  twoOutcomeProfile <- createSimulationProfile(
    twoOutcomeSimData,
    outcomeModels = list(cyclopsModel, secondOutcomeModel)
  )
  expect_equal(twoOutcomeProfile$outcomeModels, list(customOutcomeModel, secondOutcomeModel))

  expect_error(
    createSimulationProfile(simData, outcomeModels = "not a model"),
    "outcomeModels must be a named numeric vector"
  )
  expect_error(
    createSimulationProfile(simData, outcomeModels = list(list(notCoefficients = TRUE))),
    "Outcome model 1 must be a named numeric vector"
  )
  expect_error(
    createSimulationProfile(simData, outcomeModels = list(coefficients = c(1, 2))),
    "Outcome model 1 must have names for all coefficients"
  )
  expect_error(
    createSimulationProfile(simData, outcomeModels = data.frame(x = 1, y = 2)),
    "Outcome model 1 must be a named numeric vector"
  )
  expect_error(
    createSimulationProfile(simData, outcomeModels = c(-1, 0.1)),
    "Outcome model 1 must have names for all coefficients"
  )
  expect_error(
    createSimulationProfile(twoOutcomeSimData, outcomeModels = list(customOutcomeModel, secondOutcomeModel, customOutcomeModel)),
    "outcomeModels must contain one model for each outcomeId"
  )

  expect_error(
    createSimulationProfile(
      simData,
      outcomeModels = c("(Intercept)" = -1, "9999" = 0.3)
    ),
    "9999"
  )
})

test_that("simulatePlpData works", {
  # mock predictCyclops function
  predictCyclopsType <- function(coefficients, cohorts, covariateData, modelType) {
    data.frame(rowId = cohorts$rowId, value = rep(0.5, nrow(cohorts)))
  }
  dummyProfile <- list(
    covariateInfo = list(
      covariatePrevalence = c("1002" = 1, "8532001" = 0.3, "2001" = 0.5), continuousCovariates = data.frame(
        covariateId = 1002,
        mean = 50,
        sd = 5,
        min = 30,
        max = 70
      )
    ),
    covariateRef = data.frame(
      covariateId = 2001,
      covariateName = "continuous feature",
      stringsAsFactors = FALSE
    ), timeMax = c(100), outcomeModels = list(c("(Intercept)" = -2, "1002" = 0.04, "8532001" = 0.05)),
    metaData = list(
      databaseDetails = list(
        outcomeIds = 3
      )
    )
  )

  n <- 100 # population size for the simulation
  simData <- simulatePlpData(dummyProfile, n = n, seed = 42)
  expect_s3_class(simData, "plpData")
  expect_true(is.data.frame(simData$cohorts))
  expect_equal(nrow(simData$cohorts), n)
  expect_true("rowId" %in% names(simData$cohorts))
  expect_true("subjectId" %in% names(simData$cohorts))
  expect_true("targetId" %in% names(simData$cohorts))
  expect_true("cohortStartDate" %in% names(simData$cohorts))
  expect_s3_class(simData$cohorts$cohortStartDate, "Date")
  expect_s4_class(simData$covariateData, "CovariateData")
  expect_true("covariates" %in% names(simData$covariateData))
  expect_true("covariateRef" %in% names(simData$covariateData))
  expect_true("analysisRef" %in% names(simData$covariateData))
  covMeta <- attr(simData$covariateData, "metaData")
  expect_type(covMeta, "list")
  expect_equal(covMeta$populationSize, n)
  expect_true(is.data.frame(simData$outcomes))
  if (nrow(simData$outcomes) > 0) {
    for (col in c("rowId", "outcomeId", "outcomeCount", "daysToEvent")) {
      expect_true(col %in% names(simData$outcomes))
    }
  }
  expect_true(is.data.frame(simData$simulationTruth))
  expect_equal(nrow(simData$simulationTruth), n)
  expect_equal(
    names(simData$simulationTruth),
    c("rowId", "outcomeId", "linearPredictor", "trueRisk", "outcomeCount", "daysToEvent")
  )
  expect_equal(simData$simulationTruth$outcomeId, rep(3, n))
  expect_true(all(simData$simulationTruth$trueRisk >= 0 & simData$simulationTruth$trueRisk <= 1))
  expect_equal(nrow(simData$outcomes), sum(simData$simulationTruth$outcomeCount))
  expect_true(all(is.na(simData$simulationTruth$daysToEvent[simData$simulationTruth$outcomeCount == 0])))
  expect_true(is.list(simData$metaData))
  expect_true("databaseDetails" %in% names(simData$metaData))
  expect_equal(simData$metaData$databaseDetails$cdmDatabaseSchema, "CDM_SCHEMA")
  expect_equal(simData$metaData$databaseDetails$cdmDatabaseName, "CDM_NAME") 
  expect_equal(simData$metaData$databaseDetails$cdmVersion, 5)
  expect_equal(simData$metaData$databaseDetails$targetId, 1) 
  expect_equal(simData$metaData$databaseDetails$outcomeIds, 3)
})

test_that("simulatePlpData rejects outcome models with unavailable covariates", {
  invalidProfile <- list(
    covariateInfo = list(
      covariatePrevalence = c("1002" = 1),
      continuousCovariates = data.frame(
        covariateId = 1002,
        mean = 50,
        sd = 5,
        min = 30,
        max = 70
      )
    ),
    covariateRef = data.frame(
      covariateId = 1002,
      covariateName = "age",
      stringsAsFactors = FALSE
    ),
    timeMax = c(100),
    outcomeModels = list(c("(Intercept)" = -2, "1002" = 0.04, "9999" = 0.5)),
    metaData = list(databaseDetails = list(outcomeIds = 3))
  )

  expect_error(
    simulatePlpData(invalidProfile, n = 100, seed = 42),
    "9999"
  )
})

test_that("simulation outcome scoring applies PLP model preprocessing", {
  covariateData <- Andromeda::andromeda(
    covariates = data.frame(
      rowId = 1,
      covariateId = 1002,
      covariateValue = 50
    ),
    covariateRef = data.frame(
      covariateId = 1002,
      covariateName = "age",
      analysisId = 1
    ),
    analysisRef = data.frame(analysisId = 1)
  )
  class(covariateData) <- "CovariateData"
  attr(class(covariateData), "package") <- "FeatureExtraction"

  plpModel <- list(
    model = list(
      modelType = "logistic",
      coefficients = data.frame(
        betas = c(0, 1),
        covariateIds = c("(Intercept)", "1002")
      )
    ),
    preprocessing = list(
      tidyCovariates = list(
        normFactors = data.frame(covariateId = 1002, maxValue = 100),
        deletedRedundantCovariateIds = numeric(0),
        deletedInfrequentCovariateIds = numeric(0)
      ),
      featureEngineering = NULL
    ),
    modelDesign = list(
      modelSettings = list(
        settings = list(predict = "predictCyclops")
      )
    )
  )
  class(plpModel) <- "plpModel"
  attr(plpModel, "modelType") <- "binary"

  prediction <- predictSimulationOutcomeRisk(
    outcomeModel = c("(Intercept)" = 0, "1002" = 1),
    plpModel = plpModel,
    cohorts = data.frame(rowId = 1),
    covariateData = covariateData,
    metaData = list()
  )

  expect_equal(prediction$rawValue, 0.5)
  expect_equal(prediction$value, stats::plogis(0.5))
})
test_that("simulatePlpBenchmarkData generates known risks from sampled covariates", {
  cohorts <- data.frame(
    rowId = c(10, 20, 30),
    subjectId = 101:103,
    targetId = rep(1, 3),
    cohortStartDate = as.Date("2020-01-01") + 0:2,
    daysFromObsStart = rep(100, 3),
    daysToCohortEnd = rep(365, 3),
    daysToObsEnd = c(6, 8, 365),
    ageYear = c(40, 50, 60),
    gender = c(8532, 8507, 8532)
  )
  covariates <- data.frame(
    rowId = c(10, 20, 30, 30),
    covariateId = c(100, 100, 100, 200),
    covariateValue = c(1, 2, 0, 1)
  )
  covariateRef <- data.frame(
    covariateId = c(100, 200),
    covariateName = c("test covariate", "other covariate"),
    analysisId = c(1, 1)
  )
  analysisRef <- data.frame(
    analysisId = 1,
    analysisName = "binary",
    isBinary = "Y"
  )
  plpData <- list(
    cohorts = cohorts,
    outcomes = data.frame(rowId = integer(), outcomeId = integer(), outcomeCount = integer(), daysToEvent = integer()),
    covariateData = list(
      covariates = covariates,
      covariateRef = covariateRef,
      analysisRef = analysisRef
    ),
    timeRef = NULL,
    metaData = list(databaseDetails = list(outcomeIds = 3))
  )
  class(plpData) <- "plpData"

  benchmarkData <- simulatePlpBenchmarkData(
    plpData = plpData,
    outcomeModel = c("(Intercept)" = -1, "100" = 0.5),
    n = 8,
    riskWindowStart = 5,
    riskWindowEnd = 9,
    outcomeId = 99,
    seed = 42
  )

  expect_s3_class(benchmarkData, "plpData")
  expect_equal(nrow(benchmarkData$cohorts), 8)
  expect_equal(benchmarkData$cohorts$rowId, 1:8)
  expect_equal(length(unique(benchmarkData$cohorts$subjectId)), 8)
  expect_equal(benchmarkData$metaData$databaseDetails$outcomeIds, 99)
  expect_equal(attr(benchmarkData$covariateData, "metaData")$cohortId, 1)
  expect_equal(attr(benchmarkData$covariateData, "metaData")$cohortIds, 1)

  truth <- benchmarkData$simulationTruth
  expect_null(attr(benchmarkData, "simulationTruth"))
  expect_equal(benchmarkData$simulationSettings$outcomeId, 99)
  expect_equal(benchmarkData$simulationSettings$targetId, 1)
  expect_true(benchmarkData$simulationSettings$sampleWithReplacement)
  expect_true(is.data.frame(truth))
  expect_equal(nrow(truth), 8)
  expect_true(all(truth$sourceRowId %in% cohorts$rowId))
  sourceSubjects <- cohorts$subjectId[match(truth$sourceRowId, cohorts$rowId)]
  expect_equal(truth$sourceSubjectId, sourceSubjects)
  expect_true(anyDuplicated(truth$sourceRowId) > 0)

  simulatedCovariates <- benchmarkData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 100) %>%
    dplyr::collect()
  covariate100 <- rep(0, 8)
  covariate100[simulatedCovariates$rowId] <- simulatedCovariates$covariateValue
  expectedLinearPredictor <- -1 + 0.5 * covariate100
  expect_equal(truth$linearPredictor, expectedLinearPredictor)
  expect_equal(truth$trueRisk, stats::plogis(expectedLinearPredictor))

  if (nrow(benchmarkData$outcomes) > 0) {
    outcomeCohorts <- merge(
      benchmarkData$outcomes,
      benchmarkData$cohorts[, c("rowId", "daysToObsEnd")],
      by = "rowId"
    )
    expect_true(all(benchmarkData$outcomes$daysToEvent >= 5))
    expect_true(all(benchmarkData$outcomes$daysToEvent <= 9))
    expect_true(all(outcomeCohorts$daysToEvent <= outcomeCohorts$daysToObsEnd))
    expect_equal(unique(benchmarkData$outcomes$outcomeId), 99)
  }
})

test_that("simulatePlpBenchmarkData resamples Andromeda covariateData", {
  covariateData <- Andromeda::andromeda(
    covariates = data.frame(
      rowId = c(1, 2, 3, 3),
      covariateId = c(100, 100, 100, 200),
      covariateValue = c(1, 0, 2, 1)
    ),
    covariateRef = data.frame(
      covariateId = c(100, 200),
      covariateName = c("test covariate", "other covariate"),
      analysisId = c(1, 1)
    ),
    analysisRef = data.frame(
      analysisId = 1,
      analysisName = "binary",
      isBinary = "Y"
    )
  )
  attr(covariateData, "metaData") <- list(
    populationSize = 3,
    cohortIds = 11,
    covariateSettings = "preserve"
  )
  on.exit(Andromeda::close(covariateData), add = TRUE)

  plpData <- list(
    cohorts = data.frame(rowId = 1:3, subjectId = 1:3, targetId = 11, daysToObsEnd = 365),
    outcomes = data.frame(rowId = integer(), outcomeId = integer(), outcomeCount = integer(), daysToEvent = integer()),
    covariateData = covariateData,
    metaData = list(databaseDetails = list(outcomeIds = 3))
  )
  class(plpData) <- "plpData"

  benchmarkData <- simulatePlpBenchmarkData(
    plpData = plpData,
    outcomeModel = c("(Intercept)" = -2, "100" = 0.75),
    n = 6,
    seed = 42
  )
  on.exit(Andromeda::close(benchmarkData$covariateData), add = TRUE)

  expect_s3_class(benchmarkData, "plpData")
  expect_s4_class(benchmarkData$covariateData, "CovariateData")
  expect_equal(attr(benchmarkData$covariateData, "metaData")$populationSize, 6)
  expect_equal(attr(benchmarkData$covariateData, "metaData")$cohortId, 11)
  expect_equal(attr(benchmarkData$covariateData, "metaData")$cohortIds, 11)
  expect_equal(attr(benchmarkData$covariateData, "metaData")$covariateSettings, "preserve")

  simulatedCovariates <- benchmarkData$covariateData$covariates %>%
    dplyr::collect()
  expect_true(all(simulatedCovariates$rowId %in% 1:6))
  expect_true(all(simulatedCovariates$covariateId %in% c(100, 200)))

  truth <- benchmarkData$simulationTruth
  covariate100 <- rep(0, 6)
  covariate100[simulatedCovariates$rowId[simulatedCovariates$covariateId == 100]] <-
    simulatedCovariates$covariateValue[simulatedCovariates$covariateId == 100]
  expect_equal(truth$linearPredictor, -2 + 0.75 * covariate100)
})

test_that("simulatePlpBenchmarkData uses predictPlp for PLP model outcome generation", {
  covariateData <- Andromeda::andromeda(
    covariates = data.frame(
      rowId = c(1, 2),
      covariateId = c(100, 100),
      covariateValue = c(10, 20)
    ),
    covariateRef = data.frame(
      covariateId = 100,
      covariateName = "continuous covariate",
      analysisId = 1
    ),
    analysisRef = data.frame(
      analysisId = 1,
      analysisName = "continuous",
      isBinary = "N"
    )
  )
  on.exit(Andromeda::close(covariateData), add = TRUE)

  plpData <- list(
    cohorts = data.frame(rowId = 1:2, subjectId = 1:2, targetId = 1, daysToObsEnd = 365),
    outcomes = data.frame(rowId = integer(), outcomeId = integer(), outcomeCount = integer(), daysToEvent = integer()),
    covariateData = covariateData,
    metaData = list(databaseDetails = list(outcomeIds = 3))
  )
  class(plpData) <- "plpData"

  plpModel <- createGlmModel(
    coefficients = data.frame(covariateId = 100, coefficient = 1),
    intercept = -2,
    tidyCovariates = list(
      normFactors = data.frame(covariateId = 100, maxValue = 10),
      deletedRedundantCovariateIds = numeric(),
      deletedInfrequentCovariateIds = numeric()
    )
  )

  benchmarkData <- simulatePlpBenchmarkData(
    plpData = plpData,
    outcomeModel = plpModel,
    n = 6,
    seed = 42
  )
  on.exit(Andromeda::close(benchmarkData$covariateData), add = TRUE)

  truth <- benchmarkData$simulationTruth
  sourceCovariates <- data.frame(sourceRowId = 1:2, sourceValue = c(10, 20))
  truth <- merge(truth, sourceCovariates, by = "sourceRowId", sort = FALSE)
  expectedLinearPredictor <- -2 + truth$sourceValue / 10
  expect_equal(truth$linearPredictor, expectedLinearPredictor)

  simulatedCovariates <- benchmarkData$covariateData$covariates %>%
    dplyr::collect()
  expect_true(any(simulatedCovariates$covariateValue == 20))
})

test_that("simulatePlpBenchmarkData accepts runPlp results as outcome models", {
  covariateData <- Andromeda::andromeda(
    covariates = data.frame(rowId = 1, covariateId = 100, covariateValue = 1),
    covariateRef = data.frame(covariateId = 100, covariateName = "test covariate", analysisId = 1),
    analysisRef = data.frame(analysisId = 1, analysisName = "binary", isBinary = "Y")
  )
  on.exit(Andromeda::close(covariateData), add = TRUE)

  plpData <- list(
    cohorts = data.frame(rowId = 1, subjectId = 1, targetId = 1, daysToObsEnd = 365),
    outcomes = data.frame(rowId = integer(), outcomeId = integer(), outcomeCount = integer(), daysToEvent = integer()),
    covariateData = covariateData,
    metaData = list(databaseDetails = list(outcomeIds = 3))
  )
  class(plpData) <- "plpData"

  plpResult <- list(
    model = createGlmModel(
      coefficients = data.frame(covariateId = 100, coefficient = 0.5),
      intercept = -1
    )
  )
  class(plpResult) <- "runPlp"

  benchmarkData <- simulatePlpBenchmarkData(
    plpData = plpData,
    outcomeModel = plpResult,
    n = 3,
    seed = 42
  )
  on.exit(Andromeda::close(benchmarkData$covariateData), add = TRUE)

  expect_equal(benchmarkData$simulationTruth$linearPredictor, rep(-0.5, 3))
})

test_that("simulatePlpBenchmarkData can omit simulation truth", {
  plpData <- list(
    cohorts = data.frame(rowId = 1:2, subjectId = 1:2, targetId = 1, daysToObsEnd = 365),
    outcomes = data.frame(rowId = integer(), outcomeId = integer(), outcomeCount = integer(), daysToEvent = integer()),
    covariateData = list(
      covariates = data.frame(rowId = 1:2, covariateId = 100, covariateValue = c(0, 1)),
      covariateRef = data.frame(covariateId = 100, covariateName = "test covariate", analysisId = 1)
    ),
    metaData = list(databaseDetails = list(outcomeIds = 3))
  )
  class(plpData) <- "plpData"

  benchmarkData <- simulatePlpBenchmarkData(
    plpData = plpData,
    outcomeModel = c("(Intercept)" = -1, "100" = 1),
    n = 5,
    seed = 42,
    returnTruth = FALSE
  )

  expect_s3_class(benchmarkData, "plpData")
  expect_null(attr(benchmarkData, "simulationTruth"))
  expect_null(benchmarkData$simulationTruth)
  expect_equal(benchmarkData$simulationSettings$returnTruth, FALSE)
})

test_that("simulatePlpBenchmarkData truth and settings survive save/load", {
  covariateData <- Andromeda::andromeda(
    covariates = data.frame(rowId = 1:2, covariateId = 100, covariateValue = c(0, 1)),
    covariateRef = data.frame(covariateId = 100, covariateName = "test covariate", analysisId = 1)
  )
  on.exit(Andromeda::close(covariateData), add = TRUE)

  plpData <- list(
    cohorts = data.frame(rowId = 1:2, subjectId = 1:2, targetId = 1, daysToObsEnd = 365),
    outcomes = data.frame(rowId = integer(), outcomeId = integer(), outcomeCount = integer(), daysToEvent = integer()),
    covariateData = covariateData,
    metaData = list(databaseDetails = list(outcomeIds = 3))
  )
  class(plpData) <- "plpData"

  benchmarkData <- simulatePlpBenchmarkData(
    plpData = plpData,
    outcomeModel = c("(Intercept)" = -1, "100" = 1),
    n = 5,
    seed = 42
  )
  on.exit(Andromeda::close(benchmarkData$covariateData), add = TRUE)

  saveLoc <- file.path(tempdir(), paste0("benchmarkSaveLoad", sample.int(1e6, 1)))
  on.exit(unlink(saveLoc, recursive = TRUE), add = TRUE)
  savePlpData(benchmarkData, saveLoc)
  loadedData <- loadPlpData(saveLoc)
  on.exit(Andromeda::close(loadedData$covariateData), add = TRUE)

  expect_equal(loadedData$simulationSettings, benchmarkData$simulationSettings)
  expect_equal(loadedData$simulationTruth, benchmarkData$simulationTruth)
  expect_null(attr(loadedData, "simulationTruth"))
})

test_that("simulatePlpBenchmarkData returns well-formed data when no outcomes are generated", {
  plpData <- list(
    cohorts = data.frame(rowId = 1:2, subjectId = 1:2, targetId = 1, daysToObsEnd = 365),
    outcomes = data.frame(rowId = integer(), outcomeId = integer(), outcomeCount = integer(), daysToEvent = integer()),
    covariateData = list(
      covariates = data.frame(rowId = 1:2, covariateId = 100, covariateValue = c(0, 1)),
      covariateRef = data.frame(covariateId = 100, covariateName = "test covariate", analysisId = 1)
    ),
    metaData = list(databaseDetails = list(outcomeIds = 3))
  )
  class(plpData) <- "plpData"

  benchmarkData <- simulatePlpBenchmarkData(
    plpData = plpData,
    outcomeModel = c("(Intercept)" = -1000, "100" = 1),
    n = 5,
    seed = 42
  )

  expect_equal(nrow(benchmarkData$outcomes), 0)
  expect_equal(names(benchmarkData$outcomes), c("rowId", "outcomeId", "outcomeCount", "daysToEvent"))
  expect_equal(benchmarkData$metaData$databaseDetails$outcomeIds, 3)
  expect_equal(attr(benchmarkData$outcomes, "metaData")$outcomeIds, 3)

  truth <- benchmarkData$simulationTruth
  expect_equal(truth$outcomeCount, rep(0, 5))
  expect_true(all(is.na(truth$daysToEvent)))
})

test_that("simulatePlpBenchmarkData validates benchmark inputs", {
  plpData <- list(
    cohorts = data.frame(rowId = 1:2, subjectId = 1:2, targetId = 1, daysToObsEnd = 365),
    covariateData = list(
      covariates = data.frame(rowId = 1:2, covariateId = 100, covariateValue = c(0, 1)),
      covariateRef = data.frame(covariateId = 100, covariateName = "test covariate", analysisId = 1)
    ),
    metaData = list(databaseDetails = list(outcomeIds = 3))
  )
  class(plpData) <- "plpData"

  expect_error(
    simulatePlpBenchmarkData(plpData, c("(Intercept)" = -1), seed = 1.5),
    "Seed"
  )
  expect_error(
    simulatePlpBenchmarkData(plpData, c("(Intercept)" = -1), seed = NA),
    "Seed"
  )
  expect_error(
    simulatePlpBenchmarkData(list(covariateData = plpData$covariateData), c("(Intercept)" = -1)),
    "plpData must contain cohorts"
  )
  missingTargetData <- plpData
  missingTargetData$cohorts$targetId <- NULL
  expect_error(
    simulatePlpBenchmarkData(missingTargetData, c("(Intercept)" = -1)),
    "targetId"
  )
  multipleTargetData <- plpData
  multipleTargetData$cohorts$targetId <- 1:2
  expect_error(
    simulatePlpBenchmarkData(multipleTargetData, c("(Intercept)" = -1)),
    "exactly one finite targetId"
  )
  missingObsEndData <- plpData
  missingObsEndData$cohorts$daysToObsEnd <- NULL
  expect_error(
    simulatePlpBenchmarkData(missingObsEndData, c("(Intercept)" = -1)),
    "daysToObsEnd"
  )
  expect_error(
    simulatePlpBenchmarkData(plpData, list("(Intercept)" = -1)),
    "named finite numeric vector"
  )
  expect_error(
    simulatePlpBenchmarkData(plpData, c("(Intercept)" = NA)),
    "named finite numeric vector"
  )
  expect_error(
    simulatePlpBenchmarkData(plpData, c("(Intercept)" = Inf)),
    "named finite numeric vector"
  )
  expect_error(
    simulatePlpBenchmarkData(plpData, stats::setNames(c(-1, 1), c("100", "100"))),
    "coefficient names must be unique"
  )
  expect_error(
    simulatePlpBenchmarkData(plpData, stats::setNames(c(-1, 1), c("(Intercept)", ""))),
    "names for all coefficients"
  )
  expect_error(
    suppressWarnings(simulatePlpBenchmarkData(plpData, c("(Intercept)" = -1, abc = 1))),
    "numeric covariate IDs"
  )
  expect_error(
    simulatePlpBenchmarkData(plpData, c("(Intercept)" = -1), n = 0),
    "positive finite scalar integer"
  )
  expect_error(
    simulatePlpBenchmarkData(plpData, c("(Intercept)" = -1), n = NA),
    "positive finite scalar integer"
  )
  expect_error(
    simulatePlpBenchmarkData(plpData, c("(Intercept)" = -1), n = Inf),
    "positive finite scalar integer"
  )
  expect_error(
    simulatePlpBenchmarkData(plpData, c("(Intercept)" = -1), riskWindowStart = 1.5),
    "riskWindowStart"
  )
  expect_error(
    simulatePlpBenchmarkData(plpData, c("(Intercept)" = -1), riskWindowStart = NA),
    "riskWindowStart"
  )
  expect_error(
    simulatePlpBenchmarkData(plpData, c("(Intercept)" = -1), riskWindowEnd = 1.5),
    "riskWindowEnd"
  )
  expect_error(
    simulatePlpBenchmarkData(plpData, c("(Intercept)" = -1), riskWindowEnd = Inf),
    "riskWindowEnd"
  )
  expect_error(
    simulatePlpBenchmarkData(plpData, c("(Intercept)" = -1), riskWindowStart = 10, riskWindowEnd = 1),
    "greater than or equal"
  )
  expect_error(
    simulatePlpBenchmarkData(plpData, c("(Intercept)" = -1), targetOutcomeRate = 1),
    "between 0 and 1"
  )
  expect_error(
    simulatePlpBenchmarkData(plpData, c("(Intercept)" = -1), targetOutcomeRate = NA),
    "between 0 and 1"
  )
  expect_error(
    simulatePlpBenchmarkData(plpData, c("(Intercept)" = -1), returnTruth = NA),
    "returnTruth"
  )
  expect_error(
    simulatePlpBenchmarkData(plpData, c("(Intercept)" = -1), returnTruth = "TRUE"),
    "returnTruth"
  )
  missingOutcomeData <- plpData
  missingOutcomeData$metaData$databaseDetails$outcomeIds <- NULL
  expect_error(
    simulatePlpBenchmarkData(missingOutcomeData, c("(Intercept)" = -1)),
    "outcomeId must be specified"
  )
  multipleOutcomeData <- plpData
  multipleOutcomeData$metaData$databaseDetails$outcomeIds <- c(3, 4)
  expect_error(
    simulatePlpBenchmarkData(multipleOutcomeData, c("(Intercept)" = -1)),
    "outcomeId must be specified"
  )
  explicitOutcomeData <- simulatePlpBenchmarkData(
    multipleOutcomeData,
    c("(Intercept)" = -1),
    outcomeId = 9,
    seed = 42
  )
  expect_equal(explicitOutcomeData$simulationSettings$outcomeId, 9)
  expect_error(
    simulatePlpBenchmarkData(plpData, c("(Intercept)" = -1), outcomeId = NA),
    "outcomeId"
  )

  missingFollowUpData <- plpData
  missingFollowUpData$cohorts$daysToObsEnd[1] <- NA
  expect_error(
    simulatePlpBenchmarkData(missingFollowUpData, c("(Intercept)" = -1)),
    "must not contain missing values"
  )
  noObservableData <- plpData
  noObservableData$cohorts$daysToObsEnd <- 0
  expect_error(
    simulatePlpBenchmarkData(noObservableData, c("(Intercept)" = -1), riskWindowStart = 1),
    "No cohort rows"
  )
  missingCohortEndData <- plpData
  missingCohortEndData$cohorts$daysToCohortEnd <- c(365, NA)
  expect_error(
    simulatePlpBenchmarkData(missingCohortEndData, c("(Intercept)" = -1)),
    "daysToCohortEnd"
  )
  expect_error(
    simulatePlpBenchmarkData(
      plpData,
      c("(Intercept)" = 0, "100" = Inf),
      targetOutcomeRate = 0.5
    ),
    "named finite numeric vector"
  )

  benchmarkData <- simulatePlpBenchmarkData(plpData, c("100" = 1), n = 5, seed = 42)
  truth <- benchmarkData$simulationTruth
  expect_equal(truth$linearPredictor, as.numeric(truth$sourceRowId == 2))
})

test_that("simulatePlpBenchmarkData can target the mean true risk", {
  cohorts <- data.frame(rowId = 1:2, subjectId = 1:2, targetId = 1, daysToObsEnd = 365)
  covariates <- data.frame(rowId = c(1, 2), covariateId = c(100, 100), covariateValue = c(0, 1))
  covariateRef <- data.frame(covariateId = 100, covariateName = "test covariate", analysisId = 1)
  analysisRef <- data.frame(analysisId = 1, analysisName = "binary", isBinary = "Y")
  plpData <- list(
    cohorts = cohorts,
    outcomes = data.frame(rowId = integer(), outcomeId = integer(), outcomeCount = integer(), daysToEvent = integer()),
    covariateData = list(covariates = covariates, covariateRef = covariateRef, analysisRef = analysisRef),
    metaData = list(databaseDetails = list(outcomeIds = 3))
  )
  class(plpData) <- "plpData"

  benchmarkData <- simulatePlpBenchmarkData(
    plpData = plpData,
    outcomeModel = c("(Intercept)" = -1, "100" = 1),
    n = 20,
    targetOutcomeRate = 0.25,
    seed = 123
  )
  truth <- benchmarkData$simulationTruth
  expect_equal(mean(truth$trueRisk), 0.25, tolerance = 1e-5)
})

test_that("simulatePlpBenchmarkData rejects unavailable outcome covariates", {
  plpData <- list(
    cohorts = data.frame(rowId = 1, subjectId = 1, targetId = 1, daysToObsEnd = 365),
    covariateData = list(
      covariates = data.frame(rowId = 1, covariateId = 100, covariateValue = 1),
      covariateRef = data.frame(covariateId = 100, covariateName = "test covariate", analysisId = 1)
    ),
    metaData = list(databaseDetails = list(outcomeIds = 3))
  )
  class(plpData) <- "plpData"

  expect_error(
    simulatePlpBenchmarkData(
      plpData = plpData,
      outcomeModel = c("(Intercept)" = -1, "999" = 1)
    ),
    "999"
  )
})

test_that("simulatePlpBenchmarkData samples only rows observable at risk start", {
  plpData <- list(
    cohorts = data.frame(
      rowId = 1:2,
      subjectId = 1:2,
      targetId = 1,
      daysToObsEnd = c(0, 10)
    ),
    covariateData = list(
      covariates = data.frame(rowId = 1:2, covariateId = 100, covariateValue = 1),
      covariateRef = data.frame(covariateId = 100, covariateName = "test covariate", analysisId = 1)
    ),
    metaData = list(databaseDetails = list(outcomeIds = 3))
  )
  class(plpData) <- "plpData"

  expect_warning(
    benchmarkData <- simulatePlpBenchmarkData(
      plpData = plpData,
      outcomeModel = c("(Intercept)" = 20),
      n = 10,
      riskWindowStart = 1,
      riskWindowEnd = 20,
      seed = 42
    ),
    "without observable time"
  )
  truth <- benchmarkData$simulationTruth
  expect_equal(unique(truth$sourceRowId), 2)
  expect_true(all(benchmarkData$outcomes$daysToEvent <= 10))
})

test_that("simulatePlpBenchmarkData samples events within target cohort end", {
  plpData <- list(
    cohorts = data.frame(
      rowId = 1:2,
      subjectId = 1:2,
      targetId = 242,
      cohortStartDate = as.Date("2020-01-01"),
      daysFromObsStart = 365,
      daysToCohortEnd = c(5, 20),
      daysToObsEnd = 365,
      ageYear = 50,
      gender = 8532
    ),
    outcomes = data.frame(rowId = integer(), outcomeId = integer(), outcomeCount = integer(), daysToEvent = integer()),
    covariateData = list(
      covariates = data.frame(rowId = 1:2, covariateId = 100, covariateValue = 1),
      covariateRef = data.frame(covariateId = 100, covariateName = "test covariate", analysisId = 1)
    ),
    metaData = list(databaseDetails = list(outcomeIds = 3))
  )
  class(plpData) <- "plpData"

  benchmarkData <- simulatePlpBenchmarkData(
    plpData = plpData,
    outcomeModel = c("(Intercept)" = 20),
    n = 20,
    riskWindowStart = 1,
    riskWindowEnd = 20,
    outcomeId = 99,
    seed = 42
  )
  outcomeCohorts <- merge(
    benchmarkData$outcomes,
    benchmarkData$cohorts[, c("rowId", "daysToCohortEnd")],
    by = "rowId"
  )
  expect_true(all(outcomeCohorts$daysToEvent <= outcomeCohorts$daysToCohortEnd))
  expect_equal(attr(benchmarkData$covariateData, "metaData")$cohortId, 242)

  population <- createStudyPopulation(
    plpData = benchmarkData,
    outcomeId = 99,
    populationSettings = createStudyPopulationSettings(
      removeSubjectsWithPriorOutcome = FALSE,
      requireTimeAtRisk = FALSE,
      riskWindowStart = 1,
      riskWindowEnd = 20,
      restrictTarToCohortEnd = TRUE
    )
  )
  truth <- benchmarkData$simulationTruth
  expect_equal(mean(population$outcomeCount), mean(truth$outcomeCount))
})

test_that("simulatePlpBenchmarkData rejects temporal covariate data", {
  plpData <- list(
    cohorts = data.frame(rowId = 1, subjectId = 1, targetId = 1, daysToObsEnd = 365),
    covariateData = list(
      covariates = data.frame(rowId = 1, covariateId = 100, covariateValue = 1, timeId = 1),
      covariateRef = data.frame(covariateId = 100, covariateName = "test covariate", analysisId = 1),
      timeRef = data.frame(timeId = 1, startDay = -365, endDay = -1)
    ),
    metaData = list(databaseDetails = list(outcomeIds = 3))
  )
  class(plpData) <- "plpData"

  expect_error(
    simulatePlpBenchmarkData(
      plpData = plpData,
      outcomeModel = c("(Intercept)" = -1, "100" = 1)
    ),
    "temporal covariateData"
  )
})

test_that("simulatePlpBenchmarkData targets outcome rate with shifted linear predictors", {
  plpData <- list(
    cohorts = data.frame(rowId = 1:2, subjectId = 1:2, targetId = 1, daysToObsEnd = 365),
    covariateData = list(
      covariates = data.frame(rowId = 1:2, covariateId = 100, covariateValue = c(0, 1)),
      covariateRef = data.frame(covariateId = 100, covariateName = "test covariate", analysisId = 1)
    ),
    metaData = list(databaseDetails = list(outcomeIds = 3))
  )
  class(plpData) <- "plpData"

  benchmarkData <- simulatePlpBenchmarkData(
    plpData = plpData,
    outcomeModel = c("(Intercept)" = 100, "100" = 1),
    n = 20,
    targetOutcomeRate = 0.2,
    seed = 42
  )
  truth <- benchmarkData$simulationTruth
  expect_equal(mean(truth$trueRisk), 0.2, tolerance = 1e-5)
})
