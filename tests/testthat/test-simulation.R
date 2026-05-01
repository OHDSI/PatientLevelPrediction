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
