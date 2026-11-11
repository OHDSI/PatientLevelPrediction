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
# Test unit for the creation of the study externalValidatePlp
if (isTRUE(eunomiaAvailable) && identical(Sys.getenv("NOT_CRAN"), "true")) {
  modelVal <- loadPlpModel(file.path(saveLoc, "Test", "plpResult", "model"))
  validationDatabaseDetailsVal <- databaseDetails # from run multiple tests
  validationRestrictPlpDataSettingsVal <- createRestrictPlpDataSettings(washoutPeriod = 0, sampleSize = NULL)
  recalSet <- createValidationSettings(recalibrate = "weakRecalibration")
  saveLocation <- file.path(saveLoc, "extern")
  setEV <- function(model = modelVal,
                    validationDatabaseDetails = validationDatabaseDetailsVal,
                    validationRestrictPlpDataSettings = validationRestrictPlpDataSettingsVal,
                    settings = recalSet,
                    outputFolder = saveLocation) {
    result <- externalValidateDbPlp(
      plpModel = model,
      validationDatabaseDetails = validationDatabaseDetails,
      validationRestrictPlpDataSettings = validationRestrictPlpDataSettings,
      settings = settings,
      outputFolder = outputFolder
    )

    return(result)
  }
}

test_that("incorrect input externalValidateDbPlp checks work", {
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  # fails when plpResult is NULL
  expect_error(externalValidateDbPlp(setEV(model = NULL)))
  # fails when plpResult is not class 'plpResult'
  expect_error(externalValidateDbPlp(setEV(model = list())))


  expect_error(externalValidateDbPlp(
    setEV(validationDatabaseDetails = NULL)
  ))

  expect_error(externalValidateDbPlp(
    setEV(validationRestrictPlpDataSettings = NULL)
  ))

  expect_error(externalValidateDbPlp(
    setEV(outputFolder = NULL)
  ))
})



test_that("external validate", {
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  exVal <- setEV()
  expect_equal(class(exVal[[1]]), "externalValidatePlp")
})

test_that("fromDesignOrModel helper works", {
  settingName <- "restrictPlpDataSettings"
  validationDesign <- list(
    targetId = 1,
    outcomeId = 2,
    restrictPlpDataSettings = list(a = 1, b = 2)
  )
  modelDesigns <- list(
    list(
      targetId = 1,
      outcomeId = 2,
      restrictPlpDataSettings = list(a = 3, b = 4)
    ),
    list(
      targetId = 1,
      outcomeId = 2,
      restrictPlpDataSettings = list(a = 3, b = 4)
    )
  )
  output <- fromDesignOrModel(validationDesign, modelDesigns, settingName)

  expect_equal(output[[settingName]], list(a = 1, b = 2))
  validationDesign[[settingName]] <- NULL
  output <- fromDesignOrModel(validationDesign, modelDesigns, settingName)
  expect_equal(output[[settingName]], list(a = 3, b = 4))
})

test_that("createValidationDesign errors", {
  expect_error(createValidationDesign(
    targetId = NULL, outcomeId = 2,
    plpModelList = list()
  ))
  expect_error(createValidationDesign(
    targetId = 1, outcomeId = NULL,
    plpModelList = list()
  ))
  expect_error(createValidationDesign(
    targetId = "a", outcomeId = 2,
    plpModelList = list()
  ))
  expect_error(createValidationDesign(
    targetId = 1, outcomeId = "a",
    plpModelList = list()
  ))
  expect_error(createValidationDesign(
    targetId = 1, outcomeId = 2,
    plpModelList = list(),
    populationSettings = list()
  ))
  expect_error(createValidationDesign(
    targetId = 1, outcomeId = 2,
    plpModelList = list(),
    recalibrate = 1
  ))
  expect_error(createValidationDesign(
    targetId = 1, outcomeId = 2,
    plpModelList = list(),
    runCovariateSummary = 1
  ))
})

test_that("createValidationDesign works with minimal required arguments", {
  targetId <- 1
  outcomeId <- 2
  plpModelList <- list()

  design <- createValidationDesign(
    targetId = targetId,
    outcomeId = outcomeId,
    plpModelList = plpModelList
  )
  expect_s3_class(design, "validationDesign")
  expect_equal(design$targetId, targetId)
  expect_equal(design$outcomeId, outcomeId)
  expect_equal(design$plpModelList, plpModelList)
})

test_that("single createValidationDesign works with all arguments", {
  targetId <- 1
  outcomeId <- 2
  plpModelList <- list("model1", "model2")
  populationSettings <- createStudyPopulationSettings()
  restrictPlpDataSettings <- createRestrictPlpDataSettings() 
  recalibrate <- c("recalibrationInTheLarge")
  runCovariateSummary <- FALSE

  design <- createValidationDesign(
    targetId = targetId,
    outcomeId = outcomeId,
    plpModelList = plpModelList,
    populationSettings = populationSettings,
    restrictPlpDataSettings = restrictPlpDataSettings,
    recalibrate = recalibrate,
    runCovariateSummary = runCovariateSummary
  )
  expect_s3_class(design, "validationDesign")
  expect_equal(design$targetId, targetId)
  expect_equal(design$outcomeId, outcomeId)
  expect_equal(design$plpModelList, plpModelList)
  expect_equal(design$populationSettings, populationSettings)
  expect_equal(design$restrictPlpDataSettings, restrictPlpDataSettings)
  expect_equal(design$recalibrate, recalibrate)
  expect_equal(design$runCovariateSummary, runCovariateSummary)
})

test_that("createValidationDesigns correctly handles multiple restrictSettings", {
  targetId <- 1
  outcomeId <- 2
  plpModelList <- list()
  restrictPlpDataSettings <- list(createRestrictPlpDataSettings(), createRestrictPlpDataSettings())

  design <- createValidationDesign(
    targetId = targetId,
    outcomeId = outcomeId,
    plpModelList = plpModelList,
    restrictPlpDataSettings = restrictPlpDataSettings
  )
  expect_s3_class(design[[1]], "validationDesign")
  expect_equal(design[[1]]$targetId, targetId)
  expect_equal(design[[1]]$outcomeId, outcomeId)
  expect_equal(design[[1]]$plpModelList, plpModelList)
  expect_equal(design[[1]]$restrictPlpDataSettings, restrictPlpDataSettings[[1]])
  expect_equal(design[[2]]$restrictPlpDataSettings, restrictPlpDataSettings[[2]])
  expect_equal(length(design), length(restrictPlpDataSettings))
})

test_that("createValidationSettings errors with <10 outcomes", {
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  tinyRestrictPlpDataSettings <- createRestrictPlpDataSettings(
    sampleSize = 30,
  )

  validationDesign <- createValidationDesign(
    targetId = 1,
    outcomeId = 3,
    plpModelList = list(modelVal),
    restrictPlpDataSettings = tinyRestrictPlpDataSettings
  )

  expect_output(
    validateExternal(
      validationDesignList = validationDesign,
      databaseDetails = databaseDetails,
      logSettings = createLogSettings(),
      outputFolder = saveLocation
    ),
    "skipping validation for design and database"
  )
})

test_that("createDownloadTasks handles single design correctly", {
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  design <- createValidationDesign(
    targetId = 1,
    outcomeId = 2,
    plpModelList = list(modelVal),
    restrictPlpDataSettings = createRestrictPlpDataSettings()
  )
  result <- createDownloadTasks(list(design))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 5)
})

test_that("createDownloadTasks handles multiple designs correctly", {
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  design1 <- createValidationDesign(
    targetId = 1,
    outcomeId = 2,
    plpModelList = list(modelVal),
    restrictPlpDataSettings = createRestrictPlpDataSettings()
  )
  design2 <- createValidationDesign(
    targetId = 3,
    outcomeId = 4,
    plpModelList = list(modelVal),
    restrictPlpDataSettings = createRestrictPlpDataSettings()
  )
  result <- createDownloadTasks(list(design1, design2))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 5)
})

test_that("createDownloadTasks handles duplicated designs correctly", {
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  design <- createValidationDesign(
    targetId = 1,
    outcomeId = 2,
    plpModelList = list(modelVal),
    restrictPlpDataSettings = createRestrictPlpDataSettings()
  )
  result <- createDownloadTasks(list(design, design))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)

  design2 <- createValidationDesign(
    targetId = 3,
    outcomeId = 4,
    plpModelList = list(modelVal),
    restrictPlpDataSettings = createRestrictPlpDataSettings()
  )

  results <- createDownloadTasks(list(design, design2, design))
  expect_s3_class(results, "data.frame")
  expect_equal(nrow(results), 2)
})

test_that("createDownloadTasks with different restrictSettings", {
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  design <- createValidationDesign(
    targetId = 1,
    outcomeId = 2,
    plpModelList = list(modelVal),
    restrictPlpDataSettings = createRestrictPlpDataSettings()
  )
  design2 <- createValidationDesign(
    targetId = 3,
    outcomeId = 4,
    plpModelList = list(modelVal),
    restrictPlpDataSettings = createRestrictPlpDataSettings()
  )
  design3 <- createValidationDesign(
    targetId = 1,
    outcomeId = 2,
    plpModelList = list(modelVal),
    restrictPlpDataSettings = createRestrictPlpDataSettings(sampleSize = 100)
  )

  result <- createDownloadTasks(list(design, design2, design3))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})

test_that("createDownloadTasks works with multiple outcomeIds", {
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  design1 <- createValidationDesign(
    targetId = 1,
    outcomeId = 2,
    plpModelList = list(modelVal),
    restrictPlpDataSettings = createRestrictPlpDataSettings()
  )
  design2 <- createValidationDesign(
    targetId = 1,
    outcomeId = 3,
    plpModelList = list(modelVal),
    restrictPlpDataSettings = createRestrictPlpDataSettings()
  )
  result <- createDownloadTasks(list(design1, design2))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(length(result[1, ]$outcomeIds[[1]]), 2)

  design3 <- createValidationDesign(
    targetId = 1,
    outcomeId = 3,
    plpModelList = list(modelVal),
    restrictPlpDataSettings = createRestrictPlpDataSettings(sampleSize = 100)
  )
  result <- createDownloadTasks(list(design1, design2, design3))
  expect_equal(nrow(result), 2)
})

test_that("createDownloadTasks with multiple covSettings", {
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  modelVal2 <- modelVal
  design1 <- createValidationDesign(
    targetId = 1,
    outcomeId = 2,
    plpModelList = list(modelVal),
    restrictPlpDataSettings = createRestrictPlpDataSettings()
  )
  modelVal2$modelDesign$covariateSettings <-
    FeatureExtraction::createCovariateSettings(useChads2 = TRUE)
  design2 <- createValidationDesign(
    targetId = 1,
    outcomeId = 2,
    plpModelList = list(modelVal2),
    restrictPlpDataSettings = createRestrictPlpDataSettings()
  )
  result <- createDownloadTasks(list(design1, design2))
  expect_equal(nrow(result), 1)
  expect_equal(length(result[1, ]$covariateSettings[[1]]), 2)
})

test_that("createDownloadTasks when restrictSettings come from models", {
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  design1 <- createValidationDesign(
    targetId = 1,
    outcomeId = 2,
    plpModelList = list(modelVal)
  )
  result <- createDownloadTasks(list(design1))
  expect_s3_class(result[1, ]$restrictPlpDataSettings[[1]], "restrictPlpDataSettings")
})


taskModel <- function(...) {
  settings <- FeatureExtraction::createCovariateSettings(...)
  structure(
    list(modelDesign = list(
      covariateSettings = list(settings),
      restrictPlpDataSettings = createRestrictPlpDataSettings()
    )),
    class = "plpModel"
  )
}

taskDesign <- function(models, outcomeId = 2) {
  createValidationDesign(
    targetId = 1,
    outcomeId = outcomeId,
    plpModelList = models,
    restrictPlpDataSettings = createRestrictPlpDataSettings()
  )
}

test_that("createDownloadTasks separates models that define a covariate differently", {
  model365 <- taskModel(useConditionOccurrenceLongTerm = TRUE, longTermStartDays = -365)
  model730 <- taskModel(useConditionOccurrenceLongTerm = TRUE, longTermStartDays = -730)

  # within one design
  result <- createDownloadTasks(list(taskDesign(list(model365, model730))))
  expect_equal(nrow(result), 2)
  expect_equal(sapply(result$covariateSettings, length), c(1, 1))

  # across designs
  result <- createDownloadTasks(list(taskDesign(list(model365)), taskDesign(list(model730))))
  expect_equal(nrow(result), 2)
})

test_that("createDownloadTasks separates models with different concept restrictions", {
  modelA <- taskModel(useConditionOccurrenceLongTerm = TRUE, includedCovariateConceptIds = 101)
  modelB <- taskModel(useConditionOccurrenceLongTerm = TRUE, includedCovariateConceptIds = 202)
  result <- createDownloadTasks(list(taskDesign(list(modelA, modelB))))
  expect_equal(nrow(result), 2)
})

test_that("createDownloadTasks keeps compatible models in one task", {
  shared <- taskModel(useConditionOccurrenceLongTerm = TRUE, longTermStartDays = -365)
  sameDefinition <- taskModel(
    useConditionOccurrenceLongTerm = TRUE, useDemographicsGender = TRUE, longTermStartDays = -365
  )
  otherWindowOtherAnalysis <- taskModel(useDrugExposureLongTerm = TRUE, longTermStartDays = -730)

  result <- createDownloadTasks(list(taskDesign(list(shared, sameDefinition, otherWindowOtherAnalysis))))
  expect_equal(nrow(result), 1)
  expect_equal(length(result$covariateSettings[[1]]), 3)

  # identical settings stay in one task and are not repeated
  result <- createDownloadTasks(list(taskDesign(list(shared, shared))))
  expect_equal(nrow(result), 1)
  expect_equal(length(result$covariateSettings[[1]]), 1)
})

test_that("createDownloadTasks merges cohort covariates with different covariateIds", {
  cohortModel <- function(cohortId) {
    settings <- createCohortCovariateSettings(
      cohortName = paste("cohort", cohortId), settingId = 1,
      cohortDatabaseSchema = "s", cohortTable = "t", cohortId = cohortId
    )
    structure(
      list(modelDesign = list(
        covariateSettings = list(settings),
        restrictPlpDataSettings = createRestrictPlpDataSettings()
      )),
      class = "plpModel"
    )
  }
  result <- createDownloadTasks(list(taskDesign(list(cohortModel(1), cohortModel(2)))))
  expect_equal(nrow(result), 1)

  # the same cohort covariate with another window keeps its covariateId, so it conflicts
  windowModel <- function(startDay) {
    settings <- createCohortCovariateSettings(
      cohortName = "cohort", settingId = 1,
      cohortDatabaseSchema = "s", cohortTable = "t", cohortId = 1, startDay = startDay
    )
    structure(
      list(modelDesign = list(
        covariateSettings = list(settings),
        restrictPlpDataSettings = createRestrictPlpDataSettings()
      )),
      class = "plpModel"
    )
  }
  result <- createDownloadTasks(list(taskDesign(list(windowModel(-30), windowModel(-60)))))
  expect_equal(nrow(result), 2)
})

test_that("splitDesignsByDownloadTask preserves model order and task assignment", {
  model365a <- taskModel(useConditionOccurrenceLongTerm = TRUE, longTermStartDays = -365)
  model730 <- taskModel(useConditionOccurrenceLongTerm = TRUE, longTermStartDays = -730)
  model365b <- taskModel(useConditionOccurrenceLongTerm = TRUE, longTermStartDays = -365)

  designs <- list(taskDesign(list(model365a, model730, model365b)))
  tasks <- createDownloadTasks(designs)
  splitDesigns <- splitDesignsByDownloadTask(designs, tasks)

  expect_equal(nrow(tasks), 2)
  expect_equal(length(splitDesigns), 3)
  expect_equal(lengths(lapply(splitDesigns, `[[`, "plpModelList")), c(1, 1, 1))
  expect_equal(
    sapply(splitDesigns, function(x) x$downloadTaskId),
    c(1L, 2L, 1L)
  )
  expect_s3_class(splitDesigns[[1]], "validationDesign")

  # no conflicts: the design passes through unchanged
  compatible <- list(taskDesign(list(model365a, model365b)))
  compatibleTasks <- createDownloadTasks(compatible)
  compatibleSplit <- splitDesignsByDownloadTask(compatible, compatibleTasks)
  expect_equal(length(compatibleSplit), 1)
  expect_equal(length(compatibleSplit[[1]]$plpModelList), 2)
})

test_that("getData extracts the covariate settings of the design's download task", {
  model365 <- taskModel(useConditionOccurrenceLongTerm = TRUE, longTermStartDays = -365)
  model730 <- taskModel(useConditionOccurrenceLongTerm = TRUE, longTermStartDays = -730)
  designs <- list(taskDesign(list(model365, model730)))
  tasks <- createDownloadTasks(designs)
  splitDesigns <- splitDesignsByDownloadTask(designs, tasks)
  database <- list(cdmDatabaseName = "db")

  folder <- withr::local_tempdir()
  captured <- list()
  with_mocked_bindings(
    {
      getData(splitDesigns[[1]], database, folder, tasks)
      getData(splitDesigns[[2]], database, folder, tasks)
    },
    getPlpData = function(databaseDetails, restrictPlpDataSettings, covariateSettings) {
      captured[[length(captured) + 1]] <<- covariateSettings
      structure(list(covariateData = NULL, metaData = list()), class = "plpData")
    },
    savePlpData = function(plpData, file) invisible(NULL)
  )

  expect_equal(length(captured), 2)
  expect_equal(captured[[1]][[1]]$longTermStartDays, -365)
  expect_equal(captured[[2]][[1]]$longTermStartDays, -730)
})

test_that("validateExternal gives each model the extraction of its own task", {
  model365 <- taskModel(useConditionOccurrenceLongTerm = TRUE, longTermStartDays = -365)
  model730 <- taskModel(useConditionOccurrenceLongTerm = TRUE, longTermStartDays = -730)
  model365b <- taskModel(useConditionOccurrenceLongTerm = TRUE, longTermStartDays = -365)
  design <- taskDesign(list(model365, model730, model365b))
  database <- structure(
    list(cdmDatabaseName = "db", cdmDatabaseSchema = "s", cdmDatabaseId = "1"),
    class = "databaseDetails"
  )

  validated <- list()
  outputFolder <- withr::local_tempdir()
  with_mocked_bindings(
    validateExternal(
      validationDesignList = design,
      databaseDetails = database,
      logSettings = createLogSettings(),
      outputFolder = outputFolder
    ),
    getData = function(design, database, outputFolder, downloadTasks) {
      list(taskId = design$downloadTaskId, settings = downloadTasks$covariateSettings[[design$downloadTaskId]])
    },
    getPopulation = function(validationDesign, modelDesigns, plpData) {
      data.frame(outcomeCount = rep(1, 20))
    },
    validateModel = function(plpModel, plpData, population, recalibrate, runCovariateSummary,
                             outputFolder, databaseName, analysisName) {
      validated[[length(validated) + 1]] <<- list(
        analysisName = analysisName,
        taskId = plpData$taskId,
        modelStart = plpModel$modelDesign$covariateSettings[[1]]$longTermStartDays
      )
      invisible(NULL)
    },
    insertResultsToSqlite = function(...) invisible(NULL),
    createDatabaseList = function(...) invisible(NULL)
  )

  expect_equal(sapply(validated, `[[`, "analysisName"), c("Analysis_1", "Analysis_2", "Analysis_3"))
  expect_equal(sapply(validated, `[[`, "modelStart"), c(-365, -730, -365))
  expect_equal(sapply(validated, `[[`, "taskId"), c(1L, 2L, 1L))
})
