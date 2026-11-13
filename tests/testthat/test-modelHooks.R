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

# A package with model hooks that is loaded but not attached, as a package
# that supplies a model (such as DeepPatientLevelPrediction) is when its
# hooks are stored as "pkg::fn" strings and the model is used later
local_hook_package <- function(env = parent.frame()) {
  skip_if_not_installed("pkgload")
  dir <- tempfile("plpHookTest")
  dir.create(file.path(dir, "R"), recursive = TRUE)
  writeLines(c(
    "Package: plpHookTest", "Version: 0.0.1", "Title: Model Hooks For Tests",
    "Description: Model hooks for the PatientLevelPrediction tests.",
    "License: Apache License 2.0", "Encoding: UTF-8"
  ), file.path(dir, "DESCRIPTION"))
  writeLines(
    paste0("export(", c("trainHook", "predictHook", "varImpHook", "saveLoadHook"), ")"),
    file.path(dir, "NAMESPACE")
  )
  writeLines(c(
    "trainHook <- function(dataMatrix, labels, hyperParameters, settings) {",
    "  list(value = hyperParameters$value)",
    "}",
    "predictHook <- function(plpModel, data, cohort) {",
    "  model <- if (inherits(plpModel, 'plpModel')) plpModel$model else plpModel",
    "  prediction <- cohort",
    "  if (!is.null(prediction$originalRowId)) {",
    "    prediction$rowId <- prediction$originalRowId",
    "    prediction$originalRowId <- NULL",
    "  }",
    "  prediction$value <- rep(model$value, nrow(prediction))",
    "  attr(prediction, 'metaData') <- list(modelType = 'binary')",
    "  prediction",
    "}",
    "varImpHook <- function(model, covariateMap) NULL",
    "saveLoadHook <- function() {",
    "  list(",
    "    fileName = 'model.rds',",
    "    save = function(model, file) saveRDS(model, file),",
    "    load = function(file) readRDS(file)",
    "  )",
    "}"
  ), file.path(dir, "R", "hooks.R"))
  pkgload::load_all(dir, attach = FALSE, export_all = FALSE, quiet = TRUE)
  withr::defer({
    pkgload::unload("plpHookTest")
    unlink(dir, recursive = TRUE)
  }, envir = env)
  invisible(dir)
}

hookModelSettings <- function(hooks) {
  settings <- utils::modifyList(list(
    modelName = "hookModel",
    modelType = "binary",
    seed = 42,
    prepareData = "toSparseM",
    train = "plpHookTest::trainHook",
    predict = "plpHookTest::predictHook",
    variableImportance = "plpHookTest::varImpHook",
    saveType = "plpHookTest::saveLoadHook"
  ), hooks)
  structure(
    list(param = list(value = list(0.3)), settings = settings),
    class = "modelSettings"
  )
}

constantMetric <- createHyperparameterSettings(
  tuningMetric = createTuningMetric(fun = function(prediction) 0.5, name = "constant")
)

test_that("resolveHook accepts functions, names and pkg::fn", {
  local_hook_package()
  expect_identical(resolveHook(mean, "train"), mean)
  # the package's own functions, even when not exported
  expect_identical(resolveHook("toSparseM", "prepareData"), toSparseM)
  expect_identical(resolveHook("PatientLevelPrediction::predictPlp", "predict"), predictPlp)
  # an exported function of a package that is loaded but not attached
  expect_false("package:plpHookTest" %in% search())
  expect_true(is.function(resolveHook("plpHookTest::trainHook", "train")))
  expect_false("package:plpHookTest" %in% search())
})

test_that("resolveHook fails with a message that names the hook", {
  local_hook_package()
  expect_error(resolveHook("noSuchFunction", "train"), "'noSuchFunction' given as the train hook")
  expect_error(
    resolveHook("plpHookTest::noSuchFunction", "predict"),
    "'plpHookTest::noSuchFunction' given as the predict hook"
  )
  expect_error(resolveHook("notAPackage123::fn", "saveType"), "needs the package notAPackage123")
  expect_error(resolveHook(NULL, "variableImportance"), "variableImportance hook must be a function")
  expect_error(resolveHook("a::b::c", "train"), "not of the form pkg::function")
})

test_that("a model with pkg::fn hooks from an unattached package fits, saves, loads and predicts", {
  skip_if_no_eunomia()
  local_hook_package()
  saveDir <- tempfile("hookModel")
  withr::defer(unlink(saveDir, recursive = TRUE))

  plpModel <- fitPlp(
    trainData = copyTrainData(tinyTrainData),
    modelSettings = hookModelSettings(list()),
    hyperparameterSettings = constantMetric,
    analysisId = "hooks",
    analysisPath = saveDir
  )
  expect_equal(plpModel$model$value, 0.3)

  savePlpModel(plpModel, file.path(saveDir, "model"))
  # the save handler names its own file
  expect_true(file.exists(file.path(saveDir, "model", "model.rds")))
  expect_false(file.exists(file.path(saveDir, "model", "model.json")))

  loaded <- loadPlpModel(file.path(saveDir, "model"))
  expect_equal(loaded$model$value, 0.3)
  prediction <- predictPlp(loaded, tinyTrainData, tinyTrainData$labels)
  expect_equal(nrow(prediction), nrow(tinyTrainData$labels))
  expect_true(all(prediction$value == 0.3))
})

test_that("hooks given as function objects work in fitting", {
  skip_if_no_eunomia()
  local_hook_package()
  plpModel <- fitPlp(
    trainData = copyTrainData(tinyTrainData),
    modelSettings = hookModelSettings(list(
      prepareData = toSparseM,
      train = getExportedValue("plpHookTest", "trainHook"),
      predict = getExportedValue("plpHookTest", "predictHook"),
      variableImportance = getExportedValue("plpHookTest", "varImpHook")
    )),
    hyperparameterSettings = constantMetric,
    analysisId = "hookFunctions",
    analysisPath = tempdir()
  )
  expect_equal(plpModel$model$value, 0.3)
})

test_that("saving a model whose saveType cannot be found fails instead of saving nothing", {
  local_hook_package()
  expect_error(
    saveModelPart(list(value = 1), "plpHookTest::noSuchHandler", tempfile()),
    "given as the saveType hook"
  )
  expect_error(saveModelPart(list(value = 1), NULL, tempfile()), "no saveType")
})

test_that("saveTypes of earlier versions still mean a folder of model files", {
  for (saveType in c("file", "xgboost", "lightgbm")) {
    expect_null(saveHandler(saveType))
  }
  expect_null(saveHandler(NULL))
  expect_equal(saveHandler("RtoJson")$fileName, "model.json")
})

test_that("hooks are all found before fitting starts", {
  skip_if_no_eunomia()
  local_hook_package()
  trained <- FALSE
  countingTrain <- function(dataMatrix, labels, hyperParameters, settings) {
    trained <<- TRUE
    list(value = hyperParameters$value)
  }
  for (hooks in list(
    list(saveType = "plpHookTest::saveLoadHookMisspelled"),
    list(variableImportance = "plpHookTest::varImpMisspelled"),
    list(predict = "plpHookTest::predictMisspelled")
  )) {
    expect_error(
      fitPlp(
        trainData = copyTrainData(tinyTrainData),
        modelSettings = hookModelSettings(c(list(train = countingTrain), hooks)),
        hyperparameterSettings = constantMetric,
        analysisId = "missingHook",
        analysisPath = tempdir()
      ),
      "Misspelled' given as the"
    )
  }
  expect_false(trained)
})

test_that("a save handler writes only inside the model folder", {
  handler <- function(fileName) {
    function() list(fileName = fileName, save = function(model, file) NULL, load = function(file) NULL)
  }
  for (fileName in c("../model.rds", "/tmp/model.rds", "sub/model.rds", "..", "")) {
    expect_error(saveHandler(handler(fileName)), "plain file or folder name")
  }
  expect_equal(saveHandler(handler("model.pt"))$fileName, "model.pt")
})
