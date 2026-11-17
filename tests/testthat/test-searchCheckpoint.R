# Copyright 2026 Observational Health Data Sciences and Informatics
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

# A model without tuning hooks whose fits draw from R's random stream, so a
# resumed search matches an uninterrupted one only if the stream is restored.
# calls$stopAt makes the train call with that number fail, as a crash would.
createResumeFixture <- function(n = 60, folds = 3, calls = new.env()) {
  withr::with_seed(1, {
    x <- matrix(stats::rnorm(n * 2), ncol = 2)
    labels <- data.frame(
      rowId = seq_len(n),
      outcomeCount = as.numeric(x[, 1] + stats::rnorm(n) > 0),
      index = rep(seq_len(folds), length.out = n)
    )
  })
  calls$n <- 0
  calls$candidates <- list()
  settings <- list(
    modelType = "binary",
    modelName = "resumeModel",
    resumeSearch = TRUE,
    train = function(dataMatrix, labels, hyperParameters, settings) {
      calls$n <- calls$n + 1
      if (identical(calls$n, calls$stopAt)) {
        stop("simulated crash")
      }
      calls$candidates[[length(calls$candidates) + 1]] <- hyperParameters
      if (isTRUE(hyperParameters$skip)) {
        stopCandidate("cannot fit this one")
      }
      list(slope = hyperParameters$slope + stats::rnorm(1, sd = 0.2))
    },
    predict = function(plpModel, data, cohort) {
      cohort$value <- stats::plogis(plpModel$slope * data[, 1])
      cohort
    }
  )
  list(data = x, labels = labels, settings = settings, calls = calls)
}

# A model with tuning hooks whose warm start (the number of earlier fits on
# the fold) changes its fits; calls$stopCandidate makes that candidate fail
createStatefulResumeFixture <- function(calls) {
  fixture <- createResumeFixture()
  fixture$settings <- list(
    modelType = "binary",
    modelName = "statefulResumeModel",
    seed = 11,
    resumeSearch = TRUE,
    checkpointState = TRUE,
    train = function(dataMatrix, labels, hyperParameters, settings, context) {
      if (isTRUE(context$candidateIndex == calls$stopCandidate)) {
        stop("simulated crash")
      }
      list(
        slope = hyperParameters$slope + 0.1 * length(context$state) + stats::rnorm(1, sd = 0.1),
        stateSize = length(context$state)
      )
    },
    predict = function(plpModel, data, cohort) {
      cohort$value <- stats::plogis(plpModel$slope * data[, 1])
      cohort
    },
    updateState = function(state, model, hyperParameters, settings) {
      c(state, list(model$slope))
    },
    fitInfo = function(model, hyperParameters, settings) list(stateSize = model$stateSize),
    parallelFolds = "fork"
  )
  fixture
}

runSearch <- function(fixture, param, analysisPath, ..., settings = fixture$settings,
                      labels = fixture$labels, hyperparamSettings = createHyperparameterSettings(...)) {
  tuneHyperparameters(
    data = fixture$data, labels = labels, param = param, settings = settings,
    hyperparamSettings = hyperparamSettings, analysisPath = analysisPath
  )
}

withoutTimes <- function(history) {
  lapply(history, function(entry) {
    entry$seconds <- NULL
    entry
  })
}

expectSameSearch <- function(resumed, uninterrupted) {
  expect_equal(withoutTimes(resumed$paramSearch), withoutTimes(uninterrupted$paramSearch))
  expect_equal(resumed$prediction, uninterrupted$prediction)
  expect_equal(resumed$model, uninterrupted$model)
  expect_equal(resumed$finalParam, uninterrupted$finalParam)
}

savedCandidates <- function(analysisPath) {
  files <- list.files(file.path(analysisPath, "hyperparameterSearch"), "search.rds", recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    return(0)
  }
  length(readRDS(files[1])$history)
}

slopes <- list(slope = list(-1, 0.5, 1, 2, 3))

test_that("an interrupted search resumes after the last finished candidate, with the same result", {
  fixture <- createResumeFixture()
  path <- withr::local_tempdir()
  set.seed(5)
  uninterrupted <- runSearch(fixture, slopes, analysisPath = NULL)
  expect_false(dir.exists(file.path(path, "hyperparameterSearch")))

  # the 8th fold fit is the 2nd fold of the 3rd candidate
  fixture$calls$n <- 0
  fixture$calls$stopAt <- 8
  set.seed(5)
  expect_error(runSearch(fixture, slopes, analysisPath = path), "simulated crash")
  expect_equal(savedCandidates(path), 2)

  # another random state: the search restores the saved one
  fixture$calls$n <- 0
  fixture$calls$stopAt <- NULL
  fixture$calls$candidates <- list()
  set.seed(999)
  expect_output(
    resumed <- runSearch(fixture, slopes, analysisPath = path),
    "Resuming the hyperparameter search of resumeModel after 2 candidates"
  )
  # the 3 remaining candidates on 3 folds, and the final fit
  expect_equal(fixture$calls$n, 3 * 3 + 1)
  expect_equal(unique(vapply(fixture$calls$candidates[1:9], function(x) x$slope, numeric(1))), c(1, 2, 3))
  expectSameSearch(resumed, uninterrupted)
})

test_that("the random stream after a resumed search is that of an uninterrupted one", {
  fixture <- createResumeFixture()
  set.seed(5)
  runSearch(fixture, slopes, analysisPath = NULL)
  expected <- stats::runif(3)

  path <- withr::local_tempdir()
  fixture$calls$stopAt <- 5
  fixture$calls$n <- 0
  set.seed(5)
  expect_error(runSearch(fixture, slopes, analysisPath = path), "simulated crash")
  fixture$calls$stopAt <- NULL
  set.seed(1234)
  suppressMessages(capture.output(runSearch(fixture, slopes, analysisPath = path)))
  expect_equal(stats::runif(3), expected)
})

test_that("a random search without a seed resumes with the same candidates", {
  fixture <- createResumeFixture()
  grid <- list(slope = as.list(seq(-2, 3, by = 0.5)))
  set.seed(42)
  uninterrupted <- runSearch(fixture, grid, analysisPath = NULL, search = "random", sampleSize = 4)

  path <- withr::local_tempdir()
  fixture$calls$n <- 0
  fixture$calls$stopAt <- 7
  set.seed(42)
  expect_error(runSearch(fixture, grid, analysisPath = path, search = "random", sampleSize = 4), "simulated crash")
  fixture$calls$stopAt <- NULL
  fixture$calls$n <- 0
  set.seed(7)
  capture.output(resumed <- runSearch(fixture, grid, analysisPath = path, search = "random", sampleSize = 4))
  expect_equal(fixture$calls$n, 2 * 3 + 1)
  expectSameSearch(resumed, uninterrupted)
})

test_that("a generator that draws random numbers between fits resumes with the same candidates", {
  makeGenerator <- function() {
    list(
      initialize = function(definition, settings) invisible(NULL),
      getNext = function(history) {
        if (length(history) >= 4) {
          return(NULL)
        }
        list(slope = round(stats::runif(1, -2, 3), 3))
      }
    )
  }
  fixture <- createResumeFixture()
  set.seed(3)
  uninterrupted <- runSearch(fixture, list(slope = list(0)), analysisPath = NULL, search = "custom", generator = makeGenerator())

  path <- withr::local_tempdir()
  fixture$calls$n <- 0
  fixture$calls$stopAt <- 9
  set.seed(3)
  expect_error(
    runSearch(fixture, list(slope = list(0)), analysisPath = path, search = "custom", generator = makeGenerator()),
    "simulated crash"
  )
  expect_equal(savedCandidates(path), 2)
  fixture$calls$stopAt <- NULL
  fixture$calls$n <- 0
  capture.output(resumed <- runSearch(fixture, list(slope = list(0)), analysisPath = path, search = "custom", generator = makeGenerator()))
  expect_equal(fixture$calls$n, 2 * 3 + 1)
  expectSameSearch(resumed, uninterrupted)
})

test_that("skipped candidates and a selector's earlier choice survive a resume", {
  fixture <- createResumeFixture()
  # the first candidate with a performance, whatever comes later
  firstSelector <- function(history, metric) {
    which(!vapply(history, function(x) is.na(x$cvPerformance), logical(1)))[1]
  }
  grid <- list(list(slope = 0, skip = TRUE), list(slope = 0.5, skip = FALSE), list(slope = 3, skip = FALSE),
               list(slope = 1, skip = FALSE))
  set.seed(8)
  expect_warning(
    uninterrupted <- runSearch(fixture, grid, analysisPath = NULL, selector = firstSelector),
    class = "plpCandidateWarning"
  )
  expect_equal(uninterrupted$finalParam$slope, 0.5)

  path <- withr::local_tempdir()
  fixture$calls$n <- 0
  # the skipped candidate stops after its first fold: 1 + 3 + 3 calls, then a crash
  fixture$calls$stopAt <- 8
  set.seed(8)
  expect_error(
    suppressWarnings(runSearch(fixture, grid, analysisPath = path, selector = firstSelector)),
    "simulated crash"
  )
  expect_equal(savedCandidates(path), 3)
  saved <- list.files(file.path(path, "hyperparameterSearch"), recursive = TRUE)
  # the selector can choose any earlier candidate, so the predictions of both
  # fitted candidates are kept
  expect_equal(sort(basename(saved)), c("prediction-2.rds", "prediction-3.rds", "search.rds"))

  fixture$calls$stopAt <- NULL
  fixture$calls$n <- 0
  output <- capture.output(resumed <- runSearch(fixture, grid, analysisPath = path, selector = firstSelector))
  expect_true(any(grepl("was skipped in the saved search: cannot fit this one", output)))
  expect_equal(fixture$calls$n, 3 + 1)
  expectSameSearch(resumed, uninterrupted)
})

test_that("only the best candidate's predictions are kept without a selector", {
  fixture <- createResumeFixture()
  path <- withr::local_tempdir()
  fixture$calls$stopAt <- 13
  set.seed(5)
  expect_error(runSearch(fixture, slopes, analysisPath = path), "simulated crash")
  saved <- basename(list.files(file.path(path, "hyperparameterSearch"), recursive = TRUE))
  expect_length(grep("^prediction-", saved), 1)
  expect_false(any(grepl("\\.tmp$", saved)))
})

test_that("a change in the data, labels, settings or metric starts the search over", {
  fixture <- createResumeFixture()
  path <- withr::local_tempdir()
  interrupt <- function(...) {
    fixture$calls$n <- 0
    fixture$calls$stopAt <- 5
    set.seed(5)
    expect_error(runSearch(fixture, slopes, analysisPath = path, ...), "simulated crash")
    fixture$calls$stopAt <- NULL
    fixture$calls$n <- 0
  }
  rerunsAll <- function(...) {
    output <- capture.output(runSearch(fixture, slopes, analysisPath = path, ...))
    expect_false(any(grepl("Resuming", output)))
    expect_equal(fixture$calls$n, 5 * 3 + 1)
    output
  }

  interrupt()
  otherLabels <- fixture$labels
  otherLabels$index <- rev(otherLabels$index)
  output <- rerunsAll(labels = otherLabels)
  expect_true(any(grepl("are for other data or settings", output)))

  unlink(path, recursive = TRUE)
  interrupt()
  otherSettings <- fixture$settings
  otherSettings$extra <- 1
  rerunsAll(settings = otherSettings)

  unlink(path, recursive = TRUE)
  interrupt()
  rerunsAll(tuningMetric = auprcMetric)

  unlink(path, recursive = TRUE)
  interrupt()
  original <- fixture$data
  fixture$data[1, 1] <- fixture$data[1, 1] + 1e-9
  rerunsAll()
  fixture$data <- original

  # a custom metric made by a factory with another argument
  unlink(path, recursive = TRUE)
  makeMetric <- function(threshold) {
    createTuningMetric(function(prediction) mean((prediction$value > threshold) == prediction$outcomeCount),
                       name = "accuracy")
  }
  interrupt(tuningMetric = makeMetric(0.5))
  rerunsAll(tuningMetric = makeMetric(0.4))
})

test_that("equal values in another form resume", {
  fixture <- createResumeFixture()
  path <- withr::local_tempdir()
  fixture$calls$stopAt <- 5
  set.seed(5)
  expect_error(runSearch(fixture, slopes, analysisPath = path), "simulated crash")
  fixture$calls$stopAt <- NULL
  fixture$calls$n <- 0
  # rowId as a compact sequence instead of an ordinary vector, and other row names
  labels <- fixture$labels[sample(nrow(fixture$labels)), ]
  labels <- labels[order(labels$rowId), ]
  labels$rowId <- seq_len(nrow(labels))
  expect_false(identical(.row_names_info(labels), .row_names_info(fixture$labels)))
  output <- capture.output(runSearch(fixture, slopes, analysisPath = path, labels = labels))
  expect_true(any(grepl("Resuming", output)))
})

test_that("searches are saved only for models that ask for it, or with the option on", {
  fixture <- createResumeFixture()
  path <- withr::local_tempdir()
  interrupt <- function(settings) {
    fixture$calls$n <- 0
    fixture$calls$stopAt <- 5
    expect_error(runSearch(fixture, slopes, analysisPath = path, settings = settings), "simulated crash")
    dir.exists(file.path(path, "hyperparameterSearch"))
  }
  settings <- fixture$settings
  settings$resumeSearch <- NULL
  expect_false(interrupt(settings))
  withr::with_options(list(plp.resumeSearch = TRUE), expect_true(interrupt(settings)))
  unlink(path, recursive = TRUE)
  withr::with_options(list(plp.resumeSearch = FALSE), expect_false(interrupt(fixture$settings)))
  expect_true(interrupt(fixture$settings))
})

test_that("searches are not saved without an analysisPath, with the option off, or when PLP cannot fingerprint them", {
  fixture <- createResumeFixture()
  path <- withr::local_tempdir()

  withr::with_options(list(plp.resumeSearch = FALSE), {
    fixture$calls$stopAt <- 5
    expect_error(runSearch(fixture, slopes, analysisPath = path), "simulated crash")
  })
  expect_false(dir.exists(file.path(path, "hyperparameterSearch")))

  settings <- fixture$settings
  settings$handle <- new.env()
  fixture$calls$n <- 0
  output <- capture.output(
    expect_error(runSearch(fixture, slopes, analysisPath = path, settings = settings), "simulated crash")
  )
  expect_true(any(grepl("is not checkpointed: the model settings contain an object of type environment", output)))
  expect_false(dir.exists(file.path(path, "hyperparameterSearch")))
})

test_that("a model's fingerprint replaces the data and settings in the fingerprint", {
  fixture <- createResumeFixture()
  path <- withr::local_tempdir()
  settings <- fixture$settings
  settings$handle <- new.env()
  settings$device <- "cuda:0"
  settings$fingerprint <- function(dataMatrix, labels, settings) {
    list(data = dataMatrix, layers = settings$layers)
  }
  settings$layers <- 2
  fixture$calls$stopAt <- 5
  set.seed(5)
  expect_error(runSearch(fixture, slopes, analysisPath = path, settings = settings), "simulated crash")
  expect_equal(savedCandidates(path), 1)

  # a change the model's fingerprint leaves out resumes
  fixture$calls$stopAt <- NULL
  fixture$calls$n <- 0
  settings$device <- "cuda:1"
  output <- capture.output(runSearch(fixture, slopes, analysisPath = path, settings = settings))
  expect_true(any(grepl("Resuming", output)))
  expect_equal(fixture$calls$n, 4 * 3 + 1)

  # one it covers starts over
  unlink(path, recursive = TRUE)
  fixture$calls$stopAt <- 5
  fixture$calls$n <- 0
  expect_error(runSearch(fixture, slopes, analysisPath = path, settings = settings), "simulated crash")
  fixture$calls$stopAt <- NULL
  fixture$calls$n <- 0
  settings$layers <- 3
  output <- capture.output(runSearch(fixture, slopes, analysisPath = path, settings = settings))
  expect_false(any(grepl("Resuming", output)))
})

test_that("an unreadable checkpoint starts the search over", {
  fixture <- createResumeFixture()
  path <- withr::local_tempdir()
  fixture$calls$stopAt <- 5
  expect_error(runSearch(fixture, slopes, analysisPath = path), "simulated crash")
  file <- list.files(file.path(path, "hyperparameterSearch"), "search.rds", recursive = TRUE, full.names = TRUE)
  writeLines("not an rds file", file)
  fixture$calls$stopAt <- NULL
  fixture$calls$n <- 0
  warnings <- capturedWarnings(capture.output(result <- runSearch(fixture, slopes, analysisPath = path)))
  expect_true(any(grepl("Could not read the checkpoint", warnings)))
  expect_equal(fixture$calls$n, 5 * 3 + 1)
  expect_length(result$paramSearch, 5)
})

test_that("a fold state with an external pointer is not saved", {
  fixture <- createResumeFixture()
  path <- withr::local_tempdir()
  file <- file.path(path, "x.rds")
  expect_error(
    writeCheckpointFile(list(state = list(pointer = methods::new("externalptr"))), file),
    "external pointer"
  )
  expect_false(file.exists(file))
  expect_length(list.files(path), 0)
})

test_that("fold states are saved for models that allow it, and the search resumes with 1 or 2 fold workers", {
  skip_on_os("windows")
  calls <- new.env()
  fixture <- createStatefulResumeFixture(calls)
  grid <- list(slope = list(0.5, 1, 2, 3))
  set.seed(5)
  uninterrupted <- runSearch(fixture, grid, analysisPath = NULL)

  for (workers in 1:2) {
    path <- withr::local_tempdir()
    calls$stopCandidate <- 3
    set.seed(5)
    expect_error(runSearch(fixture, grid, analysisPath = path, foldWorkers = 2), "simulated crash")
    expect_equal(savedCandidates(path), 2)
    calls$stopCandidate <- NULL
    capture.output(resumed <- runSearch(fixture, grid, analysisPath = path, foldWorkers = workers))
    expectSameSearch(resumed, uninterrupted)
    # the warm starts after the resume come from the saved fold states
    expect_equal(
      vapply(resumed$paramSearch[[4]]$info, function(f) f$stateSize, numeric(1)),
      c(3, 3, 3)
    )
  }

  # without checkpointState, the search is not saved
  fixture$settings$checkpointState <- NULL
  path <- withr::local_tempdir()
  output <- capture.output(runSearch(fixture, grid, analysisPath = path))
  expect_true(any(grepl("its fold state is not saved", output)))
  expect_false(dir.exists(file.path(path, "hyperparameterSearch")))
})

test_that("fitPlp removes the checkpoint of a finished search; runPlp keeps it until the result is saved", {
  fixture <- createResumeFixture()
  path <- withr::local_tempdir()
  set.seed(5)
  result <- runSearch(fixture, slopes, analysisPath = path)
  expect_true(dir.exists(file.path(path, "hyperparameterSearch")))
  removeSearchCheckpoint(result$checkpoint)
  expect_false(dir.exists(file.path(path, "hyperparameterSearch")))

  skip_if_no_eunomia()
  modelSettings <- list(
    param = list(slope = list(1)),
    settings = list(
      modelName = "checkpointedModel",
      resumeSearch = TRUE,
      train = function(dataMatrix, labels, hyperParameters, settings) list(),
      predict = function(plpModel, data, cohort) {
        cohort$value <- rep(0.5, nrow(cohort))
        if (!is.null(cohort$originalRowId)) {
          cohort$rowId <- cohort$originalRowId
          cohort$originalRowId <- NULL
        }
        cohort
      },
      prepareData = "toSparseM"
    )
  )
  class(modelSettings) <- "modelSettings"
  hyperparameterSettings <- createHyperparameterSettings(
    tuningMetric = createTuningMetric(fun = function(prediction) 0.5, name = "constant")
  )
  path <- withr::local_tempdir()
  # as in runPlp()
  .plpCheckpoint$deferRemoval <- TRUE
  fitPlp(tinyTrainData, modelSettings, hyperparameterSettings, analysisId = "checkpoint", analysisPath = path)
  .plpCheckpoint$deferRemoval <- NULL
  expect_true(dir.exists(file.path(path, "hyperparameterSearch")))
  keepSearchCheckpoints(path, "evaluation")
  expect_true(dir.exists(file.path(path, "hyperparameterSearch")))
  keepSearchCheckpoints(path, character(0))
  expect_false(dir.exists(file.path(path, "hyperparameterSearch")))

  fitPlp(tinyTrainData, modelSettings, hyperparameterSettings, analysisId = "checkpoint", analysisPath = path)
  expect_false(dir.exists(file.path(path, "hyperparameterSearch")))
})

test_that("runPlp resumes a search that crashed, and removes it once the result is saved", {
  skip_if_no_eunomia()
  skip_on_cran()
  calls <- new.env()
  calls$n <- 0
  modelSettings <- list(
    param = list(risk = list(0.2, 0.4, 0.6)),
    settings = list(
      modelName = "crashingModel",
      seed = 3,
      resumeSearch = TRUE,
      train = function(dataMatrix, labels, hyperParameters, settings) {
        calls$n <- calls$n + 1
        if (identical(calls$n, calls$stopAt)) {
          stop("simulated crash")
        }
        list(risk = hyperParameters$risk)
      },
      predict = function(plpModel, data, cohort) {
        if (inherits(plpModel, "plpModel") && isTRUE(calls$failTest)) {
          stop("simulated failure of the test prediction")
        }
        # in the search the model, on new data the plpModel
        risk <- if (inherits(plpModel, "plpModel")) plpModel$model$risk else plpModel$risk
        # risks that vary, so that the evaluation can run
        cohort$value <- risk * (1 + 0.5 * (cohort$rowId %% 2))
        if (!is.null(cohort$originalRowId)) {
          cohort$rowId <- cohort$originalRowId
          cohort$originalRowId <- NULL
        }
        cohort
      },
      prepareData = "toSparseM",
      saveType = "RtoJson"
    )
  )
  class(modelSettings) <- "modelSettings"
  saveDirectory <- withr::local_tempdir()
  run <- function() {
    runPlp(
      plpData = plpData,
      outcomeId = outcomeId,
      analysisId = "resume",
      populationSettings = populationSettings,
      splitSettings = createDefaultSplitSetting(splitSeed = 12),
      modelSettings = modelSettings,
      hyperparameterSettings = createHyperparameterSettings(
        tuningMetric = createTuningMetric(function(prediction) -abs(mean(prediction$value) - 0.4), name = "closeness")
      ),
      logSettings = createLogSettings(verbosity = "ERROR"),
      executeSettings = createExecuteSettings(
        runSplitData = TRUE, runPreprocessData = TRUE, runModelDevelopment = TRUE
      ),
      saveDirectory = saveDirectory
    )
  }
  searchDirectory <- file.path(saveDirectory, "resume", "hyperparameterSearch")

  # the crash is in the 3rd fold of the 3rd candidate; runPlp logs the error and
  # saves a result without a model (noModel), and the search stays saved
  calls$stopAt <- 9
  crashed <- run()
  expect_true(crashed$model$noModel)
  expect_true(dir.exists(searchDirectory))

  # the search finishes, but the test prediction fails: runPlp saves a result
  # without a test prediction, and the search stays saved
  calls$stopAt <- NULL
  calls$n <- 0
  calls$failTest <- TRUE
  withoutTest <- run()
  expect_false(isTRUE(withoutTest$model$noModel))
  expect_false("Test" %in% withoutTest$prediction$evaluationType)
  expect_true(dir.exists(searchDirectory))
  # the 3 folds of the 3rd candidate and the final fit
  expect_equal(calls$n, 3 + 1)

  calls$failTest <- NULL
  calls$n <- 0
  result <- run()
  # all candidates were saved: only the final fit
  expect_equal(calls$n, 1)
  expect_true("Test" %in% result$prediction$evaluationType)
  expect_equal(result$model$trainDetails$finalModelParameters$risk, 0.4)
  expect_length(result$model$trainDetails$hyperParamSearch, 3)
  expect_false(dir.exists(searchDirectory))
})
