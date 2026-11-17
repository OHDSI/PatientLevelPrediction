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

# A model with tuning hooks: its prediction is a logistic function of the
# first column with a random slope (so that seeds matter), and its state is
# the list of fits it made on its fold
createStatefulFixture <- function(n = 60, folds = 3, calls = new.env()) {
  set.seed(1)
  x <- matrix(stats::rnorm(n * 2), ncol = 2)
  labels <- data.frame(
    rowId = seq_len(n),
    outcomeCount = as.numeric(x[, 1] + stats::rnorm(n) > 0),
    index = rep(seq_len(folds), length.out = n)
  )
  calls$prepare <- list()
  calls$final <- NULL
  settings <- list(
    modelType = "binary",
    modelName = "statefulModel",
    seed = 11,
    prepareFold = function(dataMatrix, labels, settings) {
      calls$prepare[[length(calls$prepare) + 1]] <- sort(unique(labels$index))
      list(x = dataMatrix, prepared = TRUE)
    },
    train = function(dataMatrix, labels, hyperParameters, settings, context) {
      stopifnot(isTRUE(dataMatrix$prepared))
      if (isTRUE(context$final)) {
        calls$final <- context
      }
      if (isTRUE(context$foldId == hyperParameters$skipFold)) {
        stopCandidate("this fold cannot fit it")
      }
      if (isTRUE(context$foldId == hyperParameters$crashFold)) {
        tools::pskill(Sys.getpid(), tools::SIGKILL)
      }
      list(
        slope = hyperParameters$slope + stats::rnorm(1, sd = 0.1),
        foldId = context$foldId,
        stateFolds = vapply(context$state, function(s) s$foldId, numeric(1)),
        stateSize = length(context$state)
      )
    },
    predict = function(plpModel, data, cohort) {
      x <- if (is.list(data) && !is.null(data$x)) data$x else data
      cohort$value <- stats::plogis(plpModel$slope * x[, 1])
      cohort
    },
    updateState = function(state, model, hyperParameters, settings) {
      c(state, list(list(foldId = model$foldId, slope = model$slope)))
    },
    fitInfo = function(model, hyperParameters, settings) {
      list(
        stateSize = model$stateSize,
        ownFoldsOnly = all(model$stateFolds == model$foldId)
      )
    },
    parallelFolds = c("fork", "psock")
  )
  list(data = x, labels = labels, settings = settings, calls = calls)
}

runStateful <- function(fixture, param, ...) {
  tuneHyperparameters(
    data = fixture$data, labels = fixture$labels, param = param,
    settings = fixture$settings,
    hyperparamSettings = createHyperparameterSettings(...)
  )
}

withoutTimes <- function(history) {
  lapply(history, function(entry) {
    entry$seconds <- NULL
    entry
  })
}

test_that("a model without tuning hooks is called as before", {
  received <- new.env()
  settings <- list(
    modelType = "binary",
    train = function(dataMatrix, labels, hyperParameters, settings) {
      received$args <- c(received$args, list(names(match.call())[-1]))
      list(risk = hyperParameters$risk)
    },
    predict = function(plpModel, data, cohort) {
      cohort$value <- rep(plpModel$risk, nrow(cohort))
      cohort
    }
  )
  expect_false(resolveTuningHooks(settings)$stateful)
  labels <- data.frame(rowId = 1:6, outcomeCount = c(0, 1, 0, 1, 0, 1), index = rep(1:2, each = 3))
  result <- tuneHyperparameters(
    data = matrix(1:6, ncol = 1), labels = labels, param = list(risk = list(0.2)),
    settings = settings, hyperparamSettings = createHyperparameterSettings(foldWorkers = 2)
  )
  expect_true(all(vapply(received$args, function(a) setequal(a, c("dataMatrix", "labels", "hyperParameters", "settings")), logical(1))))
  expect_equal(length(received$args), 3)
  expect_equal(result$paramSearch[[1]]$foldIds, 1:2)
  expect_length(result$paramSearch[[1]]$seconds, 2)
  expect_null(result$paramSearch[[1]]$info)
})

test_that("prepareFold runs once per fold and once on all rows, and each fold keeps its own state", {
  fixture <- createStatefulFixture()
  result <- runStateful(fixture, list(slope = list(0.5, 1, 2)))

  # the folds' training rows, then all rows for the final fit
  expect_equal(fixture$calls$prepare, list(c(2, 3), c(1, 3), c(1, 2), 1:3))
  info <- lapply(result$paramSearch, function(entry) entry$info)
  expect_equal(
    vapply(info, function(candidate) vapply(candidate, function(f) f$stateSize, numeric(1)), numeric(3)),
    matrix(rep(0:2, each = 3), nrow = 3)
  )
  expect_true(all(unlist(lapply(info, function(candidate) lapply(candidate, function(f) f$ownFoldsOnly)))))
  # the final fit starts cold
  expect_true(fixture$calls$final$final)
  expect_null(fixture$calls$final$state)
  expect_null(fixture$calls$final$foldId)
})

test_that("fold fits are seeded per candidate and fold, without changing the caller's random stream", {
  fixture <- createStatefulFixture()
  set.seed(5)
  before <- .Random.seed
  first <- runStateful(fixture, list(slope = list(0.5, 1)))
  expect_identical(.Random.seed, before)
  second <- runStateful(fixture, list(slope = list(0.5, 1)))
  expect_equal(withoutTimes(first$paramSearch), withoutTimes(second$paramSearch))
  expect_equal(first$prediction, second$prediction)
})

# R CMD check allows at most 2 cores (_R_CHECK_LIMIT_CORES_); with 3 folds on
# 2 workers, one worker keeps two folds
test_that("one and several forked workers give the same results", {
  skip_on_os("windows")
  run <- function(workers) {
    fixture <- createStatefulFixture()
    runStateful(fixture, list(slope = list(0.5, 1, 2)), foldWorkers = workers)
  }
  one <- run(1)
  for (workers in 2) {
    several <- run(workers)
    expect_equal(withoutTimes(several$paramSearch), withoutTimes(one$paramSearch))
    expect_identical(several$prediction, one$prediction)
    expect_identical(several$model, one$model)
  }
})

test_that("a candidate one fold cannot fit is skipped in every fold and leaves the later results unchanged", {
  run <- function(workers) {
    fixture <- createStatefulFixture()
    param <- list(
      list(slope = 0.5), list(slope = 1, skipFold = 2), list(slope = 2)
    )
    expect_warning(
      result <- runStateful(fixture, param, foldWorkers = workers),
      class = "plpCandidateWarning"
    )
    result
  }
  one <- run(1)
  expect_true(is.na(one$paramSearch[[2]]$cvPerformance))
  expect_equal(one$paramSearch[[2]]$error, "this fold cannot fit it")
  # fold 2's state did not take the failed fit; the other folds took theirs
  expect_equal(
    vapply(one$paramSearch[[3]]$info, function(f) f$stateSize, numeric(1)),
    c(2, 1, 2)
  )
  skip_on_os("windows")
  several <- run(2)
  expect_equal(withoutTimes(several$paramSearch), withoutTimes(one$paramSearch))
  expect_identical(several$prediction, one$prediction)
})

# A model whose fold fits write to the log, and the log lines of a search
loggedFoldFits <- function(workers, type) {
  fixture <- createStatefulFixture()
  fixture$settings$parallelFolds <- type
  train <- fixture$settings$train
  fixture$settings$train <- function(dataMatrix, labels, hyperParameters, settings, context) {
    ParallelLogger::logInfo("fold fit ", context$foldId)
    train(dataMatrix, labels, hyperParameters, settings, context)
  }
  logFile <- tempfile(fileext = ".txt")
  withr::defer(unlink(logFile))
  loggers <- ParallelLogger::getLoggers()
  withr::defer({
    ParallelLogger::clearLoggers()
    for (logger in loggers) ParallelLogger::registerLogger(logger)
  })
  ParallelLogger::clearLoggers()
  ParallelLogger::addDefaultFileLogger(logFile, name = "TUNING_TEST")
  runStateful(fixture, list(slope = list(0.5, 1)), foldWorkers = workers)
  # the fold fits; the final fit (no fold) runs in this process
  grep("fold fit [0-9]", readLines(logFile), value = TRUE)
}

test_that("fold workers write to the log, naming the worker", {
  skip_on_os("windows")
  lines <- loggedFoldFits(2, "fork")
  # 2 candidates on 3 folds
  expect_length(lines, 6)
  expect_true(any(grepl("Thread 1", lines)) && any(grepl("Thread 2", lines)))
})

test_that("a worker that dies stops the search with an error", {
  skip_on_os("windows")
  fixture <- createStatefulFixture()
  expect_error(
    runStateful(fixture, list(list(slope = 1, crashFold = 2)), foldWorkers = 2)
  )
})

test_that("foldWorkers is ignored for a model that does not support parallel folds", {
  fixture <- createStatefulFixture()
  fixture$settings$parallelFolds <- NULL
  one <- runStateful(fixture, list(slope = list(0.5, 1)))
  two <- runStateful(fixture, list(slope = list(0.5, 1)), foldWorkers = 2)
  expect_equal(withoutTimes(two$paramSearch), withoutTimes(one$paramSearch))
  expect_error(
    resolveTuningHooks(utils::modifyList(fixture$settings, list(parallelFolds = "thread"))),
    "parallelFolds"
  )
})

test_that("a model's default tuning metric is used unless the settings name one", {
  fixture <- createStatefulFixture()
  fixture$settings$tuningMetric <- "logLikelihoodMetric"
  byDefault <- runStateful(fixture, list(slope = list(0.5, 1)))
  expect_equal(byDefault$paramSearch[[1]]$metric, "logLikelihood")
  explicit <- runStateful(fixture, list(slope = list(0.5, 1)), tuningMetric = aucMetric)
  expect_equal(explicit$paramSearch[[1]]$metric, "AUC")
  # settings saved as JSON keep the difference
  saved <- ParallelLogger::convertJsonToSettings(
    ParallelLogger::convertSettingsToJson(createHyperparameterSettings())
  )
  expect_true(saved$tuningMetricIsDefault)
  expect_null(createHyperparameterSettings(tuningMetric = aucMetric)$tuningMetricIsDefault)
})

test_that("logLikelihoodMetric is the mean Bernoulli log-likelihood with clipped risks", {
  prediction <- data.frame(outcomeCount = c(0, 1, 2, 0), value = c(0.2, 0.7, 1, 0))
  p <- c(0.2, 0.7, 1 - 1e-15, 1e-15)
  expected <- mean(c(log(0.8), log(0.7), log(p[3]), log(1 - p[4])))
  expect_equal(logLikelihoodMetric$fun(prediction), expected)
  expect_true(logLikelihoodMetric$maximize)
  expect_true(is.finite(logLikelihoodMetric$fun(data.frame(outcomeCount = 1, value = 0))))
})

test_that("a metric can return NA or an infinite value", {
  metric <- createTuningMetric(fun = function(prediction) NA, name = "missing")
  expect_identical(metric$fun(data.frame()), NA_real_)
  metric <- createTuningMetric(fun = function(prediction) -Inf, name = "minusInf")
  expect_identical(metric$fun(data.frame()), -Inf)
  metric <- createTuningMetric(fun = function(prediction) c(1, 2), name = "two")
  expect_error(metric$fun(data.frame()), "one numeric value")
})

test_that("a selector chooses the candidate, and its CV predictions are kept", {
  fixture <- createStatefulFixture()
  last <- function(history, metric) length(history)
  result <- runStateful(fixture, list(slope = list(2, 1, 0.5)), selector = last)
  expect_equal(result$finalParam$slope, 0.5)
  cv <- result$prediction[result$prediction$evaluationType == "CV", ]
  expect_equal(nrow(cv), nrow(fixture$labels))
  reference <- runStateful(fixture, list(slope = list(2, 1, 0.5)))
  # the selected candidate's predictions, not the best one's
  expect_false(isTRUE(all.equal(
    cv$value, reference$prediction$value[reference$prediction$evaluationType == "CV"]
  )))
  expect_error(
    runStateful(fixture, list(slope = list(2, 1)), selector = function(history, metric) 5),
    "index of one candidate"
  )
  expect_error(createHyperparameterSettings(selector = 1), "selector")
})

test_that("a generator given by name is made for each search and survives JSON", {
  assign(
    "plpTestGeneratorFactory",
    function(values) {
      i <- 0
      list(
        initialize = function(definition, settings) i <<- 0,
        getNext = function(history) {
          i <<- i + 1
          if (i > length(values)) NULL else list(slope = values[[i]])
        }
      )
    },
    envir = globalenv()
  )
  withr::defer(rm("plpTestGeneratorFactory", envir = globalenv()))
  settings <- createHyperparameterSettings(
    generator = "plpTestGeneratorFactory", generatorArgs = list(values = list(0.5, 2))
  )
  expect_equal(settings$search, "custom")
  saved <- ParallelLogger::convertJsonToSettings(ParallelLogger::convertSettingsToJson(settings))
  fixture <- createStatefulFixture()
  for (s in list(settings, saved, saved)) {
    result <- tuneHyperparameters(
      data = fixture$data, labels = fixture$labels, param = list(slope = list(1)),
      settings = fixture$settings, hyperparamSettings = s
    )
    expect_equal(vapply(result$paramSearch, function(e) e$param$slope, numeric(1)), c(0.5, 2))
  }
  expect_error(createHyperparameterSettings(generatorArgs = list(a = 1)), "generator given by name")
  expect_error(createHyperparameterSettings(foldWorkers = 0), "foldWorkers")
})

test_that("PSOCK workers give the same results as one worker", {
  # PSOCK workers load the installed package: this runs in R CMD check and
  # covr, which test an installed build, and with devtools::load_all() only
  # when PLP_TEST_PSOCK is set (with an installed build of this version)
  loadedForDevelopment <- exists(".__DEVTOOLS__", envir = asNamespace("PatientLevelPrediction"), inherits = FALSE)
  skip_if(
    loadedForDevelopment && !identical(Sys.getenv("PLP_TEST_PSOCK"), "true"),
    "loaded with load_all() and PLP_TEST_PSOCK is not set"
  )
  run <- function(workers) {
    fixture <- createStatefulFixture()
    fixture$settings$parallelFolds <- "psock"
    # a sparse matrix, whose methods the workers have to load
    fixture$data <- Matrix::Matrix(fixture$data, sparse = TRUE)
    runStateful(fixture, list(slope = list(0.5, 1, 2)), foldWorkers = workers)
  }
  one <- run(1)
  several <- run(2)
  expect_equal(withoutTimes(several$paramSearch), withoutTimes(one$paramSearch))
  expect_identical(several$prediction, one$prediction)
  # socket workers get PLP's loggers
  lines <- loggedFoldFits(2, "psock")
  expect_length(lines, 6)
  expect_true(any(grepl("Thread 2", lines)))
})

test_that("the worker side of PSOCK workers loads the data's package and keeps the data", {
  env <- get(".plpTuning", envir = asNamespace("PatientLevelPrediction"))
  on.exit(rm(list = ls(env, all.names = TRUE), envir = env), add = TRUE)
  expect_true(loadPlpOnWorker(.libPaths()))
  payload <- list(data = Matrix::Matrix(diag(2), sparse = TRUE), labels = data.frame(rowId = 1:2), baseSeed = 3)
  expect_true(receiveFoldData(payload))
  expect_identical(env$data, payload$data)
  expect_identical(env$baseSeed, 3)
})

test_that("the fold workers are forked where possible, else PSOCK when the model allows it", {
  hooks <- function(parallelFolds) list(parallelFolds = parallelFolds)
  expect_equal(foldWorkerType(hooks(c("fork", "psock")), 2, 3, canFork = TRUE), "fork")
  expect_equal(foldWorkerType(hooks(c("fork", "psock")), 2, 3, canFork = FALSE), "psock")
  expect_equal(foldWorkerType(hooks("psock"), 2, 3, canFork = TRUE), "psock")
  expect_output(
    expect_null(foldWorkerType(hooks("fork"), 2, 3, modelName = "forkOnly", canFork = FALSE)),
    "foldWorkers is ignored: forkOnly only supports forked workers"
  )
  expect_null(foldWorkerType(hooks("fork"), 1, 3, canFork = TRUE))
  expect_null(foldWorkerType(hooks("fork"), 2, 1, canFork = TRUE))
})

test_that("a model with tuning hooks but no prepareFold is trained on its fold's rows", {
  fixture <- createStatefulFixture()
  fixture$settings$prepareFold <- NULL
  fixture$settings$train <- function(dataMatrix, labels, hyperParameters, settings, context) {
    stopifnot(is.matrix(dataMatrix), nrow(dataMatrix) == nrow(labels))
    list(slope = hyperParameters$slope)
  }
  result <- runStateful(fixture, list(slope = list(0.5, 1)))
  expect_length(result$paramSearch, 2)
  expect_equal(result$model$slope, result$finalParam$slope)
})

test_that("tuning objects given by name are found, and errors name what is missing", {
  expect_identical(resolveTuningObject("aucMetric", "tuningMetric"), aucMetric)
  expect_identical(resolveTuningObject("PatientLevelPrediction::auprcMetric", "tuningMetric"), auprcMetric)
  expect_error(resolveTuningObject("noSuchMetric", "tuningMetric"), "Could not find 'noSuchMetric' given as the tuningMetric")
  expect_error(
    resolveTuningMetric(list(tuningMetric = list(fun = "not a function", name = "x")), list()),
    "The tuning metric must be a list with fun, maximize and name"
  )
  expect_error(
    resolveTuningMetric(list(tuningMetric = aucMetric, tuningMetricIsDefault = TRUE), list(tuningMetric = "noSuchMetric")),
    "Could not find 'noSuchMetric'"
  )
})

test_that("the log-likelihood clips risks of 0 and 1", {
  # called directly: the metric object holds the function as it was when the package was built
  prediction <- data.frame(outcomeCount = c(0, 1, 2), value = c(0, 1, 0.5))
  expect_equal(computeLogLikelihood(prediction), mean(c(log1p(-1e-15), log(1 - 1e-15), log(0.5))))
})

test_that("a selector must return one candidate with a performance", {
  fixture <- createStatefulFixture()
  run <- function(selector, ...) {
    runStateful(fixture, list(slope = list(0.5, 1)), selector = selector, ...)
  }
  expect_error(run(function(history, metric) 5L), "The selector must return the index of one candidate")
  expect_error(run(function(history, metric) c(1L, 2L)), "The selector must return the index of one candidate")
  skipFirst <- createTuningMetric(
    function(prediction) if (all(abs(prediction$value - stats::plogis(0.5 * 0)) < 1)) NA_real_ else 1,
    name = "missing"
  )
  expect_error(
    run(function(history, metric) 1L, tuningMetric = skipFirst),
    "The selector chose a candidate without a performance"
  )
  # a selector that chooses none: every candidate is without a performance
  expect_error(run(function(history, metric) integer(0)), "were skipped or had no performance")
})

test_that("a generator given by name must make a generator, and its arguments must be named", {
  expect_error(
    createHyperparameterSettings(generator = "PatientLevelPrediction::aucMetric", generatorArgs = list(1)),
    "`generatorArgs` must be a named list"
  )
  settings <- createHyperparameterSettings(generator = "base::identity", generatorArgs = list(x = "not a generator"))
  expect_error(
    prepareHyperparameterGrid(list(slope = list(1)), settings),
    "The generator function 'base::identity' must return a function or an object with initialize and getNext methods"
  )
})
