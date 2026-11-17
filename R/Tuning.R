# @file Tuning.R
#
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

# Fold runners for the hyperparameter search of models with tuning hooks.
#
# A model's settings can name these optional hooks (functions or their names):
# - prepareFold(dataMatrix, labels, settings): called once per fold on the
#   fold's training rows, and once on all training rows for the final fit. What
#   it returns is passed to train() as dataMatrix.
# - train(dataMatrix, labels, hyperParameters, settings, context): a train
#   function with a context argument gets list(foldId, final, state,
#   candidateIndex). state is what updateState() returned for this fold after
#   the earlier candidates (NULL for the first candidate and for the final fit).
# - updateState(state, model, hyperParameters, settings): returns the fold's
#   new state after a successful fit, for example the coefficients to start the
#   next candidate from. The state stays with its fold and is not in the
#   search history.
# - fitInfo(model, hyperParameters, settings): a small named list of scalars
#   about a fold fit, kept in the search history.
# - parallelFolds: "fork" and/or "psock", the kinds of worker processes in
#   which the model can be fitted, for createHyperparameterSettings(foldWorkers).
#
# Models without these hooks are fitted exactly as before. For models with
# them, every fold runs every candidate, each fold fit has its own seed (from
# the model's seed, the candidate and the fold), and a fold's state changes
# only after its own successful fit, so that one worker and several workers
# give the same results.

# Data shared with forked workers; it is set just before the workers are
# forked, so that they get it without copying, and cleared afterwards
.plpTuning <- new.env(parent = emptyenv())

resolveTuningHooks <- function(settings) {
  trainFun <- resolveHook(settings$train, "train")
  optional <- function(name) {
    if (!is.null(settings[[name]])) {
      resolveHook(settings[[name]], name)
    }
  }
  hooks <- list(
    train = trainFun,
    predict = resolveHook(settings$predict, "predict"),
    prepareFold = optional("prepareFold"),
    updateState = optional("updateState"),
    fitInfo = optional("fitInfo"),
    trainContext = "context" %in% names(formals(trainFun))
  )
  parallelFolds <- as.character(unlist(settings$parallelFolds))
  unknown <- setdiff(parallelFolds, c("fork", "psock"))
  if (length(unknown) > 0) {
    stop("parallelFolds can only hold \"fork\" and \"psock\", not: ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  hooks$parallelFolds <- parallelFolds
  hooks$stateful <- !is.null(hooks$prepareFold) || !is.null(hooks$updateState) ||
    !is.null(hooks$fitInfo) || hooks$trainContext || length(parallelFolds) > 0
  hooks
}

# Seed of one fold fit, from the base seed, the candidate and the fold
tuningSeed <- function(baseSeed, candidateIndex, foldId) {
  as.integer((as.numeric(baseSeed) + 1000003 * candidateIndex + 7919 * as.numeric(foldId)) %% 2147483647)
}

createFoldRunner <- function(foldId, data, labels, settings, hooks, baseSeed) {
  runner <- new.env(parent = emptyenv())
  inTrain <- labels$index != foldId
  runner$foldId <- foldId
  runner$trainLabels <- labels[inTrain, , drop = FALSE]
  trainData <- data[inTrain, , drop = FALSE]
  runner$trainData <- if (is.null(hooks$prepareFold)) {
    trainData
  } else {
    withLocalSeed(tuningSeed(baseSeed, 0, foldId), do.call(
      hooks$prepareFold,
      list(dataMatrix = trainData, labels = runner$trainLabels, settings = settings)
    ))
  }
  runner$testData <- data[!inTrain, , drop = FALSE]
  runner$testLabels <- labels[!inTrain, , drop = FALSE]
  runner$state <- NULL
  runner
}

# Fits one candidate on one fold; a candidate the model cannot fit
# (stopCandidate()) gives an error message instead of a prediction, and leaves
# the fold's state as it was
runFoldCandidate <- function(runner, candidate, candidateIndex, baseSeed, settings, hooks) {
  start <- Sys.time()
  result <- tryCatch(
    withLocalSeed(tuningSeed(baseSeed, candidateIndex, runner$foldId), {
      args <- list(
        dataMatrix = runner$trainData,
        labels = runner$trainLabels,
        hyperParameters = candidate,
        settings = settings
      )
      if (hooks$trainContext) {
        args$context <- list(
          foldId = runner$foldId,
          final = FALSE,
          state = runner$state,
          candidateIndex = candidateIndex
        )
      }
      model <- do.call(hooks$train, args)
      prediction <- do.call(
        hooks$predict,
        list(plpModel = model, data = runner$testData, cohort = runner$testLabels)
      )
      info <- if (!is.null(hooks$fitInfo)) {
        do.call(hooks$fitInfo, list(model = model, hyperParameters = candidate, settings = settings))
      }
      if (!is.null(hooks$updateState)) {
        runner$state <- do.call(
          hooks$updateState,
          list(state = runner$state, model = model, hyperParameters = candidate, settings = settings)
        )
      }
      list(prediction = prediction, info = info, error = NULL)
    }),
    plpCandidateError = function(e) list(prediction = NULL, info = NULL, error = conditionMessage(e))
  )
  result$foldId <- runner$foldId
  result$seconds <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  result
}

# Runs the folds of each candidate, in this process or in worker processes
# that each keep their folds for the whole search. Returns a list with
# run(candidate, candidateIndex), which gives the fold results in the order of
# foldIds, and stop(), which frees the folds.
createFoldExecutor <- function(foldIds, data, labels, settings, hooks, baseSeed, foldWorkers = 1) {
  type <- foldWorkerType(hooks, foldWorkers, length(foldIds), settings$modelName)
  if (is.null(type)) {
    runners <- lapply(foldIds, createFoldRunner, data = data, labels = labels,
                      settings = settings, hooks = hooks, baseSeed = baseSeed)
    return(list(
      workers = 1L,
      type = "sequential",
      run = function(candidate, candidateIndex) {
        lapply(runners, runFoldCandidate, candidate = candidate, candidateIndex = candidateIndex,
               baseSeed = baseSeed, settings = settings, hooks = hooks)
      },
      stop = function() {
        runners <<- NULL
        invisible(NULL)
      }
    ))
  }

  workers <- min(foldWorkers, length(foldIds))
  # static assignment: fold i always runs on worker (i - 1) %% workers + 1
  assignment <- (seq_along(foldIds) - 1L) %% workers + 1L
  foldsByWorker <- lapply(seq_len(workers), function(w) foldIds[assignment == w])
  ParallelLogger::logInfo(paste0(
    "Fitting the ", length(foldIds), " folds on ", workers, " ", type, " workers"
  ))
  cluster <- startFoldCluster(type, workers, data, labels, settings, hooks, baseSeed)
  stopped <- FALSE
  stopWorkers <- function() {
    if (!stopped) {
      stopped <<- TRUE
      try(parallel::stopCluster(cluster), silent = TRUE)
    }
    invisible(NULL)
  }
  ok <- FALSE
  on.exit(if (!ok) stopWorkers(), add = TRUE)
  parallel::clusterApply(cluster, seq_len(workers), initFoldWorker, foldsByWorker = foldsByWorker)
  ok <- TRUE
  list(
    workers = workers,
    type = type,
    run = function(candidate, candidateIndex) {
      byWorker <- parallel::clusterApply(
        cluster, seq_len(workers), runWorkerCandidate,
        candidate = candidate, candidateIndex = candidateIndex
      )
      results <- unlist(byWorker, recursive = FALSE)
      results[match(foldIds, vapply(results, function(x) x$foldId, numeric(1)))]
    },
    stop = stopWorkers
  )
}

# The kind of workers to use, or NULL to fit the folds in this process
foldWorkerType <- function(hooks, foldWorkers, nFolds, modelName = NULL,
                           canFork = .Platform$OS.type == "unix") {
  if (foldWorkers <= 1 || nFolds < 2) {
    return(NULL)
  }
  if (length(hooks$parallelFolds) == 0) {
    ParallelLogger::logInfo(paste0(
      "foldWorkers is ignored: ", modelName %||% "the model", " does not support parallel folds"
    ))
    return(NULL)
  }
  if (canFork && "fork" %in% hooks$parallelFolds) {
    return("fork")
  }
  if ("psock" %in% hooks$parallelFolds) {
    return("psock")
  }
  ParallelLogger::logInfo(paste0(
    "foldWorkers is ignored: ", modelName %||% "the model", " only supports forked workers"
  ))
  NULL
}

startFoldCluster <- function(type, workers, data, labels, settings, hooks, baseSeed) {
  if (identical(type, "fork")) {
    # forked workers get the data from this process's memory
    .plpTuning$data <- data
    .plpTuning$labels <- labels
    .plpTuning$settings <- settings
    .plpTuning$hooks <- hooks
    .plpTuning$baseSeed <- baseSeed
    on.exit(rm(list = ls(.plpTuning, all.names = TRUE), envir = .plpTuning), add = TRUE)
    # Objects that are already garbage are finalized here, not in a worker:
    # a finalizer that calls into Java or a database library would wait
    # forever in a forked worker on a lock held by one of this process's threads
    gc(verbose = FALSE)
    return(parallel::makeForkCluster(workers))
  }
  # with PLP's loggers and Andromeda settings in each worker, as
  # createLearningCurve() and the feature importance get them
  cluster <- ParallelLogger::makeCluster(numberOfThreads = workers)
  ok <- FALSE
  on.exit(if (!ok) parallel::stopCluster(cluster), add = TRUE)
  parallel::clusterCall(cluster, loadPlpOnWorker, .libPaths())
  payload <- list(data = data, labels = labels, settings = settings, hooks = hooks, baseSeed = baseSeed)
  parallel::clusterCall(cluster, receiveFoldData, payload)
  ok <- TRUE
  cluster
}

# The functions below run in the workers

loadPlpOnWorker <- function(libPaths) {
  .libPaths(libPaths)
  loadNamespace("PatientLevelPrediction")
  TRUE
}

receiveFoldData <- function(payload) {
  # the methods of an S4 data matrix (such as Matrix's sparse matrices) are
  # not loaded with it
  package <- attr(class(payload$data), "package")
  if (isS4(payload$data) && !is.null(package)) {
    loadNamespace(package)
  }
  env <- get(".plpTuning", envir = asNamespace("PatientLevelPrediction"))
  for (name in names(payload)) {
    assign(name, payload[[name]], envir = env)
  }
  TRUE
}

initFoldWorker <- function(worker, foldsByWorker) {
  # log lines name the worker, as ParallelLogger's clusters do; forked workers
  # keep the loggers they inherited, socket workers get them from
  # ParallelLogger::makeCluster()
  options(PARALLELLOGGER_THREAD_NUMBER = worker)
  foldIds <- foldsByWorker[[worker]]
  env <- get(".plpTuning", envir = asNamespace("PatientLevelPrediction"))
  env$runners <- lapply(foldIds, createFoldRunner, data = env$data, labels = env$labels,
                        settings = env$settings, hooks = env$hooks, baseSeed = env$baseSeed)
  rm(list = c("data", "labels"), envir = env)
  TRUE
}

runWorkerCandidate <- function(worker, candidate, candidateIndex) {
  env <- get(".plpTuning", envir = asNamespace("PatientLevelPrediction"))
  lapply(env$runners, runFoldCandidate, candidate = candidate, candidateIndex = candidateIndex,
         baseSeed = env$baseSeed, settings = env$settings, hooks = env$hooks)
}

# The metric to tune on: the model's default metric when the hyperparameter
# settings did not set one
resolveTuningMetric <- function(hyperparamSettings, settings) {
  metric <- hyperparamSettings$tuningMetric
  if (isTRUE(hyperparamSettings$tuningMetricIsDefault) && !is.null(settings$tuningMetric)) {
    metric <- settings$tuningMetric
    if (is.character(metric)) {
      metric <- resolveTuningObject(metric, "tuningMetric")
    }
  }
  if (!is.list(metric) || !is.function(metric$fun) || is.null(metric$name)) {
    stop("The tuning metric must be a list with fun, maximize and name, as made by createTuningMetric()",
         call. = FALSE)
  }
  metric
}

resolveTuningObject <- function(name, what) {
  if (grepl("::", name, fixed = TRUE)) {
    parts <- strsplit(name, ":::?")[[1]]
    value <- get0(parts[2], envir = asNamespace(parts[1]), inherits = FALSE)
  } else {
    value <- get0(name, envir = asNamespace("PatientLevelPrediction"), inherits = FALSE)
  }
  if (is.null(value)) {
    stop("Could not find '", name, "' given as the ", what, call. = FALSE)
  }
  value
}

# The selector: from the hyperparameter settings, else the model's, else selectBest()
resolveSelector <- function(hyperparamSettings, settings) {
  selector <- hyperparamSettings$selector %||% settings$selector
  if (is.null(selector)) {
    return(NULL)
  }
  resolveHook(selector, "selector")
}
