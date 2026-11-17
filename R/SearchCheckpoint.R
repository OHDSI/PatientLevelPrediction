# @file SearchCheckpoint.R
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

# Checkpoints of the hyperparameter search, so that an interrupted search
# resumes where it stopped. They are off by default: a model whose candidates
# take long to fit (such as a deep learning model) turns them on with
# settings$resumeSearch = TRUE, and options(plp.resumeSearch = TRUE / FALSE)
# turns them on or off for every model.
#
# After each candidate, tuneHyperparameters() saves its search history, the
# random number generator state, the cross-validation predictions it still
# needs and (for models that ask for it) each fold's state under
# <analysisPath>/hyperparameterSearch/<fingerprint>/. A rerun with the same
# fingerprint replays the candidates, checks that they are the same, and goes
# on with the next one; the results are those of an uninterrupted search. The
# directory is removed when the model is complete: by runPlp() after it saved
# the result (so that a crash in the evaluation does not lose the search), or
# at the end of fitClassifier() when it is called otherwise.
#
# The fingerprint covers the data, the labels and folds, the model settings,
# the hyperparameter settings and tuning metric, and the versions of PLP and
# the model's package. A model can replace the data and settings parts with
# settings$fingerprint(dataMatrix, labels, settings), for example when its data
# is not an R object PLP can hash. Searches PLP cannot fingerprint (objects such
# as environments, external pointers or database connections in the data or
# settings) are not checkpointed.

checkpointFormatVersion <- 1L

# runPlp() sets deferRemoval while it fits, and removes the checkpoints itself
.plpCheckpoint <- new.env(parent = emptyenv())

# Whether the search of a model is checkpointed: the option when it is set,
# else the model's setting
resumeSearchEnabled <- function(settings) {
  option <- getOption("plp.resumeSearch")
  if (!is.null(option)) {
    return(isTRUE(option))
  }
  isTRUE(settings$resumeSearch)
}

# The checkpoint of one search, or NULL when the search is not checkpointed
openSearchCheckpoint <- function(analysisPath, data, labels, param, settings,
                                 hyperparamSettings, metric, hooks) {
  if (is.null(analysisPath) || !resumeSearchEnabled(settings)) {
    return(NULL)
  }
  modelName <- settings$modelName %||% "the model"
  if (!is.null(hooks$updateState) && !isTRUE(settings$checkpointState)) {
    ParallelLogger::logInfo(paste0(
      "The hyperparameter search of ", modelName, " is not checkpointed: its fold state ",
      "is not saved (settings$checkpointState)"
    ))
    return(NULL)
  }
  parts <- tryCatch(
    searchFingerprintParts(data, labels, param, settings, hyperparamSettings, metric, hooks),
    plpUnhashable = function(e) {
      ParallelLogger::logInfo(paste0(
        "The hyperparameter search of ", modelName, " is not checkpointed: ",
        conditionMessage(e)
      ))
      NULL
    }
  )
  if (is.null(parts)) {
    return(NULL)
  }
  fingerprint <- rlang::hash(parts)
  others <- setdiff(list.files(file.path(analysisPath, "hyperparameterSearch")), fingerprint)
  if (length(others) > 0) {
    ParallelLogger::logInfo(paste0(
      "The saved hyperparameter searches in ", file.path(analysisPath, "hyperparameterSearch"),
      " are for other data or settings, and are not used"
    ))
  }
  createSearchCheckpoint(
    file.path(analysisPath, "hyperparameterSearch", fingerprint),
    fingerprint,
    parts,
    modelName
  )
}

searchFingerprintParts <- function(data, labels, param, settings, hyperparamSettings, metric, hooks) {
  modelPart <- NULL
  if (!is.null(settings$fingerprint)) {
    modelFingerprint <- resolveHook(settings$fingerprint, "fingerprint")
    modelPart <- do.call(modelFingerprint, list(dataMatrix = data, labels = labels, settings = settings))
  }
  hyper <- hyperparamSettings
  # the results do not depend on the number of fold workers
  hyper$foldWorkers <- NULL
  hyper$tuningMetric <- NULL
  hyper$tuningMetricIsDefault <- NULL
  if (!is.null(hyper$generator) && !is.character(hyper$generator) && !is.function(hyper$generator)) {
    # a generator object by its methods; the replay of the candidates checks the rest
    methods <- intersect(c("initialize", "getNext", "finalize"), names(hyper$generator))
    hyper$generator <- lapply(stats::setNames(methods, methods), function(m) hyper$generator[[m]])
  }
  list(
    formatVersion = checkpointFormatVersion,
    data = if (is.null(modelPart)) fingerprintValue(data, "the data") else NULL,
    settings = if (is.null(modelPart)) fingerprintValue(settings, "the model settings") else NULL,
    model = if (!is.null(modelPart)) fingerprintValue(modelPart, "the model's fingerprint"),
    labels = fingerprintValue(as.list(labels), "the labels"),
    param = fingerprintValue(param, "the hyperparameters"),
    hyperparameterSettings = fingerprintValue(hyper, "the hyperparameter settings"),
    metric = fingerprintValue(
      list(name = metric$name, maximize = metric$maximize, fun = metric$fun),
      "the tuning metric"
    ),
    hooks = fingerprintValue(hooks[c("train", "predict", "prepareFold", "updateState")], "the model's functions"),
    versions = list(
      PatientLevelPrediction = as.character(utils::packageVersion("PatientLevelPrediction")),
      model = hookPackageVersion(hooks$train)
    )
  )
}

# The package and version of a model function, for a model from another package
hookPackageVersion <- function(fn) {
  env <- topenv(environment(fn))
  if (!isNamespace(env)) {
    return(NULL)
  }
  name <- getNamespaceName(env)
  if (identical(unname(name), "PatientLevelPrediction")) {
    return(NULL)
  }
  paste(name, as.character(getNamespaceVersion(env)))
}

# A value reduced to what identifies it: vectors by their hash (also when they
# are ALTREP sequences or wrappers, which hash differently from equal ordinary
# vectors), lists element by element with their attributes, data frames by
# their columns, functions by their code and the captured values they use.
# Objects whose content PLP cannot see signal a plpUnhashable condition.
fingerprintValue <- function(x, what) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.function(x)) {
    # environments that a function captures (such as an R6 object) are not
    # looked into, as in DeepPatientLevelPrediction's fingerprint
    return(fingerprintFunction(x, valueFingerprint = function(v) {
      if (is.environment(v)) list(type = "environment") else fingerprintValue(v, what)
    }))
  }
  if (isS4(x)) {
    if (methods::is(x, "Matrix")) {
      # factors is a cache of factorizations, not part of the matrix
      slots <- setdiff(methods::slotNames(x), "factors")
      return(list(
        class = class(x),
        slots = lapply(stats::setNames(slots, slots), function(s) fingerprintValue(methods::slot(x, s), what))
      ))
    }
    unhashable(what, paste0("an object of class ", class(x)[1]))
  }
  if (is.environment(x) || typeof(x) %in% c("externalptr", "weakref", "bytecode", "promise", "symbol", "language")) {
    unhashable(what, paste0("an object of type ", typeof(x), if (!is.null(attr(x, "class"))) paste0(" (", class(x)[1], ")")))
  }
  attrs <- attributes(x)
  if (is.data.frame(x)) {
    # data frames that are identical() can differ in the form of their row names
    attrs$row.names <- NULL
  }
  attrs <- if (length(attrs) > 0) lapply(attrs, fingerprintValue, what = what)
  if (is.list(x)) {
    return(list(
      type = "list",
      values = lapply(seq_along(x), function(i) fingerprintValue(.subset2(x, i), what)),
      attributes = attrs
    ))
  }
  if (is.atomic(x)) {
    value <- x
    attributes(value) <- NULL
    return(list(type = typeof(x), hash = rlang::hash(c(value)), attributes = attrs))
  }
  unhashable(what, paste0("an object of type ", typeof(x)))
}

unhashable <- function(what, object) {
  stop(errorCondition(
    paste0(what, " contain ", object, ", whose content PLP cannot fingerprint"),
    class = "plpUnhashable"
  ))
}

createSearchCheckpoint <- function(directory, fingerprint, parts, modelName) {
  checkpoint <- new.env(parent = emptyenv())
  checkpoint$directory <- directory
  checkpoint$fingerprint <- fingerprint
  checkpoint$parts <- parts
  checkpoint$modelName <- modelName
  checkpoint$active <- TRUE
  checkpoint$predictionFiles <- integer(0)
  checkpoint
}

checkpointFile <- function(checkpoint, name) {
  file.path(checkpoint$directory, name)
}

# The saved search, or NULL to start from the first candidate
loadSearchCheckpoint <- function(checkpoint) {
  if (is.null(checkpoint)) {
    return(NULL)
  }
  file <- checkpointFile(checkpoint, "search.rds")
  if (!file.exists(file)) {
    return(NULL)
  }
  saved <- tryCatch(
    {
      saved <- readRDS(file)
      if (!identical(saved$fingerprint, checkpoint$fingerprint) ||
        !identical(saved$formatVersion, checkpointFormatVersion)) {
        stop("it is from another search")
      }
      saved$predictions <- lapply(saved$predictionIndex, function(i) {
        readRDS(checkpointFile(checkpoint, predictionFileName(i)))
      })
      saved
    },
    error = function(e) {
      ParallelLogger::logWarn(paste0(
        "Could not read the checkpoint of the hyperparameter search in ", checkpoint$directory,
        " (", conditionMessage(e), "); starting the search over"
      ))
      NULL
    }
  )
  if (!is.null(saved)) {
    checkpoint$predictionFiles <- saved$predictionIndex
  }
  saved
}

predictionFileName <- function(i) {
  sprintf("prediction-%d.rds", as.integer(i))
}

# Writes a file so that it is either complete or not there: to a temporary file
# in the same directory, then renamed
writeCheckpointFile <- function(object, file) {
  temporary <- tempfile(pattern = paste0(basename(file), "-"), tmpdir = dirname(file), fileext = ".tmp")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  # an external pointer (such as a Python object) would be saved as a null
  # pointer, and fail only when the search resumes
  saveRDS(object, temporary, compress = FALSE, refhook = function(x) {
    if (typeof(x) == "externalptr") {
      stop("it holds an external pointer, which cannot be saved", call. = FALSE)
    }
    NULL
  })
  for (attempt in 1:3) {
    renamed <- suppressWarnings(file.rename(temporary, file))
    if (renamed) {
      return(invisible(TRUE))
    }
    # on Windows the target can be held open briefly, for example by a virus scanner
    Sys.sleep(0.5 * attempt)
  }
  stop("could not rename ", temporary, " to ", file)
}

# Saves the search after a candidate. predictions: the cross-validation
# predictions the search still needs, named by candidate index; new ones are
# written once, and files no longer needed are removed.
commitSearchCheckpoint <- function(checkpoint, history, randomSeeds, predictions, foldStates = NULL) {
  if (is.null(checkpoint) || !checkpoint$active) {
    return(invisible(NULL))
  }
  tryCatch(
    {
      dir.create(checkpoint$directory, recursive = TRUE, showWarnings = FALSE)
      index <- as.integer(names(predictions))
      for (i in setdiff(index, checkpoint$predictionFiles)) {
        writeCheckpointFile(predictions[[as.character(i)]], checkpointFile(checkpoint, predictionFileName(i)))
      }
      writeCheckpointFile(
        list(
          formatVersion = checkpointFormatVersion,
          fingerprint = checkpoint$fingerprint,
          history = history,
          randomSeeds = randomSeeds,
          predictionIndex = index,
          foldStates = foldStates
        ),
        checkpointFile(checkpoint, "search.rds")
      )
      for (i in setdiff(checkpoint$predictionFiles, index)) {
        unlink(checkpointFile(checkpoint, predictionFileName(i)))
      }
      checkpoint$predictionFiles <- index
    },
    error = function(e) {
      checkpoint$active <- FALSE
      ParallelLogger::logWarn(paste0(
        "Could not save the checkpoint of the hyperparameter search of ", checkpoint$modelName,
        " (", conditionMessage(e), "); the search goes on without checkpoints"
      ))
    }
  )
  invisible(NULL)
}

# Removes the checkpoint of a finished search, and the hyperparameterSearch
# directory when no other search has one there
removeSearchCheckpoint <- function(checkpoint) {
  if (is.null(checkpoint)) {
    return(invisible(NULL))
  }
  unlink(checkpoint$directory, recursive = TRUE)
  parent <- dirname(checkpoint$directory)
  if (dir.exists(parent) && length(list.files(parent, all.files = TRUE, no.. = TRUE)) == 0) {
    unlink(parent, recursive = TRUE)
  }
  invisible(NULL)
}

# At the end of runPlp(), after its result is saved: removes the saved
# searches of the analysis when every step succeeded, and otherwise says that
# they are kept
keepSearchCheckpoints <- function(analysisPath, failedSteps) {
  directory <- file.path(analysisPath, "hyperparameterSearch")
  if (is.null(analysisPath) || !dir.exists(directory)) {
    return(invisible(NULL))
  }
  if (length(failedSteps) > 0) {
    ParallelLogger::logInfo(paste0(
      "The saved hyperparameter search in ", directory, " is kept because the ",
      paste(failedSteps, collapse = " and "), " failed; running the analysis again resumes it"
    ))
  } else {
    unlink(directory, recursive = TRUE)
  }
  invisible(NULL)
}

getRandomSeed <- function() {
  if (!exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    # the generator is seeded on first use; doing it now lets the state be saved
    stats::runif(1)
  }
  get(".Random.seed", envir = globalenv(), inherits = FALSE)
}

setRandomSeed <- function(seed) {
  assign(".Random.seed", seed, envir = globalenv())
}
