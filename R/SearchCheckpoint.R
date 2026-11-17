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

checkpointFormatVersion <- 2L

# runPlp() sets deferRemoval while it fits; fitClassifier() then adds the
# directory of the search it finished to completed, for runPlp() to remove
# once its result is saved
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
                                 hyperparamSettings, metric, hooks, selector = NULL) {
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
    searchFingerprintParts(data, labels, param, settings, hyperparamSettings, metric, hooks, selector),
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

searchFingerprintParts <- function(data, labels, param, settings, hyperparamSettings, metric, hooks,
                                   selector = NULL) {
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
  generator <- hyper$generator
  if (is.character(generator)) {
    # a generator given by name, by the code of the function it names
    generator <- resolveHook(generator, "generator")
  }
  if (!is.null(hyper$generator) && !is.function(generator)) {
    # A generator object by the code of its methods: the values they capture
    # (counters, remaining candidates) change as it is used, and initialize()
    # resets them. The replay of the candidates checks what the generator does.
    methods <- intersect(c("initialize", "getNext", "finalize"), names(generator))
    generator <- lapply(stats::setNames(methods, methods), function(m) deparse(generator[[m]]))
  }
  hyper$generator <- NULL
  hyper$selector <- NULL
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
    generator = fingerprintValue(generator, "the hyperparameter generator"),
    selector = fingerprintValue(selector, "the selector"),
    # the packages of the functions, for changes in their code that the
    # fingerprint of a function does not see (the functions it calls)
    versions = c(
      PatientLevelPrediction = as.character(utils::packageVersion("PatientLevelPrediction")),
      unlist(lapply(
        c(hooks[c("train", "predict", "prepareFold", "updateState")],
          list(generator = if (is.function(generator)) generator, selector = selector, metric = metric$fun)),
        functionPackageVersion
      ))
    )
  )
}

# The package and version of a function from a package other than PLP
functionPackageVersion <- function(fn) {
  if (!is.function(fn) || is.primitive(fn)) {
    return(NULL)
  }
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
fingerprintValue <- function(x, what, visiting = new.env(parent = emptyenv())) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.function(x)) {
    return(fingerprintClosure(x, what, visiting))
  }
  if (isS4(x)) {
    if (methods::is(x, "Matrix")) {
      # factors is a cache of factorizations, not part of the matrix
      slots <- setdiff(methods::slotNames(x), "factors")
      return(list(
        class = class(x),
        slots = lapply(stats::setNames(slots, slots), function(s) fingerprintValue(methods::slot(x, s), what, visiting))
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
  attrs <- if (length(attrs) > 0) lapply(attrs, fingerprintValue, what = what, visiting = visiting)
  if (is.list(x)) {
    return(list(
      type = "list",
      values = lapply(seq_along(x), function(i) fingerprintValue(.subset2(x, i), what, visiting)),
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

# A function by its code and the values it uses from the environments it was
# made in (not a package's): the variables its body names (not its arguments),
# and, for helper functions made in the same environment, the variables
# those name too. Other functions it uses are fingerprinted the same way; a
# function that is reached again while it is being fingerprinted (such as a
# train function inside the settings it captures) is identified by its code.
# Environments it captures (such as an R6 object) are not looked into, as in
# DeepPatientLevelPrediction's fingerprint.
fingerprintClosure <- function(fn, what, visiting) {
  code <- deparse(fn)
  env <- environment(fn)
  if (is.primitive(fn) || !is.environment(env)) {
    return(list(code = code))
  }
  key <- paste(format(env), rlang::hash(code))
  if (exists(key, envir = visiting, inherits = FALSE)) {
    return(list(code = code, again = TRUE))
  }
  assign(key, TRUE, envir = visiting)
  on.exit(rm(list = key, envir = visiting), add = TRUE)

  usedNames <- function(f) setdiff(unique(all.names(body(f))), names(formals(f)))
  pending <- usedNames(fn)
  captured <- list()
  while (is.environment(env) && !nzchar(environmentName(env)) && length(pending) > 0) {
    found <- intersect(pending, ls(env, all.names = TRUE))
    if (length(found) == 0) {
      env <- parent.env(env)
      next
    }
    pending <- setdiff(pending, found)
    for (name in found) {
      value <- tryCatch(get(name, envir = env), error = function(e) NULL)
      if (is.function(value) && identical(environment(value), env)) {
        # a helper made with this function: its code, and what it uses here
        captured[[name]] <- deparse(value)
        pending <- union(pending, setdiff(usedNames(value), names(captured)))
      } else if (is.function(value)) {
        captured[[name]] <- fingerprintClosure(value, what, visiting)
      } else if (is.environment(value)) {
        captured[[name]] <- list(type = "environment")
      } else {
        captured[[name]] <- fingerprintValue(value, what, visiting)
      }
    }
  }
  list(code = code, captured = captured[sort(names(captured))])
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
  # The prediction files of this search, named by candidate index. Their names
  # hold a token of this run, so that two runs of the same search (which PLP
  # does not lock against each other) never write or delete each other's.
  checkpoint$predictionFiles <- character(0)
  checkpoint$token <- paste0(Sys.getpid(), "-", format(as.numeric(Sys.time()) * 1000, scientific = FALSE))
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
      saved$predictionIndex <- as.integer(names(saved$predictionFiles))
      saved$predictions <- lapply(unname(saved$predictionFiles), function(name) {
        readRDS(checkpointFile(checkpoint, name))
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
    checkpoint$predictionFiles <- saved$predictionFiles
  }
  saved
}

predictionFileName <- function(i, token) {
  sprintf("prediction-%d-%s.rds", as.integer(i), token)
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
      index <- as.character(as.integer(names(predictions)))
      files <- checkpoint$predictionFiles[intersect(index, names(checkpoint$predictionFiles))]
      for (i in setdiff(index, names(files))) {
        name <- predictionFileName(i, checkpoint$token)
        writeCheckpointFile(predictions[[i]], checkpointFile(checkpoint, name))
        files[i] <- name
      }
      files <- files[index]
      writeCheckpointFile(
        list(
          formatVersion = checkpointFormatVersion,
          fingerprint = checkpoint$fingerprint,
          history = history,
          randomSeeds = randomSeeds,
          predictionFiles = files,
          foldStates = foldStates
        ),
        checkpointFile(checkpoint, "search.rds")
      )
      unlink(checkpointFile(checkpoint, setdiff(checkpoint$predictionFiles, files)))
      checkpoint$predictionFiles <- files
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
# searches that this run finished (completed) when every step succeeded, and
# otherwise says that the saved searches of the analysis are kept. Searches of
# other models or settings in the same analysis folder are left alone.
finishSearchCheckpoints <- function(analysisPath, completed, failedSteps) {
  if (is.null(analysisPath)) {
    return(invisible(NULL))
  }
  if (length(failedSteps) > 0) {
    directory <- file.path(analysisPath, "hyperparameterSearch")
    if (dir.exists(directory)) {
      ParallelLogger::logInfo(paste0(
        "The saved hyperparameter search in ", directory, " is kept because the ",
        paste(failedSteps, collapse = " and "), " failed; running the analysis again resumes it"
      ))
    }
    return(invisible(NULL))
  }
  for (directory in completed) {
    removeSearchCheckpoint(list(directory = directory))
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
