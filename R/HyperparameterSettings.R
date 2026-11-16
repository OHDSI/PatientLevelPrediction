#' Create Hyperparameter Settings
#' @param search The type of hyperparameter search to perform. Options are "grid" for grid search, "random" for random search, and "custom" for a user-defined search strategy.
#' @param tuningMetric The metric to optimize during hyperparameter tuning:
#'   [aucMetric] (the default), [auprcMetric], [logLikelihoodMetric] or one
#'   made with [createTuningMetric()]. When it is not given, a model that names
#'   its own default metric (such as iterative hard thresholding, which uses
#'   [logLikelihoodMetric]) is tuned on that metric.
#' @param sampleSize Sample size in case of random sampling
#' @param randomSeed Random seed for random sampling
#' @param generator Optional custom hyperparameter generator. This can be either
#' a function with arguments `definition`, `expanded`, and `settings` that
#' returns a finite list of named parameter lists, or an object with lifecycle
#' methods. Lifecycle generator objects must provide `initialize(definition,
#' settings)` and `getNext(history)` methods, and may provide an optional
#' `finalize(history)` method. A candidate the model skipped with
#' [stopCandidate()] is in the history with `cvPerformance = NA` and its
#' message in `error`.
#'
#' The generator can also be the name of a function (`"pkg::fn"`) that returns
#' such a generator when called with `generatorArgs`. It is called afresh for
#' every search, and the settings stay valid after they are saved as JSON (a
#' lifecycle object's methods lose the state they share when saved as JSON).
#' @param validation How each candidate is scored: `"cv"` (the default) fits it
#'   once per fold and scores it on the left-out fold, `"trainValidation"` fits
#'   it once on all folds but the last and scores it on the last fold, for
#'   models where one fit takes long. The size of the validation part is set by
#'   the number of folds in the split settings (with `nfold = 5` it is a fifth of
#'   the training data). The retained predictions of the selected candidate
#'   have evaluationType `"CV"` or `"TrainValidation"`. Cyclops models select
#'   their penalty with their own cross-validation and ignore this setting.
#' @param selector Optional function (or its name) `function(history, metric)`
#'   that returns the index of the selected candidate in `history`, for example
#'   a one-standard-error rule. The default selects the best mean performance,
#'   the first one on ties. A model can name its own default selector.
#' @param foldWorkers Number of worker processes that fit the folds of a
#'   candidate in parallel, for models that support it (their settings name
#'   `parallelFolds`); other models fit the folds one after the other. Each
#'   fold stays on one worker for the whole search, and the results are the
#'   same as with one worker.
#' @param generatorArgs A named list of arguments for a `generator` given by
#'   name.
#' @export
createHyperparameterSettings <- function(
  search = "grid",
  tuningMetric = aucMetric,
  sampleSize = NULL,
  randomSeed = NULL,
  generator = NULL,
  validation = "cv",
  selector = NULL,
  foldWorkers = 1,
  generatorArgs = list()
) {
  stopifnot(search %in% c("grid", "random", "custom"))
  if (!is.character(validation) || length(validation) != 1L ||
    !validation %in% c("cv", "trainValidation")) {
    stop("`validation` must be \"cv\" or \"trainValidation\"", call. = FALSE)
  }
  if (identical(search, "random")) {
    stopifnot(length(sampleSize) == 1, is.numeric(sampleSize))
  }
  if (!is.null(generator)) {
    if (!isHyperparameterGenerator(generator)) {
      stop(
        "`generator` must be a function, the name of a function that returns a generator, ",
        "or an object with initialize and getNext methods; finalize is optional.",
        call. = FALSE
      )
    }
    search <- "custom"
  }
  if (!is.list(generatorArgs) || (length(generatorArgs) > 0 && is.null(names(generatorArgs)))) {
    stop("`generatorArgs` must be a named list", call. = FALSE)
  }
  if (length(generatorArgs) > 0 && !is.character(generator)) {
    stop("`generatorArgs` are only used with a generator given by name", call. = FALSE)
  }
  if (!is.null(selector) && !is.function(selector) &&
    !(is.character(selector) && length(selector) == 1L && !is.na(selector) && nzchar(selector))) {
    stop("`selector` must be a function or the name of one", call. = FALSE)
  }
  if (!is.numeric(foldWorkers) || length(foldWorkers) != 1L || is.na(foldWorkers) ||
    foldWorkers < 1 || foldWorkers != round(foldWorkers)) {
    stop("`foldWorkers` must be a whole number of at least 1", call. = FALSE)
  }

  settings <- list(
    search = search,
    tuningMetric = tuningMetric,
    sampleSize = sampleSize,
    randomSeed = randomSeed,
    generator = generator,
    validation = validation
  )
  # the new settings are only stored when they are used, so that settings
  # that do not use them look as they did before
  if (missing(tuningMetric)) {
    settings$tuningMetricIsDefault <- TRUE
  }
  if (!is.null(selector)) {
    settings$selector <- selector
  }
  if (foldWorkers != 1) {
    settings$foldWorkers <- as.integer(foldWorkers)
  }
  if (length(generatorArgs) > 0) {
    settings$generatorArgs <- generatorArgs
  }
  structure(settings, class = "hyperparameterSettings")
}

# evaluationType of the retained predictions with validation = "trainValidation"
trainValidationEvaluationType <- "TrainValidation"

isHyperparameterGenerator <- function(generator) {
  if (is.function(generator)) {
    return(TRUE)
  }
  if (is.character(generator)) {
    return(length(generator) == 1L && !is.na(generator) && nzchar(generator))
  }

  getMember <- function(name) {
    tryCatch(generator[[name]], error = function(e) NULL)
  }

  initialize <- getMember("initialize")
  getNext <- getMember("getNext")
  finalize <- getMember("finalize")

  is.function(initialize) &&
    is.function(getNext) &&
    (is.null(finalize) || is.function(finalize))
}


#' Prepare Hyperparameter 
#' @param paramDefinition A list defining the hyperparameters and their possible values.
#' @param hyperSettings An object of class \code{hyperparameterSettings} created using \code{createHyperparameterSettings}.
#' @return An iterator object with methods to get the next hyperparameter combination and finalize the search.
#' @keywords internal
prepareHyperparameterGrid <- function(paramDefinition, 
                                      hyperSettings) {
      settings <- hyperSettings %||% createHyperparameterSettings()

      makeSequentialIterator <- function(pool) {
        i <- 0L
        list(
          getNext = function(history) {
            i <<- i + 1L
            if (i > length(pool)) return(NULL)
            pool[[i]]
          },
          finalize = function(history) invisible(NULL)
        )
      }
      if (is.null(paramDefinition)) {
          empty <- list(list())
          return(makeSequentialIterator(empty))
      }

      # Backwards compatibility: already-expanded grids stored as a list of
      # parameter combinations (outer list unnamed, inner lists named)
      # and special case after json roundtrip: outer list with names "1", "2", ..., like in strategus specs
      if (is.list(paramDefinition) &&
          length(paramDefinition) > 0 &&
          is.list(paramDefinition[[1]]) &&
          !is.null(names(paramDefinition[[1]]))) {
        expanded <- unname(paramDefinition)
      } else {
        expanded <- listCartesian(paramDefinition)
      }

      if (identical(settings$search, "grid")) {
        return(makeSequentialIterator(expanded))
      }

      if (identical(settings$search, "random")) {
        if (!is.null(settings$randomSeed)) {
          oldSeedExists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
          if (oldSeedExists) oldSeed <- get(".Random.seed", envir = .GlobalEnv)
          set.seed(settings$randomSeed)
          on.exit({
            if (oldSeedExists) {
              assign(".Random.seed", oldSeed, envir = .GlobalEnv)
            } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
              rm(".Random.seed", envir = .GlobalEnv)
            }
          }, add = TRUE)
        }
        idx <- sample.int(
          length(expanded),
          size = min(settings$sampleSize, length(expanded))
        )
        return(makeSequentialIterator(expanded[idx]))
      }

      if (identical(settings$search, "custom")) {
        generator <- settings$generator
        if (is.character(generator)) {
          # a generator given by name is made afresh for every search
          generator <- do.call(
            resolveHook(generator, "generator"),
            as.list(settings$generatorArgs)
          )
          if (is.character(generator) || !isHyperparameterGenerator(generator)) {
            stop(
              "The generator function '", settings$generator, "' must return a function ",
              "or an object with initialize and getNext methods",
              call. = FALSE
            )
          }
        }
        if (is.function(generator)) {
          pool <- generator(
            definition = paramDefinition, 
            expanded = expanded, 
            settings = settings
          )
          return(makeSequentialIterator(pool))
        }
        generator$initialize(
          definition = paramDefinition,
          settings = settings
        )
        return(list(
          getNext = function(history) generator$getNext(history),
          finalize = function(history) {
              finalizeFn <- generator$finalize %||% function(...) invisible(NULL)
              finalizeFn(history)
          }
        ))
      }
      stop(sprintf("Unknown hyper-parameter search strategy '%s'.", settings$search))
}

#' Create a tuning metric descriptor
#'
#' @param fun Function (or function name) that returns a single numeric score
#'   when given a prediction data frame. A missing score (`NA`, for example the
#'   AUC of a fold with one class) is recorded and left out of the mean over
#'   the folds.
#' @param maximize Logical; TRUE if larger is better.
#' @param name Friendly name for logs and summaries.
#' @param funArgs Optional named list of extra arguments passed to `fun`.
#' @export
createTuningMetric <- function(fun,
                               maximize = TRUE,
                               name,
                               funArgs = list()) {
  if (is.character(fun)) {
    funName <- fun
    fun <- get(funName, envir = parent.frame())
    if (!is.function(fun)) {
      stop(sprintf("`%s` is not a function.", funName), call. = FALSE)
    }
  }
  if (!is.function(fun)) {
    stop("`fun` must be a function or the name of a function.", call. = FALSE)
  }
  if (!is.list(funArgs)) {
    stop("`funArgs` must be a named list.", call. = FALSE)
  }
  if (!is.character(name) || length(name) != 1 || is.na(name) || !nzchar(name)) {
    stop("`name` must be a non-empty character string.", call. = FALSE)
  }
  metricFun <- function(prediction) {
    result <- do.call(fun, c(list(prediction = prediction), funArgs))
    if (length(result) == 1 && is.logical(result) && is.na(result)) {
      result <- NA_real_
    }
    if (!is.numeric(result) || length(result) != 1) {
      stop("Metric function must return one numeric value.", call. = FALSE)
    }
    as.numeric(result)
  }
  structure(
    list(
      fun = metricFun,
      maximize = isTRUE(maximize),
      name = name,
      funArgs = funArgs
    )
  )
}

#' Tuning metrics
#'
#' @description
#' Metrics for [createHyperparameterSettings()]:
#' - `aucMetric`: the area under the ROC curve (maximized);
#' - `auprcMetric`: the area under the precision-recall curve (maximized);
#' - `logLikelihoodMetric`: the mean held-out Bernoulli log-likelihood per
#'   row (maximized), with the predicted risks clipped to
#'   \[1e-15, 1 - 1e-15\]. It rewards calibrated risks, and it is defined on
#'   folds with one class.
#' @format A tuning metric, as made by [createTuningMetric()].
#' @examples
#' settings <- createHyperparameterSettings(tuningMetric = logLikelihoodMetric)
#' prediction <- data.frame(outcomeCount = c(0, 1, 0, 1), value = c(0.1, 0.8, 0.3, 0.6))
#' logLikelihoodMetric$fun(prediction)
#' @name tuningMetrics
NULL

#' @rdname tuningMetrics
#' @export
aucMetric <- createTuningMetric(
  fun = computeAuc,
  maximize = TRUE,
  name = "AUC"
)

#' @rdname tuningMetrics
#' @export
auprcMetric <- createTuningMetric(
  fun = function(prediction) {
    positive <- prediction$value[prediction$outcomeCount > 0]
    negative <- prediction$value[prediction$outcomeCount == 0]
    pr <- PRROC::pr.curve(scores.class0 = positive, scores.class1 = negative)
    auprc <- pr$auc.integral
  },
  maximize = TRUE,
  name = "AUPRC"
)

computeLogLikelihood <- function(prediction) {
  p <- pmin(pmax(as.numeric(prediction$value), 1e-15), 1 - 1e-15)
  y <- as.numeric(prediction$outcomeCount > 0)
  mean(y * log(p) + (1 - y) * log1p(-p))
}

#' @rdname tuningMetrics
#' @export
logLikelihoodMetric <- createTuningMetric(
  fun = computeLogLikelihood,
  maximize = TRUE,
  name = "logLikelihood"
)


#' Cartesian product
#' 
#' Computes the Cartesian product of all the combinations of elements in a list
#' 
#' @param allList a list of lists
#' @return A list with all possible combinations from the input list of lists
#' @examples
#' listCartesian(list(list(1, 2), list(3, 4)))
#' @export
listCartesian <- function(allList) {
  combinations <- expand.grid(allList, stringsAsFactors = FALSE)
  results <- lapply(seq_len(nrow(combinations)),
                    function(i) lapply(combinations, function(x) x[i][[1]]))
  return(results)
}

#' Computes grid performance for a hyperparameter combination (backwards compatible)
#'
#' @param prediction A data.frame with predictions and an `index` column for folds.
#' @param param A list of hyperparameters (values may include `NULL`).
#' @param performanceFunct String or function to compute performance on a prediction data.frame.
#'   Default is `PatientLevelPrediction::computeAuc`.
#' @return A list with overall and per-fold performance plus the parameter summary.
#' @examples
#' prediction <- data.frame(
#'   rowId = c(1, 2, 3, 4, 5),
#'   outcomeCount = c(0, 1, 0, 1, 0),
#'   value = c(0.1, 0.9, 0.2, 0.8, 0.3),
#'   index = c(1, 1, 1, 1, 1)
#' )
#' param <- list(hyperParam1 = 5, hyperParam2 = 100)
#' computeGridPerformance(prediction, param, performanceFunct = PatientLevelPrediction::computeAuc)
#' @export
computeGridPerformance <- function(
    prediction,
    param,
    performanceFunct = "computeAuc") {
  perfFun <- performanceFunct
  if (is.character(performanceFunct)) {
    perfFun <- get0(performanceFunct, envir = parent.frame(), inherits = TRUE)
  }
  if (is.null(perfFun)) {
    perfFun <- get0(
      performanceFunct, 
      envir = asNamespace("PatientLevelPrediction"), 
      inherits = FALSE
    ) 
  }
  if (!is.function(perfFun)) {
    stop("performanceFunct must be a function or a function name. Fun: ", perfFun)
  }

  computeMetric <- function(pred) {
    perfFun(prediction = pred)
  }

  performance <- computeMetric(prediction)

  if (!"index" %in% colnames(prediction)) {
    prediction$index <- 1
  }

  performanceFold <- vapply(
    unique(prediction$index),
    function(i) computeMetric(prediction[prediction$index == i, ]),
    numeric(1)
  )

  paramString <- param
  for (ind in seq_along(paramString)) {
    if (is.null(paramString[[ind]])) {
      paramString[[ind]] <- "null"
    }
  }

  paramValues <- unlist(paramString)
  names(paramValues) <- names(param)

  metricName <- if (is.character(performanceFunct)) {
    performanceFunct
  } else {
    deparse(substitute(performanceFunct))
  }

  hyperSummary <- as.data.frame(c(
    data.frame(
      metric = metricName,
      fold = c("CV", as.character(unique(prediction$index))),
      value = c(performance, performanceFold)
    ),
    paramValues
  ))

  list(
    metric = metricName,
    cvPerformance = performance,
    cvPerformancePerFold = performanceFold,
    param = param,
    hyperSummary = hyperSummary
  )
}
