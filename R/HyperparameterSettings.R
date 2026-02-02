#' Create Hyperparameter Settings
#' @param search The type of hyperparameter search to perform. Options are "grid" for grid search, "random" for random search, and "custom" for a user-defined search strategy.
#' @param tuningMetric The metric to optimize during hyperparameter tuning. Common
#'   choices include `aucMetric` and `auprcMetric`.
#' @param sampleSize Sample size in case of random sampling
#' @param randomSeed Random seed for random sampling
#' @param generator An object with `initialize`, `getNext` and `finalize` methods 
#' for custom flexible hyperparameter tuning.
#' @export
createHyperparameterSettings <- function(
  search = "grid",
  tuningMetric = aucMetric,
  sampleSize = NULL,
  randomSeed = NULL,
  generator = NULL
) {
  stopifnot(search %in% c("grid", "random", "custom"))
  if (identical(search, "random")) {
    stopifnot(length(sampleSize) == 1, is.numeric(sampleSize))
  }
  if (!is.null(generator)) {
    stopifnot(is.function(generator))
    search <- "custom"
  }

  structure(
    list(
      search = search,
      tuningMetric = tuningMetric,
      sampleSize = sampleSize,
      randomSeed = randomSeed,
      generator = generator
    ),
    class = "hyperparameterSettings"
  )
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
        return(makeSequentialIterator(paramDefinition))
      }
      expanded <- listCartesian(paramDefinition)

      if (identical(settings$search, "grid")) {
        return(makeSequentialIterator(expanded))
      }

      if (identical(settings$search, "random")) {
        idx <- sample.int(
          length(expanded),
          size = min(settings$sampleSize, length(expanded))
        )
        return(makeSequentialIterator(expanded[idx]))
      }

      if (identical(settings$search, "custom")) {
        generator <- settings$generator
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
#'   when given a prediction data frame.
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
    if (!is.numeric(result) || length(result) != 1 || !is.finite(result)) {
      stop("Metric function must return one finite numeric value.", call. = FALSE)
    }
    result
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

aucMetric <- createTuningMetric(
  fun = computeAuc,
  maximize = TRUE,
  name = "AUC"
)

auprcMetric <- createTuningMetric(
  fun = function(prediction) {
    positive <- prediction$value[prediction$outcomeCount == 1]
    negative <- prediction$value[prediction$outcomeCount == 0]
    pr <- PRROC::pr.curve(scores.class0 = positive, scores.class1 = negative)
    auprc <- pr$auc.integral
  },
  maximize = TRUE,
  name = "AUPRC"
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
      fold = c("CV", as.character(seq_along(performanceFold))),
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
