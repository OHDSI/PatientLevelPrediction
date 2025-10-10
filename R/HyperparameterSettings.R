#' Create Hyperparameter Settings
#' @param search The type of hyperparameter search to perform. Options are "grid" for grid search, "random" for random search, and "custom" for a user-defined search strategy.
#' @param tuningMetric The metric to optimize during hyperparameter tuning.
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
      expanded <- listCartesian(paramDefinition)

      if (identical(settings$search, "grid")) {
        return(makeSequentialIterator(expanded))
      }

      if (identical(settings$earch, "random")) {
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
#' @param name Friendly name for logs and summaries; defaults to the function name.
#' @param funArgs Optional named list of extra arguments passed to `fun`.
#' @export
createTuningMetric <- function(fun,
                               maximize = TRUE,
                               name = NULL,
                               funArgs = list()) {
  if (is.character(fun)) {
    funName <- fun
    fun <- get(funName, envir = parent.frame())
    if (!is.function(fun)) {
      stop(sprintf("`%s` is not a function.", funName), call. = FALSE)
    }
    if (is.null(name)) {
      name <- funName
    }
  }
  if (!is.function(fun)) {
    stop("`fun` must be a function or the name of a function.", call. = FALSE)
  }
  if (!is.list(funArgs)) {
    stop("`funArgs` must be a named list.", call. = FALSE)
  }
  name <- name %||% rlang::friendly_type_of(fun)
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
