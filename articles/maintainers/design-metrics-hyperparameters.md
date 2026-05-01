# Design for custom hyperparameter search and tuning metrics

## 1. New helper: `createHyperparameterSettings()`

Add a file `R/HyperparameterSettings.R`:

``` r

  createHyperparameterSettings <- function(
      search = "grid",
      tuningMetric = NULL,
      sampleSize = NULL,
      randomSeed = NULL,
      searchControl = list(),
      generator = NULL) {
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
        searchControl = searchControl,
        generator = generator
      ),
      class = "hyperparameterSettings"
    )
 }
```

- search: `grid` (default, current behavior), `random` (sample), or
  `custom` if you provide generator.
- `tuningMetric`: object created with
  [`createTuningMetric()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createTuningMetric.md);
  `NULL` falls back to AUC/maximize.
- `sampleSize`: required when `search = "random"`; number of parameter
  sets to draw.
- `randomSeed`: optional, ensures reproducible random search.
- `searchControl`: free-form list for future knobs (e.g., stratified
  sampling).
- `generator`: optional function that receives the base parameter grid
  and a copy of the settings and must return a list of parameter sets;
  if present we force search = “custom”.

Document this with `roxygen2` tags and run
[`roxygen2::roxygenize()`](https://roxygen2.r-lib.org/reference/roxygenize.html)
to document. \`\`

———

## 2. Extend `createModelDesign()`

In `R/RunMultiplePlp.R`:

- Update signature:

``` r

  createModelDesign <- function(
      ...,
      hyperparameterSettings = createHyperparameterSettings(),
      runCovariateSummary = TRUE) {
    ...
    settings$hyperparameterSettings <- hyperparameterSettings
    class(settings) <- "modelDesign"
    settings
  }
```

- Keep defaults so existing calls stay untouched.
- Update docs describing `hyperparameterSettings`.

———

## 3. Propagate through `runPlp` pipeline

- `R/RunPlp.R`: When building settings for the training call, pass both
  `hyperparameterSettings` and their `tuningMetric` into
  [`fitPlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/fitPlp.md):

``` r

  hyperparameterSettings <- modelDesign$hyperparameterSettings %||% createHyperparameterSettings()
  settings <- list(
    trainData = data$Train,
    modelSettings = modelSettings,
    hyperparameterSettings = hyperparameterSettings,
    analysisId = analysisId,
    analysisPath = analysisPath
  )
```

Note: `%||%` means take `modelDesign$hyperparameterSettings` if it
exists, otherwise default to
[`createHyperparameterSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createHyperparameterSettings.md)

- Update `runPlp` signature.

``` r
runPlp(
    ...
    modelSettings = setLassoLogisticRegression(),
    hyperparameterSettings = createHyperparameterSettings(),
    ...
)
```

    Update docs to describe `hyperparameterSettings`

- R/Fit.R: Update function signature:

``` r
  fitPlp <- function(trainData,
                     modelSettings,
                     hyperparameterSettings = createHyperparameterSettings(),
                     search = "grid",
                     analysisId,
                     analysisPath) {
    ...
    args <- list(
      trainData = trainData,
      modelSettings = modelSettings,
      hyperparameterSettings = hyperparameterSettings,
      analysisId = analysisId,
      analysisPath = analysisPath
    )
```

(Deprecate search if you like, but keep it for compatibility and let it
override `hyperparameterSettings$search` when non-default.)

———

## 4. Update classifier fitters

Each classifier with CV (e.g., `fitSklearn`, `fitRclassifier`,
`fitGradientBoostingMachine`, `fitLightGBM)` gains a
`hyperparameterSettings` argument.

Within these functions:

- Call a new helper
  `prepareHyperparameterGrid(param, hyperparameterSettings)` (see next
  section) to obtain the actual list of parameter sets to evaluate.
- Pass both the `tuningMetric` and the final grid into the CV routines.

———

## 5. Helpers for search preparation

- Replace the current “return a list of params” approach with a
  lightweight iterator façade:
  [`prepareHyperparameterGrid()`](https://ohdsi.github.io/PatientLevelPrediction/reference/prepareHyperparameterGrid.md)
  now returns an object exposing `R next = function(history)` … (and
  optionally finalize). The CV code simply calls candidate \<-
  iterator\$next(history) until it gets NULL, so grid, random, and
  custom searches all execute through the same loop.
- For grid and random, wrap the fully expanded combinations in that
  iterator: keep a private index (or shuffled index for random), yield
  one entry per call, and stop once the pool is exhausted. Seeding logic
  stays unchanged but is now encapsulated in the iterator’s closure.
- For custom searches accept both simple generators (functions returning
  a finite list) and adaptive generators. Detect which shape was
  supplied and wrap it so the iterator always exposes the next(history)
  contract. Provide adapters so legacy one-shot functions still work
  while more advanced strategies get a stateful interface.
- Document an adaptive generator contract: initialize(definition,
  settings) to set up any internal state, next(history) to emit the next
  configuration using past performance, finalize(history) to clean up if
  needed. The iterator constructor calls `initialize()` once, then
  delegates each `next()` invocation, allowing techniques like Bayesian
  optimization that fit surrogate models or maintain other state between
  trials.
- Retain backwards compatibility by allowing code that still expects a
  plain list to receive one (wrap the iterator for now and emit a
  deprecation warning), but direct all new fitters and CV routines to
  the iterator-based API so every search strategy—static or
  adaptive—lives behind the same helper and can hold state safely inside
  its closure/environment.

Example code: -
[`prepareHyperparameterGrid()`](https://ohdsi.github.io/PatientLevelPrediction/reference/prepareHyperparameterGrid.md)
returns an iterator object with next/finalize so every search path looks
the same:

``` r
    prepareHyperparameterGrid <- function(paramDefinition, hyperSettings, modelName = NULL) {
      settings <- hyperSettings %||% createHyperparameterSettings()
      makeSequentialIterator <- function(pool) {
        i <- 0L
        list(
          next = function(history) {
            i <<- i + 1L
            if (i > length(pool)) return(NULL)
            pool[[i]]
          },
          finalize = function(history) invisible(NULL)
        )
      }
      if (is.null(paramDefinition) {
          empty <- list(list())
          return(makeSequentialIterator(empty))
      }
      expanded <- expandParamDefinition(paramDefinition)

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
          next = function(history) generator$next(history),
          finalize = function(history) {
              finalizeFn <- generator$finalize %||% function(...) invisible(NULL)
              finalizeFn(history)
        ))
      }

      stop(sprintf("Unknown hyper-parameter search strategy '%s'.", settings$search))
    }
```

- The unified CV loop consumes the iterator uniformly:

``` r

    iterator <- prepareHyperparameterGrid(
      paramDefinition, 
      hyperSettings, 
      modelName
      )
    history <- list()

    repeat {
      candidate <- iterator$next(history)
      if (is.null(candidate)) break

      perf <- evaluateCandidate(candidate, data, metric)
      history[[length(history) + 1L]] <- list(param = candidate, performance = perf)
    }

    iterator$finalize(history)
    best <- selectBest(history, metric)
```

Grid, random, and adaptive custom searches all plug into this structure
by returning an iterator that maintains its own state inside the closure
or generator object.

———

## 6. Modify CV routines

- `gridCvPython(..., hyperparameterSettings, metric)` (or similar) now
  receives the metric and uses it:

``` r
evaluated <- lapply(grid, function(entry) {
  computeGridPerformance(
    prediction = entry$prediction,
    param = entry$param,
    metric = metric
  )
})

whichBest <- if (metric$maximize) {
  which.max(vapply(evaluated, `[[`, numeric(1), "cvPerformance"))
} else {
  which.min(vapply(evaluated, `[[`, numeric(1), "cvPerformance"))
```

- [`computeGridPerformance()`](https://ohdsi.github.io/PatientLevelPrediction/reference/computeGridPerformance.md)
  signature changes:

``` r

  computeGridPerformance <- function(prediction, param, metric) {
    perf <- metric$fun(prediction)
    folds <- vapply(unique(prediction$index), function(foldId) {
      metric$fun(prediction[prediction$index == foldId, ])
    }, numeric(1))

    ...
    list(
      metric = metric$label,
      cvPerformance = perf,
      cvPerformancePerFold = folds,
      param = param,
      hyperSummary = ...
    )
  }
```

For metrics needing thresholds, the supplied function can capture the
threshold:

``` r

  createTuningMetric(
    fun = function(prediction) {
      pred <- ifelse(prediction$value >= 0.6, 1, 0)
      mean(pred == (prediction$outcomeCount > 0))
    },
    maximize = TRUE,
    label = "Accuracy@0.6"
  )
```

Because the function itself closes over 0.6, no extra plumbing is
needed.

———

## 7. Backward compatibility

- `modelSettings$param` can remain unchanged, so any package that
  provides a custom parameter grid continues to work.
- If someone relies on attributes like attr(param, “settings”)\$search,
  we keep them for now but warn in documentation that
  [`createHyperparameterSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createHyperparameterSettings.md)
  is the new home for search configuration.
- Keep the positional search argument on
  [`fitPlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/fitPlp.md)
  (and maybe on the classifier fitters) but internally let it override
  the design-level setting only when explicitly set, e.g.:

``` r

  if (!missing(search)) {
    hyperparameterSettings$search <- search
  }
```

This means existing code that calls `fitPlp(..., search = "grid")` keeps
working.

———

## 8. Usage example

``` r

  metric <- createTuningMetric(
    fun = function(prediction) mean(ifelse(prediction$value >= 0.55, 1, 0) == (prediction$outcomeCount > 0)),
    maximize = TRUE,
    label = "Accuracy@0.55"
  )

  hyperSettings <- createHyperparameterSettings(
    search = "random",
    tuningMetric = metric,
    sampleSize = 10,
    randomSeed = 42
  )

  modelDesign <- createModelDesign(
    targetId = 1,
    outcomeId = 2,
    modelSettings = setAdaBoost(),
    hyperparameterSettings = hyperSettings
  )

  result <- runMultiplePlp(databaseDetails = databaseDetails, modelDesignList = list(modelDesign))
```

———

## 9. Tests & docs

- New tests for
  [`prepareHyperparameterGrid()`](https://ohdsi.github.io/PatientLevelPrediction/reference/prepareHyperparameterGrid.md)
  random sampling behavior and custom generator.
- Tests verifying the metric and search settings affect the CV
  selection.
- Tests
- Doc updates: `createModelDesign`, `runPlp`, `fitPlp`, `set*` helpers
  referencing the new design-level configuration.
- Test reverse depencies like `DeepPatientLevelPrediction`, if it
  doesn’t work with the new design, it’s a breaking change and needs to
  be fixed
