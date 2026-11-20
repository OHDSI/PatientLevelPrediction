# Create Hyperparameter Settings

Create Hyperparameter Settings

## Usage

``` r
createHyperparameterSettings(
  search = "grid",
  tuningMetric = aucMetric,
  sampleSize = NULL,
  randomSeed = NULL,
  generator = NULL,
  validation = "cv",
  selector = NULL,
  foldWorkers = 1,
  generatorArgs = list()
)
```

## Arguments

- search:

  The type of hyperparameter search to perform. Options are "grid" for
  grid search, "random" for random search, and "custom" for a
  user-defined search strategy.

- tuningMetric:

  The metric to optimize during hyperparameter tuning:
  [aucMetric](https://ohdsi.github.io/PatientLevelPrediction/reference/tuningMetrics.md)
  (the default),
  [auprcMetric](https://ohdsi.github.io/PatientLevelPrediction/reference/tuningMetrics.md),
  [logLikelihoodMetric](https://ohdsi.github.io/PatientLevelPrediction/reference/tuningMetrics.md)
  or one made with
  [`createTuningMetric()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createTuningMetric.md).
  When it is not given, a model that names its own default metric (such
  as iterative hard thresholding, which uses
  [logLikelihoodMetric](https://ohdsi.github.io/PatientLevelPrediction/reference/tuningMetrics.md))
  is tuned on that metric.

- sampleSize:

  Sample size in case of random sampling

- randomSeed:

  Random seed for random sampling

- generator:

  Optional custom hyperparameter generator. This can be either a
  function with arguments `definition`, `expanded`, and `settings` that
  returns a finite list of named parameter lists, or an object with
  lifecycle methods. Lifecycle generator objects must provide
  `initialize(definition, settings)` and `getNext(history)` methods, and
  may provide an optional `finalize(history)` method. A candidate the
  model skipped with
  [`stopCandidate()`](https://ohdsi.github.io/PatientLevelPrediction/reference/stopCandidate.md)
  is in the history with `cvPerformance = NA` and its message in
  `error`.

  The generator can also be the name of a function (`"pkg::fn"`) that
  returns such a generator when called with `generatorArgs`. It is
  called afresh for every search, and the settings stay valid after they
  are saved as JSON (a lifecycle object's methods lose the state they
  share when saved as JSON).

- validation:

  How each candidate is scored: `"cv"` (the default) fits it once per
  fold and scores it on the left-out fold, `"trainValidation"` fits it
  once on all folds but the last and scores it on the last fold, for
  models where one fit takes long. The size of the validation part is
  set by the number of folds in the split settings (with `nfold = 5` it
  is a fifth of the training data). The retained predictions of the
  selected candidate have evaluationType `"CV"` or `"TrainValidation"`.
  Cyclops models select their penalty with their own cross-validation
  and ignore this setting.

- selector:

  Optional function (or its name) `function(history, metric)` that
  returns the index of the selected candidate in `history`, for example
  a one-standard-error rule. The default selects the best mean
  performance, the first one on ties. A model can name its own default
  selector.

- foldWorkers:

  Number of worker processes that fit the folds of a candidate in
  parallel, for models that support it (their settings name
  `parallelFolds`); other models fit the folds one after the other. Each
  fold stays on one worker for the whole search, and the results are the
  same as with one worker.

- generatorArgs:

  A named list of arguments for a `generator` given by name.
