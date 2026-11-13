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
  validation = "cv"
)
```

## Arguments

- search:

  The type of hyperparameter search to perform. Options are "grid" for
  grid search, "random" for random search, and "custom" for a
  user-defined search strategy.

- tuningMetric:

  The metric to optimize during hyperparameter tuning. Common choices
  include `aucMetric` and `auprcMetric`.

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
