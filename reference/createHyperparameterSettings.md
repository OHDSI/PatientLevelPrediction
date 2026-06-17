# Create Hyperparameter Settings

Create Hyperparameter Settings

## Usage

``` r
createHyperparameterSettings(
  search = "grid",
  tuningMetric = aucMetric,
  sampleSize = NULL,
  randomSeed = NULL,
  generator = NULL
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
  may provide an optional `finalize(history)` method.
