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

  An object with `initialize`, `getNext` and `finalize` methods for
  custom flexible hyperparameter tuning.
