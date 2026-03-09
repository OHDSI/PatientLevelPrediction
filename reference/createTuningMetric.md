# Create a tuning metric descriptor

Create a tuning metric descriptor

## Usage

``` r
createTuningMetric(fun, maximize = TRUE, name, funArgs = list())
```

## Arguments

- fun:

  Function (or function name) that returns a single numeric score when
  given a prediction data frame.

- maximize:

  Logical; TRUE if larger is better.

- name:

  Friendly name for logs and summaries.

- funArgs:

  Optional named list of extra arguments passed to `fun`.
