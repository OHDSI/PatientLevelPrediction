# Skip a hyperparameter candidate that cannot be fitted

Called from a model's train or predict function to say that the current
hyperparameter candidate cannot be fitted, for example because it runs
out of memory. The hyperparameter search records the candidate with a
missing performance and this message, warns, and goes on with the next
candidate. The search stops if every candidate is skipped.

Any other error in train or predict stops the search, so a model that
never calls `stopCandidate()` behaves as before.

## Usage

``` r
stopCandidate(message, ...)
```

## Arguments

- message:

  Why the candidate cannot be fitted.

- ...:

  Further fields for the condition, passed to
  [`errorCondition()`](https://rdrr.io/r/base/conditions.html).

## Value

Does not return; signals an error of class `plpCandidateError`.

## Examples

``` r
train <- function(dataMatrix, labels, hyperParameters, settings) {
  if (hyperParameters$size > 1000) {
    stopCandidate("size larger than the memory allows")
  }
  list(size = hyperParameters$size)
}
tryCatch(
  train(NULL, NULL, list(size = 5000), NULL),
  plpCandidateError = function(e) conditionMessage(e)
)
#> [1] "size larger than the memory allows"
```
