# Compute the area under the ROC curve

Compute the area under the ROC curve

## Usage

``` r
computeAuc(prediction, confidenceInterval = FALSE)
```

## Arguments

- prediction:

  A prediction object as generated using the
  [`predict`](https://rdrr.io/r/stats/predict.html) functions.

- confidenceInterval:

  Should 95 percebt confidence intervals be computed?

## Value

A data.frame containing the AUC and optionally the 95% confidence
interval

## Details

Computes the area under the ROC curve for the predicted probabilities,
given the true observed outcomes.

## Examples

``` r
prediction <- data.frame(
  value = c(0.1, 0.2, 0.3, 0.4, 0.5),
  outcomeCount = c(0, 1, 0, 1, 1))
computeAuc(prediction)
#> [1] 0.8333333
```
