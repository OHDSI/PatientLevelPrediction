# Compute the area under the Precision-Recall curve

Compute the area under the Precision-Recall curve

## Usage

``` r
computeAuprc(prediction)
```

## Arguments

- prediction:

  A prediction object as generated using the
  [`predict`](https://rdrr.io/r/stats/predict.html) functions.

## Value

A numeric value containing the AUPRC

## Details

Computes the area under the Precision-Recall curve for the predicted
scores, given the true observed outcomes.

## Examples

``` r
prediction <- data.frame(
  value = c(0.1, 0.2, 0.3, 0.4, 0.5),
  outcomeCount = c(0, 1, 0, 1, 1))
computeAuprc(prediction)
#> [1] 0.904106
```
