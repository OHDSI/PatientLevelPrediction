# Calculates the prediction distribution

Calculates the prediction distribution

## Usage

``` r
getPredictionDistribution_binary(prediction, evalColumn, ...)
```

## Arguments

- prediction:

  A prediction object

- evalColumn:

  A column that is used to stratify the results

- ...:

  Other inputs

## Value

The 0.00, 0.1, 0.25, 0.5, 0.75, 0.9, 1.00 quantile pf the prediction,
the mean and standard deviation per class

## Details

Calculates the quantiles from a predition object
