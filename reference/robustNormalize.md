# A function that normalizes continous by the interquartile range and optionally forces the resulting values to be between -3 and 3 with f(x) = x / sqrt(1 + (x/3)^2) '@details uses (value - median) / iqr to normalize the data and then can applies the function f(x) = x / sqrt(1 + (x/3)^2) to the normalized values. This forces the values to be between -3 and 3 while preserving the relative ordering of the values. based on https://arxiv.org/abs/2407.04491 for more details

A function that normalizes continous by the interquartile range and
optionally forces the resulting values to be between -3 and 3 with f(x)
= x / sqrt(1 + (x/3)^2) '@details uses (value - median) / iqr to
normalize the data and then can applies the function f(x) = x / sqrt(1 +
(x/3)^2) to the normalized values. This forces the values to be between
-3 and 3 while preserving the relative ordering of the values. based on
https://arxiv.org/abs/2407.04491 for more details

## Usage

``` r
robustNormalize(trainData, featureEngineeringSettings, done = FALSE)
```

## Arguments

- trainData:

  The training data to be normalized

- featureEngineeringSettings:

  The settings for the normalization

- done:

  Whether the data has already been normalized (bool)

## Value

The `trainData` object with normalized data
