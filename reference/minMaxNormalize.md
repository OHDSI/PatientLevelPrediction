# A function that normalizes continous features to have values between 0 and 1

A function that normalizes continous features to have values between 0
and 1

## Usage

``` r
minMaxNormalize(trainData, featureEngineeringSettings, done = FALSE)
```

## Arguments

- trainData:

  The data to normalize: the training data, or new data when
  `done = TRUE`. It is changed in place.

- featureEngineeringSettings:

  The settings for the normalization

- done:

  Whether the normalization has already been fitted (bool)

## Value

`trainData`, normalized, with the step recorded for prediction

## Details

uses value - min / (max - min) to normalize the data
