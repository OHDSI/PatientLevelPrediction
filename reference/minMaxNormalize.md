# A function that normalizes continous features to have values between 0 and 1

A function that normalizes continous features to have values between 0
and 1

## Usage

``` r
minMaxNormalize(trainData, featureEngineeringSettings, done = FALSE)
```

## Arguments

- trainData:

  The training data to be normalized

- featureEngineeringSettings:

  The settings for the normalization

- done:

  Whether the data has already been normalized (bool)

## Value

The normalized data

## Details

uses value - min / (max - min) to normalize the data
