# A function that removes rare features from the data

A function that removes rare features from the data

## Usage

``` r
removeRareFeatures(trainData, featureEngineeringSettings, done = FALSE)
```

## Arguments

- trainData:

  The data to be normalized

- featureEngineeringSettings:

  The settings for the normalization

- done:

  Whether to find and remove rare features or remove them only (bool)

## Value

The data with rare features removed

## Details

removes features that are present in less than a certain fraction of the
population
