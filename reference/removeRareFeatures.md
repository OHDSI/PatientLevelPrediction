# A function that removes rare features from the data

A function that removes rare features from the data

## Usage

``` r
removeRareFeatures(trainData, featureEngineeringSettings, done = FALSE)
```

## Arguments

- trainData:

  The data to remove rare features from: the training data, or new data
  when `done = TRUE`. It is changed in place.

- featureEngineeringSettings:

  The settings for the rare feature removal

- done:

  Whether to find and remove rare features or remove them only (bool)

## Value

`trainData`, with rare features removed and the step recorded for
prediction

## Details

removes features that are present in less than a certain fraction of the
population
