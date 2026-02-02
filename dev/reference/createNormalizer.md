# Create the settings for normalizing the data @param type The type of normalization to use, either "minmax" or "robust"

Create the settings for normalizing the data @param type The type of
normalization to use, either "minmax" or "robust"

## Usage

``` r
createNormalizer(type = "minmax", settings = list())
```

## Arguments

- type:

  The type of normalization to use, either "minmax" or "robust"

- settings:

  A list of settings for the normalization. For robust normalization,
  the settings list can contain a boolean value for clip, which clips
  the values to be between -3 and 3 after normalization. See
  https://arxiv.org/abs/2407.04491

## Value

An object of class `featureEngineeringSettings`

An object of class `featureEngineeringSettings`'

## Examples

``` r
# create a minmax normalizer that normalizes the data between 0 and 1
normalizer <- createNormalizer(type = "minmax")
# create a robust normalizer that normalizes the data by the interquartile range
# and squeezes the values to be between -3 and 3
normalizer <- createNormalizer(type = "robust", settings = list(clip = TRUE))
```
