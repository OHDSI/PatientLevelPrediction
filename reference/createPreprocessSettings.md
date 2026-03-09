# Create the settings for preprocessing the trainData.

Create the settings for preprocessing the trainData.

## Usage

``` r
createPreprocessSettings(
  minFraction = 0.001,
  normalize = TRUE,
  removeRedundancy = TRUE
)
```

## Arguments

- minFraction:

  The minimum fraction of target population who must have a covariate
  for it to be included in the model training

- normalize:

  Whether to normalise the covariates before training (Default: TRUE)

- removeRedundancy:

  Whether to remove redundant features (Default: TRUE) Redundant
  features are features that within an analysisId together cover all
  observations. For example with ageGroups, if you have ageGroup 0-18
  and 18-100 and all patients are in one of these groups, then one of
  these groups is redundant.

## Value

An object of class `preprocessingSettings`

## Details

Returns an object of class `preprocessingSettings` that specifies how to
preprocess the training data

## Examples

``` r
# Create the settings for preprocessing, remove no features, normalise the data
createPreprocessSettings(minFraction = 0.0, normalize = TRUE, removeRedundancy = FALSE)
#> $minFraction
#> [1] 0
#> 
#> $normalize
#> [1] TRUE
#> 
#> $removeRedundancy
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "preprocessSettings"
```
