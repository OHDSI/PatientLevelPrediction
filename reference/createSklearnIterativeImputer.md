# Create scikit-learn Iterative Imputer settings

This function creates settings for a dense iterative imputer powered by
scikit-learn's `IterativeImputer` through `reticulate`.

## Usage

``` r
createSklearnIterativeImputer(
  missingThreshold = 0.3,
  methodSettings = list(),
  addMissingIndicator = FALSE
)
```

## Arguments

- missingThreshold:

  The threshold for missing values to remove a feature

- methodSettings:

  A list of settings for sklearn `IterativeImputer`. Supported settings
  are:

  - maxIter

  - tol

  - samplePosterior

  - nNearestFeatures

  - initialStrategy

  - imputationOrder

  - skipComplete

  - randomState

  - minValue

  - maxValue

- addMissingIndicator:

  Add a binary missingness indicator per feature that passes the
  imputation missingness threshold.

## Value

The settings for the sklearn iterative imputer of class
`featureEngineeringSettings`

## Examples
