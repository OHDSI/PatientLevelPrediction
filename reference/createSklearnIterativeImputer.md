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

## Details

scikit-learn fits the imputer; at prediction its fitted steps (the
starting value of each feature and the coefficients of each
BayesianRidge model) are applied in R, so a saved model can be loaded
and applied without Python. With `samplePosterior = TRUE` the imputer
draws random values, which cannot be repeated in R, so such a model can
only be applied in the R session it was fitted in.

## Examples
