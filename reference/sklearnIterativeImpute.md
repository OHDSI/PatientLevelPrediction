# scikit-learn Iterative Imputation

This function imputes missing values using scikit-learn
`IterativeImputer` through `reticulate`.

## Usage

``` r
sklearnIterativeImpute(trainData, featureEngineeringSettings, done = FALSE)
```

## Arguments

- trainData:

  The data to impute: the training data, or new data when `done = TRUE`.
  It is changed in place.

- featureEngineeringSettings:

  The settings for the imputation

- done:

  Whether the imputer has already been fitted (bool)

## Value

`trainData`, imputed, with the step recorded for prediction
