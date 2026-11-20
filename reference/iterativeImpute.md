# Imputation

This function does single imputation with predictive mean matching

## Usage

``` r
iterativeImpute(trainData, featureEngineeringSettings, done = FALSE)
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
