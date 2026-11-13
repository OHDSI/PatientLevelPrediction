# Record a fitted feature engineering step so it is applied at prediction

Call this at the end of a feature engineering 'implement' function,
before returning `trainData`. When the model is applied to new data,
PatientLevelPrediction calls the recorded steps again, in the order they
were recorded.

## Usage

``` r
recordFeatureEngineeringStep(trainData, funct, settings)
```

## Arguments

- trainData:

  The data the implement function received and changed.

- funct:

  The name of the implement function to call at prediction.

- settings:

  A named list of the other arguments for that call.

## Value

`trainData`, with the step added to
`attr(trainData$covariateData, "metaData")$featureEngineering`.

## Details

At prediction the function named `funct` is called with `trainData` (a
copy of the data to transform) and the elements of `settings` as
arguments. Put everything the function needs to repeat the step in
`settings`, such as the `featureEngineeringSettings` and the values
learned from the training data. The model is saved as JSON, so
`settings` should hold only numbers, strings, logicals, data frames and
lists of these.

## Examples

``` r
covariateData <- Andromeda::andromeda(
  covariates = data.frame(rowId = 1:3, covariateId = 1, covariateValue = c(1, 5, 9))
)
trainData <- list(covariateData = covariateData)
trainData <- recordFeatureEngineeringStep(
  trainData,
  funct = "capCovariateValues",
  settings = list(featureEngineeringSettings = list(), cap = 5)
)
attr(trainData$covariateData, "metaData")$featureEngineering
#> [[1]]
#> [[1]]$funct
#> [1] "capCovariateValues"
#> 
#> [[1]]$settings
#> [[1]]$settings$featureEngineeringSettings
#> list()
#> 
#> [[1]]$settings$cap
#> [1] 5
#> 
#> 
#> 
Andromeda::close(covariateData)
```
