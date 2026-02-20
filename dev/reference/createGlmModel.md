# createGlmModel

Create a generalized linear model that can be used in the
PatientLevelPrediction package.

## Usage

``` r
createGlmModel(
  coefficients,
  intercept = 0,
  mapping = "logistic",
  targetId = NULL,
  outcomeId = NULL,
  populationSettings = createStudyPopulationSettings(),
  restrictPlpDataSettings = createRestrictPlpDataSettings(),
  covariateSettings = FeatureExtraction::createDefaultCovariateSettings(),
  featureEngineering = NULL,
  tidyCovariates = NULL,
  requireDenseMatrix = FALSE,
  modelName = "existingGlm"
)
```

## Arguments

- coefficients:

  A dataframe containing two columns, coefficients and covariateId, both
  of type numeric. The covariateId column must contain valid
  covariateIds that match those used in the `FeatureExtraction` package.

- intercept:

  A numeric value representing the intercept of the model.

- mapping:

  A string representing the mapping from the linear predictors to
  outcome probabilities. For generalized linear models this is the
  inverse of the link function. Supported values is only "logistic" for
  logistic regression model at the moment.

- targetId:

  Add the development targetId here

- outcomeId:

  Add the development outcomeId here

- populationSettings:

  Add development population settings (this includes the time-at-risk
  settings).

- restrictPlpDataSettings:

  Add development restriction settings

- covariateSettings:

  Add the covariate settings here to specify how the model covariates
  are created from the OMOP CDM

- featureEngineering:

  Add any feature engineering here (e.g., if you need to modify the
  covariates before applying the model) This is a list of lists
  containing a string named funct specifying the engineering function to
  call and settings that are inputs to that function. funct must take as
  input trainData (a plpData object) and settings (a list).

- tidyCovariates:

  Add any tidyCovariates mappings here (e.g., if you need to normalize
  the covariates)

- requireDenseMatrix:

  Specify whether the model needs a dense matrix (TRUE or FALSE)

- modelName:

  A name that will be used for the model type in the shiny viewer

## Value

A model object containing the model (Coefficients and intercept) and the
prediction function.

## Examples

``` r
coefficients <- data.frame(
  covariateId = c(1002),
  coefficient = c(0.05))
model <- createGlmModel(coefficients, intercept = -2.5)
data("simulationProfile")
plpData <- simulatePlpData(simulationProfile, n = 50, seed = 42)
#> Generating covariates
#> Generating cohorts
#> Generating outcomes
prediction <- predictPlp(model, plpData, plpData$cohorts)
#> did FE
#> did tidy
#> predict risk probabilities using predictGlm
#> Prediction took 0.16 secs
#> Prediction done in: 0.166 secs
# see the predicted risk values
prediction$value
#>  [1] 0.3208213 0.3543437 0.3318122 0.2994329 0.3658644 0.3543437 0.3318122
#>  [8] 0.3429895 0.2994329 0.3318122 0.4013123 0.4133824 0.3208213 0.3318122
#> [15] 0.3893608 0.3429895 0.3429895 0.3429895 0.3658644 0.4255575 0.3208213
#> [22] 0.3775407 0.4133824 0.3429895 0.3318122 0.3775407 0.3100255 0.3658644
#> [29] 0.3318122 0.4378235 0.3318122 0.3893608 0.3318122 0.3429895 0.3318122
#> [36] 0.3208213 0.3100255 0.3429895 0.3893608 0.3318122 0.2994329 0.3429895
#> [43] 0.3318122 0.3893608 0.3318122 0.2788848 0.3775407 0.2890505 0.3658644
#> [50] 0.4133824
```
