# predict using a logistic regression model

Predict risk with a given plpModel containing a generalized linear
model.

## Usage

``` r
predictGlm(plpModel, data, cohort)
```

## Arguments

- plpModel:

  An object of type `plpModel` - a patient level prediction model

- data:

  An object of type `plpData` - the patient level prediction data
  extracted from the CDM.

- cohort:

  The population dataframe created using `createStudyPopulation` who
  will have their risks predicted or a cohort without the outcome known

## Value

A dataframe containing the prediction for each person in the population

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
prediction <- predictGlm(model, plpData, plpData$cohorts)
#> predict risk probabilities using predictGlm
#> Prediction took 0.131 secs
# see the predicted risk values
head(prediction)
#>   rowId subjectId targetId cohortStartDate daysFromObsStart daysToCohortEnd
#> 1     1     2e+10        1      2009-05-12              100             826
#> 2     2     2e+10        1      2011-09-12              710             791
#> 3     3     2e+10        1      2009-07-05              458             938
#> 4     4     2e+10        1      2009-08-22              603             425
#> 5     5     2e+10        1      2012-09-16              212             252
#> 6     6     2e+10        1      2009-02-03              504             444
#>   daysToObsEnd ageYear gender     value rawValue
#> 1         1654      38   8532 0.3543437    -0.60
#> 2         1406      40   8532 0.3775407    -0.50
#> 3         1429      37   8507 0.3429895    -0.65
#> 4          563      40   8532 0.3775407    -0.50
#> 5          964      42   8532 0.4013123    -0.40
#> 6          667      34   8532 0.3100255    -0.80
```
