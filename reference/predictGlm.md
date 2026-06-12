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
#> Prediction took 0.164 secs
# see the predicted risk values
head(prediction)
#>   rowId subjectId targetId cohortStartDate daysFromObsStart daysToCohortEnd
#> 1     1     2e+10        1      2010-09-05              398             864
#> 2     2     2e+10        1      2010-06-22              130             899
#> 3     3     2e+10        1      2009-04-18              998             105
#> 4     4     2e+10        1      2007-10-18              301             176
#> 5     5     2e+10        1      2011-06-30              256             587
#> 6     6     2e+10        1      2008-03-12              126             141
#>   daysToObsEnd ageYear gender     value rawValue
#> 1         1553      35   8507 0.3208213    -0.75
#> 2         1311      38   8532 0.3543437    -0.60
#> 3          827      36   8532 0.3318122    -0.70
#> 4         1003      33   8507 0.2994329    -0.85
#> 5         1217      39   8532 0.3658644    -0.55
#> 6          414      38   8532 0.3543437    -0.60
```
