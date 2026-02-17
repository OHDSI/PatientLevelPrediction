# recalibratePlp

Recalibrating a model using the recalibrationInTheLarge or
weakRecalibration methods

## Usage

``` r
recalibratePlp(
  prediction,
  analysisId,
  typeColumn = "evaluationType",
  method = c("recalibrationInTheLarge", "weakRecalibration")
)
```

## Arguments

- prediction:

  A prediction dataframe

- analysisId:

  The model analysisId

- typeColumn:

  The column name where the strata types are specified

- method:

  Method used to recalibrate ('recalibrationInTheLarge' or
  'weakRecalibration' )

## Value

A prediction dataframe with the recalibrated predictions added

## Details

'recalibrationInTheLarge' calculates a single correction factor for the
average predicted risks to match the average observed risks.
'weakRecalibration' fits a glm model to the logit of the predicted
risks, also known as Platt scaling/logistic recalibration.

## Examples

``` r
prediction <- data.frame(rowId = 1:100,
                         value = runif(100),
                         outcomeCount = stats::rbinom(100, 1, 0.1),
                         evaluationType = rep("validation", 100))
attr(prediction, "metaData") <- list(modelType = "binary")
# since value is unformally distributed but outcomeCount is not (prob <- 0.1)
# the predictions are mis-calibrated
outcomeRate <- mean(prediction$outcomeCount)
observedRisk <- mean(prediction$value)
message("outcome rate is: ", outcomeRate)
#> outcome rate is: 0.1
message("observed risk is: ", observedRisk)
#> observed risk is: 0.483010764499195
# lets recalibrate the predictions
prediction <- recalibratePlp(prediction, 
                             analysisId = "recalibration", 
                             method = "recalibrationInTheLarge")
recalibratedRisk <- mean(prediction$value)
message("recalibrated risk with recalibration in the large is: ", recalibratedRisk)
#> recalibrated risk with recalibration in the large is: 0.330723466843302
prediction <- recalibratePlp(prediction, 
                             analysisId = "recalibration", 
                             method = "weakRecalibration")
recalibratedRisk <- mean(prediction$value)
message("recalibrated risk with weak recalibration is: ", recalibratedRisk)
#> recalibrated risk with weak recalibration is: 0.215361733421654
```
