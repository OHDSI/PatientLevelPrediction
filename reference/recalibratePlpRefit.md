# recalibratePlpRefit

Recalibrating a model by refitting it

## Usage

``` r
recalibratePlpRefit(plpModel, newPopulation, newData, returnModel = FALSE)
```

## Arguments

- plpModel:

  The trained plpModel (runPlp\$model)

- newPopulation:

  The population created using createStudyPopulation() who will have
  their risks predicted

- newData:

  An object of type `plpData` - the patient level prediction data
  extracted from the CDM.

- returnModel:

  Logical: return the refitted model

## Value

A prediction data frame with the original model's predictions for
`newPopulation` (evaluationType "validation") and one prediction per
patient from the refitted model (evaluationType "recalibrationRefit").
The refitted model is fitted on these same patients, so its predictions
are in-sample and their performance is optimistic. With
`returnModel = TRUE`, a list with this `prediction` and the refitted
`model`. `NULL` if the refit fails.

## Examples
