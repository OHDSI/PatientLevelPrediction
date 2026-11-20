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

An prediction dataframe with the predictions of the recalibrated model
added

## Examples
