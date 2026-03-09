# externalValidateDbPlp - Validate a model on new databases

This function extracts data using a user specified connection and
cdm_schema, applied the model and then calcualtes the performance

## Usage

``` r
externalValidateDbPlp(
  plpModel,
  validationDatabaseDetails = createDatabaseDetails(),
  validationRestrictPlpDataSettings = createRestrictPlpDataSettings(),
  settings = createValidationSettings(recalibrate = "weakRecalibration"),
  logSettings = createLogSettings(verbosity = "INFO", logName = "validatePLP"),
  outputFolder = NULL
)
```

## Arguments

- plpModel:

  The model object returned by runPlp() containing the trained model

- validationDatabaseDetails:

  A list of objects of class `databaseDetails` created using
  `createDatabaseDetails`

- validationRestrictPlpDataSettings:

  A list of population restriction settings created by
  [`createRestrictPlpDataSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createRestrictPlpDataSettings.md)

- settings:

  A settings object of class `validationSettings` created using
  `createValidationSettings`

- logSettings:

  An object of `logSettings` created using `createLogSettings`
  specifying how the logging is done

- outputFolder:

  The directory to save the validation results to (subfolders are
  created per database in validationDatabaseDetails)

## Value

An externalValidatePlp object containing the following components

- model: The model object

- executionSummary: A list of execution details

- prediction: A dataframe containing the predictions

- performanceEvaluation: A dataframe containing the performance metrics

- covariateSummary: A dataframe containing the covariate summary

## Details

Users need to input a trained model (the output of runPlp()) and new
database connections. The function will return a list of length equal to
the number of cdm_schemas input with the performance on the new data

## Examples
