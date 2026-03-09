# externally validate the multiple plp models across new datasets

This function loads all the models in a multiple plp analysis folder and
validates the models on new data

## Usage

``` r
validateMultiplePlp(
  analysesLocation,
  validationDatabaseDetails,
  validationRestrictPlpDataSettings = createRestrictPlpDataSettings(),
  recalibrate = NULL,
  cohortDefinitions = NULL,
  saveDirectory = NULL
)
```

## Arguments

- analysesLocation:

  The location where the multiple plp analyses are

- validationDatabaseDetails:

  A single or list of validation database settings created using
  [`createDatabaseDetails()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createDatabaseDetails.md)

- validationRestrictPlpDataSettings:

  The settings specifying the extra restriction settings when extracting
  the data created using
  [`createRestrictPlpDataSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createRestrictPlpDataSettings.md).

- recalibrate:

  A vector of recalibration methods (currently supports
  'RecalibrationintheLarge' and/or 'weakRecalibration')

- cohortDefinitions:

  A list of cohortDefinitions

- saveDirectory:

  The location to save to validation results

## Value

Nothing. The results are saved to the saveDirectory

## Details

Users need to input a location where the results of the multiple plp
analyses are found and the connection and database settings for the new
data

## Examples
