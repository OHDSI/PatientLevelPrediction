# Run a list of predictions diagnoses

Run a list of predictions diagnoses

## Usage

``` r
diagnoseMultiplePlp(
  databaseDetails = createDatabaseDetails(),
  modelDesignList = list(createModelDesign(targetId = 1, outcomeId = 2, modelSettings =
    setLassoLogisticRegression()), createModelDesign(targetId = 1, outcomeId = 3,
    modelSettings = setLassoLogisticRegression())),
  cohortDefinitions = NULL,
  logSettings = createLogSettings(verbosity = "DEBUG", timeStamp = TRUE, logName =
    "diagnosePlp Log"),
  saveDirectory = NULL
)
```

## Arguments

- databaseDetails:

  The database settings created using
  [`createDatabaseDetails()`](https://ohdsi.github.io/PatientLevelPrediction/dev/reference/createDatabaseDetails.md)

- modelDesignList:

  A list of model designs created using
  [`createModelDesign()`](https://ohdsi.github.io/PatientLevelPrediction/dev/reference/createModelDesign.md)

- cohortDefinitions:

  A list of cohort definitions for the target and outcome cohorts

- logSettings:

  The setting spexcifying the logging for the analyses created using
  [`createLogSettings()`](https://ohdsi.github.io/PatientLevelPrediction/dev/reference/createLogSettings.md)

- saveDirectory:

  Name of the folder where all the outputs will written to.

## Value

A data frame with the following columns:

|                    |                                                            |
|--------------------|------------------------------------------------------------|
| `analysisId`       | The unique identifier for a set of analysis choices.       |
| `targetId`         | The ID of the target cohort populations.                   |
| `outcomeId`        | The ID of the outcomeId.                                   |
| `dataLocation`     | The location where the plpData was saved                   |
| `the settings ids` | The ids for all other settings used for model development. |

## Details

This function will run all specified prediction design diagnoses.
