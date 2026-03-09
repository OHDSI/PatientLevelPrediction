# Run a list of predictions analyses

Run a list of predictions analyses

## Usage

``` r
runMultiplePlp(
  databaseDetails = createDatabaseDetails(),
  modelDesignList = list(createModelDesign(targetId = 1, outcomeId = 2, modelSettings =
    setLassoLogisticRegression()), createModelDesign(targetId = 1, outcomeId = 3,
    modelSettings = setLassoLogisticRegression())),
  onlyFetchData = FALSE,
  skipDiagnostics = FALSE,
  cohortDefinitions = NULL,
  logSettings = createLogSettings(verbosity = "DEBUG", timeStamp = TRUE, logName =
    "runPlp Log"),
  saveDirectory = NULL,
  sqliteLocation = file.path(saveDirectory, "sqlite")
)
```

## Arguments

- databaseDetails:

  The database settings created using
  [`createDatabaseDetails()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createDatabaseDetails.md)

- modelDesignList:

  A list of model designs created using
  [`createModelDesign()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createModelDesign.md)

- onlyFetchData:

  Only fetches and saves the data object to the output folder without
  running the analysis.

- skipDiagnostics:

  Skip the diagnostics for speed if you just want the models to be
  developed and evaluated.

- cohortDefinitions:

  A list of cohort definitions for the target and outcome cohorts

- logSettings:

  The setting specifying the logging for the analyses created using
  [`createLogSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createLogSettings.md)

- saveDirectory:

  Name of the folder where all the outputs will written to.

- sqliteLocation:

  (optional) The location of the sqlite database with the results

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

This function will run all specified predictions as defined using .

## Examples
