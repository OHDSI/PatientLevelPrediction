# validateExternal - Validate model performance on new data

validateExternal - Validate model performance on new data

## Usage

``` r
validateExternal(
  validationDesignList,
  databaseDetails,
  logSettings = createLogSettings(verbosity = "INFO", logName = "validatePLP"),
  outputFolder,
  cohortDefinitions = NULL
)
```

## Arguments

- validationDesignList:

  A list of objects created with `createValidationDesign`

- databaseDetails:

  A list of objects of class `databaseDetails` created using
  `createDatabaseDetails`

- logSettings:

  An object of `logSettings` created using `createLogSettings`

- outputFolder:

  The directory to save the validation results to

- cohortDefinitions:

  A cohortDefinitionSet object created with `CohortGenerator`
  (subfolders are created per database in validationDatabaseDetails)

## Value

A list of results

## Examples
