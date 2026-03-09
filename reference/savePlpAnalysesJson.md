# Save the modelDesignList to a json file

Save the modelDesignList to a json file

## Usage

``` r
savePlpAnalysesJson(
  modelDesignList = list(createModelDesign(targetId = 1, outcomeId = 2, modelSettings =
    setLassoLogisticRegression()), createModelDesign(targetId = 1, outcomeId = 3,
    modelSettings = setLassoLogisticRegression())),
  cohortDefinitions = NULL,
  saveDirectory = NULL
)
```

## Arguments

- modelDesignList:

  A list of modelDesigns created using
  [`createModelDesign()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createModelDesign.md)

- cohortDefinitions:

  A list of the cohortDefinitions (generally extracted from ATLAS)

- saveDirectory:

  The directory to save the modelDesignList settings

## Value

The json string of the ModelDesignList

## Details

This function creates a json file with the modelDesignList saved

## Examples

``` r
modelDesign <- createModelDesign(targetId = 1, 
                                 outcomeId = 2,
                                 modelSettings = setLassoLogisticRegression())
saveLoc <- file.path(tempdir(), "loadPlpAnalysesJson")
jsonFile <- savePlpAnalysesJson(modelDesignList = modelDesign, saveDirectory = saveLoc)
# clean up
unlink(saveLoc, recursive = TRUE)
```
