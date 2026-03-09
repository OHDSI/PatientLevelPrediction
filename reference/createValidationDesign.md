# createValidationDesign - Define the validation design for external validation

createValidationDesign - Define the validation design for external
validation

## Usage

``` r
createValidationDesign(
  targetId,
  outcomeId,
  populationSettings = NULL,
  restrictPlpDataSettings = NULL,
  plpModelList,
  recalibrate = NULL,
  runCovariateSummary = TRUE
)
```

## Arguments

- targetId:

  The targetId of the target cohort to validate on

- outcomeId:

  The outcomeId of the outcome cohort to validate on

- populationSettings:

  A list of population restriction settings created by
  `createPopulationSettings`. Default is NULL and then this is taken
  from the model

- restrictPlpDataSettings:

  A list of plpData restriction settings created by
  `createRestrictPlpDataSettings`. Default is NULL and then this is
  taken from the model.

- plpModelList:

  A list of plpModels objects created by `runPlp` or a path to such
  objects

- recalibrate:

  A vector of characters specifying the recalibration method to apply,

- runCovariateSummary:

  whether to run the covariate summary for the validation data

## Value

A validation design object of class `validationDesign` or a list of such
objects

## Examples

``` r
# create a validation design for targetId 1 and outcomeId 2 one l1 model and 
# one gradient boosting model
createValidationDesign(1, 2, plpModelList = list(
"pathToL1model", "PathToGBMModel"))
#> $targetId
#> [1] 1
#> 
#> $outcomeId
#> [1] 2
#> 
#> $populationSettings
#> NULL
#> 
#> $plpModelList
#> $plpModelList[[1]]
#> [1] "pathToL1model"
#> 
#> $plpModelList[[2]]
#> [1] "PathToGBMModel"
#> 
#> 
#> $restrictPlpDataSettings
#> NULL
#> 
#> $recalibrate
#> NULL
#> 
#> $runCovariateSummary
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "validationDesign"
```
