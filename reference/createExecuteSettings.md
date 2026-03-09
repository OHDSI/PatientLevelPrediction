# Creates list of settings specifying what parts of runPlp to execute

Creates list of settings specifying what parts of runPlp to execute

## Usage

``` r
createExecuteSettings(
  runSplitData = FALSE,
  runSampleData = FALSE,
  runFeatureEngineering = FALSE,
  runPreprocessData = FALSE,
  runModelDevelopment = FALSE,
  runCovariateSummary = FALSE
)
```

## Arguments

- runSplitData:

  TRUE or FALSE whether to split data into train/test

- runSampleData:

  TRUE or FALSE whether to over or under sample

- runFeatureEngineering:

  TRUE or FALSE whether to do feature engineering

- runPreprocessData:

  TRUE or FALSE whether to do preprocessing

- runModelDevelopment:

  TRUE or FALSE whether to develop the model

- runCovariateSummary:

  TRUE or FALSE whether to create covariate summary

## Value

list with TRUE/FALSE for each part of runPlp

## Details

define what parts of runPlp to execute

## Examples

``` r
# create settings with only split and model development
createExecuteSettings(runSplitData = TRUE, runModelDevelopment = TRUE) 
#> $runSplitData
#> [1] TRUE
#> 
#> $runSampleData
#> [1] FALSE
#> 
#> $runFeatureEngineering
#> [1] FALSE
#> 
#> $runPreprocessData
#> [1] FALSE
#> 
#> $runModelDevelopment
#> [1] TRUE
#> 
#> $runCovariateSummary
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "executeSettings"
```
