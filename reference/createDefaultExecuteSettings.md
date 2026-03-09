# Creates default list of settings specifying what parts of runPlp to execute

Creates default list of settings specifying what parts of runPlp to
execute

## Usage

``` r
createDefaultExecuteSettings()
```

## Value

list with TRUE for split, preprocess, model development and covariate
summary

## Details

runs split, preprocess, model development and covariate summary

## Examples

``` r
createDefaultExecuteSettings()
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
#> [1] TRUE
#> 
#> $runModelDevelopment
#> [1] TRUE
#> 
#> $runCovariateSummary
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "executeSettings"
```
