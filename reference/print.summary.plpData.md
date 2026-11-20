# Print a summary.plpData object

Print a summary.plpData object

## Usage

``` r
# S3 method for class 'summary.plpData'
print(x, ...)
```

## Arguments

- x:

  The summary.plpData object to print

- ...:

  Additional arguments

## Value

A message describing the object

## Examples

``` r
data("simulationProfile")
plpData <- simulatePlpData(simulationProfile, n = 10, seed = 42)
#> Generating covariates
#> Generating cohorts
#> Generating outcomes
summary <- summary(plpData)
print(summary)
#> plpData object summary
#> 
#> At risk cohort concept ID: 1
#> Outcome concept ID(s): 3
#> 
#> People: 10
#> 
#> Outcome counts:
#>   Event count Person count
#> 3           5            5
#> 
#> Covariates:
#> Number of covariates: 98
#> Number of non-zero covariate values: 44
```
