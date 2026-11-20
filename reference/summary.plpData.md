# Summarize a plpData object

Summarize a plpData object

## Usage

``` r
# S3 method for class 'plpData'
summary(object, ...)
```

## Arguments

- object:

  The plpData object to summarize

- ...:

  Additional arguments

## Value

A summary of the object containing the number of people, outcomes and
covariates

## Examples

``` r
data("simulationProfile")
plpData <- simulatePlpData(simulationProfile, n = 10, seed = 42)
#> Generating covariates
#> Generating cohorts
#> Generating outcomes
summary(plpData)
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
