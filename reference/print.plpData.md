# Print a plpData object

Print a plpData object

## Usage

``` r
# S3 method for class 'plpData'
print(x, ...)
```

## Arguments

- x:

  The plpData object to print

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
print(plpData)
#> plpData object
#> 
#> At risk concept ID: 1
#> Outcome concept ID(s): 3
```
