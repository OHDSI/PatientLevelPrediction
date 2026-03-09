# Generate simulated data

`simulateplpData` creates a plpData object with simulated data.

## Usage

``` r
simulatePlpData(plpDataSimulationProfile, n = 10000, seed = NULL)
```

## Arguments

- plpDataSimulationProfile:

  An object of type `plpDataSimulationProfile` as generated using the  
  `createplpDataSimulationProfile` function.

- n:

  The size of the population to be generated.

- seed:

  An optional seed for the random number generator. If provided

## Value

An object of type `plpData`.

## Details

This function generates simulated data that is in many ways similar to
the original data on which the simulation profile is based.

## Examples

``` r
# first load the simulation profile to use
data("simulationProfile")
# then generate the simulated data
plpData <- simulatePlpData(simulationProfile, n = 100, seed = 42)
#> Generating covariates
#> Generating cohorts
#> Generating outcomes
nrow(plpData$cohorts)
#> [1] 100
```
