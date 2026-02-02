# Load the plpData from a folder

`loadPlpData` loads an object of type plpData from a folder in the file
system.

## Usage

``` r
loadPlpData(file, readOnly = TRUE)
```

## Arguments

- file:

  The name of the folder containing the data.

- readOnly:

  If true, the data is opened read only.

## Value

An object of class plpData.

## Details

The data will be written to a set of files in the folder specified by
the user.

## Examples

``` r
data("simulationProfile")
plpData <- simulatePlpData(simulationProfile, n = 500, seed = 42)
#> Generating covariates
#> Generating cohorts
#> Generating outcomes
saveLoc <- file.path(tempdir(), "loadPlpData")
savePlpData(plpData, saveLoc)
dir(saveLoc)
#> [1] "cohorts.rds"  "covariates"   "metaData.rds" "outcomes.rds" "timeRef.rds" 
# clean up
unlink(saveLoc, recursive = TRUE)
```
