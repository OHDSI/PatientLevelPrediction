# Save the plpData to folder

`savePlpData` saves an object of type plpData to folder.

## Usage

``` r
savePlpData(plpData, file, envir = NULL, overwrite = FALSE)
```

## Arguments

- plpData:

  An object of type `plpData` as generated using `getPlpData`.

- file:

  The name of the folder where the data will be written. The folder
  should not yet exist.

- envir:

  The environment for to evaluate variables when saving

- overwrite:

  Whether to force overwrite an existing file

## Value

Called for its side effect, the data will be written to a set of files
in the folder specified by the user.

## Examples

``` r
data("simulationProfile")
plpData <- simulatePlpData(simulationProfile, n = 500, seed = 42)
#> Generating covariates
#> Generating cohorts
#> Generating outcomes
saveLoc <- file.path(tempdir(), "savePlpData")
savePlpData(plpData, saveLoc)
dir(saveLoc, full.names = TRUE)
#> [1] "/tmp/RtmpRfhodk/savePlpData/cohorts.rds"        
#> [2] "/tmp/RtmpRfhodk/savePlpData/covariates"         
#> [3] "/tmp/RtmpRfhodk/savePlpData/metaData.rds"       
#> [4] "/tmp/RtmpRfhodk/savePlpData/outcomes.rds"       
#> [5] "/tmp/RtmpRfhodk/savePlpData/simulationTruth.rds"
#> [6] "/tmp/RtmpRfhodk/savePlpData/timeRef.rds"        

# clean up
unlink(saveLoc, recursive = TRUE)
```
