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
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpGKyq1Y/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> Generating outcomes
saveLoc <- file.path(tempdir(), "loadPlpData")
savePlpData(plpData, saveLoc)
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpGKyq1Y/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpGKyq1Y/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
dir(saveLoc)
#> [1] "cohorts.rds"         "covariates"          "metaData.rds"       
#> [4] "outcomes.rds"        "simulationTruth.rds" "timeRef.rds"        
# clean up
unlink(saveLoc, recursive = TRUE)
```
