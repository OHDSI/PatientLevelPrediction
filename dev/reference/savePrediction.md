# Saves the prediction dataframe to a json file

Saves the prediction dataframe to a json file

## Usage

``` r
savePrediction(prediction, dirPath, fileName = "prediction.json")
```

## Arguments

- prediction:

  The prediciton data.frame

- dirPath:

  The directory to save the prediction json

- fileName:

  The name of the json file that will be saved

## Value

                           The file location where the prediction was saved

## Details

Saves the prediction data frame returned by predict.R to an json file
and returns the fileLocation where the prediction is saved

## Examples

``` r
prediction <- data.frame(
  rowIds = c(1, 2, 3),
  outcomeCount = c(0, 1, 0),
  value = c(0.1, 0.9, 0.2)
)
saveLoc <- file.path(tempdir())
savePrediction(prediction, saveLoc)
#> [1] "/tmp/RtmptH6Gkj/prediction.json"
dir(saveLoc)
#>  [1] "bslib-e2f5c5dd465040cad5f3efff813f21bc"
#>  [2] "downlit"                               
#>  [3] "file22d0109bca74.duckdb"               
#>  [4] "file22d0109bca74.duckdb.wal"           
#>  [5] "file22d029ca6fdc.duckdb"               
#>  [6] "file22d029ca6fdc.duckdb.wal"           
#>  [7] "file22d044c5ec38.duckdb"               
#>  [8] "file22d044c5ec38.duckdb.wal"           
#>  [9] "file22d04e95e60a.duckdb"               
#> [10] "file22d04e95e60a.duckdb.wal"           
#> [11] "file22d05281d586.duckdb"               
#> [12] "file22d05281d586.duckdb.wal"           
#> [13] "file22d05799d141"                      
#> [14] "file22d0586beb50.duckdb"               
#> [15] "file22d0586beb50.duckdb.wal"           
#> [16] "file22d060d3163d"                      
#> [17] "file22d06585bd36.duckdb"               
#> [18] "file22d06585bd36.duckdb.wal"           
#> [19] "file22d06fa8e9bb"                      
#> [20] "file22d0785e25df"                      
#> [21] "file22d07e6706e8.duckdb"               
#> [22] "prediction.json"                       
#> [23] "temp_libpath22d06e0f27b5"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
