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
#> [1] "/tmp/RtmpAqRl34/prediction.json"
dir(saveLoc)
#>  [1] "bslib-e2f5c5dd465040cad5f3efff813f21bc"
#>  [2] "downlit"                               
#>  [3] "file1e7d124e0e59.duckdb"               
#>  [4] "file1e7d124e0e59.duckdb.wal"           
#>  [5] "file1e7d209f413c.duckdb"               
#>  [6] "file1e7d209f413c.duckdb.wal"           
#>  [7] "file1e7d244696c8"                      
#>  [8] "file1e7d263fc68d.duckdb"               
#>  [9] "file1e7d263fc68d.duckdb.wal"           
#> [10] "file1e7d3d614917.duckdb"               
#> [11] "file1e7d3d614917.duckdb.wal"           
#> [12] "file1e7d416601fc"                      
#> [13] "file1e7d4211539a.duckdb"               
#> [14] "file1e7d4dfcaec7.duckdb"               
#> [15] "file1e7d4dfcaec7.duckdb.wal"           
#> [16] "file1e7d6318adcc.duckdb"               
#> [17] "file1e7d6318adcc.duckdb.wal"           
#> [18] "file1e7d69f4ca3d.duckdb"               
#> [19] "file1e7d69f4ca3d.duckdb.wal"           
#> [20] "file1e7daef2e65"                       
#> [21] "file1e7db02dcd9.duckdb"                
#> [22] "file1e7db02dcd9.duckdb.wal"            
#> [23] "file1e7dc556e3d"                       
#> [24] "prediction.json"                       
#> [25] "temp_libpath1e7d219a6584"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
