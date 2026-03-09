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
#> [1] "/tmp/Rtmp3qoST7/prediction.json"
dir(saveLoc)
#>  [1] "bslib-e2f5c5dd465040cad5f3efff813f21bc"
#>  [2] "downlit"                               
#>  [3] "file1eac16946da4.duckdb"               
#>  [4] "file1eac16946da4.duckdb.wal"           
#>  [5] "file1eac1b658789.duckdb"               
#>  [6] "file1eac1b658789.duckdb.wal"           
#>  [7] "file1eac284eece3.duckdb"               
#>  [8] "file1eac284eece3.duckdb.wal"           
#>  [9] "file1eac2a2e3a75.duckdb"               
#> [10] "file1eac2a2e3a75.duckdb.wal"           
#> [11] "file1eac33209d25"                      
#> [12] "file1eac36bd67f8.duckdb"               
#> [13] "file1eac3a1155b4.duckdb"               
#> [14] "file1eac3a1155b4.duckdb.wal"           
#> [15] "file1eac3fcaa48c"                      
#> [16] "file1eac4c1f936e.duckdb"               
#> [17] "file1eac4c1f936e.duckdb.wal"           
#> [18] "file1eac64c941b3"                      
#> [19] "file1eac74249834"                      
#> [20] "file1eac7f610216.duckdb"               
#> [21] "file1eac7f610216.duckdb.wal"           
#> [22] "prediction.json"                       
#> [23] "temp_libpath1eac10ba7f3"               

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
