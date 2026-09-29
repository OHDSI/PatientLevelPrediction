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
#> [1] "/tmp/RtmpZhLLjC/prediction.json"
dir(saveLoc)
#>  [1] "bslib-596ae0e61b03dfeeffb4bf83f997516c"
#>  [2] "downlit"                               
#>  [3] "file1e0f28486bf0"                      
#>  [4] "file1e0f29c4bdb3"                      
#>  [5] "file1e0f2c3452f3.duckdb"               
#>  [6] "file1e0f2c3452f3.duckdb.wal"           
#>  [7] "file1e0f34979d15.duckdb"               
#>  [8] "file1e0f34979d15.duckdb.wal"           
#>  [9] "file1e0f4212a037"                      
#> [10] "file1e0f4b307d61.duckdb"               
#> [11] "file1e0f4b307d61.duckdb.wal"           
#> [12] "file1e0f50edf209.duckdb"               
#> [13] "file1e0f50edf209.duckdb.wal"           
#> [14] "file1e0f6c947f31.duckdb"               
#> [15] "file1e0f6c947f31.duckdb.wal"           
#> [16] "file1e0f6cbabbb0.duckdb"               
#> [17] "file1e0f6cbabbb0.duckdb.wal"           
#> [18] "file1e0f7d93a8d7.duckdb"               
#> [19] "file1e0f7d93a8d7.duckdb.wal"           
#> [20] "file1e0f7f28e745"                      
#> [21] "prediction.json"                       
#> [22] "temp_libpath1e0f76bd4f8e"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
