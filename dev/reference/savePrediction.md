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
#> [1] "/tmp/Rtmp6rBTNM/prediction.json"
dir(saveLoc)
#>  [1] "bslib-e2f5c5dd465040cad5f3efff813f21bc"
#>  [2] "downlit"                               
#>  [3] "file1e9d16a62025.duckdb"               
#>  [4] "file1e9d16a62025.duckdb.wal"           
#>  [5] "file1e9d1c9d1c6"                       
#>  [6] "file1e9d38f5afe.duckdb"                
#>  [7] "file1e9d38f5afe.duckdb.wal"            
#>  [8] "file1e9d4a525281.duckdb"               
#>  [9] "file1e9d4a525281.duckdb.wal"           
#> [10] "file1e9d4a91967f.duckdb"               
#> [11] "file1e9d4a91967f.duckdb.wal"           
#> [12] "file1e9d624633e9.duckdb"               
#> [13] "file1e9d624633e9.duckdb.wal"           
#> [14] "file1e9d6522c90a"                      
#> [15] "file1e9d65fa7632.duckdb"               
#> [16] "file1e9d65fa7632.duckdb.wal"           
#> [17] "file1e9d6a97fe5.duckdb"                
#> [18] "file1e9d72d1ca97.duckdb"               
#> [19] "file1e9d72d1ca97.duckdb.wal"           
#> [20] "file1e9d7598838a"                      
#> [21] "file1e9dc9aa36c"                       
#> [22] "prediction.json"                       
#> [23] "temp_libpath1e9d64edfe01"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
