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
#> [1] "/tmp/RtmpRfhodk/prediction.json"
dir(saveLoc)
#>  [1] "bslib-2639a81f5f9cc8217c1e54b2e8333794"
#>  [2] "downlit"                               
#>  [3] "file1ecb14d700a3"                      
#>  [4] "file1ecb180d057a.duckdb"               
#>  [5] "file1ecb180d057a.duckdb.wal"           
#>  [6] "file1ecb1e5507c1.duckdb"               
#>  [7] "file1ecb1e5507c1.duckdb.wal"           
#>  [8] "file1ecb2ff98af5.duckdb"               
#>  [9] "file1ecb2ff98af5.duckdb.wal"           
#> [10] "file1ecb387e5f9.duckdb"                
#> [11] "file1ecb387e5f9.duckdb.wal"            
#> [12] "file1ecb443714ee.duckdb"               
#> [13] "file1ecb443714ee.duckdb.wal"           
#> [14] "file1ecb4a3884ef.duckdb"               
#> [15] "file1ecb4a48da82"                      
#> [16] "file1ecb5a72afe6.duckdb"               
#> [17] "file1ecb5a72afe6.duckdb.wal"           
#> [18] "file1ecb6609c229.duckdb"               
#> [19] "file1ecb6609c229.duckdb.wal"           
#> [20] "file1ecb7d210e43"                      
#> [21] "file1ecb7d2a8e2e"                      
#> [22] "prediction.json"                       
#> [23] "temp_libpath1ecb6b8f2b79"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
