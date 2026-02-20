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
#> [1] "/tmp/RtmpVJNngQ/prediction.json"
dir(saveLoc)
#>  [1] "bslib-e2f5c5dd465040cad5f3efff813f21bc"
#>  [2] "downlit"                               
#>  [3] "file2111160ee141.duckdb"               
#>  [4] "file2111160ee141.duckdb.wal"           
#>  [5] "file21111c59e4e3.duckdb"               
#>  [6] "file21111c59e4e3.duckdb.wal"           
#>  [7] "file211132274105"                      
#>  [8] "file211153af1869.duckdb"               
#>  [9] "file21115622773d.duckdb"               
#> [10] "file21115622773d.duckdb.wal"           
#> [11] "file21115b44b456"                      
#> [12] "file21116664599e.duckdb"               
#> [13] "file21116664599e.duckdb.wal"           
#> [14] "file211168a00828"                      
#> [15] "file211172af436d.duckdb"               
#> [16] "file211172af436d.duckdb.wal"           
#> [17] "file211172e59289"                      
#> [18] "file2111d1f01cc.duckdb"                
#> [19] "file2111d1f01cc.duckdb.wal"            
#> [20] "file2111d857fd8.duckdb"                
#> [21] "file2111d857fd8.duckdb.wal"            
#> [22] "prediction.json"                       
#> [23] "temp_libpath21112c0af908"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
