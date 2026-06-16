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
#> [1] "/tmp/RtmpXReRkl/prediction.json"
dir(saveLoc)
#>  [1] "bslib-2639a81f5f9cc8217c1e54b2e8333794"
#>  [2] "downlit"                               
#>  [3] "file1e732496f4fb"                      
#>  [4] "file1e732c77a60a.duckdb"               
#>  [5] "file1e732ff8be42"                      
#>  [6] "file1e733639d25c.duckdb"               
#>  [7] "file1e733639d25c.duckdb.wal"           
#>  [8] "file1e734068556e.duckdb"               
#>  [9] "file1e7343e4c717"                      
#> [10] "file1e7344b89e51.duckdb"               
#> [11] "file1e73488e9b68.duckdb"               
#> [12] "file1e734f0b4c14.duckdb"               
#> [13] "file1e734f0b4c14.duckdb.wal"           
#> [14] "file1e73629773.duckdb"                 
#> [15] "file1e73629773.duckdb.wal"             
#> [16] "file1e73663f7b20.duckdb"               
#> [17] "file1e73663f7b20.duckdb.wal"           
#> [18] "file1e737432824.duckdb"                
#> [19] "file1e737432824.duckdb.wal"            
#> [20] "file1e737a605aef.duckdb"               
#> [21] "file1e737a605aef.duckdb.wal"           
#> [22] "file1e737f50567.duckdb"                
#> [23] "file1e7387112e8"                       
#> [24] "prediction.json"                       
#> [25] "temp_libpath1e7348cdd67a"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
