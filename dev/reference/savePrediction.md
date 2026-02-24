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
#> [1] "/tmp/Rtmp1RpNip/prediction.json"
dir(saveLoc)
#>  [1] "bslib-e2f5c5dd465040cad5f3efff813f21bc"
#>  [2] "downlit"                               
#>  [3] "file21111c64cb7.duckdb"                
#>  [4] "file21111c64cb7.duckdb.wal"            
#>  [5] "file2111235302b"                       
#>  [6] "file21112c699438.duckdb"               
#>  [7] "file21113410ede.duckdb"                
#>  [8] "file21113410ede.duckdb.wal"            
#>  [9] "file211135dc0c64"                      
#> [10] "file2111388c1033.duckdb"               
#> [11] "file2111388c1033.duckdb.wal"           
#> [12] "file211140be7a2f.duckdb"               
#> [13] "file211140be7a2f.duckdb.wal"           
#> [14] "file211145238680.duckdb"               
#> [15] "file211145238680.duckdb.wal"           
#> [16] "file21114f71540a"                      
#> [17] "file211157dd49c2"                      
#> [18] "file21115b5ce7b9.duckdb"               
#> [19] "file21115b5ce7b9.duckdb.wal"           
#> [20] "file21115d66cf5c.duckdb"               
#> [21] "file21115d66cf5c.duckdb.wal"           
#> [22] "prediction.json"                       
#> [23] "temp_libpath211140adee14"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
