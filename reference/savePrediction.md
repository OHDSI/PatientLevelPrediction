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
#> [1] "/tmp/Rtmp0xpFtU/prediction.json"
dir(saveLoc)
#>  [1] "bslib-2639a81f5f9cc8217c1e54b2e8333794"
#>  [2] "downlit"                               
#>  [3] "file1f1a11fe79c3.duckdb"               
#>  [4] "file1f1a13c83ca8"                      
#>  [5] "file1f1a1431f008"                      
#>  [6] "file1f1a19514f44"                      
#>  [7] "file1f1a1f161a56.duckdb"               
#>  [8] "file1f1a1f161a56.duckdb.wal"           
#>  [9] "file1f1a24d18ee1.duckdb"               
#> [10] "file1f1a24d18ee1.duckdb.wal"           
#> [11] "file1f1a2b61828e.duckdb"               
#> [12] "file1f1a2b61828e.duckdb.wal"           
#> [13] "file1f1a3756c6c3.duckdb"               
#> [14] "file1f1a3756c6c3.duckdb.wal"           
#> [15] "file1f1a38bb521b.duckdb"               
#> [16] "file1f1a38bb521b.duckdb.wal"           
#> [17] "file1f1a61ddefb0.duckdb"               
#> [18] "file1f1a79eafa36.duckdb"               
#> [19] "file1f1a79eafa36.duckdb.wal"           
#> [20] "file1f1a7e54948a"                      
#> [21] "file1f1ab7cac3f.duckdb"                
#> [22] "file1f1ab7cac3f.duckdb.wal"            
#> [23] "prediction.json"                       
#> [24] "temp_libpath1f1adafad64"               

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
