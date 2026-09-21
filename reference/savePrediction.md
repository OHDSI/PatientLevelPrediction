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
#> [1] "/tmp/RtmpaMoAgp/prediction.json"
dir(saveLoc)
#>  [1] "bslib-596ae0e61b03dfeeffb4bf83f997516c"
#>  [2] "downlit"                               
#>  [3] "file1ce111391bb4"                      
#>  [4] "file1ce112ed4688"                      
#>  [5] "file1ce1176dcac3.duckdb"               
#>  [6] "file1ce1176dcac3.duckdb.wal"           
#>  [7] "file1ce121e64569.duckdb"               
#>  [8] "file1ce121e64569.duckdb.wal"           
#>  [9] "file1ce139aff623.duckdb"               
#> [10] "file1ce139aff623.duckdb.wal"           
#> [11] "file1ce14854b40c"                      
#> [12] "file1ce14da7529.duckdb"                
#> [13] "file1ce14da7529.duckdb.wal"            
#> [14] "file1ce158ba65eb.duckdb"               
#> [15] "file1ce158ba65eb.duckdb.wal"           
#> [16] "file1ce15b305e69.duckdb"               
#> [17] "file1ce15b305e69.duckdb.wal"           
#> [18] "file1ce15bbf1a65.duckdb"               
#> [19] "file1ce15bbf1a65.duckdb.wal"           
#> [20] "file1ce16b760efd"                      
#> [21] "prediction.json"                       
#> [22] "temp_libpath1ce11709a21f"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
