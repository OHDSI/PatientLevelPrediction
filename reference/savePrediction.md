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
#> [1] "/tmp/Rtmpvtv7Kb/prediction.json"
dir(saveLoc)
#>  [1] "bslib-3c9f20102fef19cec6d808200c837da2"
#>  [2] "downlit"                               
#>  [3] "duckdb"                                
#>  [4] "file1cef2c32c977"                      
#>  [5] "file1cef34b3a91b"                      
#>  [6] "file1cef3ae31308.duckdb"               
#>  [7] "file1cef3ae31308.duckdb.wal"           
#>  [8] "file1cef43211545"                      
#>  [9] "file1cef55188e4a.duckdb"               
#> [10] "file1cef55188e4a.duckdb.wal"           
#> [11] "file1cef663881e3.duckdb"               
#> [12] "file1cef663881e3.duckdb.wal"           
#> [13] "file1cef7bf1a2ac"                      
#> [14] "file1cefa67d949.duckdb"                
#> [15] "prediction.json"                       
#> [16] "temp_libpath1cef2c91d65b"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
