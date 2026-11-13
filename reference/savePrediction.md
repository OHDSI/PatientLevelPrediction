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
#> [1] "/tmp/Rtmp9ANmSu/prediction.json"
dir(saveLoc)
#>  [1] "bslib-3c9f20102fef19cec6d808200c837da2"
#>  [2] "downlit"                               
#>  [3] "duckdb"                                
#>  [4] "file1f62202aee13"                      
#>  [5] "file1f62374562bb"                      
#>  [6] "file1f6250cba551"                      
#>  [7] "file1f62564db733.duckdb"               
#>  [8] "file1f62564db733.duckdb.wal"           
#>  [9] "file1f625e88418a.duckdb"               
#> [10] "file1f625e88418a.duckdb.wal"           
#> [11] "file1f626dd8cbff.duckdb"               
#> [12] "file1f626dd8cbff.duckdb.wal"           
#> [13] "file1f627a6c8dd2"                      
#> [14] "prediction.json"                       
#> [15] "temp_libpath1f623b8e0f2b"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
