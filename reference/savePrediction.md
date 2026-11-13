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
#> [1] "/tmp/RtmpGKyq1Y/prediction.json"
dir(saveLoc)
#>  [1] "bslib-3c9f20102fef19cec6d808200c837da2"
#>  [2] "downlit"                               
#>  [3] "duckdb"                                
#>  [4] "file1c1e18a448d4.duckdb"               
#>  [5] "file1c1e18a448d4.duckdb.wal"           
#>  [6] "file1c1e1df72526.duckdb"               
#>  [7] "file1c1e1df72526.duckdb.wal"           
#>  [8] "file1c1e24d29cfb.duckdb"               
#>  [9] "file1c1e24d29cfb.duckdb.wal"           
#> [10] "file1c1e2c18fd2"                       
#> [11] "file1c1e421d688"                       
#> [12] "file1c1e516e910e"                      
#> [13] "file1c1e51954f72"                      
#> [14] "prediction.json"                       
#> [15] "temp_libpath1c1e5c34da67"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
