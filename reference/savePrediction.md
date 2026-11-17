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
#> [1] "/tmp/RtmptkjC7L/prediction.json"
dir(saveLoc)
#>  [1] "bslib-3c9f20102fef19cec6d808200c837da2"
#>  [2] "downlit"                               
#>  [3] "duckdb"                                
#>  [4] "file1e3610494481.duckdb"               
#>  [5] "file1e3610494481.duckdb.wal"           
#>  [6] "file1e361cd9d.duckdb"                  
#>  [7] "file1e361cd9d.duckdb.wal"              
#>  [8] "file1e3646797a6e.duckdb"               
#>  [9] "file1e3646797a6e.duckdb.wal"           
#> [10] "file1e364c1b6d7d.duckdb"               
#> [11] "file1e364c1b6d7d.duckdb.wal"           
#> [12] "file1e3656801136"                      
#> [13] "file1e3663a50a2f"                      
#> [14] "file1e36662bace5"                      
#> [15] "file1e366981d49a"                      
#> [16] "prediction.json"                       
#> [17] "temp_libpath1e361512819b"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
