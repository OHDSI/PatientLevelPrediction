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
#> [1] "/tmp/RtmpGx3hts/prediction.json"
dir(saveLoc)
#>  [1] "bslib-3c9f20102fef19cec6d808200c837da2"
#>  [2] "downlit"                               
#>  [3] "duckdb"                                
#>  [4] "file2c8113711ca1.duckdb"               
#>  [5] "file2c8113711ca1.duckdb.wal"           
#>  [6] "file2c814e55182c.duckdb"               
#>  [7] "file2c814e55182c.duckdb.wal"           
#>  [8] "file2c815014dfa0"                      
#>  [9] "file2c81549d9feb"                      
#> [10] "file2c815627050f"                      
#> [11] "file2c816d297b3b.duckdb"               
#> [12] "file2c816d297b3b.duckdb.wal"           
#> [13] "file2c816fd2f72c.duckdb"               
#> [14] "file2c816fd2f72c.duckdb.wal"           
#> [15] "file2c817db27c6c"                      
#> [16] "prediction.json"                       
#> [17] "temp_libpath2c816bb94a84"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
