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
#> [1] "/tmp/RtmpsfwXou/prediction.json"
dir(saveLoc)
#>  [1] "bslib-2639a81f5f9cc8217c1e54b2e8333794"
#>  [2] "downlit"                               
#>  [3] "file1f8614f49fa5.duckdb"               
#>  [4] "file1f8614f49fa5.duckdb.wal"           
#>  [5] "file1f86175f9b8d"                      
#>  [6] "file1f8627e0f1b4.duckdb"               
#>  [7] "file1f8627e0f1b4.duckdb.wal"           
#>  [8] "file1f8646526429"                      
#>  [9] "file1f8649fea8d4"                      
#> [10] "file1f867c29cf07"                      
#> [11] "prediction.json"                       
#> [12] "temp_libpath1f862c9f0e2f"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
