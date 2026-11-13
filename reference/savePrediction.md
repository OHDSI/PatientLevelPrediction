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
#> [1] "/tmp/Rtmpf7DTHg/prediction.json"
dir(saveLoc)
#>  [1] "bslib-3c9f20102fef19cec6d808200c837da2"
#>  [2] "downlit"                               
#>  [3] "duckdb"                                
#>  [4] "file1cd01fb6b7e8.duckdb"               
#>  [5] "file1cd01fb6b7e8.duckdb.wal"           
#>  [6] "file1cd03800b40f"                      
#>  [7] "file1cd05cb61300"                      
#>  [8] "file1cd0613e28b9"                      
#>  [9] "file1cd06351719d.duckdb"               
#> [10] "file1cd06351719d.duckdb.wal"           
#> [11] "file1cd073e84479"                      
#> [12] "file1cd07a2b5947.duckdb"               
#> [13] "file1cd07a2b5947.duckdb.wal"           
#> [14] "file1cd0c15fb0f.duckdb"                
#> [15] "file1cd0c15fb0f.duckdb.wal"            
#> [16] "prediction.json"                       
#> [17] "temp_libpath1cd031b1a5ef"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
