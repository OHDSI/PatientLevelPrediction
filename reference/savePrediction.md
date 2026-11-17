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
#> [1] "/tmp/RtmpqsNDM6/prediction.json"
dir(saveLoc)
#>  [1] "bslib-3c9f20102fef19cec6d808200c837da2"
#>  [2] "downlit"                               
#>  [3] "duckdb"                                
#>  [4] "file1c101f86e795"                      
#>  [5] "file1c10325e99d3"                      
#>  [6] "file1c1036d44aed.duckdb"               
#>  [7] "file1c1036d44aed.duckdb.wal"           
#>  [8] "file1c10438d062c.duckdb"               
#>  [9] "file1c105c9093ba"                      
#> [10] "file1c105f37e6bb.duckdb"               
#> [11] "file1c105f37e6bb.duckdb.wal"           
#> [12] "file1c10647a3fc1.duckdb"               
#> [13] "file1c10647a3fc1.duckdb.wal"           
#> [14] "file1c106a68bd2e"                      
#> [15] "prediction.json"                       
#> [16] "temp_libpath1c103f20d6e9"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
