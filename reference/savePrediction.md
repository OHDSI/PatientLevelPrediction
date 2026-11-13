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
#> [1] "/tmp/RtmpDU7niT/prediction.json"
dir(saveLoc)
#>  [1] "bslib-3c9f20102fef19cec6d808200c837da2"
#>  [2] "downlit"                               
#>  [3] "duckdb"                                
#>  [4] "file2c6e117c609b.duckdb"               
#>  [5] "file2c6e117c609b.duckdb.wal"           
#>  [6] "file2c6e143061a9.duckdb"               
#>  [7] "file2c6e143061a9.duckdb.wal"           
#>  [8] "file2c6e2888d910"                      
#>  [9] "file2c6e46661f81.duckdb"               
#> [10] "file2c6e46661f81.duckdb.wal"           
#> [11] "file2c6e5cdd8da4"                      
#> [12] "file2c6e640cbeb9"                      
#> [13] "file2c6e77d37133"                      
#> [14] "prediction.json"                       
#> [15] "temp_libpath2c6e255d5543"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
