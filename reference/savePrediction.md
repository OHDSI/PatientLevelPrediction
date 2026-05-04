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
#> [1] "/tmp/RtmpTkIGVn/prediction.json"
dir(saveLoc)
#>  [1] "bslib-96e575f327515dbfeed2e31a38955eab"
#>  [2] "downlit"                               
#>  [3] "file1ea6175093fa"                      
#>  [4] "file1ea619f347dd"                      
#>  [5] "file1ea6224b30f3.duckdb"               
#>  [6] "file1ea6224b30f3.duckdb.wal"           
#>  [7] "file1ea624a1bebc"                      
#>  [8] "file1ea6346f613e.duckdb"               
#>  [9] "file1ea65ca6250"                       
#> [10] "file1ea66a589dbc.duckdb"               
#> [11] "file1ea66a589dbc.duckdb.wal"           
#> [12] "file1ea66a7fa937.duckdb"               
#> [13] "file1ea66a7fa937.duckdb.wal"           
#> [14] "prediction.json"                       
#> [15] "temp_libpath1ea6229aefcc"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
