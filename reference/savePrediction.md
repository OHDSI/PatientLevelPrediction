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
#> [1] "/tmp/RtmppeeqHG/prediction.json"
dir(saveLoc)
#>  [1] "bslib-75c6fa7174b435157de4484d8e348aad"
#>  [2] "downlit"                               
#>  [3] "duckdb"                                
#>  [4] "file1f6119566da5"                      
#>  [5] "file1f611aede891.duckdb"               
#>  [6] "file1f611aede891.duckdb.wal"           
#>  [7] "file1f61263f7e6f.duckdb"               
#>  [8] "file1f61263f7e6f.duckdb.wal"           
#>  [9] "file1f614068ef73"                      
#> [10] "file1f6143a1a948.duckdb"               
#> [11] "file1f6143a1a948.duckdb.wal"           
#> [12] "file1f6143b8dc12.duckdb"               
#> [13] "file1f6143b8dc12.duckdb.wal"           
#> [14] "file1f61443ab32e"                      
#> [15] "file1f615043be32.duckdb"               
#> [16] "file1f615043be32.duckdb.wal"           
#> [17] "file1f616d1fd352"                      
#> [18] "prediction.json"                       
#> [19] "temp_libpath1f6165b40920"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
