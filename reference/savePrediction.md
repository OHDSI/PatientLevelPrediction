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
#> [1] "/tmp/Rtmp8wLpGq/prediction.json"
dir(saveLoc)
#>  [1] "bslib-596ae0e61b03dfeeffb4bf83f997516c"
#>  [2] "downlit"                               
#>  [3] "file1c1116de934d.duckdb"               
#>  [4] "file1c1116de934d.duckdb.wal"           
#>  [5] "file1c1138508162.duckdb"               
#>  [6] "file1c1138508162.duckdb.wal"           
#>  [7] "file1c114020d7e7.duckdb"               
#>  [8] "file1c114020d7e7.duckdb.wal"           
#>  [9] "file1c114866a69c"                      
#> [10] "file1c114ae8083e.duckdb"               
#> [11] "file1c114ae8083e.duckdb.wal"           
#> [12] "file1c114b4ff093"                      
#> [13] "file1c114b733f59.duckdb"               
#> [14] "file1c114b733f59.duckdb.wal"           
#> [15] "file1c1151660018.duckdb"               
#> [16] "file1c1151660018.duckdb.wal"           
#> [17] "file1c11568ca141.duckdb"               
#> [18] "file1c11568ca141.duckdb.wal"           
#> [19] "file1c1176d173b9"                      
#> [20] "file1c11e81317d"                       
#> [21] "prediction.json"                       
#> [22] "temp_libpath1c11756a9fee"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
