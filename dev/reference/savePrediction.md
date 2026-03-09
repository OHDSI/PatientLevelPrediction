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
#> [1] "/tmp/Rtmpdi1Kvw/prediction.json"
dir(saveLoc)
#>  [1] "bslib-e2f5c5dd465040cad5f3efff813f21bc"
#>  [2] "downlit"                               
#>  [3] "file1f292621d9ab.duckdb"               
#>  [4] "file1f292621d9ab.duckdb.wal"           
#>  [5] "file1f293a18e0ee"                      
#>  [6] "file1f2949dcc379.duckdb"               
#>  [7] "file1f2949dcc379.duckdb.wal"           
#>  [8] "file1f294c64abba"                      
#>  [9] "file1f294f0a0ab5"                      
#> [10] "file1f2958709fcc.duckdb"               
#> [11] "file1f2958709fcc.duckdb.wal"           
#> [12] "file1f2967173448.duckdb"               
#> [13] "file1f2969beb00"                       
#> [14] "file1f2975426ea8.duckdb"               
#> [15] "file1f2975426ea8.duckdb.wal"           
#> [16] "file1f29774fdd99.duckdb"               
#> [17] "file1f29774fdd99.duckdb.wal"           
#> [18] "file1f29c356e84.duckdb"                
#> [19] "file1f29c356e84.duckdb.wal"            
#> [20] "file1f29d05a99c.duckdb"                
#> [21] "file1f29d05a99c.duckdb.wal"            
#> [22] "prediction.json"                       
#> [23] "temp_libpath1f293c4befc2"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
