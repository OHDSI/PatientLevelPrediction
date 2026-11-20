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
#> [1] "/tmp/Rtmp1anFSg/prediction.json"
dir(saveLoc)
#>  [1] "bslib-3c9f20102fef19cec6d808200c837da2"
#>  [2] "downlit"                               
#>  [3] "duckdb"                                
#>  [4] "file1ec510e0e492.duckdb"               
#>  [5] "file1ec510e0e492.duckdb.wal"           
#>  [6] "file1ec5274521cf"                      
#>  [7] "file1ec52cdb973e.duckdb"               
#>  [8] "file1ec52cdb973e.duckdb.wal"           
#>  [9] "file1ec52e35a631.duckdb"               
#> [10] "file1ec52e35a631.duckdb.wal"           
#> [11] "file1ec531e66e5.duckdb"                
#> [12] "file1ec531e66e5.duckdb.wal"            
#> [13] "file1ec5408863b6"                      
#> [14] "file1ec545476a3d"                      
#> [15] "file1ec54dd9c1f3.duckdb"               
#> [16] "file1ec54dd9c1f3.duckdb.wal"           
#> [17] "file1ec54e88b048"                      
#> [18] "file1ec56145977c.duckdb"               
#> [19] "file1ec56145977c.duckdb.wal"           
#> [20] "file1ec576a520e7.duckdb"               
#> [21] "file1ec5f00c6e2.duckdb"                
#> [22] "file1ec5f00c6e2.duckdb.wal"            
#> [23] "prediction.json"                       
#> [24] "temp_libpath1ec53a5ae686"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
