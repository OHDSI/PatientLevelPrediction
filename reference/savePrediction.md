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
#> [1] "/tmp/Rtmp5fcPNF/prediction.json"
dir(saveLoc)
#>  [1] "bslib-596ae0e61b03dfeeffb4bf83f997516c"
#>  [2] "downlit"                               
#>  [3] "file1e381f921b52.duckdb"               
#>  [4] "file1e3820cf636e"                      
#>  [5] "file1e382b08c5d7.duckdb"               
#>  [6] "file1e382b08c5d7.duckdb.wal"           
#>  [7] "file1e3830785ca9.duckdb"               
#>  [8] "file1e3830785ca9.duckdb.wal"           
#>  [9] "file1e3833f9097e"                      
#> [10] "file1e3836dc04bc"                      
#> [11] "file1e383d04223f"                      
#> [12] "file1e387d5d5b92.duckdb"               
#> [13] "file1e387d5d5b92.duckdb.wal"           
#> [14] "prediction.json"                       
#> [15] "temp_libpath1e3871a839c5"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
