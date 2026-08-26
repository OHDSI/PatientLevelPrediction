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
#> [1] "/tmp/Rtmp7KXb1s/prediction.json"
dir(saveLoc)
#>  [1] "bslib-596ae0e61b03dfeeffb4bf83f997516c"
#>  [2] "downlit"                               
#>  [3] "file1d511484bc1d.duckdb"               
#>  [4] "file1d511484bc1d.duckdb.wal"           
#>  [5] "file1d5123acb847"                      
#>  [6] "file1d5143f2687a.duckdb"               
#>  [7] "file1d5143f2687a.duckdb.wal"           
#>  [8] "file1d514fb679d0"                      
#>  [9] "file1d5152fc4d37"                      
#> [10] "file1d515f2fb73e"                      
#> [11] "file1d51d273998.duckdb"                
#> [12] "file1d51d273998.duckdb.wal"            
#> [13] "prediction.json"                       
#> [14] "temp_libpath1d51413e13dc"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
