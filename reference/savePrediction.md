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
#> [1] "/tmp/RtmpTLnJi6/prediction.json"
dir(saveLoc)
#>  [1] "bslib-596ae0e61b03dfeeffb4bf83f997516c"
#>  [2] "downlit"                               
#>  [3] "file1e96137c2ac6"                      
#>  [4] "file1e9625346524.duckdb"               
#>  [5] "file1e9625346524.duckdb.wal"           
#>  [6] "file1e9646d7792e.duckdb"               
#>  [7] "file1e9646d7792e.duckdb.wal"           
#>  [8] "file1e967891cecf.duckdb"               
#>  [9] "file1e967891cecf.duckdb.wal"           
#> [10] "file1e967d3a866"                       
#> [11] "file1e967dd50655"                      
#> [12] "file1e96a8fb6ae"                       
#> [13] "prediction.json"                       
#> [14] "temp_libpath1e964a89d8e9"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
