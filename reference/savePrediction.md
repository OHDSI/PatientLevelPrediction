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
#> [1] "/tmp/RtmpFZeDTR/prediction.json"
dir(saveLoc)
#>  [1] "bslib-96e575f327515dbfeed2e31a38955eab"
#>  [2] "downlit"                               
#>  [3] "file214111e78964.duckdb"               
#>  [4] "file214111e78964.duckdb.wal"           
#>  [5] "file214128f3b87a.duckdb"               
#>  [6] "file21413e045e6"                       
#>  [7] "file214153c538db.duckdb"               
#>  [8] "file214153c538db.duckdb.wal"           
#>  [9] "file214158d4fad4"                      
#> [10] "file21415ea9bc94.duckdb"               
#> [11] "file21415ea9bc94.duckdb.wal"           
#> [12] "file21417dcf687b"                      
#> [13] "file2141fe3de20"                       
#> [14] "prediction.json"                       
#> [15] "temp_libpath2141a01c046"               

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
