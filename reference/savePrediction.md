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
#> [1] "/tmp/RtmpF4D6dF/prediction.json"
dir(saveLoc)
#>  [1] "bslib-96e575f327515dbfeed2e31a38955eab"
#>  [2] "downlit"                               
#>  [3] "file20641306b3ed"                      
#>  [4] "file206427a4dc51.duckdb"               
#>  [5] "file206427a4dc51.duckdb.wal"           
#>  [6] "file206436286d5"                       
#>  [7] "file206448b6c836.duckdb"               
#>  [8] "file206479fded10.duckdb"               
#>  [9] "file206479fded10.duckdb.wal"           
#> [10] "file20647f82394b"                      
#> [11] "file2064b3857e4.duckdb"                
#> [12] "file2064b3857e4.duckdb.wal"            
#> [13] "file2064de5e5da"                       
#> [14] "prediction.json"                       
#> [15] "temp_libpath206474ca5ecb"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
