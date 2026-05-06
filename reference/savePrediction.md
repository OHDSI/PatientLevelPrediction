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
#> [1] "/tmp/RtmpeLqdLm/prediction.json"
dir(saveLoc)
#>  [1] "bslib-96e575f327515dbfeed2e31a38955eab"
#>  [2] "downlit"                               
#>  [3] "file1eb31945c9b9.duckdb"               
#>  [4] "file1eb321cd9775.duckdb"               
#>  [5] "file1eb321cd9775.duckdb.wal"           
#>  [6] "file1eb32ae156c2"                      
#>  [7] "file1eb335aa923c.duckdb"               
#>  [8] "file1eb335aa923c.duckdb.wal"           
#>  [9] "file1eb3416ca3c5.duckdb"               
#> [10] "file1eb3416ca3c5.duckdb.wal"           
#> [11] "file1eb36dbec127.duckdb"               
#> [12] "file1eb36dbec127.duckdb.wal"           
#> [13] "file1eb36e78b12c.duckdb"               
#> [14] "file1eb36e78b12c.duckdb.wal"           
#> [15] "file1eb36ffae866"                      
#> [16] "file1eb373d8bc8d"                      
#> [17] "file1eb373ff56d8.duckdb"               
#> [18] "file1eb377011b17"                      
#> [19] "file1eb37af15414.duckdb"               
#> [20] "file1eb37af15414.duckdb.wal"           
#> [21] "prediction.json"                       
#> [22] "temp_libpath1eb34d33cd55"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
