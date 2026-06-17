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
#> [1] "/tmp/Rtmpky4M2t/prediction.json"
dir(saveLoc)
#>  [1] "bslib-2639a81f5f9cc8217c1e54b2e8333794"
#>  [2] "downlit"                               
#>  [3] "file1e2419e0cb9d.duckdb"               
#>  [4] "file1e2419e0cb9d.duckdb.wal"           
#>  [5] "file1e242d889e06.duckdb"               
#>  [6] "file1e242d889e06.duckdb.wal"           
#>  [7] "file1e2437189af7.duckdb"               
#>  [8] "file1e244eecedc4"                      
#>  [9] "file1e24595c0f43.duckdb"               
#> [10] "file1e24595c0f43.duckdb.wal"           
#> [11] "file1e245ea7ccd6.duckdb"               
#> [12] "file1e245ea7ccd6.duckdb.wal"           
#> [13] "file1e24623fed1d"                      
#> [14] "file1e2466d57ef9"                      
#> [15] "file1e246886e798"                      
#> [16] "file1e246c1785da.duckdb"               
#> [17] "file1e246c1785da.duckdb.wal"           
#> [18] "file1e2489054aa.duckdb"                
#> [19] "file1e2489054aa.duckdb.wal"            
#> [20] "file1e24aec9e9f.duckdb"                
#> [21] "file1e24aec9e9f.duckdb.wal"            
#> [22] "prediction.json"                       
#> [23] "temp_libpath1e241ef36924"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
