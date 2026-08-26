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
#> [1] "/tmp/Rtmph87Ai6/prediction.json"
dir(saveLoc)
#>  [1] "bslib-596ae0e61b03dfeeffb4bf83f997516c"
#>  [2] "downlit"                               
#>  [3] "file1d20109936fa"                      
#>  [4] "file1d2018e28095.duckdb"               
#>  [5] "file1d2029da90f0.duckdb"               
#>  [6] "file1d2029da90f0.duckdb.wal"           
#>  [7] "file1d202ecf5820.duckdb"               
#>  [8] "file1d202ecf5820.duckdb.wal"           
#>  [9] "file1d2030b816aa"                      
#> [10] "file1d203270b71d"                      
#> [11] "file1d203a299480.duckdb"               
#> [12] "file1d203a299480.duckdb.wal"           
#> [13] "file1d20527f7042.duckdb"               
#> [14] "file1d20527f7042.duckdb.wal"           
#> [15] "file1d205806e9b3"                      
#> [16] "file1d205b94949e.duckdb"               
#> [17] "file1d205b94949e.duckdb.wal"           
#> [18] "file1d206c10983.duckdb"                
#> [19] "file1d206c10983.duckdb.wal"            
#> [20] "file1d207834d326.duckdb"               
#> [21] "file1d207834d326.duckdb.wal"           
#> [22] "prediction.json"                       
#> [23] "temp_libpath1d209ed1554"               

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
