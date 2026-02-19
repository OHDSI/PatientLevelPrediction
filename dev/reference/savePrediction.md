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
#> [1] "/tmp/Rtmpt2Ub4R/prediction.json"
dir(saveLoc)
#>  [1] "bslib-e2f5c5dd465040cad5f3efff813f21bc"
#>  [2] "downlit"                               
#>  [3] "file1ed6161a1a4d.duckdb"               
#>  [4] "file1ed6161a1a4d.duckdb.wal"           
#>  [5] "file1ed616aade84.duckdb"               
#>  [6] "file1ed616aade84.duckdb.wal"           
#>  [7] "file1ed638f2013c.duckdb"               
#>  [8] "file1ed638f2013c.duckdb.wal"           
#>  [9] "file1ed63dbc28c0"                      
#> [10] "file1ed63e39b2ea.duckdb"               
#> [11] "file1ed63e39b2ea.duckdb.wal"           
#> [12] "file1ed6581c193e"                      
#> [13] "file1ed659a162ff.duckdb"               
#> [14] "file1ed659a162ff.duckdb.wal"           
#> [15] "file1ed65f026f51.duckdb"               
#> [16] "file1ed665869789"                      
#> [17] "file1ed678fb84c7"                      
#> [18] "file1ed67f05808d.duckdb"               
#> [19] "file1ed67f05808d.duckdb.wal"           
#> [20] "file1ed6ebf9432.duckdb"                
#> [21] "file1ed6ebf9432.duckdb.wal"            
#> [22] "prediction.json"                       
#> [23] "temp_libpath1ed611b72b39"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
