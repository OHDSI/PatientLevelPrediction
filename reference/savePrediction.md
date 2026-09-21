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
#> [1] "/tmp/RtmpVA5Qhz/prediction.json"
dir(saveLoc)
#>  [1] "bslib-596ae0e61b03dfeeffb4bf83f997516c"
#>  [2] "downlit"                               
#>  [3] "file1da9178e7e55.duckdb"               
#>  [4] "file1da9178e7e55.duckdb.wal"           
#>  [5] "file1da9276c5355.duckdb"               
#>  [6] "file1da9276c5355.duckdb.wal"           
#>  [7] "file1da92e55e1fb.duckdb"               
#>  [8] "file1da92e55e1fb.duckdb.wal"           
#>  [9] "file1da93168ba23"                      
#> [10] "file1da93264f28d.duckdb"               
#> [11] "file1da93264f28d.duckdb.wal"           
#> [12] "file1da947fe7d58.duckdb"               
#> [13] "file1da947fe7d58.duckdb.wal"           
#> [14] "file1da94c59441c"                      
#> [15] "file1da95c2fd5c3.duckdb"               
#> [16] "file1da95c2fd5c3.duckdb.wal"           
#> [17] "file1da97442ee13"                      
#> [18] "file1da97dc2b076"                      
#> [19] "file1da98c3b48.duckdb"                 
#> [20] "file1da98c3b48.duckdb.wal"             
#> [21] "prediction.json"                       
#> [22] "temp_libpath1da92d409efd"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
