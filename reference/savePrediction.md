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
#> [1] "/tmp/RtmpKatQSO/prediction.json"
dir(saveLoc)
#>  [1] "bslib-e2f5c5dd465040cad5f3efff813f21bc"
#>  [2] "downlit"                               
#>  [3] "file1f6b10bcdf7a.duckdb"               
#>  [4] "file1f6b2818f436"                      
#>  [5] "file1f6b28b4b221.duckdb"               
#>  [6] "file1f6b28b4b221.duckdb.wal"           
#>  [7] "file1f6b34b7ba8b"                      
#>  [8] "file1f6b35b83f58"                      
#>  [9] "file1f6b35eebdae.duckdb"               
#> [10] "file1f6b35eebdae.duckdb.wal"           
#> [11] "file1f6b39fc8822.duckdb"               
#> [12] "file1f6b39fc8822.duckdb.wal"           
#> [13] "file1f6b4335ba54.duckdb"               
#> [14] "file1f6b4335ba54.duckdb.wal"           
#> [15] "file1f6b4387540c.duckdb"               
#> [16] "file1f6b4387540c.duckdb.wal"           
#> [17] "file1f6b4667a601.duckdb"               
#> [18] "file1f6b4667a601.duckdb.wal"           
#> [19] "file1f6b4ab0892e.duckdb"               
#> [20] "file1f6b4ab0892e.duckdb.wal"           
#> [21] "file1f6b59760721.duckdb"               
#> [22] "file1f6b59760721.duckdb.wal"           
#> [23] "file1f6b7b3fc5e4"                      
#> [24] "prediction.json"                       
#> [25] "temp_libpath1f6bd71c672"               

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
