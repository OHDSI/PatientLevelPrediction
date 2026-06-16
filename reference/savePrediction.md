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
#> [1] "/tmp/Rtmp8FOikQ/prediction.json"
dir(saveLoc)
#>  [1] "bslib-2639a81f5f9cc8217c1e54b2e8333794"
#>  [2] "downlit"                               
#>  [3] "file21772b62858f.duckdb"               
#>  [4] "file21772b62858f.duckdb.wal"           
#>  [5] "file21772ef03820.duckdb"               
#>  [6] "file21773112e24d"                      
#>  [7] "file217743c128c7.duckdb"               
#>  [8] "file2177440bd51c.duckdb"               
#>  [9] "file217758081149.duckdb"               
#> [10] "file217758081149.duckdb.wal"           
#> [11] "file21775aa6e480.duckdb"               
#> [12] "file21775aa6e480.duckdb.wal"           
#> [13] "file21775da3325b.duckdb"               
#> [14] "file21775da3325b.duckdb.wal"           
#> [15] "file21776f6f8b96.duckdb"               
#> [16] "file21776f6f8b96.duckdb.wal"           
#> [17] "file217771e0b265"                      
#> [18] "file217774421e34.duckdb"               
#> [19] "file217774421e34.duckdb.wal"           
#> [20] "file217777d8e5c2"                      
#> [21] "file217779c802ff.duckdb"               
#> [22] "file217779c802ff.duckdb.wal"           
#> [23] "file21777b452494"                      
#> [24] "prediction.json"                       
#> [25] "temp_libpath21772daed50b"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
