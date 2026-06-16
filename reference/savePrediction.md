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
#> [1] "/tmp/RtmpXMNgNi/prediction.json"
dir(saveLoc)
#>  [1] "bslib-2639a81f5f9cc8217c1e54b2e8333794"
#>  [2] "downlit"                               
#>  [3] "file1f1b11924dbf"                      
#>  [4] "file1f1b1b64aa04.duckdb"               
#>  [5] "file1f1b1b64aa04.duckdb.wal"           
#>  [6] "file1f1b1b8208b0.duckdb"               
#>  [7] "file1f1b230def89.duckdb"               
#>  [8] "file1f1b2a3b8a59.duckdb"               
#>  [9] "file1f1b2a3b8a59.duckdb.wal"           
#> [10] "file1f1b3701b29b"                      
#> [11] "file1f1b387f367a.duckdb"               
#> [12] "file1f1b387f367a.duckdb.wal"           
#> [13] "file1f1b5913b7c6.duckdb"               
#> [14] "file1f1b5913b7c6.duckdb.wal"           
#> [15] "file1f1b59afab8e.duckdb"               
#> [16] "file1f1b5afb0b6b.duckdb"               
#> [17] "file1f1b62bcf840.duckdb"               
#> [18] "file1f1b6305cc22.duckdb"               
#> [19] "file1f1b6305cc22.duckdb.wal"           
#> [20] "file1f1b74ed2bfe.duckdb"               
#> [21] "file1f1b74ed2bfe.duckdb.wal"           
#> [22] "file1f1b76969033"                      
#> [23] "file1f1bf8f789f"                       
#> [24] "prediction.json"                       
#> [25] "temp_libpath1f1b164c2d78"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
