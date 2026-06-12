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
#> [1] "/tmp/RtmpfYHDeq/prediction.json"
dir(saveLoc)
#>  [1] "bslib-2639a81f5f9cc8217c1e54b2e8333794"
#>  [2] "downlit"                               
#>  [3] "file1f4c1ae0aa6a.duckdb"               
#>  [4] "file1f4c1ae0aa6a.duckdb.wal"           
#>  [5] "file1f4c1f0e8128.duckdb"               
#>  [6] "file1f4c1f0e8128.duckdb.wal"           
#>  [7] "file1f4c24de07d3.duckdb"               
#>  [8] "file1f4c2866eacb.duckdb"               
#>  [9] "file1f4c2866eacb.duckdb.wal"           
#> [10] "file1f4c3b9097c.duckdb"                
#> [11] "file1f4c3b9097c.duckdb.wal"            
#> [12] "file1f4c569a38b2"                      
#> [13] "file1f4c6288da0f.duckdb"               
#> [14] "file1f4c6288da0f.duckdb.wal"           
#> [15] "file1f4c67942833.duckdb"               
#> [16] "file1f4c67942833.duckdb.wal"           
#> [17] "file1f4c770bb590"                      
#> [18] "file1f4cb75f2ae.duckdb"                
#> [19] "file1f4cb75f2ae.duckdb.wal"            
#> [20] "file1f4cbe74138"                       
#> [21] "file1f4cf8d5df1"                       
#> [22] "prediction.json"                       
#> [23] "temp_libpath1f4c19b68977"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
