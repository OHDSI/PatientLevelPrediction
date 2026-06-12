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
#> [1] "/tmp/RtmpEJ5Zht/prediction.json"
dir(saveLoc)
#>  [1] "bslib-2639a81f5f9cc8217c1e54b2e8333794"
#>  [2] "downlit"                               
#>  [3] "file219d12284c2f.duckdb"               
#>  [4] "file219d12284c2f.duckdb.wal"           
#>  [5] "file219d143b58f4.duckdb"               
#>  [6] "file219d143b58f4.duckdb.wal"           
#>  [7] "file219d156f827a"                      
#>  [8] "file219d17f1ca29"                      
#>  [9] "file219d21478a5e"                      
#> [10] "file219d30d6e5d1.duckdb"               
#> [11] "file219d30d6e5d1.duckdb.wal"           
#> [12] "file219d44d1f01.duckdb"                
#> [13] "file219d44d1f01.duckdb.wal"            
#> [14] "file219d5ae3d5c0.duckdb"               
#> [15] "file219d5ae3d5c0.duckdb.wal"           
#> [16] "file219d615ab918.duckdb"               
#> [17] "file219d615ab918.duckdb.wal"           
#> [18] "file219d7f4b2a3b.duckdb"               
#> [19] "file219d7feb2e5f.duckdb"               
#> [20] "file219d7feb2e5f.duckdb.wal"           
#> [21] "file219dc9ac49"                        
#> [22] "prediction.json"                       
#> [23] "temp_libpath219d3e6979eb"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
