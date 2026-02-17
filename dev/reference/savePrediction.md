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
#> [1] "/tmp/RtmpWVzlHx/prediction.json"
dir(saveLoc)
#>  [1] "bslib-e2f5c5dd465040cad5f3efff813f21bc"
#>  [2] "downlit"                               
#>  [3] "file1e8b1e02bec2.duckdb"               
#>  [4] "file1e8b1e02bec2.duckdb.wal"           
#>  [5] "file1e8b2dc57a3d.duckdb"               
#>  [6] "file1e8b2dc57a3d.duckdb.wal"           
#>  [7] "file1e8b2de44bbd"                      
#>  [8] "file1e8b32d6cc53"                      
#>  [9] "file1e8b496ffd37"                      
#> [10] "file1e8b5098d678.duckdb"               
#> [11] "file1e8b5098d678.duckdb.wal"           
#> [12] "file1e8b63811fe"                       
#> [13] "file1e8b65929b12.duckdb"               
#> [14] "file1e8b65929b12.duckdb.wal"           
#> [15] "file1e8bbc39f2f.duckdb"                
#> [16] "file1e8bbc39f2f.duckdb.wal"            
#> [17] "file1e8bf470d9e.duckdb"                
#> [18] "prediction.json"                       
#> [19] "temp_libpath1e8b460a6333"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
