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
#> [1] "/tmp/RtmpNTC70J/prediction.json"
dir(saveLoc)
#>  [1] "bslib-e2f5c5dd465040cad5f3efff813f21bc"
#>  [2] "downlit"                               
#>  [3] "file1e9417fb6886.duckdb"               
#>  [4] "file1e9417fb6886.duckdb.wal"           
#>  [5] "file1e94321efada.duckdb"               
#>  [6] "file1e94321efada.duckdb.wal"           
#>  [7] "file1e943c954f1a.duckdb"               
#>  [8] "file1e943c954f1a.duckdb.wal"           
#>  [9] "file1e9447f77c8f"                      
#> [10] "file1e94544ad476.duckdb"               
#> [11] "file1e945ada06df.duckdb"               
#> [12] "file1e945ada06df.duckdb.wal"           
#> [13] "file1e946051cde4.duckdb"               
#> [14] "file1e946051cde4.duckdb.wal"           
#> [15] "file1e946e2571.duckdb"                 
#> [16] "file1e946e2571.duckdb.wal"             
#> [17] "file1e947b41dce"                       
#> [18] "file1e947bb6e1b4"                      
#> [19] "file1e947f6d68e3.duckdb"               
#> [20] "file1e947f6d68e3.duckdb.wal"           
#> [21] "file1e94d4a4e26"                       
#> [22] "prediction.json"                       
#> [23] "temp_libpath1e9463032cc"               

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
