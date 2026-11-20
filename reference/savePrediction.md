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
#> [1] "/tmp/RtmpDxolU0/prediction.json"
dir(saveLoc)
#>  [1] "bslib-3c9f20102fef19cec6d808200c837da2"
#>  [2] "downlit"                               
#>  [3] "duckdb"                                
#>  [4] "file1f0b191bd03f.duckdb"               
#>  [5] "file1f0b191bd03f.duckdb.wal"           
#>  [6] "file1f0b306d21ea.duckdb"               
#>  [7] "file1f0b306d21ea.duckdb.wal"           
#>  [8] "file1f0b340f7bad.duckdb"               
#>  [9] "file1f0b340f7bad.duckdb.wal"           
#> [10] "file1f0b34326529"                      
#> [11] "file1f0b3c280433"                      
#> [12] "file1f0b3e35ba28.duckdb"               
#> [13] "file1f0b3e35ba28.duckdb.wal"           
#> [14] "file1f0b4bf7ed3a.duckdb"               
#> [15] "file1f0b4bf7ed3a.duckdb.wal"           
#> [16] "file1f0b4c917ab5.duckdb"               
#> [17] "file1f0b4c917ab5.duckdb.wal"           
#> [18] "file1f0b5977389c"                      
#> [19] "file1f0b5cace834"                      
#> [20] "file1f0b7ff352bf.duckdb"               
#> [21] "file1f0bccc7531.duckdb"                
#> [22] "file1f0bccc7531.duckdb.wal"            
#> [23] "file1f0bf4007d3.duckdb"                
#> [24] "prediction.json"                       
#> [25] "temp_libpath1f0b6a8fca8e"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
