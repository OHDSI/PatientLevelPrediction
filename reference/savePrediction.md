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
#> [1] "/tmp/RtmpIP5J7I/prediction.json"
dir(saveLoc)
#>  [1] "bslib-3c9f20102fef19cec6d808200c837da2"
#>  [2] "downlit"                               
#>  [3] "duckdb"                                
#>  [4] "file1d3a10efb068"                      
#>  [5] "file1d3a16952395.duckdb"               
#>  [6] "file1d3a16952395.duckdb.wal"           
#>  [7] "file1d3a22ba567.duckdb"                
#>  [8] "file1d3a2a627357.duckdb"               
#>  [9] "file1d3a2a627357.duckdb.wal"           
#> [10] "file1d3a405a0857"                      
#> [11] "file1d3a436e7266"                      
#> [12] "file1d3a43e7940a.duckdb"               
#> [13] "file1d3a43e7940a.duckdb.wal"           
#> [14] "file1d3a49282d88"                      
#> [15] "file1d3a55a0bec7.duckdb"               
#> [16] "file1d3a55a0bec7.duckdb.wal"           
#> [17] "file1d3a5d42361c.duckdb"               
#> [18] "file1d3a5d42361c.duckdb.wal"           
#> [19] "file1d3a64b3fa6e.duckdb"               
#> [20] "file1d3a64b3fa6e.duckdb.wal"           
#> [21] "file1d3a6755e3d2.duckdb"               
#> [22] "file1d3a6755e3d2.duckdb.wal"           
#> [23] "file1d3a8ee789b.duckdb"                
#> [24] "file1d3a8ee789b.duckdb.wal"            
#> [25] "prediction.json"                       
#> [26] "temp_libpath1d3a1990a390"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
