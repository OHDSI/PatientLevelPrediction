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
#> [1] "/tmp/RtmpkKkjda/prediction.json"
dir(saveLoc)
#>  [1] "bslib-2639a81f5f9cc8217c1e54b2e8333794"
#>  [2] "downlit"                               
#>  [3] "file1f1c1f7f96ac.duckdb"               
#>  [4] "file1f1c1f7f96ac.duckdb.wal"           
#>  [5] "file1f1c2aaab69a.duckdb"               
#>  [6] "file1f1c3bac0df3.duckdb"               
#>  [7] "file1f1c3bac0df3.duckdb.wal"           
#>  [8] "file1f1c3d5cfc31"                      
#>  [9] "file1f1c3dc610df.duckdb"               
#> [10] "file1f1c432a6390"                      
#> [11] "file1f1c52631c46.duckdb"               
#> [12] "file1f1c52631c46.duckdb.wal"           
#> [13] "file1f1c5898cfda"                      
#> [14] "file1f1c58dc2723.duckdb"               
#> [15] "file1f1c58dc2723.duckdb.wal"           
#> [16] "file1f1c5a1f27fd"                      
#> [17] "file1f1c5dd1fa41.duckdb"               
#> [18] "file1f1c6210cc0e.duckdb"               
#> [19] "file1f1c6210cc0e.duckdb.wal"           
#> [20] "file1f1c637cffbf.duckdb"               
#> [21] "file1f1c79b2d7f.duckdb"                
#> [22] "file1f1c79b2d7f.duckdb.wal"            
#> [23] "file1f1c7a8eac42.duckdb"               
#> [24] "prediction.json"                       
#> [25] "temp_libpath1f1cdf3c6cc"               

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
