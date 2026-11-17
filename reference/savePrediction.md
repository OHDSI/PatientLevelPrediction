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
#> [1] "/tmp/RtmpnQyFOs/prediction.json"
dir(saveLoc)
#>  [1] "bslib-3c9f20102fef19cec6d808200c837da2"
#>  [2] "downlit"                               
#>  [3] "duckdb"                                
#>  [4] "file1cef14b241c1"                      
#>  [5] "file1cef21b92cf5.duckdb"               
#>  [6] "file1cef393cec34"                      
#>  [7] "file1cef3f6fba28"                      
#>  [8] "file1cef4cffb140.duckdb"               
#>  [9] "file1cef4cffb140.duckdb.wal"           
#> [10] "file1cef519e8f60.duckdb"               
#> [11] "file1cef519e8f60.duckdb.wal"           
#> [12] "file1cef5276f47b.duckdb"               
#> [13] "file1cef55e52427"                      
#> [14] "file1cef5ce8c575.duckdb"               
#> [15] "file1cef5ce8c575.duckdb.wal"           
#> [16] "file1cef623857b9.duckdb"               
#> [17] "file1cef623857b9.duckdb.wal"           
#> [18] "file1cef64f59a94.duckdb"               
#> [19] "file1cef64f59a94.duckdb.wal"           
#> [20] "file1cef69e7294f.duckdb"               
#> [21] "prediction.json"                       
#> [22] "temp_libpath1cef29cdd56"               

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
