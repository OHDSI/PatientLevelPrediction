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
#> [1] "/tmp/RtmpeZrqmL/prediction.json"
dir(saveLoc)
#>  [1] "bslib-e2f5c5dd465040cad5f3efff813f21bc"
#>  [2] "downlit"                               
#>  [3] "file1ecb169c7cbf"                      
#>  [4] "file1ecb1760ac4.duckdb"                
#>  [5] "file1ecb193f6e9e.duckdb"               
#>  [6] "file1ecb193f6e9e.duckdb.wal"           
#>  [7] "file1ecb1d462547.duckdb"               
#>  [8] "file1ecb1d462547.duckdb.wal"           
#>  [9] "file1ecb2394c3a5"                      
#> [10] "file1ecb3b850528.duckdb"               
#> [11] "file1ecb3b850528.duckdb.wal"           
#> [12] "file1ecb49fa09ef"                      
#> [13] "file1ecb4c473884.duckdb"               
#> [14] "file1ecb4c473884.duckdb.wal"           
#> [15] "file1ecb4dd4949a.duckdb"               
#> [16] "file1ecb4dd4949a.duckdb.wal"           
#> [17] "file1ecb6e0c8c1d.duckdb"               
#> [18] "file1ecb6e0c8c1d.duckdb.wal"           
#> [19] "file1ecb6f9942ec"                      
#> [20] "file1ecb702d8e31.duckdb"               
#> [21] "file1ecb702d8e31.duckdb.wal"           
#> [22] "prediction.json"                       
#> [23] "temp_libpath1ecb2f30c8c2"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
