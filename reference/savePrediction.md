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
#> [1] "/tmp/RtmpWNfgh3/prediction.json"
dir(saveLoc)
#>  [1] "bslib-2639a81f5f9cc8217c1e54b2e8333794"
#>  [2] "downlit"                               
#>  [3] "file1e4a1d9c89d4.duckdb"               
#>  [4] "file1e4a1d9c89d4.duckdb.wal"           
#>  [5] "file1e4a2e81f9ed"                      
#>  [6] "file1e4a33e64c98"                      
#>  [7] "file1e4a392a5ce6.duckdb"               
#>  [8] "file1e4a392a5ce6.duckdb.wal"           
#>  [9] "file1e4a3f961dae"                      
#> [10] "file1e4a440e3328"                      
#> [11] "file1e4a475f37a0.duckdb"               
#> [12] "file1e4a64443340.duckdb"               
#> [13] "file1e4a64443340.duckdb.wal"           
#> [14] "prediction.json"                       
#> [15] "temp_libpath1e4a4f03e5cd"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
