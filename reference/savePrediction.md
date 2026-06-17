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
#> [1] "/tmp/RtmpjP0Xo1/prediction.json"
dir(saveLoc)
#>  [1] "bslib-2639a81f5f9cc8217c1e54b2e8333794"
#>  [2] "downlit"                               
#>  [3] "file1e791a272306.duckdb"               
#>  [4] "file1e791bf8ce8f"                      
#>  [5] "file1e793248b8d7.duckdb"               
#>  [6] "file1e793248b8d7.duckdb.wal"           
#>  [7] "file1e7933d3dccc"                      
#>  [8] "file1e7938110d12.duckdb"               
#>  [9] "file1e7938110d12.duckdb.wal"           
#> [10] "file1e7955be4551.duckdb"               
#> [11] "file1e7955be4551.duckdb.wal"           
#> [12] "file1e796b2caa7c"                      
#> [13] "file1e7975af930b.duckdb"               
#> [14] "file1e7975af930b.duckdb.wal"           
#> [15] "file1e797750935c.duckdb"               
#> [16] "file1e797750935c.duckdb.wal"           
#> [17] "file1e7977f7c72c.duckdb"               
#> [18] "file1e7977f7c72c.duckdb.wal"           
#> [19] "file1e7978e42e54"                      
#> [20] "file1e797d2340e7.duckdb"               
#> [21] "file1e797d2340e7.duckdb.wal"           
#> [22] "prediction.json"                       
#> [23] "temp_libpath1e796795c141"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
