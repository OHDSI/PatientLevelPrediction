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
#> [1] "/tmp/RtmpSZA6kM/prediction.json"
dir(saveLoc)
#>  [1] "bslib-e2f5c5dd465040cad5f3efff813f21bc"
#>  [2] "downlit"                               
#>  [3] "file1ea11b8c8b5d.duckdb"               
#>  [4] "file1ea11b8c8b5d.duckdb.wal"           
#>  [5] "file1ea125ec9bb2.duckdb"               
#>  [6] "file1ea125ec9bb2.duckdb.wal"           
#>  [7] "file1ea12ce5902"                       
#>  [8] "file1ea12d4def02"                      
#>  [9] "file1ea13296e897"                      
#> [10] "file1ea145676c3a"                      
#> [11] "file1ea15629f89a.duckdb"               
#> [12] "file1ea15629f89a.duckdb.wal"           
#> [13] "file1ea16229039d.duckdb"               
#> [14] "file1ea16598538f.duckdb"               
#> [15] "file1ea16598538f.duckdb.wal"           
#> [16] "file1ea1740a0b21.duckdb"               
#> [17] "file1ea1740a0b21.duckdb.wal"           
#> [18] "file1ea1797ca912.duckdb"               
#> [19] "file1ea1797ca912.duckdb.wal"           
#> [20] "file1ea1c32c00a.duckdb"                
#> [21] "file1ea1c32c00a.duckdb.wal"            
#> [22] "prediction.json"                       
#> [23] "temp_libpath1ea1545a3c04"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
