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
#> [1] "/tmp/RtmpjuRb6H/prediction.json"
dir(saveLoc)
#>  [1] "bslib-596ae0e61b03dfeeffb4bf83f997516c"
#>  [2] "downlit"                               
#>  [3] "file1c501c343ce9.duckdb"               
#>  [4] "file1c501c343ce9.duckdb.wal"           
#>  [5] "file1c50287fecd6"                      
#>  [6] "file1c502e5db505.duckdb"               
#>  [7] "file1c502e5db505.duckdb.wal"           
#>  [8] "file1c502e66ca02.duckdb"               
#>  [9] "file1c502e66ca02.duckdb.wal"           
#> [10] "file1c5046a232e5"                      
#> [11] "file1c504854bc6a.duckdb"               
#> [12] "file1c504854bc6a.duckdb.wal"           
#> [13] "file1c506a1b26f5.duckdb"               
#> [14] "file1c506a1b26f5.duckdb.wal"           
#> [15] "file1c506e3fee62"                      
#> [16] "file1c50748a8783"                      
#> [17] "file1c507582a133.duckdb"               
#> [18] "file1c507582a133.duckdb.wal"           
#> [19] "file1c507ee632d.duckdb"                
#> [20] "file1c507ee632d.duckdb.wal"            
#> [21] "prediction.json"                       
#> [22] "temp_libpath1c50587841e2"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
