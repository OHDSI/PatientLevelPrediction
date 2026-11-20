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
#> [1] "/tmp/RtmpIwzFak/prediction.json"
dir(saveLoc)
#>  [1] "bslib-3c9f20102fef19cec6d808200c837da2"
#>  [2] "downlit"                               
#>  [3] "duckdb"                                
#>  [4] "file1cbc1be63a70"                      
#>  [5] "file1cbc21b25c97"                      
#>  [6] "file1cbc23a0831a.duckdb"               
#>  [7] "file1cbc23a0831a.duckdb.wal"           
#>  [8] "file1cbc2870746c"                      
#>  [9] "file1cbc3465e246"                      
#> [10] "file1cbc3cd33bb7.duckdb"               
#> [11] "file1cbc3cd33bb7.duckdb.wal"           
#> [12] "file1cbc444324e7.duckdb"               
#> [13] "file1cbc444324e7.duckdb.wal"           
#> [14] "file1cbc4f116837.duckdb"               
#> [15] "file1cbc4f116837.duckdb.wal"           
#> [16] "file1cbc5937170c.duckdb"               
#> [17] "file1cbc5a290c9f.duckdb"               
#> [18] "file1cbc5a290c9f.duckdb.wal"           
#> [19] "file1cbc6891d23b.duckdb"               
#> [20] "file1cbc6891d23b.duckdb.wal"           
#> [21] "file1cbc783a9154.duckdb"               
#> [22] "file1cbc783a9154.duckdb.wal"           
#> [23] "file1cbcca94876.duckdb"                
#> [24] "file1cbcca94876.duckdb.wal"            
#> [25] "prediction.json"                       
#> [26] "temp_libpath1cbc5377131f"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
