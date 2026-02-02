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
#> [1] "/tmp/Rtmpuyh0tT/prediction.json"
dir(saveLoc)
#>  [1] "bslib-e2f5c5dd465040cad5f3efff813f21bc"
#>  [2] "downlit"                               
#>  [3] "file20c613ff7b49"                      
#>  [4] "file20c62b95333"                       
#>  [5] "file20c6467a7fff.duckdb"               
#>  [6] "file20c6467a7fff.duckdb.wal"           
#>  [7] "file20c64b093dd3.duckdb"               
#>  [8] "file20c64b093dd3.duckdb.wal"           
#>  [9] "file20c64d50635b.duckdb"               
#> [10] "file20c64d50635b.duckdb.wal"           
#> [11] "file20c65b6eb319"                      
#> [12] "file20c6653525ed"                      
#> [13] "file20c66bc19ee8.duckdb"               
#> [14] "file20c66bc19ee8.duckdb.wal"           
#> [15] "file20c67a15677c.duckdb"               
#> [16] "file20c6b576cdf.duckdb"                
#> [17] "file20c6b576cdf.duckdb.wal"            
#> [18] "prediction.json"                       
#> [19] "temp_libpath20c61980cfdb"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
