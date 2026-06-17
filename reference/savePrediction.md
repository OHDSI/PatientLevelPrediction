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
#> [1] "/tmp/RtmphVamTI/prediction.json"
dir(saveLoc)
#>  [1] "bslib-2639a81f5f9cc8217c1e54b2e8333794"
#>  [2] "downlit"                               
#>  [3] "file1e3012cc1068"                      
#>  [4] "file1e3019f1431c.duckdb"               
#>  [5] "file1e302861bf8"                       
#>  [6] "file1e302be44fd8.duckdb"               
#>  [7] "file1e302be44fd8.duckdb.wal"           
#>  [8] "file1e3034013408.duckdb"               
#>  [9] "file1e3034013408.duckdb.wal"           
#> [10] "file1e304bcf3ba5"                      
#> [11] "file1e305b07f7d3.duckdb"               
#> [12] "file1e305b07f7d3.duckdb.wal"           
#> [13] "file1e30629102b.duckdb"                
#> [14] "file1e30629102b.duckdb.wal"            
#> [15] "file1e3066fbf862"                      
#> [16] "file1e30752fbce6.duckdb"               
#> [17] "file1e30752fbce6.duckdb.wal"           
#> [18] "file1e30797ff891.duckdb"               
#> [19] "file1e30797ff891.duckdb.wal"           
#> [20] "file1e308221d6e.duckdb"                
#> [21] "file1e308221d6e.duckdb.wal"            
#> [22] "prediction.json"                       
#> [23] "temp_libpath1e3036d246b8"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
