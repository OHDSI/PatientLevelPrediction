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
#> [1] "/tmp/RtmpyOpnCm/prediction.json"
dir(saveLoc)
#>  [1] "bslib-596ae0e61b03dfeeffb4bf83f997516c"
#>  [2] "downlit"                               
#>  [3] "file1c10133af87c"                      
#>  [4] "file1c101c4954e3"                      
#>  [5] "file1c102504585e.duckdb"               
#>  [6] "file1c102504585e.duckdb.wal"           
#>  [7] "file1c103232b8d0.duckdb"               
#>  [8] "file1c103232b8d0.duckdb.wal"           
#>  [9] "file1c1035cc6952.duckdb"               
#> [10] "file1c1035cc6952.duckdb.wal"           
#> [11] "file1c103ae6cbd0.duckdb"               
#> [12] "file1c103ae6cbd0.duckdb.wal"           
#> [13] "file1c103eab0c50.duckdb"               
#> [14] "file1c103eab0c50.duckdb.wal"           
#> [15] "file1c1044f9d8d8"                      
#> [16] "file1c104782f90c.duckdb"               
#> [17] "file1c104782f90c.duckdb.wal"           
#> [18] "file1c106daa683d.duckdb"               
#> [19] "file1c106daa683d.duckdb.wal"           
#> [20] "file1c1073ce7c34"                      
#> [21] "prediction.json"                       
#> [22] "temp_libpath1c10412f5925"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
