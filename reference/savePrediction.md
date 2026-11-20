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
#> [1] "/tmp/RtmpSj0n3K/prediction.json"
dir(saveLoc)
#>  [1] "bslib-3c9f20102fef19cec6d808200c837da2"
#>  [2] "downlit"                               
#>  [3] "duckdb"                                
#>  [4] "file1c06102e614e"                      
#>  [5] "file1c061f7a66a.duckdb"                
#>  [6] "file1c063b169020.duckdb"               
#>  [7] "file1c063b169020.duckdb.wal"           
#>  [8] "file1c0649d57d5"                       
#>  [9] "file1c0656eef10.duckdb"                
#> [10] "file1c0656eef10.duckdb.wal"            
#> [11] "file1c065a81121f.duckdb"               
#> [12] "file1c065a81121f.duckdb.wal"           
#> [13] "file1c0665ac3c39"                      
#> [14] "file1c0670ed4e51.duckdb"               
#> [15] "file1c0670ed4e51.duckdb.wal"           
#> [16] "file1c0675992151"                      
#> [17] "prediction.json"                       
#> [18] "temp_libpath1c062a2a2e05"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
