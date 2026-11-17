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
#> [1] "/tmp/RtmpdnvUil/prediction.json"
dir(saveLoc)
#>  [1] "bslib-3c9f20102fef19cec6d808200c837da2"
#>  [2] "downlit"                               
#>  [3] "duckdb"                                
#>  [4] "file1c1623341c18.duckdb"               
#>  [5] "file1c1623341c18.duckdb.wal"           
#>  [6] "file1c1624bf4c56"                      
#>  [7] "file1c162c8b23c4.duckdb"               
#>  [8] "file1c162c8b23c4.duckdb.wal"           
#>  [9] "file1c1637d61c4e"                      
#> [10] "file1c163b42af53"                      
#> [11] "file1c164d7682bc.duckdb"               
#> [12] "file1c164d7682bc.duckdb.wal"           
#> [13] "file1c166bc1d219"                      
#> [14] "file1c166e0530dd.duckdb"               
#> [15] "file1c166e0530dd.duckdb.wal"           
#> [16] "file1c166e33535f.duckdb"               
#> [17] "file1c1672da4e9b.duckdb"               
#> [18] "file1c1672da4e9b.duckdb.wal"           
#> [19] "prediction.json"                       
#> [20] "temp_libpath1c167c9f72ae"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
