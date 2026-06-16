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
#> [1] "/tmp/RtmphaKfaY/prediction.json"
dir(saveLoc)
#>  [1] "bslib-2639a81f5f9cc8217c1e54b2e8333794"
#>  [2] "downlit"                               
#>  [3] "file1f1012316b20.duckdb"               
#>  [4] "file1f1017f99672.duckdb"               
#>  [5] "file1f10189c3117.duckdb"               
#>  [6] "file1f10189c3117.duckdb.wal"           
#>  [7] "file1f101a54bba9.duckdb"               
#>  [8] "file1f101a54bba9.duckdb.wal"           
#>  [9] "file1f102968e821"                      
#> [10] "file1f10334fc4e6.duckdb"               
#> [11] "file1f10334fc4e6.duckdb.wal"           
#> [12] "file1f103aa15524.duckdb"               
#> [13] "file1f1042052a17.duckdb"               
#> [14] "file1f1042052a17.duckdb.wal"           
#> [15] "file1f10532ffb3a.duckdb"               
#> [16] "file1f1057a6eb6e.duckdb"               
#> [17] "file1f1057a6eb6e.duckdb.wal"           
#> [18] "file1f1063214fd"                       
#> [19] "file1f10635f76cd"                      
#> [20] "file1f107978ad3d.duckdb"               
#> [21] "file1f107978ad3d.duckdb.wal"           
#> [22] "file1f10985a040"                       
#> [23] "prediction.json"                       
#> [24] "temp_libpath1f10589ddc84"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
