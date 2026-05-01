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
#> [1] "/tmp/RtmpDJzB64/prediction.json"
dir(saveLoc)
#>  [1] "bslib-96e575f327515dbfeed2e31a38955eab"
#>  [2] "downlit"                               
#>  [3] "file1ea111763d6c"                      
#>  [4] "file1ea129d62601.duckdb"               
#>  [5] "file1ea129d62601.duckdb.wal"           
#>  [6] "file1ea12ba615d8.duckdb"               
#>  [7] "file1ea12ba615d8.duckdb.wal"           
#>  [8] "file1ea1367ded00.duckdb"               
#>  [9] "file1ea141898ac7"                      
#> [10] "file1ea14cb02df2"                      
#> [11] "file1ea15277f692.duckdb"               
#> [12] "file1ea15277f692.duckdb.wal"           
#> [13] "file1ea1781f770"                       
#> [14] "prediction.json"                       
#> [15] "temp_libpath1ea1db894ae"               

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
