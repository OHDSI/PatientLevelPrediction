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
#> [1] "/tmp/RtmpWSyFtM/prediction.json"
dir(saveLoc)
#>  [1] "bslib-e2f5c5dd465040cad5f3efff813f21bc"
#>  [2] "downlit"                               
#>  [3] "file1eb0105b435a.duckdb"               
#>  [4] "file1eb0105b435a.duckdb.wal"           
#>  [5] "file1eb013c41507.duckdb"               
#>  [6] "file1eb013c41507.duckdb.wal"           
#>  [7] "file1eb01eefa0c2.duckdb"               
#>  [8] "file1eb03a3e43fe.duckdb"               
#>  [9] "file1eb03a3e43fe.duckdb.wal"           
#> [10] "file1eb03ebc5597.duckdb"               
#> [11] "file1eb03ebc5597.duckdb.wal"           
#> [12] "file1eb048d4bc8c.duckdb"               
#> [13] "file1eb048d4bc8c.duckdb.wal"           
#> [14] "file1eb04eadb8cd.duckdb"               
#> [15] "file1eb04eadb8cd.duckdb.wal"           
#> [16] "file1eb05491ae82"                      
#> [17] "file1eb05705daae"                      
#> [18] "file1eb0706b988d.duckdb"               
#> [19] "file1eb0706b988d.duckdb.wal"           
#> [20] "file1eb071f13437"                      
#> [21] "file1eb07a1dc8a5"                      
#> [22] "prediction.json"                       
#> [23] "temp_libpath1eb039520e59"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
