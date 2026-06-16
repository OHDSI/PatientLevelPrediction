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
#> [1] "/tmp/RtmpGZyDvv/prediction.json"
dir(saveLoc)
#>  [1] "bslib-2639a81f5f9cc8217c1e54b2e8333794"
#>  [2] "downlit"                               
#>  [3] "file1f1511b7900a"                      
#>  [4] "file1f1518f69e0f.duckdb"               
#>  [5] "file1f1529c21dd1.duckdb"               
#>  [6] "file1f1529c21dd1.duckdb.wal"           
#>  [7] "file1f152cddef9f.duckdb"               
#>  [8] "file1f152cddef9f.duckdb.wal"           
#>  [9] "file1f153fd47b3f.duckdb"               
#> [10] "file1f153fd47b3f.duckdb.wal"           
#> [11] "file1f155f636076.duckdb"               
#> [12] "file1f155f636076.duckdb.wal"           
#> [13] "file1f155faeb374"                      
#> [14] "file1f15636947fe.duckdb"               
#> [15] "file1f15636947fe.duckdb.wal"           
#> [16] "file1f1567700cd4"                      
#> [17] "file1f156a9067ae.duckdb"               
#> [18] "file1f15dce0c00"                       
#> [19] "prediction.json"                       
#> [20] "temp_libpath1f152da54acd"              

# clean up
unlink(file.path(saveLoc, "prediction.json"))
```
