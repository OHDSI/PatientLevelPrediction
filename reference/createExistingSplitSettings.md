# Create the settings for defining how the plpData are split into test/validation/train sets using an existing split - good to use for reproducing results from a different run

Create the settings for defining how the plpData are split into
test/validation/train sets using an existing split - good to use for
reproducing results from a different run

## Usage

``` r
createExistingSplitSettings(splitIds)
```

## Arguments

- splitIds:

  (data.frame) A data frame with rowId and index columns of type
  integer/numeric. Index is -1 for test set, positive integer for train
  set folds

## Value

An object of class `splitSettings`

## Examples

``` r
# rowId 1 is in fold 1, rowId 2 is in fold 2, rowId 3 is in the test set
# rowId 4 is in fold 1, rowId 5 is in fold 2
createExistingSplitSettings(splitIds = data.frame(rowId = c(1, 2, 3, 4, 5),
                                                  index = c(1, 2, -1, 1, 2)))
#> $splitIds
#>   rowId index
#> 1     1     1
#> 2     2     2
#> 3     3    -1
#> 4     4     1
#> 5     5     2
#> 
#> attr(,"fun")
#> [1] "existingSplitter"
#> attr(,"class")
#> [1] "splitSettings"
```
