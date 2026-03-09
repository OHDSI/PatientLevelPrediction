# Create a temporary model location

Create a temporary model location

## Usage

``` r
createTempModelLoc()
```

## Value

A string for the location of the temporary model location

## Examples

``` r
modelLoc <- createTempModelLoc()
dir.exists(modelLoc)
#> [1] FALSE
# clean up
unlink(modelLoc, recursive = TRUE)
```
