# Create Simple Imputer settings

This function creates the settings for a simple imputer which imputes
missing values with the mean or median

## Usage

``` r
createSimpleImputer(
  method = "mean",
  missingThreshold = 0.3,
  addMissingIndicator = FALSE
)
```

## Arguments

- method:

  The method to use for imputation, either "mean" or "median"

- missingThreshold:

  The threshold for missing values to be imputed vs removed

- addMissingIndicator:

  Add a binary missingness indicator per feature that passes the
  imputation missingness threshold.

## Value

The settings for the single imputer of class
`featureEngineeringSettings`

## Examples

``` r
# create imputer to impute values with missingness less than 10% using the median
# of observed values
createSimpleImputer(method = "median", missingThreshold = 0.10)
#> $method
#> [1] "median"
#> 
#> $missingThreshold
#> [1] 0.1
#> 
#> $addMissingIndicator
#> [1] FALSE
#> 
#> attr(,"fun")
#> [1] "simpleImpute"
#> attr(,"class")
#> [1] "featureEngineeringSettings"
```
