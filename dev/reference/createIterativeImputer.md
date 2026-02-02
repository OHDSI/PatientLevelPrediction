# Create Iterative Imputer settings

This function creates the settings for an iterative imputer which first
removes features with more than `missingThreshold` missing values and
then imputes the missing values iteratively using chained equations

## Usage

``` r
createIterativeImputer(
  missingThreshold = 0.3,
  method = "pmm",
  methodSettings = list(pmm = list(k = 5, iterations = 5))
)
```

## Arguments

- missingThreshold:

  The threshold for missing values to remove a feature

- method:

  The method to use for imputation, currently only "pmm" is supported

- methodSettings:

  A list of settings for the imputation method to use. Currently only
  "pmm" is supported with the following settings:

  - k: The number of donors to use for matching

  - iterations: The number of iterations to use for imputation

## Value

The settings for the iterative imputer of class
`featureEngineeringSettings`

## Examples

``` r
# create imputer to impute values with missingness less than 30% using 
# predictive mean matching in 5 iterations with 5 donors
createIterativeImputer(missingThreshold = 0.3, method = "pmm",
                       methodSettings = list(pmm = list(k = 5, iterations = 5)))
#> $missingThreshold
#> [1] 0.3
#> 
#> $method
#> [1] "pmm"
#> 
#> $methodSettings
#> $methodSettings$k
#> [1] 5
#> 
#> $methodSettings$iterations
#> [1] 5
#> 
#> 
#> attr(,"fun")
#> [1] "iterativeImpute"
#> attr(,"class")
#> [1] "featureEngineeringSettings"
```
