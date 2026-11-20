# Predictive mean matching using lasso

Predictive mean matching using lasso

## Usage

``` r
pmmFit(data, k = 5, alpha = 1)
```

## Arguments

- data:

  An andromeda object with the following fields: xObs: covariates table
  for observed data xMiss: covariates table for missing data yObs:
  outcome variable that we want to impute

- k:

  The number of donors to use for matching (default 5)

- alpha:

  Elastic-net mixing parameter (`1` = lasso, `0` = ridge)
