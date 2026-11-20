# Create the settings for using stratified imputation.

Create the settings for using stratified imputation.

## Usage

``` r
createStratifiedImputationSettings(covariateId, ageSplits = NULL)
```

## Arguments

- covariateId:

  The covariateId that needs imputed values

- ageSplits:

  A vector of age splits in years to create age groups

## Value

An object of class `featureEngineeringSettings`

## Details

Returns an object of class `featureEngineeringSettings` that specifies
how to do stratified imputation. This function splits the covariate into
age groups and fits splines to the covariate within each age group. The
spline values are then used to impute missing values.

## Examples

``` r
# create a stratified imputation settings for covariate 1050 with age splits 
# at 50 and 70
stratifiedImputationSettings <- 
  createStratifiedImputationSettings(covariateId = 1050, ageSplits = c(50, 70))
```
