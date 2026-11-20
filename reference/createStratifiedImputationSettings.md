# Create the settings for using stratified imputation

Create the settings for using stratified imputation

## Usage

``` r
createStratifiedImputationSettings(covariateId, ageSplits = NULL)
```

## Arguments

- covariateId:

  The covariateId that needs imputed values

- ageSplits:

  A vector of ages in years at which to split the age groups. If `NULL`,
  the default, age groups are 5 years wide.

## Value

An object of class `featureEngineeringSettings`

## Details

Returns an object of class `featureEngineeringSettings` that specifies
how to do stratified imputation. Patients without a value for the
covariate get the mean value of the training patients with the same age
group and gender. Patients in an age group and gender without training
values, and patients without an age or gender, get the mean over all
training patients. Stored missing (NA) values are imputed too. The means
learned in training are reused when the model is applied to new data.

## Examples

``` r
# create a stratified imputation settings for covariate 1050 with age splits 
# at 50 and 70
stratifiedImputationSettings <- 
  createStratifiedImputationSettings(covariateId = 1050, ageSplits = c(50, 70))
```
