# createValidationSettings define optional settings for performing external validation

This function creates the settings required by externalValidatePlp

## Usage

``` r
createValidationSettings(recalibrate = NULL, runCovariateSummary = TRUE)
```

## Arguments

- recalibrate:

  A vector of characters specifying the recalibration method to apply

- runCovariateSummary:

  Whether to run the covariate summary for the validation data

## Value

A setting object of class `validationSettings` containing a list of
settings for externalValidatePlp

## Details

Users need to specify whether they want to sample or recalibate when
performing external validation

## Examples

``` r
# do weak recalibration and don't run covariate summary
createValidationSettings(recalibrate = "weakRecalibration", 
                         runCovariateSummary = FALSE)
#> $recalibrate
#> [1] "weakRecalibration"
#> 
#> $runCovariateSummary
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "validationSettings"
```
