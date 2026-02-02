# Create the settings for adding a spline for continuous variables

Create the settings for adding a spline for continuous variables

## Usage

``` r
createSplineSettings(continousCovariateId, knots, analysisId = 683)
```

## Arguments

- continousCovariateId:

  The covariateId to apply splines to

- knots:

  Either number of knots of vector of split values

- analysisId:

  The analysisId to use for the spline covariates

## Value

An object of class `featureEngineeringSettings`

## Details

Returns an object of class `featureEngineeringSettings` that specifies
the sampling function that will be called and the settings

## Examples

``` r
# create splines for age (1002) with 5 knots
createSplineSettings(continousCovariateId = 1002, knots = 5, analysisId = 683)
#> $continousCovariateId
#> [1] 1002
#> 
#> $knots
#> [1] 5
#> 
#> $analysisId
#> [1] 683
#> 
#> attr(,"fun")
#> [1] "splineCovariates"
#> attr(,"class")
#> [1] "featureEngineeringSettings"
```
