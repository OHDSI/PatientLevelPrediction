# calibrationLine

calibrationLine

## Usage

``` r
calibrationLine(prediction, numberOfStrata = 10)
```

## Arguments

- prediction:

  A prediction object

- numberOfStrata:

  The number of groups to split the prediction into

## Value

A list containing the calibrationLine coefficients, the aggregate data
used to fit the line and the Hosmer-Lemeshow goodness of fit test

## Examples

``` r
prediction <- data.frame(
  value = c(0.1, 0.2, 0.3, 0.4, 0.5),
  outcomeCount = c(0, 1, 0, 1, 1))
calibrationLine(prediction, numberOfStrata = 1)
#> Warning: NaNs produced
#> $lm
#> Intercept  Gradient 
#>      -0.3       3.0 
#> 
#> $aggregateLmData
#>   group  obs pred
#> 1     1 0.00 0.10
#> 2     2 0.75 0.35
#> 
#> $hosmerlemeshow
#>   Xsquared df pvalue
#> 1 2.924298 -1    NaN
#> 
```
