# brierScore

brierScore

## Usage

``` r
brierScore(prediction)
```

## Arguments

- prediction:

  A prediction dataframe

## Value

A list containing the brier score and the scaled brier score

## Details

Calculates the brierScore from prediction object

## Examples

``` r
prediction <- data.frame(
  value = c(0.1, 0.2, 0.3, 0.4, 0.5),
  outcomeCount = c(0, 1, 0, 1, 1))
brierScore(prediction)
#> $brier
#> [1] 0.27
#> 
#> $brierScaled
#> [1] -0.2857143
#> 
```
