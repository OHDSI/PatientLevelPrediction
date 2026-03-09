# Calculate the average precision

Calculate the average precision

## Usage

``` r
averagePrecision(prediction)
```

## Arguments

- prediction:

  A prediction object

## Value

The average precision value

## Details

Calculates the average precision from a predition object

## Examples

``` r
prediction <- data.frame(
  value = c(0.1, 0.2, 0.3, 0.4, 0.5),
  outcomeCount = c(0, 1, 0, 1, 1)
)
averagePrecision(prediction)
#> [1] 0.9166667
```
