# Calculate the Integrated Calibration Index from Austin and Steyerberg https://onlinelibrary.wiley.com/doi/full/10.1002/sim.8281

Calculate the Integrated Calibration Index from Austin and Steyerberg
https://onlinelibrary.wiley.com/doi/full/10.1002/sim.8281

## Usage

``` r
ici(prediction)
```

## Arguments

- prediction:

  the prediction object found in the plpResult object

## Value

Integrated Calibration Index value or NULL if the calculation fails

## Details

Calculate the Integrated Calibration Index

## Examples

``` r
prediction <- data.frame(rowId = 1:100, 
                        outcomeCount = stats::rbinom(1:100, 1, prob=0.5),
                        value = runif(100), 
                        evaluation = rep("Train", 100))
ici(prediction)
#> [1] 0.2304016
```
