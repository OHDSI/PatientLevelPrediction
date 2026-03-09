# Get a demographic summary

Get a demographic summary

## Usage

``` r
getDemographicSummary(prediction, predictionType, typeColumn = "evaluation")
```

## Arguments

- prediction:

  A prediction object

- predictionType:

  The type of prediction (binary or survival)

- typeColumn:

  A column that is used to stratify the results

## Value

A dataframe with the demographic summary

## Details

Generates a data.frame with a prediction summary per each 5 year age
group and gender group

## Examples
