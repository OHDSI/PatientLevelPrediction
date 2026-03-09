# evaluatePlp

Evaluates the performance of the patient level prediction model

## Usage

``` r
evaluatePlp(prediction, typeColumn = "evaluationType")
```

## Arguments

- prediction:

  The patient level prediction model's prediction

- typeColumn:

  The column name in the prediction object that is used to stratify the
  evaluation

## Value

An object of class plpEvaluation containing the following components

- evaluationStatistics: A data frame containing the evaluation
  statistics'

- thresholdSummary: A data frame containing the threshold summary'

- demographicSummary: A data frame containing the demographic summary'

- calibrationSummary: A data frame containing the calibration summary'

- predictionDistribution: A data frame containing the prediction
  distribution'

## Details

The function calculates various metrics to measure the performance of
the model

## Examples
