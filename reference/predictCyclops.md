# Create predictive probabilities

Create predictive probabilities

## Usage

``` r
predictCyclops(plpModel, data, cohort)
```

## Arguments

- plpModel:

  An object of type `predictiveModel` as generated using
  [`fitPlp`](https://ohdsi.github.io/PatientLevelPrediction/reference/fitPlp.md).

- data:

  The new plpData containing the covariateData for the new population

- cohort:

  The cohort to calculate the prediction for

## Value

The value column in the result data.frame is: logistic: probabilities of
the outcome, poisson: Poisson rate (per day) of the outome, survival:
hazard rate (per day) of the outcome.

## Details

Generates predictions for the population specified in plpData given the
model.

## Examples
