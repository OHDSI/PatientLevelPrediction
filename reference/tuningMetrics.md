# Tuning metrics

Metrics for
[`createHyperparameterSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createHyperparameterSettings.md):

- `aucMetric`: the area under the ROC curve (maximized);

- `auprcMetric`: the area under the precision-recall curve (maximized);

- `logLikelihoodMetric`: the mean held-out Bernoulli log-likelihood per
  row (maximized), with the predicted risks clipped to \[1e-15, 1 -
  1e-15\]. It rewards calibrated risks, and it is defined on folds with
  one class.

## Usage

``` r
aucMetric

auprcMetric

logLikelihoodMetric
```

## Format

A tuning metric, as made by
[`createTuningMetric()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createTuningMetric.md).

## Examples

``` r
settings <- createHyperparameterSettings(tuningMetric = logLikelihoodMetric)
prediction <- data.frame(outcomeCount = c(0, 1, 0, 1), value = c(0.1, 0.8, 0.3, 0.6))
logLikelihoodMetric$fun(prediction)
#> [1] -0.2990012
```
