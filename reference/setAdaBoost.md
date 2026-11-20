# Create setting for AdaBoost with python DecisionTreeClassifier base estimator

Create setting for AdaBoost with python DecisionTreeClassifier base
estimator

## Usage

``` r
setAdaBoost(
  nEstimators = list(10, 50, 200),
  learningRate = list(1, 0.5, 0.1),
  seed = sample(1e+06, 1)
)
```

## Arguments

- nEstimators:

  (list) The maximum number of estimators at which boosting is
  terminated. In case of perfect fit, the learning procedure is stopped
  early.

- learningRate:

  (list) Weight applied to each classifier at each boosting iteration. A
  higher learning rate increases the contribution of each classifier.
  There is a trade-off between the learningRate and nEstimators
  parameters There is a trade-off between learningRate and nEstimators.

- seed:

  A seed for the model

## Value

a modelSettings object

## Examples
