# Create setting for gradient boosting machine model using gbm_xgboost implementation

Create setting for gradient boosting machine model using gbm_xgboost
implementation

## Usage

``` r
setGradientBoostingMachine(
  ntrees = c(100, 300),
  nthread = 20,
  earlyStopRound = 25,
  maxDepth = c(4, 6, 8),
  minChildWeight = 1,
  learnRate = c(0.05, 0.1, 0.3),
  scalePosWeight = 1,
  lambda = 1,
  alpha = 0,
  seed = sample(1e+07, 1)
)
```

## Arguments

- ntrees:

  The number of trees to build

- nthread:

  The number of computer threads to use (how many cores do you have?)

- earlyStopRound:

  If the performance does not increase over earlyStopRound number of
  trees then training stops (this prevents overfitting)

- maxDepth:

  Maximum depth of each tree - a large value will lead to slow model
  training

- minChildWeight:

  Minimum sum of of instance weight in a child node - larger values are
  more conservative

- learnRate:

  The boosting learn rate

- scalePosWeight:

  Controls weight of positive class in loss - useful for imbalanced
  classes

- lambda:

  L2 regularization on weights - larger is more conservative

- alpha:

  L1 regularization on weights - larger is more conservative

- seed:

  An option to add a seed when training the final model

## Value

A modelSettings object that can be used to fit the model

## Examples

``` r
modelGbm <- setGradientBoostingMachine(
  ntrees = c(10, 100), nthread = 20,
  maxDepth = c(4, 6), learnRate = c(0.1, 0.3)
)
```
