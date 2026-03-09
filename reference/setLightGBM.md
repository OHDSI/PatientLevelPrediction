# Create setting for gradient boosting machine model using lightGBM (https://github.com/microsoft/LightGBM/tree/master/R-package).

Create setting for gradient boosting machine model using lightGBM
(https://github.com/microsoft/LightGBM/tree/master/R-package).

## Usage

``` r
setLightGBM(
  nthread = 20,
  earlyStopRound = 25,
  numIterations = c(100),
  numLeaves = c(31),
  maxDepth = c(5, 10),
  minDataInLeaf = c(20),
  learningRate = c(0.05, 0.1, 0.3),
  lambdaL1 = c(0),
  lambdaL2 = c(0),
  scalePosWeight = 1,
  isUnbalance = FALSE,
  seed = sample(1e+07, 1)
)
```

## Arguments

- nthread:

  The number of computer threads to use (how many cores do you have?)

- earlyStopRound:

  If the performance does not increase over earlyStopRound number of
  trees then training stops (this prevents overfitting)

- numIterations:

  Number of boosting iterations.

- numLeaves:

  This hyperparameter sets the maximum number of leaves. Increasing this
  parameter can lead to higher model complexity and potential
  overfitting.

- maxDepth:

  This hyperparameter sets the maximum depth . Increasing this parameter
  can also lead to higher model complexity and potential overfitting.

- minDataInLeaf:

  This hyperparameter sets the minimum number of data points that must
  be present in a leaf node. Increasing this parameter can help to
  reduce overfitting

- learningRate:

  This hyperparameter controls the step size at each iteration of the
  gradient descent algorithm. Lower values can lead to slower
  convergence but may result in better performance.

- lambdaL1:

  This hyperparameter controls L1 regularization, which can help to
  reduce overfitting by encouraging sparse models.

- lambdaL2:

  This hyperparameter controls L2 regularization, which can also help to
  reduce overfitting by discouraging large weights in the model.

- scalePosWeight:

  Controls weight of positive class in loss - useful for imbalanced
  classes

- isUnbalance:

  This parameter cannot be used at the same time with scalePosWeight,
  choose only one of them. While enabling this should increase the
  overall performance metric of your model, it will also result in poor
  estimates of the individual class probabilities.

- seed:

  An option to add a seed when training the final model

## Value

A list of settings that can be used to train a model with `runPlp`

## Examples

``` r
modelLightGbm <- setLightGBM(
  numLeaves = c(20, 31, 50), maxDepth = c(-1, 5, 10),
  minDataInLeaf = c(10, 20, 30), learningRate = c(0.05, 0.1, 0.3)
)
```
