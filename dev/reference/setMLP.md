# Create setting for neural network model with python's scikit-learn. For bigger models, consider using `DeepPatientLevelPrediction` package.

Create setting for neural network model with python's scikit-learn. For
bigger models, consider using `DeepPatientLevelPrediction` package.

## Usage

``` r
setMLP(
  hiddenLayerSizes = list(c(100), c(20)),
  activation = list("relu"),
  solver = list("adam"),
  alpha = list(0.3, 0.01, 1e-04, 1e-06),
  batchSize = list("auto"),
  learningRate = list("constant"),
  learningRateInit = list(0.001),
  powerT = list(0.5),
  maxIter = list(200, 100),
  shuffle = list(TRUE),
  tol = list(1e-04),
  warmStart = list(TRUE),
  momentum = list(0.9),
  nesterovsMomentum = list(TRUE),
  earlyStopping = list(FALSE),
  validationFraction = list(0.1),
  beta1 = list(0.9),
  beta2 = list(0.999),
  epsilon = list(1e-08),
  nIterNoChange = list(10),
  seed = sample(1e+05, 1)
)
```

## Arguments

- hiddenLayerSizes:

  (list of vectors) The ith element represents the number of neurons in
  the ith hidden layer.

- activation:

  (list) Activation function for the hidden layer.

  - "identity": no-op activation, useful to implement linear bottleneck,
    returns f(x) = x

  - "logistic": the logistic sigmoid function, returns f(x) = 1 / (1 +
    exp(-x)).

  - "tanh": the hyperbolic tan function, returns f(x) = tanh(x).

  - "relu": the rectified linear unit function, returns f(x) = max(0, x)

- solver:

  (list) The solver for weight optimization. (‘lbfgs’, ‘sgd’, ‘adam’)

- alpha:

  (list) L2 penalty (regularization term) parameter.

- batchSize:

  (list) Size of minibatches for stochastic optimizers. If the solver is
  ‘lbfgs’, the classifier will not use minibatch. When set to “auto”,
  batchSize=min(200, n_samples).

- learningRate:

  (list) Only used when solver='sgd' Learning rate schedule for weight
  updates. ‘constant’, ‘invscaling’, ‘adaptive’, default=’constant’

- learningRateInit:

  (list) Only used when solver=’sgd’ or ‘adam’. The initial learning
  rate used. It controls the step-size in updating the weights.

- powerT:

  (list) Only used when solver=’sgd’. The exponent for inverse scaling
  learning rate. It is used in updating effective learning rate when the
  learning_rate is set to ‘invscaling’.

- maxIter:

  (list) Maximum number of iterations. The solver iterates until
  convergence (determined by ‘tol’) or this number of iterations. For
  stochastic solvers (‘sgd’, ‘adam’), note that this determines the
  number of epochs (how many times each data point will be used), not
  the number of gradient steps.

- shuffle:

  (list) boolean: Whether to shuffle samples in each iteration. Only
  used when solver=’sgd’ or ‘adam’.

- tol:

  (list) Tolerance for the optimization. When the loss or score is not
  improving by at least tol for nIterNoChange consecutive iterations,
  unless learning_rate is set to ‘adaptive’, convergence is considered
  to be reached and training stops.

- warmStart:

  (list) When set to True, reuse the solution of the previous call to
  fit as initialization, otherwise, just erase the previous solution.

- momentum:

  (list) Momentum for gradient descent update. Should be between 0
  and 1. Only used when solver=’sgd’.

- nesterovsMomentum:

  (list) Whether to use Nesterov’s momentum. Only used when solver=’sgd’
  and momentum \> 0.

- earlyStopping:

  (list) boolean Whether to use early stopping to terminate training
  when validation score is not improving. If set to true, it will
  automatically set aside 10 percent of training data as validation and
  terminate training when validation score is not improving by at least
  tol for n_iter_no_change consecutive epochs.

- validationFraction:

  (list) The proportion of training data to set aside as validation set
  for early stopping. Must be between 0 and 1. Only used if
  earlyStopping is True.

- beta1:

  (list) Exponential decay rate for estimates of first moment vector in
  adam, should be in 0 to 1.

- beta2:

  (list) Exponential decay rate for estimates of second moment vector in
  adam, should be in 0 to 1.

- epsilon:

  (list) Value for numerical stability in adam.

- nIterNoChange:

  (list) Maximum number of epochs to not meet tol improvement. Only
  effective when solver=’sgd’ or ‘adam’.

- seed:

  A seed for the model

## Value

a modelSettings object

## Examples
