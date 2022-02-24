test_that("setAdaBoost settings work checks", {
  
  adset <- setAdaBoost(
    nEstimators = list(10,50, 200), 
    learningRate = list(1, 0.5, 0.1), 
    algorithm = list('SAMME.R'),
    seed = sample(1000000,1)
  )
  
  expect_equal(adset$fitFunction, "fitSklearn")
  
  expect_equal(length(adset$param), 3*3*1)
  
  expect_equal(unique(unlist(lapply(adset$param, function(x) x[[1]]))), NULL)
  expect_equal(unique(unlist(lapply(adset$param, function(x) x[[2]]))), c(10,50, 200))
  expect_equal(unique(unlist(lapply(adset$param, function(x) x[[3]]))), c(1, 0.5, 0.1))
  expect_equal(unique(lapply(adset$param, function(x) x[[4]])), list('SAMME.R'))
  
  expect_false(attr(adset$param, 'settings')$requiresDenseMatrix)
  expect_equal(attr(adset$param, 'settings')$name, 'AdaBoost')
  expect_equal(attr(adset$param, 'settings')$pythonImport, 'sklearn')
  expect_equal(attr(adset$param, 'settings')$pythonImportSecond, 'ensemble')
  expect_equal(attr(adset$param, 'settings')$pythonClassifier, "AdaBoostClassifier")
  
  
  inputs <- AdaBoostClassifierInputs(list, adset$param[[1]])
  expect_equal(
    names(inputs), 
    c("base_estimator","n_estimators","learning_rate","algorithm","random_state" )
    )
  
})


test_that("setAdaBoost errors as expected", {
  
  expect_error(setAdaBoost(nEstimators = list(-1)))
  expect_error(setAdaBoost(learningRate =  list(-1)))
  expect_error(setAdaBoost(algorithm =  list(-1)))
  expect_error(setAdaBoost(seed  =  list('seed')))
  
})






test_that("setMLP settings work checks", {
  
  mlpset <- setMLP(
    hiddenLayerSizes = list(c(100), c(20,4)), #must be integers
    activation = list('relu'),
    solver = list('adam'),
    alpha = list(0.3,0.01,0.0001,0.000001), 
    batchSize = list('auto'),
    learningRate = list('constant'),
    learningRateInit = list(0.001),
    powerT = list(0.5),
    maxIter = list(200, 100), 
    shuffle = list(TRUE),
    tol = list(0.0001),
    warmStart = list(TRUE),
    momentum = list(0.9),
    nesterovsMomentum = list(TRUE),
    earlyStopping = list(FALSE),
    validationFraction = list(0.1),
    beta1 = list(0.9), 
    beta2 = list(0.999), 
    epsilon = list(1,0.1,0.00000001), 
    nIterNoChange = list(10),
    seed = sample(100000,1)
  )
  
  expect_equal(mlpset$fitFunction, "fitSklearn")
  
  expect_equal(length(mlpset$param), 2*4*2*3)
  
  expect_equal(unique(lapply(mlpset$param, function(x) x[[1]])), list(c(100), c(20,4)))
  expect_equal(unique(unlist(lapply(mlpset$param, function(x) x[[2]]))), 'relu')
  expect_equal(unique(unlist(lapply(mlpset$param, function(x) x[[4]]))), c(0.3,0.01,0.0001,0.000001))
  expect_equal(unique(lapply(mlpset$param, function(x) x[[9]])), list(200, 100))
  
  expect_false(attr(mlpset$param, 'settings')$requiresDenseMatrix)
  expect_equal(attr(mlpset$param, 'settings')$name, 'Neural Network')
  expect_equal(attr(mlpset$param, 'settings')$pythonImport, 'sklearn')
  expect_equal(attr(mlpset$param, 'settings')$pythonImportSecond, 'neural_network')
  expect_equal(attr(mlpset$param, 'settings')$pythonClassifier, "MLPClassifier")
  
  inputs <- MLPClassifierInputs(list, mlpset$param[[1]])
  expect_equal(
    names(inputs), 
    c("hidden_layer_sizes", "activation", "solver", "alpha", "batch_size",
      "learning_rate", "learning_rate_init", "power_t", "max_iter", "shuffle",
      "random_state", "tol", "verbose", "warm_start", "momentum", "nesterovs_momentum",
      "early_stopping", "validation_fraction", "beta_1", "beta_2", "epsilon",
      "n_iter_no_change" )
  )
})








test_that("setNaiveBayes settings work checks", {
  
  nbset <- setNaiveBayes(
  )
  
  expect_equal(nbset$fitFunction, "fitSklearn")
  
  expect_equal(length(nbset$param), 1)
  
  expect_true(attr(nbset$param, 'settings')$requiresDenseMatrix)
  expect_equal(attr(nbset$param, 'settings')$name, 'Naive Bayes')
  expect_equal(attr(nbset$param, 'settings')$pythonImport, 'sklearn')
  expect_equal(attr(nbset$param, 'settings')$pythonImportSecond, 'naive_bayes')
  expect_equal(attr(nbset$param, 'settings')$pythonClassifier, "GaussianNB")
  
  inputs <- GaussianNBInputs(list, nbset$param[[1]])
  expect_equal(names(inputs),NULL)
  
})







test_that("setRandomForest settings work checks", {
  
  rfset <- setRandomForest(
    ntrees =  list(100,500),
    criterion = list('gini'),
    maxDepth = list(4,10,17),
    minSamplesSplit = list(2,5),
    minSamplesLeaf = list(1,10),
    minWeightFractionLeaf = list(0),
    mtries = list('auto', 'log2'),
    maxLeafNodes = list(NULL),
    minImpurityDecrease = list(0),
    bootstrap = list(TRUE),
    maxSamples = list(NULL, 0.9),
    oobScore = list(FALSE),
    nJobs = list(NULL),
    classWeight = list('balanced_subsample', NULL),
    seed = sample(100000,1)
  )
  
  expect_equal(rfset$fitFunction, "fitSklearn")
  
  expect_equal(length(rfset$param), 2*3*2*2*2*2*2)
  
  expect_equal(unique(lapply(rfset$param, function(x) x[[1]])), list(100,500))
  expect_equal(unique(unlist(lapply(rfset$param, function(x) x[[3]]))), c(4,10,17))
  
  expect_false(attr(rfset$param, 'settings')$requiresDenseMatrix)
  expect_equal(attr(rfset$param, 'settings')$name, 'Random forest')
  expect_equal(attr(rfset$param, 'settings')$pythonImport, 'sklearn')
  expect_equal(attr(rfset$param, 'settings')$pythonImportSecond, 'ensemble')
  expect_equal(attr(rfset$param, 'settings')$pythonClassifier, "RandomForestClassifier")
  
  inputs <- RandomForestClassifierInputs(list,  rfset$param[[1]])
  expect_equal(
    names(inputs), 
    c("n_estimators", "criterion", "max_depth", "min_samples_split", "min_samples_leaf",
      "min_weight_fraction_leaf", "max_features", "max_leaf_nodes", "min_impurity_decrease",
      "bootstrap", "max_samples", "oob_score", "n_jobs", "random_state", "verbose",
      "warm_start","class_weight")
  )
})




test_that("setSVM  settings work checks", {
  
  svmset <- setSVM (
    C = list(1,0.9,2,0.1), 
    kernel = list('rbf'),
    degree = list(1,3,5), 
    gamma = list('scale', 1e-04, 3e-05, 0.001, 0.01, 0.25),
    coef0 = list(0.0),
    shrinking = list(TRUE), 
    tol = list(0.001),
    classWeight = list('balanced', NULL), 
    cacheSize  = 500,
    seed = sample(100000,1)
  )
  
  expect_equal(svmset$fitFunction, "fitSklearn")
  
  expect_equal(length(svmset$param), 4*3*6*2)
  
  expect_equal(unique(lapply(svmset$param, function(x) x[[4]])), list('scale', 1e-04, 3e-05, 0.001, 0.01, 0.25))
  expect_equal(unique(unlist(lapply(svmset$param, function(x) x[[1]]))), c(1,0.9,2,0.1))
  
  expect_false(attr(svmset$param, 'settings')$requiresDenseMatrix)
  expect_equal(attr(svmset$param, 'settings')$name, 'Support Vector Machine')
  expect_equal(attr(svmset$param, 'settings')$pythonImport, 'sklearn')
  expect_equal(attr(svmset$param, 'settings')$pythonImportSecond, 'svm')
  expect_equal(attr(svmset$param, 'settings')$pythonClassifier, "SVC")
  
  inputs <- SVCInputs(list,  svmset$param[[1]])
  expect_equal(
    names(inputs), 
    c("C", "kernel", "degree", "gamma", "coef0", 
      "shrinking", "probability", "tol", "cache_size", 
      "class_weight", "verbose", "max_iter", "decision_function_shape",
      "break_ties", "random_state")
  )
})
