# @file PythonClassifier.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Create setting for AdaBoost with python
#' @param nEstimators    The maximum number of estimators at which boosting is terminated
#' @param learningRate   Learning rate shrinks the contribution of each classifier by learningRate.
#'                       There is a trade-off between learningRate and nEstimators .
#' @param seed           A seed for the model
#'
#' @examples
#' \dontrun{
#' model.adaBoost <- setAdaBoost(size = 4, alpha = 1e-05, seed = NULL)
#' }
#' @export
setAdaBoost <- function(nEstimators = 50, learningRate = 1, seed = NULL) {
  
  if (!class(seed) %in% c("numeric", "NULL", "integer"))
    stop("Invalid seed")
  if (!class(nEstimators) %in% c("numeric", "integer"))
    stop("nEstimators must be a numeric value >0 ")
  if (min(nEstimators) < 1)
    stop("nEstimators must be greater that 0 or -1")
  if (!class(learningRate) %in% c("numeric", "integer"))
    stop("learningRate must be a numeric value >0 and <=1")
  if (max(learningRate) > 1)
    stop("learningRate must be less that or equal to 1")
  if (min(learningRate) < 0)
    stop("learningRate must be a numeric value >0")
  
  # test python is available and the required dependancies are there:
  ##checkPython()
  
  # set seed
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  
  param <- split(
    x = expand.grid(
      nEstimators = nEstimators,
      learningRate = learningRate
    ), 
    f = 1:(length(nEstimators) * length(learningRate))
  )
  
  attr(param, 'settings') <- list(
    seed = seed[1],
    requiresDenseMatrix = F,
    name = "AdaBoost",
    pythonImport = 'sklearn',
    pythonImportSecond = 'ensemble',
    pythonClassifier = 'AdaBoostClassifier'
  )
  
  result <- list(
    fitFunction = "fitSklearn",
    param = param
  )
  class(result) <- "modelSettings"
  
  return(result)
}

#' Create setting for DecisionTree with python 
#' @param maxDepth    The maximum depth of the tree
#' @param minSamplesSplit    The minimum samples per split
#' @param minSamplesLeaf    The minimum number of samples per leaf
#' @param minImpurityDecrease  Threshold for early stopping in tree growth. A node will split if its impurity is above the threshold, otherwise it is a leaf. 
#' @param classWeight        Balance or None
#' @param seed                The random state seed
#' @param plot                Boolean whether to plot the tree (requires python pydotplus module)
#'
#' @examples
#' \dontrun{
#' model.decisionTree <- setDecisionTree(maxDepth=10,minSamplesLeaf=10, seed=NULL )
#' }
#' @export
setDecisionTree <- function(maxDepth=10 ,minSamplesSplit=2 ,minSamplesLeaf=10,
  minImpurityDecrease=10^-7,seed =NULL, classWeight='None', 
  plot=F  ){
  if(!class(seed)%in%c('numeric','NULL', 'integer'))
    stop('Invalid seed')
  if(!class(maxDepth) %in% c("numeric", "integer"))
    stop('maxDepth must be a numeric value >0 ')
  if(min(maxDepth) < 1)
    stop('maxDepth must be greater that 0 or -1')
  if(!class(minSamplesSplit) %in% c("numeric", "integer") )
    stop('minSamplesSplit must be a numeric value >1')
  if(min(minSamplesSplit) < 2)
    stop('minSamplesSplit must be greater that 1')
  if(!class(minSamplesLeaf) %in% c("numeric", "integer"))
    stop('minSamplesLeaf must be a numeric value >0')
  if(min(minSamplesLeaf) < 1)
    stop('minSamplesLeaf must be greater that 0')
  if(class(minImpurityDecrease)!='numeric')
    stop('minImpurityDecrease must be a numeric value >0 ')
  if(min(minImpurityDecrease) <= 0)
    stop('minImpurityDecrease must be greater that 0')
  if(class(classWeight) !='character')
    stop('classWeight must be a character of either None or balanced')
  if(sum(!classWeight%in%c('None','balanced'))!=0)
    stop('classWeight must be a character of either None or balanced')
  if(class(plot) !='logical')
    stop('Plot must be logical')
  
  # test python is available and the required dependancies are there:
  ##checkPython()
  
  # set seed
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  
  
  param <- split(
    x = expand.grid(
      maxDepth=maxDepth, 
      minSamplesSplit=minSamplesSplit,
      minSamplesLeaf=minSamplesLeaf,
      minImpurityDecrease=minImpurityDecrease,
      classWeight=classWeight
    ), 
    f = 1:(length(classWeight)*length(maxDepth)*length(minSamplesSplit)*length(minSamplesLeaf)*length(minImpurityDecrease))
  )
  
  attr(param, 'settings') <- list(
    seed = seed[1],
    requiresDenseMatrix = F,
    name = "Decision Tree",
    pythonImport = 'sklearn',
    pythonImportSecond = 'tree',
    pythonClassifier = 'DecisionTreeClassifier'
  )
  
  result <- list(
    fitFunction = "fitSklearn",
    param = param
  )
  class(result) <- "modelSettings"
  
  return(result)
}


#' Create setting for neural network model with python 
#' @param size       The number of hidden nodes
#' @param alpha      The l2 regularisation
#' @param maxIter    Maximum number of iterations. The solver iterates until convergence (determined by ‘tol’) or this number of iterations.
#' @param tol        Tolerance for the optimization
#' @param learningRateInit  The initial learning rate used. It controls the step-size in updating the weights.
#' @param nIterNoChange     Maximum number of epochs to not meet tol improvement.
#' @param beta1    Exponential decay rate for estimates of first moment vector in adam, should be in [0, 1).
#' @param beta2    Exponential decay rate for estimates of second moment vector in adam, should be in [0, 1).
#' @param epsilon  Value for numerical stability in adam.
#' @param seed       A seed for the model 
#'
#' @examples
#' \dontrun{
#' model.mlp <- setMLP(size=4, alpha=0.00001, seed=NULL)
#' }
#' @export
setMLP <- function(size=4, alpha=c(0.3,0.01,0.001,0.000001), maxIter = 2000, tol = 0.0001, 
  learningRateInit = 0.001, nIterNoChange = 10,
  beta1 = 0.9, beta2 = 0.999, epsilon = c(1,0.1,0.00000001), seed=NULL){
  
  if(!class(seed)%in%c('numeric','NULL','integer'))
    stop('Invalid seed')
  if(!class(size) %in% c("numeric", "integer"))
    stop('size must be a numeric value >0 ')
  if(min(size) < 1)
    stop('size must be greater that 0')
  if(!class(alpha) %in% c("numeric", "integer"))
    stop('alpha must be a numeric value >0')
  if(min(alpha) <= 0)
    stop('alpha must be greater that 0')
  
  if(!class(maxIter)%in%c('numeric','integer'))
    stop('Invalid maxIter')
  if(!class(tol)%in%c('numeric','integer'))
    stop('Invalid tol')
  if(!class(learningRateInit)%in%c('numeric','integer'))
    stop('Invalid learningRateInit')
  if(!class(nIterNoChange)%in%c('numeric','integer'))
    stop('Invalid nIterNoChange') 
  
  if(!class(beta1)%in%c('numeric','integer'))
    stop('Invalid beta1') 
  if(!class(beta2)%in%c('numeric','integer'))
    stop('Invalid beta2') 
  if(!class(epsilon)%in%c('numeric','integer'))
    stop('Invalid epsilon') 
  
  # set seed
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  
  param <- split(
    x = expand.grid(
      size = size, 
      alpha = alpha,
      maxIter = maxIter,
      tol = tol,
      learningRateInit = learningRateInit,
      nIterNoChange = nIterNoChange,
      beta1 = beta1,
      beta2 = beta2,
      epsilon = epsilon
    ), 
    f = 1:(length(size)*length(alpha)*length(maxIter)*length(tol)*length(learningRateInit)*length(nIterNoChange)*length(beta1)*length(beta2)*length(epsilon))  
  )
  
  attr(param, 'settings') <- list(
    seed = seed[1],
    requiresDenseMatrix = F,
    name = "Neural Network",
    pythonImport = 'sklearn',
    pythonImportSecond = 'neural_network',
    pythonClassifier = 'MLPClassifier'
  )
  
  result <- list(
    fitFunction = "fitSklearn",
    param = param
  )
  class(result) <- "modelSettings"
  
  return(result)
}



#' Create setting for naive bayes model with python 
#' @param variableNumber   The number of variables selected by feature selection prior to training the model (this is required due to Naive Bayes requring a non sparse matrix)
#'
#' @examples
#' \dontrun{
#' model.nb <- setNaiveBayes()
#' }
#' @export
setNaiveBayes <- function(variableNumber=2000){
  
  if(length(variableNumber)!=1)
    stop('Can only currently enter a single value for variableNumber')
  if(!class(variableNumber) %in% c("numeric", "integer"))
    stop('Can incorrect class for variableNumber - must be numeric')
  
  # test python is available and the required dependancies are there:
  ##checkPython()
  
  param <- split(
    x = expand.grid(
      ariableNumber = variableNumber
    ), 
    f = 1:(length(variableNumber))  
  )
  
  attr(param, 'settings') <- list(
    seed = seed[1],
    requiresDenseMatrix = T,
    name = "Naive Bayes",
    pythonImport = 'sklearn',
    pythonImportSecond = 'naive_baye',
    pythonClassifier = 'GaussianNB'
  )
  
  result <- list(
    fitFunction = "fitSklearn",
    param = param
  )
  class(result) <- "modelSettings"
  
  return(result)
}


#' Create setting for random forest model with python (very fast)
#'
#' @param mtries     The number of features to include in each tree (-1 defaults to square root of total features)
#' @param ntrees     The number of trees to build 
#' @param maxDepth  Maximum number of interactions - a large value will lead to slow model training
#' @param seed       An option to add a seed when training the final model
#'
#' @examples
#' \dontrun{
#' model.rf <- setRandomForest(mtries=c(-1,5,20),  ntrees=c(10,100), 
#'                            maxDepth=c(5,20))
#' }                           
#' @export
setRandomForest <- function(mtries=-1,ntrees=500,maxDepth=c(4,10,17), varImp=T, seed=NULL){
  # check seed is int
  if(!class(seed)%in%c('numeric','NULL','integer'))
    stop('Invalid seed')
  if(!class(ntrees) %in% c("numeric", "integer"))
    stop('ntrees must be a numeric value >0')
  if(sum(ntrees < 0)>0)
    stop('mtries must be greater that 0')
  if(!class(mtries) %in% c("numeric", "integer"))
    stop('mtries must be a numeric value >1 or -1')
  if(sum(mtries < -1)>0)
    stop('mtries must be greater that 0 or -1')
  if(!class(maxDepth) %in% c("numeric", "integer"))
    stop('maxDepth must be a numeric value >0')
  if(sum(maxDepth < 1)>0)
    stop('maxDepth must be greater that 0')
  if(class(varImp)!="logical")
    stop('varImp must be boolean')
  
  # set seed
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  
  param <- split(
    x = expand.grid(
      ntrees = ntrees, 
      mtries = mtries,
      maxDepth = maxDepth, 
      varImp = varImp
    ), 
    f = 1:(length(ntrees)*length(mtries)*length(maxDepth)*length(varImp))  
  )
  
  attr(param, 'settings') <- list(
    seed = seed[1],
    requiresDenseMatrix = F,
    name = "Random forest",
    pythonImport = 'sklearn',
    pythonImportSecond = 'ensemble',
    pythonClassifier = 'RandomForestClassifier'
  ) 
  
  result <- list(
    fitFunction = "fitSklearn",
    param = param
  )
  class(result) <- "modelSettings"
  
  return(result)
}


#' Create setting for SVM with python
#' @param kernel        Specifies the kernel type to be used in the algorithm. one of ‘linear’, ‘poly’, ‘rbf’, ‘sigmoid’, ‘precomputed’. If none is given ‘rbf’ will be used.
#' @param C             penalty parameter C of the error term.        
#' @param degree        degree of kernel function is significant only in poly, rbf, sigmoid
#' @param gamma         kernel coefficient for rbf and poly, by default 1/n_features will be taken.
#' @param shrinking     wether to use the shrinking heuristic.
#' @param coef0         independent term in kernel function. It is only significant in poly/sigmoid.
#' @param classWeight   Class weight based on imbalance either 'balanced' or 'none'
#' @param seed           A seed for the model
#'
#' @examples
#' \dontrun{
#' model.svm <- setSVM(kernel='rbf', seed = NULL)
#' }
#' @export
setSVM <- function(kernel='rbf', C=c(1,0.9,2,0.1), degree=c(1,3,5), 
  gamma=c(1e-04, 3e-05, 0.001, 0.01,0.25),
  shrinking = T, coef0=0.0,
  classWeight = 'balanced', seed = NULL) {
  
  if (!class(seed) %in% c("numeric", "NULL", "integer"))
    stop("Invalid seed")
  if (!kernel %in% c("rbf", 'linear', 'poly', 'sigmoid', 'precomputed'))
    stop("Invalid kernel")
  if (!class(C) %in% c("numeric", "integer"))
    stop("C must be a numeric value >0 ")
  if (min(C) < 0)
    stop("C must be greater than 0")
  if (!class(degree) %in% c("numeric", "integer"))
    stop("degree must be an integer")
  if (!class(gamma) %in% c("numeric", "integer"))
    stop("gamma must be a numeric value >0 ")
  if (min(gamma) < 0)
    stop("gamma must be greater than 0")
  if (!class(shrinking) %in% c("logical"))
    stop("shrinking must be T or F ")
  if (!class(coef0) %in% c("numeric", "integer"))
    stop("coef0 must be a numeric value ")
  if(sum(classWeight%in%c('none','balanced'))!=length(classWeight)){
    stop("classWeight must be 'balanced' or 'none'  ")
  }
  
  
  # set seed
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  
  param <- split(
    x = expand.grid(
      kernel = kernel,
      C = C,
      degree = degree,
      gamma = gamma,
      shrinking = shrinking,
      coef0 = coef0,
      classWeight = classWeight
    ), 
    f = 1:(length(kernel) * length(C) * length(degree) * length(gamma) * length(shrinking) * length(coef0) * length(classWeight) ) 
  )
  
  attr(param, 'settings') <- list(
    seed = seed[1],
    requiresDenseMatrix = F,
    name = "Support Vector Machine",
    pythonImport = 'sklearn',
    pythonImportSecond = 'svm',
    pythonClassifier = 'SVC'
  ) 
 
  result <- list(
    fitFunction = "fitSklearn",
    param = param
  )
  class(result) <- "modelSettings"
  
  return(result)
}