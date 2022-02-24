#' Create setting for lasso logistic regression
#'
#' @param variance   	Numeric: prior distribution starting variance
#' @param seed       An option to add a seed when training the model
#' @param includeCovariateIds a set of covariate IDS to limit the analysis to
#' @param noShrinkage a set of covariates whcih are to be forced to be included in the final model. default is the intercept 
#' @param threads    An option to set number of threads when training model
#' @param forceIntercept  	Logical: Force intercept coefficient into prior
#' @param upperLimit  	Numeric: Upper prior variance limit for grid-search
#' @param lowerLimit  	Numeric: Lower prior variance limit for grid-search
#' @param tolerance   Numeric: maximum relative change in convergence criterion from successive iterations to achieve convergence
#' @param maxIterations 	Integer: maximum iterations of Cyclops to attempt before returning a failed-to-converge error
#' 
#' @examples
#' model.lr <- setLassoLogisticRegression()
#' @export
setLassoLogisticRegression<- function(
  variance = 0.01, 
  seed = NULL, 
  includeCovariateIds = c(), 
  noShrinkage = c(0), 
  threads = -1, 
  forceIntercept = F,
  upperLimit = 20, 
  lowerLimit = 0.01,
  tolerance = 2e-06,
  maxIterations = 3000){
  
  checkIsClass(seed, c('numeric','NULL','integer'))
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  checkIsClass(threads, c('numeric','integer'))
  checkIsClass(variance, c('numeric','integer'))
  checkHigherEqual(variance, 0)
  
  checkIsClass(lowerLimit, c('numeric','integer'))
  checkIsClass(upperLimit, c('numeric','integer'))

  checkHigherEqual(upperLimit, lowerLimit)
    
  param <- list(
    priorParams = list(
      priorType =  "laplace",
      forceIntercept = forceIntercept,
      variance = variance, 
      exclude = noShrinkage
      ),
    includeCovariateIds = includeCovariateIds, 
    upperLimit = upperLimit, 
    lowerLimit = lowerLimit
    )
  
  attr(param, 'settings') <- list(
    priorfunction = 'Cyclops::createPrior',
    selectorType = "byPid",  # is this correct?
    crossValidationInPrior = T,
    modelType = 'logistic',
    addIntercept = T,
    useControl = T,
    seed = seed[1],
    name = "Lasso Logistic Regression",
    threads = threads[1], 
    tolerance = tolerance[1], #2e-06
    cvRepetitions = 1, #1
    maxIterations = maxIterations[1] #3000
  )
  
  attr(param, 'modelType') <- 'binary' 
  attr(param, 'saveType') <- 'RtoJson'
  
  result <- list(
    fitFunction = "fitCyclopsModel",
    param = param
  )
  class(result) <- "modelSettings"
  
  return(result)
}



#' Create setting for lasso Cox model
#'
#' @param variance   	Numeric: prior distribution starting variance
#' @param seed       An option to add a seed when training the model
#' @param includeCovariateIds a set of covariate IDS to limit the analysis to
#' @param noShrinkage a set of covariates whcih are to be forced to be included in the final model. default is the intercept 
#' @param threads    An option to set number of threads when training model
#' @param upperLimit  	Numeric: Upper prior variance limit for grid-search
#' @param lowerLimit  	Numeric: Lower prior variance limit for grid-search
#' @param tolerance   Numeric: maximum relative change in convergence criterion from successive iterations to achieve convergence
#' @param maxIterations 	Integer: maximum iterations of Cyclops to attempt before returning a failed-to-converge error
#'
#' @examples
#' model.lr <- setCoxModel()
#' @export
setCoxModel <- function(
  variance = 0.01, 
  seed = NULL, 
  includeCovariateIds = c(), 
  noShrinkage = c(), 
  threads = -1, 
  upperLimit = 20, 
  lowerLimit = 0.01,
  tolerance = 2e-07,
  maxIterations = 3000
){
  
  ensure_installed("survAUC")

  checkIsClass(seed, c('numeric','NULL','integer'))
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  checkIsClass(threads, c('numeric','integer'))
  checkIsClass(variance, c('numeric','integer'))
  checkHigherEqual(variance, 0)
  
  checkIsClass(lowerLimit, c('numeric','integer'))
  checkIsClass(upperLimit, c('numeric','integer'))
  
  checkHigherEqual(upperLimit, lowerLimit)
  
  #selectorType = "byRow",

  param <- list(
    priorParams = list(
      priorType =  "laplace",
      variance = variance, 
      exclude = noShrinkage
    ),
    includeCovariateIds = includeCovariateIds, 
    upperLimit = upperLimit, 
    lowerLimit = lowerLimit
  )
  
  attr(param, 'settings') <- list(
    priorfunction = 'Cyclops::createPrior',
    selectorType = "byRow",
    crossValidationInPrior = T,
    modelType = 'cox',
    addIntercept = F,
    useControl = T,
    seed = seed[1],
    name = "LASSO Cox Regression",
    threads = threads[1], 
    tolerance = tolerance[1], #2e-07
    cvRepetitions = 1, #1
    maxIterations = maxIterations[1] #3000
  )
  
  attr(param, 'modelType') <- 'survival' 
  attr(param, 'saveType') <- 'RtoJson'
  
  result <- list(
    fitFunction = "fitCyclopsModel",
    param = param
  )
  class(result) <- "modelSettings"
  
  return(result)
}


#' Create setting for lasso logistic regression
#'  
#' @param K              The maximum number of non-zero predictors
#' @param penalty        Specifies the IHT penalty; possible values are `BIC` or `AIC` or a numeric value
#' @param seed           An option to add a seed when training the model
#' @param exclude        A vector of numbers or covariateId names to exclude from prior
#' @param forceIntercept Logical: Force intercept coefficient into regularization
#' @param fitBestSubset  Logical: Fit final subset with no regularization 
#' @param initialRidgeVariance  integer
#' @param tolerance      numeric
#' @param maxIterations  integer
#' @param threshold      numeric
#' @param delta          numeric
#'
#' @examples
#' model.lr <- setLassoLogisticRegression()
#' @export
setIterativeHardThresholding<- function(
  K = 10, 
  penalty = "bic", 
  seed = sample(100000,1), 
  exclude = c(), 
  forceIntercept = F,
  fitBestSubset = FALSE,
  initialRidgeVariance = 10000,
  tolerance = 1e-08,
  maxIterations = 10000,
  threshold = 1e-06, 
  delta = 0
  ){
  
  ensure_installed("IterativeHardThresholding")
  
  if(K<1)
    stop('Invalid maximum number of predictors')
  if(!(penalty %in% c("aic", "bic") || is.numeric(penalty)))
    stop('Penalty must be "aic", "bic" or numeric')
  if(!is.logical(forceIntercept))
    stop("forceIntercept must be of type: logical")
  if(!is.logical(fitBestSubset))
    stop("fitBestSubset must be of type: logical")
  if(!class(seed)%in%c('numeric','NULL','integer'))
    stop('Invalid seed')


  # set seed
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  
  param <- list(
    priorParams = list(
      K = K,
      penalty = penalty, 
      exclude = exclude,
      forceIntercept = forceIntercept,
      fitBestSubset = fitBestSubset,
      initialRidgeVariance = initialRidgeVariance,
      tolerance = tolerance[1], 
      maxIterations = maxIterations[1], 
      threshold = threshold, 
      delta = delta
    )
  )
  
  attr(param, 'settings') <- list(
    priorfunction = 'IterativeHardThresholding::createIhtPrior',
    selectorType = "byRow",
    crossValidationInPrior = F,
    modelType = 'logistic',
    addIntercept = F,
    useControl = F,
    seed = seed[1],
    name = "Iterative Hard Thresholding"
  )
  
  attr(param, 'modelType') <- 'binary' 
  attr(param, 'saveType') <- 'RtoJson'
  
  result <- list(
    fitFunction = "fitCyclopsModel",
    param = param
  )
  class(result) <- "modelSettings"
  
  return(result)
}

