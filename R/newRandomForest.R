#library(R6)
HyperparameterNtrees <- R6::R6Class("HyperparameterNtrees", list(
  min = NULL,
  max = NULL,
  randomSample = 10,
  gridValues = list(),
  
  initialize = function(
    min = 1,
    max = 100000000,
    gridValues = list(100,500)
  ) {
    stopifnot(is.numeric(min), length(min) == 1, min > 0)
    stopifnot(is.numeric(max) || is.null(max), length(max) == 1 || is.null(max), max > 0 || is.null(max))
    stopifnot(is.list(gridValues))
    
    self$min <-  min
    self$max <- max
    self$gridValues <- gridValues
  },
  
  setMin = function(
    min = 1
  ) {
    self$min <-  min
  },
  
  setMax = function(
    max = 100000000
  ) {
    self$max <-  max
  },
  
  addGridValue = function(
    value 
  ) {
    self$gridValues <-  c(self$gridValues, value )
  },
  
  print = function(...) {
    cat("Ntrees: \n")
    cat("  min: ", self$min, "\n", sep = "")
    cat("  max:  ", self$max, "\n", sep = "")
    cat("  gridValues:  ", paste(self$gridValues, collapse = ','), "\n", sep = "")
    invisible(self)
  }
))


fitSklearnBase <- function(
    data, 
    trainIndexes,
    validationIndexes,
    seed, 
    requiresDenseMatrix, 
    modelName,
    pythonImport,
    pythonImportSecond,
    pythonClassifier,
    modelLocation,
    hyperparameters,
    saveToJson,
    returnPredictionOnly
)
{
  
  ParallelLogger::logInfo(paste0("Running ",modelName," model"))
  
  np <- reticulate::import('numpy')
  os <- reticulate::import('os')
  sys <- reticulate::import('sys')
  math <- reticulate::import('math')
  scipy <- reticulate::import('scipy')
  joblib <- reticulate::import('joblib')
  firstImport <- reticulate::import(pythonImport, convert=FALSE)
  
  if(!is.null(pythonImportSecond)){
    classifier <- firstImport[[pythonImportSecond]][[pythonClassifier]]
  } else{
    classifier <- firstImport[[pythonClassifier]]
  }
  
  ###########################################################################
  
    # initiate prediction
    trainY <- reticulate::r_to_py(data$labels$outcomeCount[trainIndexes])
    trainX <- reticulate::r_to_py(data$matrix[trainIndexes,])
    validationX <- reticulate::r_to_py(data$matrix[validationIndexes,])
    validationY <- data$labels[validationIndexes,]
    
      if(requiresDenseMatrix){
        ParallelLogger::logInfo('Converting sparse martix to dense matrix (CV)')
        trainX <- trainX$toarray()
        testX <- testX$toarray()
      }
      
      model <- fitPythonModel(classifier, hyperparameters, seed, trainX, trainY, np, pythonClassifier)
      
      ParallelLogger::logInfo("Calculating predictions on validation...")
      prediction <- predictValues(
        model = model, 
        data = validationX, 
        cohort = validationY, 
        type = 'binary'
        )

      if(returnPredictionOnly){
        return(prediction)
      }
      
  # saving model
  if(!dir.exists(file.path(modelLocation))){
    dir.create(file.path(modelLocation), recursive = T)
  }
  if(saveToJson){
    sklearnToJson(model=model,
                  path=file.path(modelLocation,"model.json"))
  } else{
    joblib$dump(model, file.path(modelLocation,"model.pkl"), compress = T) 
  }
  
  # feature importance
  covariateImportance <- data$covariateRef
  importanceValues <- tryCatch({reticulate::py_to_r(model$feature_importances_)}, error = function(e){ParallelLogger::logInfo(e);return(rep(1,ncol(matrixData)))})
  importanceValues[is.na(importanceValues)] <- 0
  covariateImportance$included <- 1
  #covariateImportance$included <- 0
  #covariateImportance$included[importanceValues > 0 ] <- 1
  covariateImportance$covariateValue <- unlist(importanceValues)
  
  
  return(
    list(
      prediction = prediction,
      covariateImportance = covariateImportance
    )
  )
  
}

predictSklearnBase <- function(
    plpModelLocation, #self
    covariateMap, # self
    covariateImportance, #self
    requiresDenseMatrix, # self
    saveToJson, #self
    data, 
    cohort
){
  
  if(inherits(data, 'plpData')){
    # convert
    matrixObjects <- toSparseM(
      plpData = data, 
      cohort = cohort,
      map = covariateMap
    )
    
    newData <- matrixObjects$matrix
    cohort <- matrixObjects$labels
    
  }else{
    newData <- data
  }
  
  # load model
  if(saveToJson){
    modelLocation <- reticulate::r_to_py(file.path(plpModelLocation,"model.json"))
    model <- sklearnFromJson(path=modelLocation)
  } else{
    os <- reticulate::import('os')
    joblib <- reticulate::import('joblib', convert=FALSE)
    modelLocation <- reticulate::r_to_py(file.path(plpModelLocation,"model.pkl"))
    model <- joblib$load(os$path$join(modelLocation)) 
  }
  included <- covariateImportance$columnId[covariateImportance$included>0] # does this include map?
  pythonData <- reticulate::r_to_py(newData[,included, drop = F])
  
  # make dense if needed
  if(requiresDenseMatrix){
    pythonData <- pythonData$toarray()
  }
  
  cohort <- predictValues(
    model = model, 
    data = pythonData, 
    cohort = cohort
  )
  
  return(cohort)
}

RandomForest <- R6::R6Class("RandomForest", list(
  seed = NULL,
  nJobs = NULL,
  modelLocation = NULL,
  
  covariateMap = NULL,
  covariateImportance = NULL,
  
  requiresDenseMatrix = F,
  name = "Random forest",
  fitFunction = 'fitSklearnBase',
  pythonImport = 'sklearn',
  pythonImportSecond = 'ensemble',
  pythonClassifier = 'RandomForestClassifier',
  saveToJson = T,
  saveType = 'file',
  
  resamplingFunction = NULL,
  performanceFunction = NULL,
  hyperparameterGenerator = NULL,
  
  hyperparameterSummary = NULL,
  hyperparametersFinal = NULL,
  hyperparameters = list(
    ntrees = list(
      min = 1,
      max = 100000,
      grid = NULL,
      inputName = 'n_estimators',
      type = 'integer'
      ),
    criterion = list(
      options = list('gini', 'entropy', 'log_loss'),
      inputName = 'criterion',
      type = 'character'
    ),
    maxDepth = list(
      min = 1,
      max = 50,
      grid = NULL,
      inputName = 'max_depth',
      type = 'integer'
    ),
    minSamplesSplit = list(
      min = 2,
      max = 1000, # data specific
      grid = NULL,
      inputName = 'min_samples_split',
      type = c('integer', 'numeric')
    ),
    minSamplesLeaf = list(
      min = 1,
      max = 1000, # data specific
      grid = NULL,
      inputName = 'min_samples_leaf',
      type = c('integer', 'numeric')
    ),
    minWeightFractionLeaf = list(
      min = 0,
      max = 1, # data specific
      grid = NULL,
      inputName = 'min_weight_fraction_leaf',
      type = c('numeric')
    ),
    mtries = list(
      min = 1,
      max = 100000, # data specific
      options = list('sqrt', 'log2', NULL),
      grid = NULL,
      inputName = 'max_features',
      type = c('numeric', 'character')
    ),
    maxLeafNodes = list(
      min = 1,
      max = 100000, 
      options = list(NULL),
      grid = NULL,
      inputName = 'max_leaf_nodes',
      type = c('integer', 'character')
    ),
    minImpurityDecrease = list(
      min = 0,
      max = 1, 
      grid = NULL,
      inputName = 'min_impurity_decrease',
      type = c('numeric')
    ),
    bootstrap = list(
      options = list(TRUE, FALSE),
      grid = NULL,
      inputName = 'bootstrap',
      type = c('logical')
    ),
    oobScore = list(
      options = list(TRUE, FALSE),
      grid = NULL,
      inputName = 'oob_score',
      type = c('logical')
    ),
    classWeight = list(
      options = list('balanced', 'balanced_subsample', NULL),
      grid = NULL,
      inputName = 'class_weight',
      type = c('character')
    ),
    maxSamples = list(
      min = 1,
      max = 10000, # data specific
      options = list(NULL),
      grid = NULL,
      inputName = 'max_samples',
      type = c('character', 'integer', 'float')
    )
  ),
  

  initialize = function(
    ntrees =  list(50,100,200),
    criterion = list('gini'),
    maxDepth = list(4,10,17),
    minSamplesSplit = list(2,5),
    minSamplesLeaf = list(1,10),
    minWeightFractionLeaf = list(0),
    mtries = list('sqrt', 'log2'),
    maxLeafNodes = list(NULL),
    minImpurityDecrease = list(0),
    bootstrap = list(TRUE),
    maxSamples = list(NULL, 0.9),
    oobScore = list(FALSE),
    classWeight = list(NULL),
    
    nJobs = list(NULL),
    
    hyperparameterSummary = NULL,
    seed = sample(1000000,1),
    modelLocation = file.path(tempdir(), gsub('[-: ]', '_', Sys.time())),
    resamplingFunction = CrossValidationSampler,
    performanceFunction = PerformanceFunction,
    hyperparameterGenerator = GridHyperparameter
  ) {
    
    self$hyperparameters$ntrees$grid =  lapply(ntrees, function(x){as.integer(x)})
    self$hyperparameters$criterion$grid = criterion
    self$hyperparameters$maxDepth$grid = lapply(maxDepth, function(x){as.integer(x)})
    self$hyperparameters$minSamplesSplit$grid = lapply(minSamplesSplit, function(x){if(x>=1){as.integer(x)}else{x}}) 
    self$hyperparameters$minSamplesLeaf$grid = lapply(minSamplesLeaf, function(x){if(x>=1){as.integer(x)}else{x}}) 
    self$hyperparameters$minWeightFractionLeaf$grid = minWeightFractionLeaf
    self$hyperparameters$mtries$grid = mtries
    self$hyperparameters$maxLeafNodes$grid = lapply(maxLeafNodes, function(x){if(!is.null(x)){as.integer(x)}else{x}})
    self$hyperparameters$minImpurityDecrease$grid = minImpurityDecrease
    self$hyperparameters$bootstrap$grid = bootstrap
    self$hyperparameters$maxSamples$grid = maxSamples
    self$hyperparameters$oobScore$grid = oobScore
    self$hyperparameters$classWeight$grid = classWeight
    
    self$hyperparameterSummary <- hyperparameterSummary
    self$seed <- seed
    self$nJobs <- nJobs
    self$modelLocation <- modelLocation
    
    self$resamplingFunction <- resamplingFunction$new()
    self$performanceFunction <- performanceFunction$new()
    self$hyperparameterGenerator <- hyperparameterGenerator$new(self$hyperparameters)
  },
  
  setHyperparameters = function(hyperparameters){
    self$hyperparametersFinal <- hyperparameters
    return(invisible(T))
  },
  
  tune = function(data){
    
    autoTuning(
      trainData = data, 
      self = self
    )
    
    return(invisible(T))
  },
  
  fit = function(
    data, 
    trainIndexes,
    validationIndexes,
    returnPredictionOnly = F
    ) {
    
    result <- fitSklearnBase(
      data = data, 
      trainIndexes = trainIndexes,
      validationIndexes = validationIndexes,
      seed = self$seed, 
      requiresDenseMatrix = self$requiresDenseMatrix, 
      modelName = self$modelName,
      pythonImport = self$pythonImport,
      pythonImportSecond = self$pythonImportSecond,
      pythonClassifier = self$pythonClassifier,
      modelLocation = self$modelLocation,
      hyperparameters = self$hyperparametersFinal,
      saveToJson = self$saveToJson,
      returnPredictionOnly = returnPredictionOnly
    )
    
    # store the mapping
    self$covariateMap <- data$covariateMap
    
    if(!returnPredictionOnly){
      self$covariateImportance <- result$covariateImportance
    }
    
    return(invisible(result)) # do we want to return these or set self$trainPrediction?
  },
  
  predict = function(data, cohort) {
    
    prediction <- predictSklearnBase(
      plpModelLocation = self$modelLocation, 
      covariateMap = self$covariateMap, 
      covariateImportance = self$covariateImportance,
      requiresDenseMatrix = self$requiresDenseMatrix,
      saveToJson = self$saveToJson,
      data = data, 
      cohort = cohort
    )
    
    return(prediction)
  },
  
  print = function(...) {
    cat(self$name, " \n")
    if(self$hyperparameterGenerator$type == 'grid search'){
      cat(" hyper-parameter search: ", self$hyperparameterGenerator$type, "\n", sep = "")
      hps <- names(self$hyperparameters)
      for(hp in hps){
        cat(" ", hp,": ", paste(self$hyperparameters[[hp]]$grid, collapse = ','), "\n", sep = "")
      }
    }
    invisible(self)
  }
))

#rfModel <- RandomForest$new()
#rfModel$print

#library(R6)
CrossValidationSampler <- R6::R6Class("CrossValidationSampler", list(
  seed = NULL,
  k = NULL,
  stratified = NULL,
  
  initialize = function(
    seed = sample(10000000,1),
    k = 3,
    stratified = T
  ) {
    stopifnot(is.numeric(seed), length(seed) == 1)
    stopifnot(is.numeric(k), length(k) == 1 , k > 0)
    stopifnot(is.logical(stratified), length(stratified) == 1)
    
    self$seed <- seed
    self$k <- k
    self$stratified <- stratified
  },
  
  getIndexes = function(
    data, 
    iteration
  ) {
    #data$labels - rowId, outcomeCount
    
    outPpl <- data$labels$rowId[data$labels$outcomeCount == 1]
    nonPpl <- data$labels$rowId[data$labels$outcomeCount == 0]
    set.seed(self$seed)
    
    foldSizesOutPpl <- rep(floor(length(outPpl)/self$k), self$k) 
    extras <- rep(0, self$k)
    if(length(outPpl) - self$k*floor(length(outPpl)/self$k) !=0){
      extras <- rep(1, length(outPpl) - self$k*floor(length(outPpl)/self$k))
    }
    if(length(extras) != self$k){
      extras <- c(extras, rep(0, self$k-length(extras)))
    }
    foldSizesOutPpl <- foldSizesOutPpl + extras
    
    foldSizesNonPpl <- rep(floor(length(nonPpl)/self$k), self$k) 
    extras <- rep(0, self$k)
    if(length(nonPpl) - self$k*floor(length(nonPpl)/self$k) !=0){
      extras <- rep(1, length(nonPpl) - self$k*floor(length(nonPpl)/self$k))
    }
    if(length(extras) != self$k){
      extras <- c(extras, rep(0, self$k-length(extras)))
    }
    foldSizesNonPpl <- foldSizesNonPpl + extras
    
    outPplIndexes <- unlist(lapply(1:self$k, function(i){
      rep(i, foldSizesOutPpl[i])
    }))
    nonPplIndexes <- unlist(lapply(1:self$k, function(i){
      rep(i, foldSizesNonPpl[i])
    }))
    
    outPpl <- outPpl[sample(outPplIndexes) == iteration]
    nonPpl <- nonPpl[sample(nonPplIndexes) == iteration]
    
    return(
      list(
        validationRowIds = c(outPpl, nonPpl),
        trainRowIds = data$labels$rowId[!data$labels$rowId %in% c(outPpl, nonPpl)]
        )
    )
    
  },
  
  getInterationCount = function() {
    return(self$k)
  },
  
  print = function(...) {
    cat("K-fold cross validation: \n")
    cat("  seed: ", self$seed, "\n", sep = "")
    cat("  k:  ", self$k, "\n", sep = "")
    cat("  stratified:  ", self$stratified, "\n", sep = "")
    invisible(self)
  }
))

#test <-CrossValidationSampler$new()
#debug(test$getIndexes)
#test$getInterationCount()
#res <- test$getIndexes(
#  data = list(labels = data.frame(rowId = 1:10000, outcomeCount = round(runif(10000)))), 
#  iteration = 2
#  )

meanList <- function(x){
  mean(unlist(x))
}
computeAucNew <- function(prediction){
  return(PatientLevelPrediction:::aucWithoutCi(prediction = prediction$value, truth = prediction$outcomeCount))
}


PerformanceFunction <- R6::R6Class("PerformanceFunction", list(
  maxmize = NULL,
  metricFunctionName = NULL,
  aggregateFunctionName = NULL,
  
  initialize = function(
    maxmize = T,
    metricFunctionName = 'computeAucNew',
    aggregateFunctionName = 'meanList'
  ) {
    self$maxmize <- maxmize
    self$metricFunctionName <- metricFunctionName
    self$aggregateFunctionName <- aggregateFunctionName
  },
  
  metricFunction = function(prediction){
    fun <- eval(parse(text = self$metricFunctionName))
    result <- do.call('fun', list(prediction = prediction))
    return(result)
    },
  aggregateFunction = function(x){
    fun <- eval(parse(text = self$aggregateFunctionName))
    fun(x) #do.call?
  },
  
  print = function(...) {
    cat("PerformanceFunction: \n")
    cat("  maxmize: ", self$maxmize, "\n", sep = "")
    cat("  metricFunctionName:  ", self$metricFunctionName, "\n", sep = "")
    cat("  aggregateFunctionName:  ", self$aggregateFunctionName, "\n", sep = "")
    invisible(self)
  }
))

GridHyperparameter <- R6::R6Class("GridHyperparameter", list(
  type = 'grid search',
  hyperparameterList = NULL,
  currentIndex = 1,
  converged = FALSE,
  length = NULL,
  
  initialize = function(
    hyperparameters
  ) {
    
    # convert the hyperparameters with grid to grid
    hyperparameterNames <- names(hyperparameters)
    hyperparameterList <- lapply(hyperparameterNames, function(hyperparameterName){
      hyperparameters[[hyperparameterName]]$grid
    })
    names(hyperparameterList) <- hyperparameterNames
    hyperparameterList <- PatientLevelPrediction:::listCartesian(hyperparameterList)
    
    self$hyperparameterList <- hyperparameterList
    self$length <- length(hyperparameterList)
  },
  
  getNextHyperparameters = function(hyperparameterSummary){
    if(!self$converged){
      result <- self$hyperparameterList[[self$currentIndex]]
      
      if(length(self$hyperparameterList) == self$currentIndex){
        self$converged <- TRUE
      } else{
        self$currentIndex <- self$currentIndex + 1
      }
      return(result)
    }
  },
  
  print = function(...) {
    cat("Hyperparameter grid search : \n")
    cat("  hyperparameter grid length: ", self$length, "\n", sep = "")
    cat("  current index: ", self$currentIndex, "\n", sep = "")
    cat("  converged:  ", self$converged, "\n", sep = "")
    invisible(self)
  }
))

#hyper <- GridHyperparameter$new(
#  hyperparameters = list(
#    a = list(grid = list(1,2,NULL)),
#    b = list(grid = list(4,'fdf')),
#    c = list(grid = list(4))
 # )
#    )
