# permutation feature importance - use parrallel computing...

##Input: Trained model f, feature matrix X, target vector y, error measure L(y,f).

##Estimate the original model error eorig = L(y, f(X)) (e.g. mean squared error)
##For each feature j = 1,...,p do:
##  Generate feature matrix Xperm by permuting feature j in the data X. This breaks the association between feature j and true outcome y.
##Estimate error eperm = L(Y,f(Xperm)) based on the predictions of the permuted data.
##Calculate permutation feature importance FIj= eperm/eorig. Alternatively, the difference can be used: FIj = eperm - eorig
##Sort features by descending FI.

pfi <- function(plpResult, population, plpData, covariates = NULL, cores = NULL, log = NULL){
  
  
  if(!is.null(log)){
    appender <- ParallelLogger::createFileAppender(layout = ParallelLogger::layoutParallel,
                                                   fileName = log)
    
    logger <- ParallelLogger::createLogger(name = "PAR",
                                           threshold = "INFO",
                                           appenders = list(appender))
    ParallelLogger::registerLogger(logger)  
  }
  
  
  if(is.null(covariates)){
    covariates <- plpResult$covariateSummary$covariateId[plpResult$covariateSummary$covariateValue!=0]
  }
  
  ## reduce age/gender/race/ethnicity to one
  # removeCovs <- plpData$covariateData$covariateRef %>% dplyr::filter(covariateId%in%covariates) %>% dplyr::filter(analysisId %in%c(1,2,3,4)) %>% dplyr::select(covariateId, analysisId)
  # removeCovs <- removeCovs$covariateId
  # includeCovs <- removeCovs %>% dplyr::group_by(analysisId) %>% dplyr::summarise(covariateId = min(covariateId))
  # includeCovs <- includeCovs$covariateId
  # covariates <- c(covariates[!covariates%in%removeCovs],includeCovs)
  
  plpData$covariateData <- limitCovariatesToPopulation(plpData$covariateData, population$rowId)
  if(!is.null(plpResult$model$metaData$preprocessSettings)){
    plpData$covariateData <- applyTidyCovariateData(plpData$covariateData,plpResult$model$metaData$preprocessSettings)
  }
  pred <- do.call(paste0('predict.',attr(plpResult$model, 'type')), list(plpModel=plpResult$model,
                                                                  plpData=plpData, 
                                                                  population=population))
  attr(pred, "metaData")$predictionType <-  "binary"
  auc <- computeAuc(pred)
  
  #do permulation and savePlpData to temp loc
  plpDataLocation <- file.path(tempdir(), paste0('data'))
  #plpDataLocation <- file.path('D:/testing', paste0('data'))
  savePlpData(plpData, file = plpDataLocation)
  
  if(is.null(cores)){
    ParallelLogger::logInfo(paste0('Number of cores not specified'))
    cores <- parallel::detectCores()
    ParallelLogger::logInfo(paste0('Using all ', cores))
    ParallelLogger::logInfo(paste0('Set cores input to use fewer...'))
  }
  
  cluster <- ParallelLogger::makeCluster(numberOfThreads = cores)
  ParallelLogger::clusterRequire(cluster, c("PatientLevelPrediction", "Andromeda"))
  
  
  getVpiSettings <- function(i){
    result <-list(plpModel = plpResult$model, 
                  population = population, 
                  plpDataLocation = plpDataLocation,
                  covariateId = covariates[i])
    return(result)
  }
  vpiSettings <- lapply(1:length(covariates), getVpiSettings)

  
  #lapply(vpiSettings, function(x) do.call(permutePerf, x))
  aucP <- ParallelLogger::clusterApply(cluster = cluster, 
                                                x = vpiSettings, 
                                                fun = permutePerf, 
                                                stopOnError = FALSE,
                                                progressBar = TRUE)
  ParallelLogger::stopCluster(cluster)
  
  aucP <- do.call(c, aucP)
  
  # do this in parellel

  varImp <- data.frame(covariateId = covariates,
                       pfi = auc-aucP)
  
  return(varImp)
  
}


# function to take plpModel,plpData location, load data and permute then calcualte auc
permutePerf <- function(settings){
  ParallelLogger::logInfo(paste0('Starting to permute data for covariate: ',settings$covariateId))
  plpData <- tryCatch({suppressWarnings(permute(settings$plpDataLocation, settings$covariateId))},
                     warning = function(war) {
                       ParallelLogger::logInfo(paste0('a warning: ', war))
                     }, 
                     error = function(err) {
                       ParallelLogger::logError(paste0('an error: ', err))
                       return(NULL)
                     }       
  )

  if(is.null(plpData)){
    ParallelLogger::logInfo(paste0('plpData NULL for covariate: ',settings$covariateId))
    return(0)
  }
  ParallelLogger::logInfo(paste0('Calculating prediction for permutation of covariate: ',settings$covariateId))
  
  # need to stop preprocessing and do it once...
  pred <- do.call(paste0('predict.',attr(settings$plpModel, 'type')), 
                  list(plpModel=settings$plpModel,
                       plpData=plpData, 
                       population=settings$population))
  attr(pred, "metaData")$predictionType <-  "binary"
  auc <- computeAuc(pred)
  return(auc)
  
}


permute <- function(plpDataLocation,cId){
  
  plpData <- suppressWarnings(PatientLevelPrediction::loadPlpData(plpDataLocation))
  
  #get analysisId
  analysisId <- plpData$covariateData$covariateRef %>% 
    dplyr::filter(.data$covariateId == cId) %>%
    dplyr::select(.data$analysisId) %>% dplyr::collect()
  
  # if analysis id i 4 - then gender - swap with other genders
  if(!analysisId %in% c(3,4)){
  # select covariateId data
  coi <- plpData$covariateData$covariates %>% dplyr::filter(.data$covariateId == cId) %>% dplyr::collect()
  nSamp <- length(coi$rowId)
  
  roi <- plpData$covariateData$covariates %>% dplyr::distinct(.data$rowId) %>% dplyr::collect()
  ncoi <- roi$rowId[!roi$rowId%in%coi$rowId]
  repN <- length(ncoi)
  
  if(repN >= nSamp){
    replacement <- sample(ncoi, nSamp)
    ind <- 1:nSamp
  } else{
    replacement <- ncoi
    ind <- sample(nSamp, repN)
  }
  
  newData <- tibble::as_tibble(cbind(rowId = replacement,coi[ind,-1]))
  
  #plpData$covariateData$covariates <- union_all(plpData$covariateData$covariates %>% filter(covariateId != cId), newData, copy = T)
  plpData$covariateData$covariates <- plpData$covariateData$covariates %>% dplyr::filter(.data$covariateId != cId) %>% dplyr::collect()
  Andromeda::appendToTable(plpData$covariateData$covariates, newData)
  
  } else{
    # do some code for the connected variables... 
  }
  
  return(plpData)
}

