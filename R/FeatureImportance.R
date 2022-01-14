# permutation feature importance - use parrallel computing...

##Input: Trained model f, feature matrix X, target vector y, error measure L(y,f).

##Estimate the original model error eorig = L(y, f(X)) (e.g. mean squared error)
##For each feature j = 1,...,p do:
##  Generate feature matrix Xperm by permuting feature j in the data X. This breaks the association between feature j and true outcome y.
##Estimate error eperm = L(Y,f(Xperm)) based on the predictions of the permuted data.
##Calculate permutation feature importance FIj= eperm/eorig. Alternatively, the difference can be used: FIj = eperm - eorig
##Sort features by descending FI.

#' pfi
#'
#' @description
#' Calculate the permutation feature importance for a PLP model.  
#' @details
#' The function permutes the each covariate/features <repeats> times and calculates the mean AUC change caused by the permutation.
#' @param plpResult                         An object of type \code{runPlp}
#' @param population                       The population created using createStudyPopulation() who will have their risks predicted
#' @param plpData                          An object of type \code{plpData} - the patient level prediction
#'                                         data extracted from the CDM.
#' @param repeats                          The number of times to permute each covariate
#' @param covariates                       A vector of covariates to calculate the pfi for.  If NULL it uses all covariates included in the model.
#' @param cores                            Number of cores to use when running this (it runs in parallel)
#' @param log                              A location to save the log for running pfi
#' @param logthreshold                     The log threshold (e.g., INFO, TRACE, ...)
#' 
#' @return
#' A dataframe with the covariateIds and the pfi (change in AUC caused by permuting the covariate) value
#'
# permutation feature importance
#' @export
pfi <- function(plpResult, population, plpData, repeats = 1,
                covariates = NULL, cores = NULL, log = NULL,
                logthreshold = "INFO"){
  
  
  if(!is.null(log)){
    appender <- ParallelLogger::createFileAppender(layout = ParallelLogger::layoutParallel,
                                                   fileName = log)
    
    logger <- ParallelLogger::createLogger(name = "PAR",
                                           threshold = logthreshold,
                                           appenders = list(appender))
    ParallelLogger::registerLogger(logger)  
  }
  
  
  if(is.null(covariates)){
    covariates <- plpResult$model$covariateImportance %>% 
      dplyr::filter(.data$covariateValue != 0) %>% 
      dplyr::select(.data$covariateId) %>% 
      dplyr::pull()
  }
  
  # add code to format covariateData based on plpModel
  # do feature engineering/selection
  plpData$covariateData <- do.call(
    applyFeatureengineering, 
    list(covariateData = plpData$covariateData,
      settings = plpResult$model$settings$featureEngineering
    )
  )
  
  # do preprocessing
  plpData$covariateData <- do.call(
    applyTidyCovariateData, 
    list(covariateData = plpData$covariateData,
      preprocessSettings = plpResult$model$settings$tidyCovariates
    )
  )
  
  # apply prediction function
  pred <- do.call(
    attr(plpResult$model, "predictionFunction"), 
    list(
      plpModel = plpResult$model, 
      data = plpData, 
      cohort = population
    )
  )
  
  auc <- computeAuc(pred)
  
  #do permulation and savePlpData to temp loc
  plpDataLocation <- file.path(tempdir(), paste0('data'))
  #plpDataLocation <- file.path('D:/testing', paste0('data'))
  savePlpData(plpData, file = plpDataLocation)
  
  if(is.null(cores)){
    ensure_installed('parallel')
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
                  covariateId = covariates[i],
                  repeats = repeats)
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
  
  auc <- c()
  for(i in 1:settings$repeats){
    ParallelLogger::logInfo(paste0('Starting to permute data for covariate: ',settings$covariateId))
    plpData <- tryCatch({suppressWarnings(permute(settings$plpDataLocation, settings$covariateId, 
                                                  settings$population ))},
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
    pred <- do.call(
      attr(settings$plpModel, "predictionFunction"), 
      list(
        plpModel = settings$plpModel, 
        data = plpData, 
        cohort = settings$population
      )
    )

    auct <- computeAuc(pred)
    auc <- c(auc, auct)
  }
  return(mean(auc))
  
}


permute <- function(plpDataLocation,cId,population){
  
  plpData <- suppressWarnings(PatientLevelPrediction::loadPlpData(plpDataLocation))
  
  #get analysisId
  aId <- plpData$covariateData$covariateRef %>% 
    dplyr::filter(.data$covariateId == !!cId) %>%
    dplyr::select(.data$analysisId) %>% dplyr::collect()
  
  # if analysis id is not 3 (age group), 4 (race) or 5 (ethnicity)
  if(!aId$analysisId %in% c(3,4,5)){
    
    # select covariateId data
    coi <- plpData$covariateData$covariates %>% dplyr::filter(.data$covariateId == !!cId) %>% dplyr::collect()
    nSamp <- length(coi$rowId)
  
    # find a new random selection of people and give them the covariate and value
    newPlp <- sample(population$rowId,nSamp)
    newData <- tibble::as_tibble(cbind(rowId = newPlp,coi[,-1]))
    
    # swap old covariate data with new
    plpData$covariateData$covariates <- plpData$covariateData$covariates %>% dplyr::filter(.data$covariateId != !!cId) %>% dplyr::collect()
    Andromeda::appendToTable(plpData$covariateData$covariates, newData)

  } else{
    # do some code for the connected variables... with more than 2 options
    
    # get the ppl with covariates
    haveCidData <- plpData$covariateData$covariates %>% dplyr::filter(.data$covariateId == !!cId) %>% dplyr::collect()
    nSamp <- length(haveCidData$rowId)
    
    # sample the pop to replace 
    swapPlp <- sample(population$rowId,nSamp)
    haveCidDataSwapped <- tibble::as_tibble(cbind(rowId = swapPlp,haveCidData[,-1]))
    
    # find the swapped people to switch 
    connectedCovs <- plpData$covariateData$covariateRef %>% 
      dplyr::filter(.data$analysisId == !!aId$analysisId) %>% 
      dplyr::group_by(.data$covariateId) %>% 
      dplyr::select(.data$covariateId) %>% 
      dplyr::collect()
    plpToSwap <- plpData$covariateData$covariates %>% 
      dplyr::filter(.data$covariateId %in% !!connectedCovs$covariateId) %>% 
      dplyr::filter(.data$rowId %in% swapPlp) %>% 
      dplyr::collect()
    
    swappedForCid <- tibble::as_tibble(cbind(rowId = haveCidData$rowId[1:nrow(plpToSwap)],plpToSwap[,-1]))
    

    # swap old covariate data with new
    plpData$covariateData$covariates <- plpData$covariateData$covariates %>% 
      dplyr::filter(.data$covariateId != !!cId) %>% 
      dplyr::filter(!.data$rowId %in% !!swapPlp) %>% 
      dplyr::collect()
    Andromeda::appendToTable(plpData$covariateData$covariates, 
                             rbind(haveCidDataSwapped, swappedForCid)
                             )

  }
  
  return(plpData)
}




  
