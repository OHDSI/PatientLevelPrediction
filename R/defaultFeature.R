filterCovariates <- function(plpData, index, param, libSVM=libSVM,...){
  
  include <- param$include
  conceptIds <- param$conceptIds
  covariateIds <- param$covariateIds
  analysisIds <- param$analysisIds
  quiet <- param$quiet
  if(is.null(quiet)) quiet <- F
  
  transformData <- function(plpData2){
    writeLines('transform function running...')
    covariates <- clone(plpData2$covariates)
    allCovs <- c()
    #covRef: covariateId  covariateName analysisId conceptId
    
    # include/exclude covariates as specified
    if(!is.null(conceptIds)){
      if(!quiet)
        writeLines('Extracting concepts used in model...')
      t <- ffbase::ffmatch(plpData2$covariateRef$conceptId, table=ff::as.ff(conceptIds))
      covarIds <- unique(plpData2$covariateRef$covariateId[ffbase::ffwhich(t, !is.na(t)),])
      allCovs <- c(allCovs, ff::as.ram(covarIds)) 
      t <- ffbase::ffmatch(covariates$covariateId, table=ff::as.ff(covarIds))
      if(include)
        covariates<- covariates[ffbase::ffwhich(t, !is.na(t)),]
      if(!include)
        covariates<- covariates[ffbase::ffwhich(t, is.na(t)),]
    }
    
    if(!is.null(covariateIds)){
      if(!quiet)
        writeLines('Extracting covarites used in model...')
      allCovs <- c(allCovs, covariateIds) 
      t <- ffbase::ffmatch(covariates$covariateId, table=ff::as.ff(covariateIds))
      if(include)
        covariates<- covariates[ffbase::ffwhich(t, !is.na(t)),]
      if(!include)
        covariates<- covariates[ffbase::ffwhich(t, is.na(t)),]
    }
    
    if(!is.null(analysisIds)){
      if(!quiet)
        writeLines('Extracting analysis concepts used in model...')
      t <- ffbase::ffmatch(plpData2$covariateRef$analysisId, table=ff::as.ff(analysisIds))
      if(sum(!is.na(t))==0){return(warning('No covariates'))}
      covarIds <- unique(plpData2$covariateRef$covariateId[ffbase::ffwhich(t, !is.na(t)),])
      allCovs <- c(allCovs, ff::as.ram(covarIds)) 
      t <- ffbase::ffmatch(covariates$covariateId, table=ff::as.ff(covarIds))
      if(include)
        covariates<- covariates[ffbase::ffwhich(t, !is.na(t)),]
      if(!include)
        covariates<- covariates[ffbase::ffwhich(t, is.na(t)),]
    }
    
    result <- list(covariates = covariates,
                   cohorts = ff::clone(plpData2$cohorts),
                   outcomes = ff::clone(plpData2$outcomes),
                   covariateRef = ff::clone(plpData2$covariateRef),
                   metaData = plpData2$metaData
    )
    return(result)
  }

  featureSet <- list(method='filterCovariates', 
                        param=param
                     )
                        
  
  result <- c(list(transform=transformData), featureSet)
  
  return(result)
  
}


glrm <- function(plpdata, clusterSize, ...){
  #convert to matrix
  
  #find topics
  
  #figure how to predict topics for new data and save this
  
  # save into covariate
  
  
}

