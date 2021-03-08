# fix issue with nrow - temp fix for me locally
nrow <- function(x){UseMethod("nrow",x)}
nrow.default <- base::nrow
nrow.tbl <- function(x){x %>% dplyr::tally() %>% dplyr::pull()}


# Borrowed from devtools: https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L44
is_installed <- function (pkg, version = 0) {
  installed_version <- tryCatch(utils::packageVersion(pkg), 
                                error = function(e) NA)
  !is.na(installed_version) && installed_version >= version
}

# Borrowed and adapted from devtools: https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L74
ensure_installed <- function(pkg) {
  if (!is_installed(pkg)) {
    msg <- paste0(sQuote(pkg), " must be installed for this functionality.")
    if (interactive()) {
      message(msg, "\nWould you like to install it?")
      if (utils::menu(c("Yes", "No")) == 1) {
        if(pkg%in%c('BigKnn')){
          
          # add code to check for devtools...
          dvtCheck <- tryCatch(utils::packageVersion('devtools'), 
                      error = function(e) NA)
          if(is.na(dvtCheck)){
            utils::install.packages('devtools')
          }
          
          devtools::install_github(paste0('OHDSI/',pkg))
        }else{
          utils::install.packages(pkg)
        }
      } else {
        stop(msg, call. = FALSE)
      }
    } else {
      stop(msg, call. = FALSE)
    }
  }
}

#' Check PatientLevelPrediction and its dependencies are correctly installed
#'
#' @details
#' This function checks whether PatientLevelPrediction and its dependencies are correctly installed. This will
#' check the database connectivity, some models, and large data object
#' handling (ff).
#'
#' @param connectionDetails   An R object of type\cr\code{connectionDetails} created using the function
#'                            \code{createConnectionDetails} in the \code{DatabaseConnector} package.
#' @param python              Whether to test the python models                            
#'
#' @export
checkPlpInstallation <- function(connectionDetails=NULL, python=T) {
  outCode <- 1
  if(!is.null(connectionDetails)){
    writeLines("Checking database connectivity")
    conn <- tryCatch({DatabaseConnector::connect(connectionDetails)},
                     error = function(e) {
                       return(NULL)
                     })
    if(length(conn)==0)
      outCode <- outCode*3
    
    discon <- tryCatch({DatabaseConnector::disconnect(conn)},
                       error = function(e) {
                         return(0)
                       })
    if(discon==0)
      outCode <- outCode*5
    writeLines("- Done")
  }
  
  writeLines("\nChecking R population")
  set.seed(1234)
  
  plpDataSimulationProfile <- NULL
  e <- environment()
  utils::data(plpDataSimulationProfile, envir = e)
  sampleSize <- 2000
  plpData <- simulatePlpData(plpDataSimulationProfile, n = sampleSize)
  
  # create popualtion for outcome 2
  population <- tryCatch({createStudyPopulation(plpData,
                                                outcomeId = 2,
                                                firstExposureOnly = FALSE,
                                                washoutPeriod = 0,
                                                removeSubjectsWithPriorOutcome = FALSE,
                                                priorOutcomeLookback = 99999,
                                                requireTimeAtRisk = FALSE,
                                                minTimeAtRisk=0,
                                                riskWindowStart = 0,
                                                addExposureDaysToStart = FALSE,
                                                riskWindowEnd = 365,
                                                addExposureDaysToEnd = FALSE
                                                #,verbosity=INFO
  )},
  error = function(e) {
    return(0)
  })
  
  if (length(dim(population)) != 2)
    outCode <- outCode*7
  writeLines("- Done")
  
  
  writeLines("\nChecking Models")
  modset <- tryCatch({setLassoLogisticRegression()},
                     error = function(e) {
                       return(NULL)
                     })
  if(!is.null(modset)){
    model <- tryCatch({runPlp(population, plpData, modelSettings = modset,
                                                      testFraction = 0.5, nfold = 3, 
                                                      minCovariateFraction = 0, 
                                                      saveEvaluation = F, 
                                                      savePlpData = F, 
                                                      savePlpResult = F, 
                                                      savePlpPlots = F)},
                      error = function(e) {
                        return(NULL)
                      })} else {
                        model <- NULL
                      }
  if(is.null(model) || !"runPlp"%in%class(model))
    outCode <- outCode*11
    
  writeLines("- Ok")
  
  if(python){
    modset <- tryCatch({setRandomForest()},
                       error = function(e) {
                         return(NULL)
                       })
    if(!is.null(modset)){
      model <- tryCatch({runPlp(population, plpData, modelSettings = modset,
                                                        testFraction = 0.5, nfold = 3, 
                                                        minCovariateFraction = 0, 
                                                        saveEvaluation = F, 
                                                        savePlpData = F, 
                                                        savePlpResult = F, 
                                                        savePlpPlots = F)},
                        error = function(e) {
                          return(NULL)
                        })} else {
                          model <- NULL
                        }
    if(is.null(model) || !"runPlp"%in%class(model))
      outCode <- outCode*13
    
    modset <- tryCatch({setMLP()},
                       error = function(e) {
                         return(NULL)
                       })
    if(!is.null(modset)){
      model <- tryCatch({runPlp(population, plpData, modelSettings = modset,
                                                        testFraction = 0.5, nfold = 3, 
                                                        minCovariateFraction = 0, 
                                                        saveEvaluation = F, 
                                                        savePlpData = F, 
                                                        savePlpResult = F, 
                                                        savePlpPlots = F)},
                        error = function(e) {
                          return(NULL)
                        })} else {
                          model <- NULL
                        }
    if(is.null(model) || !"runPlp"%in%class(model))
      outCode <- outCode*17
    
    modset <- tryCatch({setAdaBoost()},
                       error = function(e) {
                         return(NULL)
                       })
    if(!is.null(modset)){
      model <- tryCatch({runPlp(population, plpData, modelSettings = modset,
                                                        testFraction = 0.5, nfold = 3, 
                                                        minCovariateFraction = 0, 
                                                        saveEvaluation = F, 
                                                        savePlpData = F, 
                                                        savePlpResult = F, 
                                                        savePlpPlots = F)},
                        error = function(e) {
                          return(NULL)
                        })} else {
                          model <- NULL
                        }
    if(is.null(model) || !"runPlp"%in%class(model))
      outCode <- outCode*19
    
    modset <- tryCatch({setDecisionTree()},
                       error = function(e) {
                         return(NULL)
                       })
    if(!is.null(modset)){
      model <- tryCatch({runPlp(population, plpData, modelSettings = modset,
                                                        testFraction = 0.5, nfold = 3, 
                                                        minCovariateFraction = 0, 
                                                        saveEvaluation = F, 
                                                        savePlpData = F, 
                                                        savePlpResult = F, 
                                                        savePlpPlots = F)},
                        error = function(e) {
                          return(NULL)
                        })} else {
                          model <- NULL
                        }
    if(is.null(model) || !"runPlp"%in%class(model))
      outCode <- outCode*23
    
    modset <- tryCatch({setNaiveBayes()},
                       error = function(e) {
                         return(NULL)
                       })
    if(!is.null(modset)){
      model <- tryCatch({runPlp(population, plpData, modelSettings = modset,
                                                        testFraction = 0.5, nfold = 3, 
                                                        minCovariateFraction = 0, 
                                                        saveEvaluation = F, 
                                                        savePlpData = F, 
                                                        savePlpResult = F, 
                                                        savePlpPlots = F)},
                        error = function(e) {
                          return(NULL)
                        })} else {
                          model <- NULL
                        }
    if(is.null(model) || !"runPlp"%in%class(model))
      outCode <- outCode*29
    
  }
  
  
  modset <- tryCatch({setKNN()},
                     error = function(e) {
                       return(NULL)
                     })
  if(!is.null(modset)){
    model <- tryCatch({runPlp(population, plpData, modelSettings = modset,
                                                      testFraction = 0.5, nfold = 3, 
                                                      minCovariateFraction = 0, 
                                                      saveEvaluation = F, 
                                                      savePlpData = F, 
                                                      savePlpResult = F, 
                                                      savePlpPlots = F)},
                      error = function(e) {
                        return(NULL)
                      })} else {
                        model <- NULL
                      }
  if(is.null(model) || !"runPlp"%in%class(model))
    outCode <- outCode*31
  
  modset <- tryCatch({setGradientBoostingMachine()},
                     error = function(e) {
                       return(NULL)
                     })
  if(!is.null(modset)){
    model <- tryCatch({runPlp(population, plpData, modelSettings = modset,
                                                      testFraction = 0.5, nfold = 3, 
                                                      minCovariateFraction = 0, 
                                                      saveEvaluation = F, 
                                                      savePlpData = F, 
                                                      savePlpResult = F, 
                                                      savePlpPlots = F)},
                      error = function(e) {
                        return(NULL)
                      })} else {
                        model <- NULL
                      }
  if(is.null(model) || !"runPlp"%in%class(model))
    outCode <- outCode*37
  
  writeLines("- Done")
  
  writeLines("\nChecking support for large data objects")
  x <- Andromeda::andromeda(test = data.frame(a = 1:100, b = "test"))
  if(!"Andromeda" %in% class(x))
    outCode <- outCode*43
  writeLines("- Done")
  
  writeLines("\nPatientLevelPrediction installation check completed...")
  writeLines(paste0("\nResponse code: ", outCode))
  interpretInstallCode(outCode)
}


#' Tells you the package issue 
#'
#' @details
#' This function prints any issues found during the checkPlpInstallation() call
#'
#' @param response   The response code from checkPlpInstallation()                          
#'
#' @export
interpretInstallCode <- function(response){
  if(response==1){
    writeLines('Package working...')
  } else {
    if(response%%3==0)
      writeLines('Issue with database connection - did not connect')
    if(response%%5==0)
      writeLines('Issue with database connection - did not disconnect')
    if(response%%7==0)
      writeLines('Issue with createStudyPopulation()')
    if(response%%11==0)
      writeLines('Issue with lasso logistic regression')
    if(response%%13==0)
      writeLines('Issue with random forest')
    if(response%%17==0)
      writeLines('Issue with mlp')
    if(response%%19==0)
      writeLines('Issue with ada boost')
    if(response%%23==0)
      writeLines('Issue with decison tree')
    if(response%%29==0)
      writeLines('Issue with naive bayes')
    if(response%%31==0)
      writeLines('Issue with knn')
    if(response%%37==0)
      writeLines('Issue with gradient boosting machine')
    if(response%%43==0)
      writeLines('Issue with ffdf')
    
  }
    return(NULL)
}


clearLoggerType <- function(type='PLP log'){
  logs <- ParallelLogger::getLoggers()
  logNames <- unlist(lapply(logs, function(x) x$name))
  ind <- which(logNames==type)
  
  for(i in ind){
    ParallelLogger::unregisterLogger(logNames[i])
  }
  
  return(NULL)
}

createTempModelLoc <- function(){
  repeat{
    loc <- file.path(tempdir(), paste0('python_models_',sample(10002323,1)))
    if(!dir.exists(loc)){
      return(loc)
    }
  }
}

#' join two lists
#'
#' @details
#' This function joins two lists
#' @param a   A list 
#' @param b   Another list
#'
#' @export
listAppend <- function(a, b){
  size <- length(a) + length(b)
  x <- list()
  length(x) <- size
  for(i in 1:size){
    if(i<=length(a)){
      x[[i]] <- a[[i]]
    } else{
      x[[i]] <- b[[i-length(a)]]
    }
  }
  names(x) <- c(names(a), names(b))
  return(x)
}


#' Sets up a virtual environment to use for PLP (can be conda or python) 
#'
#' @details
#' This function creates a virtual environment that can be used by PatientLevelPrediction
#' and installs all the required package dependancies.  If using python, pip must be set up.
#'
#' @param envname   A string for the name of the virtual environment (default is 'PLP') 
#' @param envtype   An option for specifying the environment as'conda' or 'python'.  If NULL then the default is 'conda' for windows users and 'python' for non-windows users 
#'
#' @export
configurePython <- function(envname='PLP', envtype=NULL){
  
  if(is.null(envtype)){
    if(getOs()=='windows'){
      envtype=='conda'
    } else {
      envtype=='python'
    }
  }
  
  if(envtype=='conda'){
    pEnvironments <- reticulate::conda_list()
    if(length(pEnvironments) > 0 && envname %in% pEnvironments$name){
      warning(paste0('Conda environment ', envname,' exists.  You can use removePython() to remove if you want to fresh config'))
    } else {
      ParallelLogger::logInfo(paste0('Creating virtual conda environment called ', envname))
      location <- reticulate::conda_create(envname=envname, packages = "python", conda = "auto")
    }
    packages <- c('numpy','scipy','scikit-learn', 'pandas','pydotplus','keras','joblib')
    ParallelLogger::logInfo(paste0('Adding python dependancies to ', envname))
    reticulate::conda_install(envname=envname, packages = packages, forge = TRUE, pip = FALSE,
                              pip_ignore_installed = TRUE, conda = "auto")
  } else {
    pEnvironments <- reticulate::virtualenv_list()
    if(length(pEnvironments) > 0 && envname %in% pEnvironments){
      warning(paste0('Python environment ', envname,' exists.  You can use removePython() to remove if you want to fresh config'))
    } else {
      ParallelLogger::logInfo(paste0('Creating virtual python environment called ', envname))
      location <- reticulate::virtualenv_create(envname=envname, packages = "python")
    }
    packages <- c('numpy', 'scikit-learn','scipy', 'pandas','pydotplus','keras')
    ParallelLogger::logInfo(paste0('Adding python dependancies to ', envname))
    reticulate::virtualenv_install(envname=envname, packages = packages, 
                                   ignore_installed = TRUE)
  }
  
  return(location)
}

#' Use the virtual environment created using configurePython()
#'
#' @details
#' This function sets PatientLevelPrediction to use a virtual environment 
#'
#' @param envname   A string for the name of the virtual environment (default is 'PLP') 
#' @param envtype   An option for specifying the environment as'conda' or 'python'.  If NULL then the default is 'conda' for windows users and 'python' for non-windows users 
#'
#' @export
setPythonEnvironment <- function(envname='PLP', envtype=NULL){
  
  if(is.null(envtype)){
    if(getOs()=='windows'){
      envtype=='conda'
    } else {
      envtype=='python'
    }
  }
  
  if(envtype=='conda'){
    pEnvironments <- reticulate::conda_list()
    if(!envname%in%pEnvironments$name){
      return(paste0('Conda environment ', envname,' not found.  Please set up using configurePython()'))
    }
    reticulate::use_condaenv(envname)
    return(paste0('Using conda environment ',envname))
  } else {
    pEnvironments <- reticulate::virtualenv_list()
    if(!envname%in%pEnvironments$name){
      return(paste0('Python environment ', envname,' not found.  Please set up using configurePython()'))
    }
    reticulate::use_virtualenv(envname)
    return(paste0('Using python environment ',envname))
  }
  
}

getOs <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

