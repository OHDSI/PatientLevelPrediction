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
        if(pkg%in%c('BigKnn', "IterativeHardThresholding")){
          
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

createTempModelLoc <- function(){
  repeat{
    loc <- paste(tempdir(), paste0('python_models_',sample(10002323,1)), sep = '\\')
    #loc <- file.path(tempdir(), paste0('python_models_',sample(10002323,1)))
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
    packages <- c('numpy','scipy','scikit-learn', 'pandas','pydotplus','keras','joblib', 'sklearn-json')
    ParallelLogger::logInfo(paste0('Adding python dependancies to ', envname))
    reticulate::conda_install(envname=envname, packages = packages, forge = TRUE, pip = FALSE,
                              pip_ignore_installed = TRUE, conda = "auto")
  } else {
    pEnvironments <- reticulate::virtualenv_list()
    if(length(pEnvironments) > 0 && envname %in% pEnvironments){
      warning(paste0('Python environment ', envname,' exists.  You can use removePython() to remove if you want to fresh config'))
    } else {
      ParallelLogger::logInfo(paste0('Creating virtual python environment called ', envname))
      location <- reticulate::virtualenv_create(envname=envname)
    }
    packages <- c('numpy', 'scikit-learn','scipy', 'pandas','pydotplus','keras', 'sklearn-json')
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

