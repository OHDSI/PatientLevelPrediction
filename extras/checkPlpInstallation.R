#' Check PatientLevelPrediction and its dependencies are correctly installed
library(PatientLevelPrediction)

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