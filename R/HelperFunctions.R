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
checkPlpInstallation <- function(connectionDetails, python=T) {
  writeLines("Checking database connectivity")
  conn <- connect(connectionDetails)
  dbDisconnect(conn)
  writeLines("- Ok")
  
  writeLines("\nChecking R population")
  set.seed(1234)
  data(plpDataSimulationProfile)
  sampleSize <- 2000
  plpData <- simulatePlpData(plpDataSimulationProfile, n = sampleSize)
  
  # create popualtion for outcome 2
  population <- createStudyPopulation(plpData,
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
  )
  if (length(dim(population)) != 2)
    stop("Error creating population")
  writeLines("- Ok")
  
  modset <- PatientLevelPrediction::setLassoLogisticRegression()
  model <- PatientLevelPrediction::runPlp(population, plpData, modelSettings = modset,
                                          testFraction = 0.5)
    
  if (!"plpModel"%in%class(model))
    stop("Error running logistic regression")
  writeLines("- Ok")
  
  if(python){
    modset <- PatientLevelPrediction::setRandomForest()
    model <- PatientLevelPrediction::runPlp(population, plpData, modelSettings = modset,
                                            testFraction = 0.5) 
    if (!"plpModel"%in%class(model))
      stop("Error running gradient boosting machine")
    writeLines("- Ok")
  }
  
  writeLines("\nChecking support for large data objects")
  x <- ff::as.ffdf(data.frame(a = 1:100, b = "test"))
  if (nrow(x) != 100)
    stop("Error creating large data object")
  writeLines("- Ok")
  
  writeLines("\nPatientLevelPrediction is correctly installed")
  writeLines(paste0("\nResponse code: ", round(pi * 365*7)))
}