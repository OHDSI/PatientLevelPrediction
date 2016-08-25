#TODO
#' Apply train model on new data
#' 
#' Apply a Patient Level Prediction model on Patient Level Prediction Data
#' and get the predicted risk in [0,1] for each person in the population.
#' If the user inputs a population with an outcomeCount column then the 
#' function also returns the evaluation of the prediction (AUC, brier score, calibration)
#'
#' @param population   The population of people who you want to predict the risk for
#' @param plpData    The plpData for the population
#' @param plpModel   The trained PatientLevelPrediction model
#' @param logConnection        A connection to output any logging during the process
#' @param databaseOutput Whether to save the details into the prediction database
#' @param silent         Whether to turn off progress reporting
#'
#' @examples
#' \dontrun{
#' # load the model and data
#' plpData <- loadPlpData('C:/plpdata')
#' plpModel <- loadPlpModel('C:/plpmodel')
#' 
#' # use the same population settings as the model:
#' populationSettings <- plpModel$populationSettings
#' populationSettings$plpData <- plpData
#' population <- do.call(createStudyPopulation, populationSettings)
#' 
#' # get the prediction:
#' prediction <- applyModel(population, plpData, plpModel)$prediction
#' }
#' @export
applyModel <- function(population, plpData, plpModel,
                       logConnection=NULL,
                       databaseOutput=NULL,
                       silent=F){
  #check input:
  if(is.null(population))
    stop('NULL population')
  if(class(plpData)!='plpData')
    stop('Incorrect plpData class')
  if(class(plpModel)!='plpModel')
    stop('Incorrect plpModel class')
  if(!ifelse(is.null(logConnection),TRUE,"connection"%in%class(logConnection)))
    stop('logConnection not NULL or a connection')
  
  # log the trained model details
  # TODO
  
  # get prediction counts:
  peopleCount <- nrow(population)
  
  start.pred <- Sys.time()
  if(!is.null(logConnection))
    cat('Starting Prediction at ', Sys.time(), 'for ', peopleCount, ' people', file=logConnection)
  if(!silent)
    writeLines(paste('Starting Prediction ', Sys.time(), 'for ', peopleCount, ' people') )  
  
  prediction <- plpModel$predict(plpData=plpData, population=population)  
  
  if(!is.null(logConnection)){
    cat('Prediction completed at ', Sys.time(), file=logConnection)
    cat('Took: ', start.pred - Sys.time(), file=logConnection)
  }
  if(!silent)
    writeLines(paste('Prediction completed at ', Sys.time(), ' taking ', start.pred-Sys.time()) )  
  
  
  if(!'outcomeCount'%in%colnames(prediction))
    return(list(prediction=prediction))
  
  if(!is.null(logConnection)){
    cat('Starting evaluation at ', Sys.time(), file=logConnection)
  }
  if(!silent)
    writeLines(paste('Starting evaulation at ', Sys.time()) )  
  
  performance <- evaluatePlp(prediction, plpData)
  
  if(!is.null(logConnection)){
    cat('Evaluation completed at ', Sys.time(), file=logConnection)
    cat('Took: ', start.pred - Sys.time(), file=logConnection)
  }
  if(!silent)
    writeLines(paste('Evaluation completed at ', Sys.time(), ' taking ', start.pred-Sys.time()) )  
  
  result <- list(prediction=prediction,
                 performance = performance)
}

ApplyPlpPrediction <- function(plpModel, plpData, population){
  # TODO: input checks
  
  prediction <- plpModel$predict(population=population, plpData=plpData)
  
  # evaluation? - shall we add this? evaluatePlp(prediction)
  
  return(prediction)
  
}

# use metadata in plpModel to extract similar data and population for new databases:
similarPlpData <- function(plpModel, database, newCohortId=NULL, newOutcomeId=NULL, 
               newCohortTable=NULL, newOutcomeTable=NULL, 
               newCohortCDMSchema=NULL, newOutcomeCDMSchema=NULL,...){
  #todo
  
}





