#TODO
#' Apply train model on new data
#' Apply a Patient Level Prediction model on Patient Level Prediction Data and get the predicted risk
#' in [0,1] for each person in the population. If the user inputs a population with an outcomeCount
#' column then the function also returns the evaluation of the prediction (AUC, brier score,
#' calibration)
#'
#' @param population       The population of people who you want to predict the risk for
#' @param plpData          The plpData for the population
#' @param plpModel         The trained PatientLevelPrediction model
#' @param logConnection    A connection to output any logging during the process
#' @param databaseOutput   Whether to save the details into the prediction database
#' @param silent           Whether to turn off progress reporting
#'
#' @examples
#' \dontrun{
#' # load the model and data
#' plpData <- loadPlpData("C:/plpdata")
#' plpModel <- loadPlpModel("C:/plpmodel")
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
applyModel <- function(population,
                       plpData,
                       plpModel,
                       logConnection = NULL,
                       databaseOutput = NULL,
                       silent = F) {
  # check input:
  if (is.null(population))
    stop("NULL population")
  if (class(plpData) != "plpData")
    stop("Incorrect plpData class")
  if (class(plpModel) != "plpModel")
    stop("Incorrect plpModel class")
  if (!ifelse(is.null(logConnection), TRUE, "connection" %in% class(logConnection)))
    stop("logConnection not NULL or a connection")

  # log the trained model details TODO

  # get prediction counts:
  peopleCount <- nrow(population)

  start.pred <- Sys.time()
  if (!is.null(logConnection))
    cat("Starting Prediction at ", Sys.time(), "for ", peopleCount, " people", file = logConnection)
  if (!silent)
    writeLines(paste("Starting Prediction ", Sys.time(), "for ", peopleCount, " people"))

  prediction <- plpModel$predict(plpData = plpData, population = population)

  if (!is.null(logConnection)) {
    cat("Prediction completed at ", Sys.time(), file = logConnection)
    cat("Took: ", start.pred - Sys.time(), file = logConnection)
  }
  if (!silent)
    writeLines(paste("Prediction completed at ", Sys.time(), " taking ", start.pred - Sys.time()))


  if (!"outcomeCount" %in% colnames(prediction))
    return(list(prediction = prediction))

  if (!is.null(logConnection)) {
    cat("Starting evaluation at ", Sys.time(), file = logConnection)
  }
  if (!silent)
    writeLines(paste("Starting evaulation at ", Sys.time()))

  performance <- evaluatePlp(prediction, plpData)

  if (!is.null(logConnection)) {
    cat("Evaluation completed at ", Sys.time(), file = logConnection)
    cat("Took: ", start.pred - Sys.time(), file = logConnection)
  }
  if (!silent)
    writeLines(paste("Evaluation completed at ", Sys.time(), " taking ", start.pred - Sys.time()))

  result <- list(prediction = prediction, performance = performance)
}

ApplyPlpPrediction <- function(plpModel, plpData, population) {
  # TODO: input checks

  prediction <- plpModel$predict(population = population, plpData = plpData)

  # evaluation? - shall we add this? evaluatePlp(prediction)

  return(prediction)

}

#' Extract new plpData using plpModel settings
#' use metadata in plpModel to extract similar data and population for new databases:
#'
#' @param plpModel         The trained PatientLevelPrediction model or object returned by runPlp()
#' @param newConnectionDetails      The connectionDetails for the new database
#' @param newCdmDatabaseSchema      The database schema for the new CDM database 
#' @param newCohortDatabaseSchema   The database schema where the cohort table is stored
#' @param newCohortTable            The table name of the cohort table
#' @param newCohortId               The cohort_definition_id for the cohort of at risk people
#' @param newOutcomeDatabaseSchema  The database schema where the outcome table is stored
#' @param newOutcomeTable           The table name of the outcome table
#' @param newOutcomeId              The cohort_definition_id for the outcome  
#'
#' @examples
#' \dontrun{
#' # set the connection
#' connectionDetails <- DatabaseConnector::createConnectionDetails()
#'    
#' # load the model and data
#' plpModel <- loadPlpModel("C:/plpmodel")
#'
#' # extract the new data in the 'newData.dbo' schema using the model settings 
#' newDataList <- similarPlpData(plpModel=plpModel, 
#'                               newConnectionDetails = connectionDetails,
#'                               newCdmDatabaseSchema = 'newData.dbo',
#'                               newCohortDatabaseSchema = 'newData.dbo',   
#'                               newCohortTable = 'cohort', 
#'                               newCohortId = 1, 
#'                               newOutcomeDatabaseSchema = 'newData.dbo', 
#'                               newOutcomeTable = 'outcome',     
#'                               newOutcomeId = 2)    
#'                
#' # get the prediction:
#' prediction <- applyModel(newDataList$population, newDataList$plpData, plpModel)$prediction
#' }
#' @export
similarPlpData <- function(plpModel=NULL,
                           newConnectionDetails = NULL,
                           newCdmDatabaseSchema = NULL,
                           newCohortDatabaseSchema = NULL,
                           newCohortTable = NULL,
                           newCohortId = NULL,
                           newOutcomeDatabaseSchema = NULL,
                           newOutcomeTable = NULL,
                           newOutcomeId = NULL) {
  
  if(is.null(plpModel))
    return(NULL)
  if(!'plpModel'%in%class(plpModel))
    return(NULL)
  if(sum(class(plpModel)==c('list','plpModel'))==2)
    plpModel <- plpModel$model 
  
  writeLines('Loading model data extraction settings')
  dataOptions <- as.list(plpModel$metaData$call)
  dataOptions[[1]] <- NULL
  
  writeLines('Adding new settings if set...')
  if(is.null(newCdmDatabaseSchema))
    return(NULL)
  dataOptions$cdmDatabaseSchema <- newCdmDatabaseSchema
  
  if(!is.null(newConnectionDetails))
    dataOptions$connectionDetails <- newConnectionDetails # check name
  
  if(!is.null(newCohortId))
    dataOptions$cohortId <- newCohortId
  if(!is.null(newOutcomeId))
    dataOptions$outcomeIds <- newOutcomeId
  plpData <- do.call(getPlpData, dataOptions)
  
  if(!is.null(newCohortDatabaseSchema))
    dataOptions$cohortDatabaseSchema <- newCohortDatabaseSchema  # correct names?
  if(!is.null(newCohortTable))
    dataOptions$cohortTable <- newCohortTable
  
  if(!is.null(newOutcomeDatabaseSchema))
    dataOptions$outcomeDatabaseSchema <- newOutcomeDatabaseSchema # correct names?
  if(!is.null(newOutcomeTable))
    dataOptions$outcomeTable <- newOutcomeTable
  
  
  # get the popualtion
  writeLines('Loading model population settings')
  popOptions <- plpModel$populationSettings
  popOptions$cohortId <- dataOptions$cohortId
  popOptions$outcomeId <- dataOptions$outcomeIds
  popOptions$plpData <- plpData
  population <- do.call(PatientLevelPrediction::createStudyPopulation, popOptions)
  
  
  # return the popualtion and plpData for the new database
  writeLines('Returning population and plpData for new data using model settings')
  return(list(population=population,
         plpData=plpData))
}




