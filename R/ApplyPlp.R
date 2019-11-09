# @file packagePlp.R
#
# Copyright 2019 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Apply train model on new data
#' Apply a Patient Level Prediction model on Patient Level Prediction Data and get the predicted risk
#' in [0,1] for each person in the population. If the user inputs a population with an outcomeCount
#' column then the function also returns the evaluation of the prediction (AUC, brier score,
#' calibration)
#'
#' @param population       The population of people who you want to predict the risk for
#' @param plpData          The plpData for the population
#' @param plpModel         The trained PatientLevelPrediction model
#' @param calculatePerformance  Whether to also calculate the performance metrics [default TRUE]
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
                       calculatePerformance=T,
                       databaseOutput = NULL,
                       silent = F) {
  
  # check logger
  if(length(ParallelLogger::getLoggers())==0){
    logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                        threshold = "INFO",
                                        appenders = list(ParallelLogger::createConsoleAppender(layout = 'layoutTimestamp')))
    ParallelLogger::registerLogger(logger)
  }
  
  # check input:
  if (is.null(population))
    stop("NULL population")
  if (class(plpData) != "plpData")
    stop("Incorrect plpData class")
  if (class(plpModel) != "plpModel")
    stop("Incorrect plpModel class")

  # log the trained model details TODO

  # get prediction counts:
  peopleCount <- nrow(population)

  start.pred <- Sys.time()
  if (!silent)
    ParallelLogger::logInfo(paste("Starting Prediction ", Sys.time(), "for ", peopleCount, " people"))

  prediction <- plpModel$predict(plpData = plpData, population = population)

  
  if (!silent)
    ParallelLogger::logInfo(paste("Prediction completed at ", Sys.time(), " taking ", start.pred - Sys.time()))


  if (!"outcomeCount" %in% colnames(prediction))
    return(list(prediction = prediction))
  
  if(!calculatePerformance || nrow(prediction) == 1)
    return(prediction)

  if (!silent)
    ParallelLogger::logInfo(paste("Starting evaulation at ", Sys.time()))

  performance <- evaluatePlp(prediction, plpData)

  # reformatting the performance 
  analysisId <-   '000000'
  if(!is.null(plpModel$analysisId)){
    analysisId <-   plpModel$analysisId
  }
  
  nr1 <- length(unlist(performance$evaluationStatistics[-1]))
  performance$evaluationStatistics <- cbind(analysisId= rep(analysisId,nr1),
                                               Eval=rep('validation', nr1),
                                               Metric = names(unlist(performance$evaluationStatistics[-1])),
                                               Value = unlist(performance$evaluationStatistics[-1])
                                               )
  nr1 <- nrow(performance$thresholdSummary)
  performance$thresholdSummary <- cbind(analysisId=rep(analysisId,nr1),
                                              Eval=rep('validation', nr1),
                                              performance$thresholdSummary)
  nr1 <- nrow(performance$demographicSummary)
  if(!is.null(performance$demographicSummary)){
  performance$demographicSummary <- cbind(analysisId=rep(analysisId,nr1),
                                        Eval=rep('validation', nr1),
                                        performance$demographicSummary)
  }
  nr1 <- nrow(performance$calibrationSummary)
  performance$calibrationSummary <- cbind(analysisId=rep(analysisId,nr1),
                                          Eval=rep('validation', nr1),
                                          performance$calibrationSummary)
  nr1 <- nrow(performance$predictionDistribution)
  performance$predictionDistribution <- cbind(analysisId=rep(analysisId,nr1),
                                          Eval=rep('validation', nr1),
                                          performance$predictionDistribution)
  

  if (!silent)
    ParallelLogger::logInfo(paste("Evaluation completed at ", Sys.time(), " taking ", start.pred - Sys.time()))

  if (!silent)
    ParallelLogger::logInfo(paste("Starting covariate summary at ", Sys.time()))
  start.pred  <- Sys.time()
  covSum <- covariateSummary(plpData, population)
  if(exists("plpModel")){
    if(!is.null(plpModel$varImp)){
      covSum <- merge(plpModel$varImp[,colnames(plpModel$varImp)!='covariateName'], covSum, by='covariateId', all=T)
    }
  }
  
  if (!silent)
    ParallelLogger::logInfo(paste("Covariate summary completed at ", Sys.time(), " taking ", start.pred - Sys.time()))
  
  executionSummary <- list(PackageVersion = list(rVersion= R.Version()$version.string,
                                                 packageVersion = utils::packageVersion("PatientLevelPrediction")),
                           PlatformDetails= list(platform= R.Version()$platform,
                                                 cores= Sys.getenv('NUMBER_OF_PROCESSORS'),
                                                 RAM=utils::memory.size()), #  test for non-windows needed
                           # Sys.info()
                           TotalExecutionElapsedTime = NULL,
                           ExecutionDateTime = Sys.Date())
  
  result <- list(prediction = prediction, 
                 performanceEvaluation = performance,
                 inputSetting = list(outcomeId=attr(population, "metaData")$outcomeId,
                                 cohortId= plpData$metaData$call$cohortId,
                                 database = plpData$metaData$call$cdmDatabaseSchema),
                 executionSummary = executionSummary,
                 model = list(model='applying plp model',
                              modelAnalysisId = plpModel$analysisId,
                              modelSettings = plpModel$modelSettings),
                 analysisRef=list(analysisId=NULL,
                                  analysisName=NULL,
                                  analysisSettings= NULL),
                 covariateSummary=covSum)
  return(result)
}


#' Extract new plpData using plpModel settings
#' use metadata in plpModel to extract similar data and population for new databases:
#'
#' @param plpModel         The trained PatientLevelPrediction model or object returned by runPlp()
#' @param createCohorts          Create the tables for the target and outcome - requires sql in the plpModel object
#' @param newConnectionDetails      The connectionDetails for the new database
#' @param newCdmDatabaseSchema      The database schema for the new CDM database 
#' @param newCohortDatabaseSchema   The database schema where the cohort table is stored
#' @param newCohortTable            The table name of the cohort table
#' @param newCohortId               The cohort_definition_id for the cohort of at risk people
#' @param newOutcomeDatabaseSchema  The database schema where the outcome table is stored
#' @param newOutcomeTable           The table name of the outcome table
#' @param newOutcomeId              The cohort_definition_id for the outcome  
#' @param newOracleTempSchema       The temp coracle schema
#' @param sample                    The number of people to sample (default is NULL meaning use all data)
#' @param createPopulation          Whether to create the study population as well
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
                           createCohorts = T,
                           newConnectionDetails,
                           newCdmDatabaseSchema = NULL,
                           newCohortDatabaseSchema = NULL,
                           newCohortTable = NULL,
                           newCohortId = NULL,
                           newOutcomeDatabaseSchema = NULL,
                           newOutcomeTable = NULL,
                           newOutcomeId = NULL,
                           newOracleTempSchema = newCdmDatabaseSchema,
                           sample=NULL, 
                           createPopulation= T) {
  
  # check logger
  if(length(ParallelLogger::getLoggers())==0){
    logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                        threshold = "INFO",
                                        appenders = list(ParallelLogger::createConsoleAppender(layout = ParallelLogger::layoutTimestamp)))
    ParallelLogger::registerLogger(logger)
  }
  
  if(is.null(plpModel))
    return(NULL)
  if(class(plpModel)!='plpModel' && class(plpModel)!='runPlp' )
    return(NULL)
  if(class(plpModel)=='runPlp')
    plpModel <- plpModel$model 
  
  if(missing(newConnectionDetails)){
   stop('connection details not entered')
  } else {
  connection <- DatabaseConnector::connect(newConnectionDetails)
  }
  
  if(createCohorts){
    if(is.null(plpModel$metaData$cohortCreate$targetCohort$sql))
      stop('No target cohort code')
    if(is.null(plpModel$metaData$cohortCreate$outcomeCohorts[[1]]$sql))
      stop('No outcome cohort code')
    
 
    exists <- toupper(newCohortTable)%in%DatabaseConnector::getTableNames(connection , newCohortDatabaseSchema)
    if(!exists){
      ParallelLogger::logTrace('Creating temp cohort table')
    sql <- "create table @target_cohort_schema.@target_cohort_table(cohort_definition_id bigint, subject_id bigint, cohort_start_date datetime, cohort_end_date datetime)"
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    sql <- SqlRender::renderSql(sql,
                                target_cohort_schema = newCohortDatabaseSchema,
                                target_cohort_table= newCohortTable)$sql
    tryCatch(DatabaseConnector::executeSql(connection,sql),
         error = stop, finally = ParallelLogger::logTrace('Cohort table created'))
    }
    
    exists <- toupper(newOutcomeTable)%in%DatabaseConnector::getTableNames(connection , newOutcomeDatabaseSchema)
    if(!exists){
      sql <- "create table @target_cohort_schema.@target_cohort_table(cohort_definition_id bigint, subject_id bigint, cohort_start_date datetime, cohort_end_date datetime)"
      sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
      sql <- SqlRender::renderSql(sql,
                                  target_cohort_schema = newOutcomeDatabaseSchema,
                                  target_cohort_table= newOutcomeTable)$sql
      tryCatch(DatabaseConnector::executeSql(connection,sql),
           error = stop, finally = ParallelLogger::logTrace('outcome table created'))
      
    }
    
    ParallelLogger::logTrace('Populating cohort tables')
    targetSql <- plpModel$metaData$cohortCreate$targetCohort$sql
    targetSql <- SqlRender::renderSql(targetSql, 
                                      cdm_database_schema=ifelse(is.null(newCdmDatabaseSchema),plpModel$metaData$call$cdmDatabaseSchema,newCdmDatabaseSchema),
                                      target_database_schema= ifelse(is.null(newCohortDatabaseSchema),plpModel$metaData$call$cdmDatabaseSchema,newCohortDatabaseSchema),
                                      target_cohort_table = ifelse(is.null(newCohortTable),plpModel$metaData$call$newCohortTable,newCohortTable),
                                      target_cohort_id = ifelse(is.null(newCohortId),plpModel$metaData$call$cohortId, newCohortId) )$sql
    
    targetSql <- SqlRender::translateSql(targetSql, 
                                         targetDialect = ifelse(is.null(newConnectionDetails$dbms), 'pdw',newConnectionDetails$dbms)  )$sql
    DatabaseConnector::executeSql(connection, targetSql)
    
    for(outcomesql in plpModel$metaData$cohortCreate$outcomeCohorts){
      outcomeSql <- outcomesql$sql
      outcomeSql <- SqlRender::renderSql(outcomeSql, 
                                         cdm_database_schema=ifelse(is.null(newCdmDatabaseSchema),plpModel$metaData$call$cdmDatabaseSchema,newCdmDatabaseSchema),
                                         target_database_schema= ifelse(is.null(newOutcomeDatabaseSchema),plpModel$metaData$call$cdmDatabaseSchema,newOutcomeDatabaseSchema),
                                         target_cohort_table = ifelse(is.null(newOutcomeTable),plpModel$metaData$call$newOutcomeTable,newOutcomeTable),
                                         target_cohort_id = ifelse(is.null(newOutcomeId),plpModel$metaData$call$outcomeId, newOutcomeId))$sql
      outcomeSql <- SqlRender::translateSql(outcomeSql, 
                                            targetDialect = ifelse(is.null(newConnectionDetails$dbms), 'pdw',newConnectionDetails$dbms))$sql
      DatabaseConnector::executeSql(connection, outcomeSql)
      
    }
   
    
  }
  
  ParallelLogger::logTrace('Loading model data extraction settings')
  dataOptions <- as.list(plpModel$metaData$call)
  dataOptions[[1]] <- NULL
  dataOptions$sampleSize <- sample
  
  dataOptions$covariateSettings$includedCovariateIds <-  plpModel$varImp$covariateId[plpModel$varImp$covariateValue!=0]
  
  ParallelLogger::logTrace('Adding new settings if set...')
  if(is.null(newCdmDatabaseSchema))
    return(NULL)
  dataOptions$cdmDatabaseSchema <- newCdmDatabaseSchema
  
  if(!is.null(newConnectionDetails))
    dataOptions$connectionDetails <- newConnectionDetails # check name

  if(!is.null(newCohortId))
    dataOptions$cohortId <- newCohortId
  if(!is.null(newOutcomeId))
    dataOptions$outcomeIds <- newOutcomeId
  
  if(!is.null(newCohortDatabaseSchema))
    dataOptions$cohortDatabaseSchema <- newCohortDatabaseSchema  # correct names?
  if(!is.null(newCohortTable))
    dataOptions$cohortTable <- newCohortTable
  
  if(!is.null(newOutcomeDatabaseSchema))
    dataOptions$outcomeDatabaseSchema <- newOutcomeDatabaseSchema # correct names?
  if(!is.null(newOutcomeTable))
    dataOptions$outcomeTable <- newOutcomeTable
  if(!is.null(newOracleTempSchema))
    dataOptions$oracleTempSchema <- newOracleTempSchema # check name
  
  
  dataOptions$baseUrl <- NULL
  
  plpData <- do.call(getPlpData, dataOptions)
  
  if(!createPopulation) return(plpData)
  
  # get the popualtion
  ParallelLogger::logTrace('Loading model population settings')
  popOptions <- plpModel$populationSettings
  popOptions$cohortId <- dataOptions$cohortId
  popOptions$outcomeId <- dataOptions$outcomeIds
  popOptions$plpData <- plpData
  population <- do.call(PatientLevelPrediction::createStudyPopulation, popOptions)
  
  
  # return the popualtion and plpData for the new database
  ParallelLogger::logTrace('Returning population and plpData for new data using model settings')
  return(list(population=population,
              plpData=plpData))
}
