# @file RunMultiplePlp.R
#
# Copyright 2017 Observational Health Data Sciences and Informatics
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

#' Develop patient-level predcition  models for multiple outcomes, target popuations and 
#' settings
#'
#' @param outputFolder                The directory to save the results and data to - needs read/write privileges
#' @param connectionDetails          An R object of type\cr\code{connectionDetails} created using the
#'                                   function \code{createConnectionDetails} in the
#'                                   \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema            The name of the database schema that contains the OMOP CDM
#'                                     instance.  Requires read permissions to this database. On SQL
#'                                     Server, this should specifiy both the database and the schema,
#'                                     so for example 'cdm_instance.dbo'.
#' @param oracleTempSchema             For Oracle only: the name of the database schema where you want
#'                                     all temporary tables to be managed. Requires create/insert
#'                                     permissions to this database.
#' @param cohortDatabaseSchema         The name of the database schema that is the location where the
#'                                     cohort data used to define the at risk cohort is available.
#'                                     If cohortTable = DRUG_ERA, cohortDatabaseSchema is not used
#'                                     by assumed to be cdmSchema.  Requires read permissions to this
#'                                     database.
#' @param cohortTable                  The tablename that contains the at risk cohort.  If
#'                                     cohortTable <> DRUG_ERA, then expectation is cohortTable has
#'                                     format of COHORT table: cohort_concept_id, SUBJECT_ID,
#'                                     COHORT_START_DATE, COHORT_END_DATE.
#' @param outcomeDatabaseSchema            The name of the database schema that is the location where
#'                                         the data used to define the outcome cohorts is available. If
#'                                         cohortTable = CONDITION_ERA, exposureDatabaseSchema is not
#'                                         used by assumed to be cdmSchema.  Requires read permissions
#'                                         to this database.
#' @param outcomeTable                     The tablename that contains the outcome cohorts.  If
#'                                         outcomeTable <> CONDITION_OCCURRENCE, then expectation is
#'                                         outcomeTable has format of COHORT table:
#'                                         COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                         COHORT_END_DATE.
#' @param cdmVersion                   Define the OMOP CDM version used: currently support "4" and "5".
#' @param studyStartDate               A calendar date specifying the minimum date that a cohort index
#'                                     date can appear. Date format is 'yyyymmdd'.
#' @param studyEndDate                 A calendar date specifying the maximum date that a cohort index
#'                                     date can appear. Date format is 'yyyymmdd'. Important: the study
#'                                     end data is also used to truncate risk windows, meaning no outcomes
#'                                     beyond the study end date will be considered.
#' @param atRiskCohortIds            A vector containing the unique identifiers to define the at risk cohorts.  
#'                                   Each at risk cohortId is used to select the cohort_concept_id in the cohort-like table.
#'                                    
#' @param outcomeIds                   A list of cohort_definition_ids used to define outcomes.
#' @param covariateSettings            An object of type \code{covariateSettings} as created using the
#'                                     \code{createCovariateSettings} function in the
#'                                     \code{FeatureExtraction} package.  This can be a list of multiple
#'                                     settings.
#' @param timeAtRisks                A list detailing the time at risk intervals that willbe used
#'                                   to create the prediciton models (the period of time we wish to predict the outcome
#'                                   occurence within) created using the function \code{setTimeAtRisks}.
#' @param modelSettings            A list of model settings created using the \code{setGradientBoostingMachine}, 
#'                                 \code{setRandomForest}, \code{setLassoLogisticRegression}, \code{setNaiveBayes} or
#'                                 \code{setKNN}.
#' @param internalValidation       The type of internal validation for the model.  Either 
#'                                 'person' which stratifies by outcome to partion into test/train sets or
#'                                 'time' which picks a set date and all people with an at risk cohort
#'                                 start date prior to this join the train set and people after join the
#'                                 test set. 
#' @param testFraction            The fracion of the target population to include into the test set
#' @param nfold                   The number of cross validation folds to apply when finding the optimal hyperparameters
#' @param splitSeed               (default NULL) The seed used to do the random split for internalValidation='person'
#' @param indexes                The nfold validation indexes
#' @param verbosity               Sets the level of the verbosity. If the log level is at or higher in priority than the logger threshold, a message will print. The levels are:
#'                                         \itemize{
#'                                         \item{DEBUG}{Highest verbosity showing all debug statements}
#'                                         \item{TRACE}{Showing information about start and end of steps}
#'                                         \item{INFO}{Show informative information (Default)}
#'                                         \item{WARN}{Show warning messages}
#'                                         \item{ERROR}{Show error messages}
#'                                         \item{FATAL}{Be silent except for fatal errors}
#'                                         }                                             
#'
#' @export
runPlpAnalyses <- function(outputFolder = getwd(),
                           connectionDetails=NULL, 
                           cdmDatabaseSchema=NULL,
                           oracleTempSchema = cdmDatabaseSchema,
                           cohortDatabaseSchema = cdmDatabaseSchema,
                           cohortTable = "cohort",
                           outcomeDatabaseSchema = cdmDatabaseSchema,
                           outcomeTable = "cohort",
                           cdmVersion = "5",
                           studyStartDate="", studyEndDate = "", 
                           atRiskCohortIds=1, outcomeIds=2, 
                           covariateSettings=list(FeatureExtraction::createCovariateSettings(useCovariateDemographics=T,
                                                                                             useCovariateDemographicsGender=T,
                                                                                             useCovariateDemographicsRace=T,
                                                                                             useCovariateDemographicsAge=T,
                                                                                             useCovariateDemographicsYear = F,
                                                                                             useCovariateDemographicsMonth = T,
                                                                                             useCovariateConditionOccurrence=T,
                                                                                             useCovariateConditionOccurrence365d=T),
                                                  FeatureExtraction::createCovariateSettings(useCovariateDemographics=T,
                                                                                             useCovariateDemographicsGender=T,
                                                                                             useCovariateDemographicsRace=T,
                                                                                             useCovariateDemographicsAge=T,
                                                                                             useCovariateDemographicsYear = F,
                                                                                             useCovariateDemographicsMonth = T,
                                                                                             useCovariateDrugExposure=T,
                                                                                             useCovariateDrugExposure365d=T)
                                                  ),
                           timeAtRisks=list(setTimeAtRisks(riskWindowEnd=365),
                                            setTimeAtRisks(riskWindowEnd=365*2)), 
                           modelSettings=NULL,
                           internalValidation='time', testFraction=0.25, nfold=3,
                           splitSeed=NULL, indexes=NULL, # need to add these to into
                           verbosity=futile.logger::INFO){
  
  
  # add a summary of the results
  #======================
  # then do covariate, model and tar as rds in folder?
  if(!dir.exists(file.path(outputFolder, 'settings')))
    dir.create(file.path(outputFolder, 'settings'), recursive = T)
  saveRDS(covariateSettings, file.path(outputFolder, 'settings','covariateSettings.rds'))
  saveRDS(timeAtRisks, file.path(outputFolder, 'settings','timeatrisks.rds'))
  saveRDS(modelSettings, file.path(outputFolder, 'settings','modelSettings.rds'))
  #======================
  
  # setting initial values:
  analysisId <- 0
  reference <- list()

  flog.info("Running multiple prediction analyses")
  flog.seperator()
  
  flog.info("Step 1: finding superset of covariate settings...")
  
  # find union of covariates for extracting 
  unionCovariateSettings <- supersetCovariates(covariateSettings)
  
  # for each Target popualtion
  for(atRiskCohort in atRiskCohortIds){
    reference$atRiskCohort <- atRiskCohort
      
    flog.info("Extracting data for at risk cohort %s", atRiskCohort)
    
    
    # get the data for all outcomes using the union of the covaraites
    dataExtractSettings <- list(connectionDetails=connectionDetails, 
                                cdmDatabaseSchema=cdmDatabaseSchema,
                                oracleTempSchema = oracleTempSchema,
                                cohortDatabaseSchema = cohortDatabaseSchema,
                                cohortTable = cohortTable,
                                outcomeDatabaseSchema = outcomeDatabaseSchema,
                                outcomeTable = outcomeTable,
                                cdmVersion = cdmVersion,
                                outcomeIds=outcomeIds,
                                cohortId=atRiskCohort,  
                                studyStartDate = studyStartDate,
                                studyEndDate = studyEndDate,
                                excludeDrugsFromCovariates = F, #ToDo: rename to excludeFromFeatures
                                firstExposureOnly = FALSE,
                                washoutPeriod = 0,
                                covariateSettings = unionCovariateSettings
    )
    
    
    plpDataTemp <- do.call(getPlpData,dataExtractSettings)
    
    # now for each covariateSetting restrict to that setting
    plpDatas<- lapply( lapply(covariateSettings, 
                              function(x) list(covariateSetting=x, plpData=plpDataTemp)), 
                       function(x2) do.call(restrictCovariates, x2)  )
    #c(covariateSetting=covariateSettings, plpData=plpDataTemp)
    for(covId in 1:length(plpDatas)){
      reference$covId <- covId
      
      plpData <- plpDatas[[covId]]
      # save plpData
      PatientLevelPrediction::savePlpData(plpData, 
                                          file=file.path(outputFolder, 'Data', paste('cid',atRiskCohort,'covId',covId, sep='_')))
      
      # now for each O
      for (outcome in outcomeIds){
        reference$outcome <- outcome
        
        for(tid in 1:length(timeAtRisks)){
          reference$tid <- tid
          tar <- timeAtRisks[[tid]]
          
          flog.info("Creating dataset...")
          
          popSettings <- c(list(plpData=plpData,
                                outcomeId=outcome,
                                binary = T,
                                verbosity = verbosity), tar)
          
          # do the population
          population <- do.call(createStudyPopulation, popSettings)
          
          #save population
          if(!dir.exists(file.path(outputFolder, 'Populations')))
            dir.create(file.path(outputFolder, 'Populations'))
          write.csv(population, file.path(outputFolder, 'Populations', paste0('cid_',atRiskCohort,'_oid_',outcome,'_covId_',covId,'_tid_',tid,'.csv')))
          
          for(mid in 1:length(modelSettings)){
            m <- modelSettings[[mid]]
            reference$mid <- m$mid
            
            analysisId <- analysisId + 1
            reference$analysisId <- analysisId
            
            # do the settings
            rownames(plpData$covariates) <- NULL # a bug fix?
            plpSettings <- list(population=population, 
                                plpData=plpData,
                                modelSettings=m,
                                testSplit = internalValidation, testFraction=testFraction, 
                                splitSeed=splitSeed, nfold=nfold, indexes=indexes,
                                save=NULL, saveModel=F,
                                verbosity=verbosity, analysisId=analysisId)
            
            # runPlp
            result <- do.call(RunPlp, plpSettings)
            
            # save the results into suitable structure:
            flog.info("Saving result...")
            PatientLevelPrediction::savePlpResult(result, file.path(outputFolder,'Analysis',analysisId,'Results'))
            
            # append the details to the referenceTable
            if(!file.exists(file.path(outputFolder, 'referenceTable.txt')))
              write.table(t(unlist(reference)), file.path(outputFolder, 'referenceTable.txt'), 
                        row.names = F, col.names = T )
            if(file.exists(file.path(outputFolder, 'referenceTable.txt')))
              write.table(t(unlist(reference)), file.path(outputFolder, 'referenceTable.txt'), 
                        row.names = F, col.names = F, append = T )
            
            # append the details to the summaryTable
            resultSum <- result$performanceEvaluation$evaluationStatistics[result$performanceEvaluation$evaluationStatistics[,2]=='test',]
            
            if(!file.exists(file.path(outputFolder, 'summaryTable.txt')))
              write.table(resultSum, file.path(outputFolder, 'summaryTable.txt'), 
                        row.names = F, col.names = T )
            if(file.exists(file.path(outputFolder, 'summaryTable.txt')))
              write.table(resultSum, file.path(outputFolder, 'summaryTable.txt'), 
                        row.names = F, col.names = F, append = T )
            
          } # end for models
          
        } # end for tar
        
      } # end for O
      
    } # end for covariate setting
    
  } # end for T
  

}

#' setTimeAtRisk
#' 
#' create the timeAtRisks for the multiple analysis studies
#' 
#' @param includeAllOutcomes     Do you want to include people who have the outcome but are not observed for the whole at risk period?
#' @param firstExposureOnly      Only consider the first time occurence of the outcome?
#' @param washoutPeriod          The minimum prior observation in days a person required to be included 
#' @param removeSubjectsWithPriorOutcome   Remove people who have the outcome some period before the time at risk?
#' @param priorOutcomeLookback   The number of days prior to investigate for the variable removeSubjectsWithPriorOutcome
#' @param riskWindowStart        The number of days after the at risk popualtion subject's index date to start the time at risk period
#' @param addExposureDaysToStart Should the risk window start be relative to the index end date instead?
#' @param riskWindowEnd          The number of days after the at risk popualtion subject's index date to end the time at risk period
#' @param addExposureDaysToEnd Should the risk window end be relative to the index end date instead?
#' @param requireTimeAtRisk     Should you only include people with a minimum time at risk period?
#' @param minTimeAtRisk         If requireTimeAtRisk is TRUE, then this is the minimum number of days a person must be at risk
#' 
#' @export 
setTimeAtRisk <- function(includeAllOutcomes = T,
                          firstExposureOnly = F,
                          washoutPeriod = 0,
                          removeSubjectsWithPriorOutcome = T,
                          priorOutcomeLookback = 99999,
                          riskWindowStart = 1,
                          addExposureDaysToStart = F,
                          riskWindowEnd = 365,
                          addExposureDaysToEnd = F,
                          requireTimeAtRisk = T,
                          minTimeAtRisk=riskWindowEnd-riskWindowStart 
                         ){
  
  return(list(includeAllOutcomes = includeAllOutcomes,
              firstExposureOnly = firstExposureOnly,
              washoutPeriod = washoutPeriod,
              removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
              priorOutcomeLookback = priorOutcomeLookback,
              requireTimeAtRisk = requireTimeAtRisk,
              minTimeAtRisk=minTimeAtRisk, 
              riskWindowStart = riskWindowStart,
              addExposureDaysToStart = addExposureDaysToStart,
              riskWindowEnd = riskWindowEnd,
              addExposureDaysToEnd = addExposureDaysToEnd))
}


supersetCovariates <- function(covariateSettings){
  
  # union the covariates and create a metaData with the analysisIds included/exclude, conceptIds include/excluded
  if(class(covariateSettings)=='covariateSettings')
    return(covariateSettings)
  
  if(class(covariateSettings)=='list'){
    
    # return a boolean vector if any of the settings are true for it 
    # of the minimum of the deleteCovariatesSmallCount settings
    anyTrue <- function(name){
      if(!name%in%c('deleteCovariatesSmallCount','excludedCovariateConceptIds','includedCovariateConceptIds') ){
        i <- which(names(covariateSettings[[1]])==name)
        return(any(unlist(lapply(covariateSettings, function(x) x[[i]]))))
      }
      if(name=='deleteCovariatesSmallCount'){
        return(min(unlist(lapply(covariateSettings, function(x) x$deleteCovariatesSmallCount))) )
      }
      if(name=='excludedCovariateConceptIds'){
        if(sum(unlist(lapply(covariateSettings, function(x) is.null(x$excludedCovariateConceptIds))))>0){
          return(NULL)
        } else {
          return(Reduce(intersect, (lapply(covariateSettings, function(x) x$excludedCovariateConceptIds) ) ))
        }
      }
      if(name=='includedCovariateConceptIds'){
        #if(sum(unlist(lapply(covariateSettings, function(x) !is.null(x$includedCovariateConceptIds))))==0){
        #  return(NULL)
        #} else {
          return(Reduce(union,(lapply(covariateSettings, function(x) x$includedCovariateConceptIds)) ) )
        #}
      }
    }
    
    covNames <- names(FeatureExtraction::createCovariateSettings(useCovariateDemographics = T,
                                                                 useCovariateDemographicsGender = F,
                                                                 useCovariateDemographicsAge = F,
                                                                 useCovariateDrugExposure = T,
                                                                 useCovariateDrugExposure30d = T, 
                                                                 excludedCovariateConceptIds = c(13,20), 
                                                                 includedCovariateConceptIds = c(56),
                                                                 deleteCovariatesSmallCount = 10))
    
    result <- lapply(covNames, anyTrue)
    names(result) <- covNames
    
    attr(result, "fun") <- "getDbDefaultCovariateData"
    attr(result, "class") <- "covariateSettings"
    attr(result, "names") <- covNames
    
    return(result)
  }
}

#+++++++++++++++++++++++++++++++++++++++++++++
#==============================================
# create an exclude/include covariates filter
#==============================================
restrictCovariates <- function(plpData, covariateSetting){
  
  ind.all <- ff::as.ff(c(0))
  ind.covRef <- c()
  
  if(length(covariateSetting$includedCovariateIds)!=0){
    ind.all <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covariateSetting$includedCovariateIds))
    ind.all <- ffbase::ffwhich(ind.all, !is.na(ind.all))
    ind.covRef <- ffbase::ffmatch(plpData$covariateRef$covariateId, table=ff::as.ff(covariateSetting$includedCovariateIds))
    ind.covRef <- ff::as.ram(ffbase::ffwhich(ind.covRef, !is.na(ind.covRef)))
  }
  
  #=======================================
  #   DEMOGRAPHICS
  #=======================================
  if(covariateSetting$useCovariateDemographics == TRUE){
    
    # get gender covariate indexes if selected
    if(covariateSetting$useCovariateDemographicsGender==TRUE){
      covs <- plpData$covariateRef$covariateId[plpData$covariateRef$analysisId==2]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=covs)
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)))
      
      ind.covRef <- c(ind.covRef, 
                      which(ff::as.ram(plpData$covariateRef$analysisId)==2) ) # add covariateRef info
    }  
    
    # get Race covariate indexes if selected
    if(covariateSetting$useCovariateDemographicsRace==TRUE){
      covs <- plpData$covariateRef$covariateId[plpData$covariateRef$analysisId==3]
      if(sum(ff::as.ram(plpData$covariateRef$analysisId)==3)>0){
        ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
        ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)))
        ind.covRef <- c(ind.covRef, 
                        which(ff::as.ram(plpData$covariateRef$analysisId)==3) ) # add covariateRef info
      }
    }  
    
    # get Ethnicity covariate indexes if selected
    if(covariateSetting$useCovariateDemographicsEthnicity==TRUE){
      covs <- ff::as.ram(plpData$covariateRef$covariateId)[ff::as.ram(plpData$covariateRef$analysisId)==4 &
                                                            ff::as.ram(plpData$covariateRef$conceptId)!=0]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(all.ind, ffbase::ffwhich(ind.x, !is.na(ind.x)))
      ind.covRef <- c(ind.covRef, 
                      which(ff::as.ram(plpData$covariateRef$analysisId)==4 &
                        ff::as.ram(plpData$covariateRef$conceptId)!=0))
      
    }  

    # get Age covariate indexes if selected
    if(covariateSetting$useCovariateDemographicsAge==TRUE){
      covs <- ff::as.ram(plpData$covariateRef$covariateId)[ff::as.ram(plpData$covariateRef$analysisId)==4 &
                                                 ff::as.ram(plpData$covariateRef$conceptId)==0]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)))
      ind.covRef <- c(ind.covRef, 
                      which(ff::as.ram(plpData$covariateRef$analysisId)==4 &
                        ff::as.ram(plpData$covariateRef$conceptId)==0))
    }  
    
    # get Year covariate indexes if selected
    if(covariateSetting$useCovariateDemographicsYear==TRUE){
      covs <- plpData$covariateRef$covariateId[plpData$covariateRef$analysisId==5]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=covs)
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      
      ind.covRef <- c(ind.covRef, 
                      which(ff::as.ram(plpData$covariateRef$analysisId)==5) )
    } 
    
    # get Month covariate indexes if selected
    if(covariateSetting$useCovariateDemographicsMonth ==TRUE){
      covs <- plpData$covariateRef$covariateId[plpData$covariateRef$analysisId==6]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=covs)
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      
      ind.covRef <- c(ind.covRef, 
                      which(ff::as.ram(plpData$covariateRef$analysisId)==6) )
    } 
 
    
  }

  
  #=======================================
  #   CONDITIONS
  #=======================================
  # find the condition group indexes:
  condGroup <- grep('condition group', as.character(ff::as.ram(plpData$covariateRef$covariateName)))
  
  if(covariateSetting$useCovariateConditionOccurrence == TRUE){
    
  
    # get useCovariateConditionOccurrence365d covariate indexes if selected
    if(covariateSetting$useCovariateConditionOccurrence365d ==TRUE){
      covs <- plpData$covariateRef$covariateId[setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==101)),
                                                       condGroup)]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef, 
                      setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==101)),
                              condGroup) )
    } 
    
    # get useCovariateConditionOccurrence30d covariate indexes if selected
    if(covariateSetting$useCovariateConditionOccurrence30d ==TRUE){
      covs <- plpData$covariateRef$covariateId[setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==102)),
                                                       condGroup)]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef, 
                      setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==102)),
                              condGroup) )
    } 
    
    # get useCovariateConditionOccurrenceInpt180d covariate indexes if selected
    if(covariateSetting$useCovariateConditionOccurrenceInpt180d ==TRUE){
      covs <- plpData$covariateRef$covariateId[setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==103)),
                                                       condGroup)]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef, 
                      setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==103)),
                              condGroup) )
    } 
  }


  if(covariateSetting$useCovariateConditionEra == TRUE){
    
    # get useCovariateConditionEraEver covariate indexes if selected
    if(covariateSetting$useCovariateConditionEraEver ==TRUE){
      covs <- plpData$covariateRef$covariateId[setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==201)),
                                                       condGroup)]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef, 
                      setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==201)),
                              condGroup) )
    }
    
    # get useCovariateConditionEraOverlap covariate indexes if selected
    if(covariateSetting$useCovariateConditionEraOverlap ==TRUE){
      covs <- plpData$covariateRef$covariateId[setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==202)),
                                                       condGroup)]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef, 
                      setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==202)),
                              condGroup) )
    }
  } 

  if(covariateSetting$useCovariateConditionGroup == TRUE){
    #useCovariateConditionGroupMeddra, (covariate name contains condition group),
    #useCovariateConditionGroupSnomed = FALSE,
  } 

  
  #=======================================
  #   DRUGS
  #=======================================
  drugGroup <- grep('drug group', as.character(ff::as.ram(plpData$covariateRef$covariateName)))
  
  if(covariateSetting$useCovariateDrugExposure == TRUE){
    # find the condition group indexes:
   
    # get useCovariateDrugExposure365d covariate indexes if selected
    if(covariateSetting$useCovariateDrugExposure365d ==TRUE){
      covs <- plpData$covariateRef$covariateId[setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==401)),
                                                       drugGroup)]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef, 
                      setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==401)),
                              drugGroup) )
    } 

    # get useCovariateDrugExposure30d covariate indexes if selected
    if(covariateSetting$useCovariateDrugExposure30d ==TRUE){
      covs <- plpData$covariateRef$covariateId[setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==402)),
                                                       drugGroup)]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef, 
                      setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==402)),
                              drugGroup) )
    }     
    
  }

  if(covariateSetting$useCovariateDrugEra == TRUE){
    
    # get useCovariateDrugEra365d covariate indexes if selected
    if(covariateSetting$useCovariateDrugEra365d ==TRUE){
      covs <- plpData$covariateRef$covariateId[setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==501)),
                                                       drugGroup)]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef, 
                      setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==501)),
                              drugGroup) )
    } 
    # get useCovariateDrugEra30d covariate indexes if selected
    if(covariateSetting$useCovariateDrugEra30d ==TRUE){
      covs <- plpData$covariateRef$covariateId[setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==502)),
                                                       drugGroup)]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef, 
                      setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==502)),
                              drugGroup) )
    } 
    # get useCovariateDrugEraOverlap covariate indexes if selected
    if(covariateSetting$useCovariateDrugEraOverlap ==TRUE){
      covs <- plpData$covariateRef$covariateId[setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==503)),
                                                       drugGroup)]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef, 
                      setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==503)),
                              drugGroup) )
    } 
    # get useCovariateDrugEraEver covariate indexes if selected
    if(covariateSetting$useCovariateDrugEraEver ==TRUE){
      covs <- plpData$covariateRef$covariateId[setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==504)),
                                                       drugGroup)]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef, 
                      setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==504)),
                              drugGroup) )
    } 
  }
  

  if(covariateSetting$useCovariateDrugGroup==TRUE){
    covs <- plpData$covariateRef$covariateId[drugGroup]
    ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
    
    ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
    ind.covRef <- c(ind.covRef, 
                   drugGroup )
  } 
  
  
  #=======================================
  #   PROCEDURES
  #=======================================
  if(covariateSetting$useCovariateProcedureOccurrence == TRUE){
    # find the condition group indexes:
    procedureGroup <- grep('procedure group', as.character(ff::as.ram(plpData$covariateRef$covariateName)))
    
    # get useCovariateProcedureOccurrence365d covariate indexes if selected
    if(covariateSetting$useCovariateProcedureOccurrence365d ==TRUE){
      covs <- plpData$covariateRef$covariateId[setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==701)),
                                                       procedureGroup)]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef, 
                      setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==701)),
                              procedureGroup) )
    } 
    
    # get useCovariateProcedureOccurrence30d covariate indexes if selected
    if(covariateSetting$useCovariateProcedureOccurrence30d ==TRUE){
      covs <- plpData$covariateRef$covariateId[setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==702)),
                                                       procedureGroup)]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef, 
                      setdiff(which(ff::as.ram(plpData$covariateRef$analysisId==702)),
                              procedureGroup) )
    } 
    
    if(useCovariateProcedureGroup ==TRUE){
      #  useCovariateProcedureGroup, 700-800 (covariate name contains procedure group), 
      covs <- plpData$covariateRef$covariateId[procedureGroup]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef, 
                      procedureGroup )
    } 
    
  }
  


  #=======================================
  #   OBSERVATIONS
  #=======================================
  if(covariateSetting$useCovariateObservation ==TRUE){
    
    # get useCovariateObservation365d covariate indexes if selected
    if(covariateSetting$useCovariateObservation365d ==TRUE){
      covs <- plpData$covariateRef$covariateId[plpData$covariateRef$analysisId==901]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef,which(ff::as.ram(plpData$covariateRef$analysisId==901) ))
    } 
    
    # get useCovariateObservation30d covariate indexes if selected
    if(covariateSetting$useCovariateObservation30d ==TRUE){
      covs <- plpData$covariateRef$covariateId[plpData$covariateRef$analysisId==902]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef,which(ff::as.ram(plpData$covariateRef$analysisId==902) ))
    } 
    
    # get useCovariateObservationCount365d covariate indexes if selected
    if(covariateSetting$useCovariateObservationCount365d ==TRUE){
      covs <- plpData$covariateRef$covariateId[plpData$covariateRef$analysisId==905]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef,which(ff::as.ram(plpData$covariateRef$analysisId==905) ))
    } 
    
  }

  #=======================================
  #   MEASUREMENTS
  #=======================================
  if(covariateSetting$useCovariateMeasurement == TRUE){
    # get useCovariateMeasurement365d covariate indexes if selected
    if(covariateSetting$useCovariateMeasurement365d ==TRUE){
      covs <- plpData$covariateRef$covariateId[plpData$covariateRef$analysisId==901]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef,which(ff::as.ram(plpData$covariateRef$analysisId==901) ))
    } 
    
    # get useCovariateMeasurement30d covariate indexes if selected
    if(covariateSetting$useCovariateMeasurement30d ==TRUE ){
      covs <- plpData$covariateRef$covariateId[plpData$covariateRef$analysisId==902]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef,which(ff::as.ram(plpData$covariateRef$analysisId==902) ))
    } 
    
    # get useCovariateMeasurementCount365d covariate indexes if selected
    if(covariateSetting$useCovariateMeasurementCount365d ==TRUE){
      covs <- plpData$covariateRef$covariateId[plpData$covariateRef$analysisId==905]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef,which(ff::as.ram(plpData$covariateRef$analysisId==905) ))
    } 
    
    # get useCovariateMeasurementBelow covariate indexes if selected
    if(covariateSetting$useCovariateMeasurementBelow ==TRUE || covariateSetting$useCovariateMeasurementAbove ==TRUE){
      covs <- plpData$covariateRef$covariateId[plpData$covariateRef$analysisId==903]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef,which(ff::as.ram(plpData$covariateRef$analysisId==903) ))
    } 
    
    # get useCovariateConceptCounts covariate indexes if selected
    if(covariateSetting$useCovariateConceptCounts ==TRUE){
      
      ind <- ffbase::ffmatch(plpData$covariateRef$analysisId, table=ff::as.ff(1000:1007))
      covs <- plpData$covariateRef$covariateId[ffbase::ffwhich(ind, !is.na(ind))]

      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covs))
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)) )
      ind.covRef <- c(ind.covRef,ff::as.ram(ffbase::ffwhich(ind, !is.na(ind)) ) )
    }
  }

  
  #=======================================
  #   RISKS
  #=======================================
  # add the risk scores
  if(covariateSetting$useCovariateRiskScores == TRUE){
    
    # get riskScoresCharlson covariate indexes if selected
    if(covariateSetting$useCovariateRiskScoresCharlson==TRUE){
      covs <- plpData$covariateRef$covariateId[plpData$covariateRef$analysisId==1100]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=covs)
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)))
      ind.covRef <- c(ind.covRef, 
                      which(ff::as.ram(plpData$covariateRef$analysisId)==1100) )
    } 
    
    # get riskScoresDCSI covariate indexes if selected
    if(covariateSetting$useCovariateRiskScoresDCSI==TRUE){
      covs <- plpData$covariateRef$covariateId[plpData$covariateRef$analysisId==1101]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=covs)
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)))
      ind.covRef <- c(ind.covRef, 
                      which(ff::as.ram(plpData$covariateRef$analysisId)==1101) )
    } 
    
    # get riskScoresCHADS2 covariate indexes if selected
    if(covariateSetting$useCovariateRiskScoresCHADS2==TRUE){
      covs <- plpData$covariateRef$covariateId[plpData$covariateRef$analysisId==1102]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=covs)
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)))
      ind.covRef <- c(ind.covRef, 
                      which(ff::as.ram(plpData$covariateRef$analysisId)==1102) )
    }
    
    # get riskScoresCHADS2VASc covariate indexes if selected
    if(covariateSetting$useCovariateRiskScoresCHADS2VASc==TRUE){
      covs <- plpData$covariateRef$covariateId[plpData$covariateRef$analysisId==1103]
      ind.x <- ffbase::ffmatch(plpData$covariates$covariateId, table=covs)
      
      ind.all <- union(ind.all, ffbase::ffwhich(ind.x, !is.na(ind.x)))
      ind.covRef <- c(ind.covRef, 
                      which(ff::as.ram(plpData$covariateRef$analysisId)==1103) )
    } 
   
  }

  # do the exclusions
  if(length(covariateSetting$excludedCovariateIds)!=0){
    ind.ex <- ffbase::ffmatch(plpData$covariates$covariateId, table=ff::as.ff(covariateSetting$excludedCovariateIds))
    ind.all <- ff::as.ff(setdiff(ff::as.ram(ind.all), ff::as.ram(ffbase::ffwhich(ind.ex, !is.na(ind.ex)))))
    ind.covRef <- ffbase::ffmatch(plpData$covariateRef$covariateId, table=ff::as.ff(covariateSetting$excludedCovariateIds))
    ind.covRef <- setdiff(ind.covRef, ff::as.ram(ffbase::ffwhich(ind.covRef, !is.na(ind.covRef))))
  }
  
  # remove small counts:
  if(!is.null(covariateSetting$deleteCovariatesSmallCount)){
    oneVals <- ff::as.ff(rep(1, length(plpData$covariates$covariateValue)))
    grp_qty <- bySumFf(oneVals, plpData$covariates$covariateId)
    
    smallRemove <- ff::as.ram(grp_qty$bins)[ff::as.ram(grp_qty$sums)<covariateSetting$deleteCovariatesSmallCount]
    
    if(length(smallRemove)>0){
      # we currently remove the included ones if they are under this threshold - keep?
      ind.x <- ffbase::ffmatch(x = plpData$covariates$covariateId, table = ff::as.ff(smallRemove))
      ind.all <-  ff::as.ff(setdiff(ff::as.ram(ind.all), ff::as.ram(ffbase::ffwhich(ind.x, !is.na(ind.x)))))
      ind.x <- ffbase::ffmatch(x = plpData$covariateRef$covariateId, table = ff::as.ff(smallRemove))
      ind.covRef <- setdiff(ind.covRef, ff::as.ram(ffbase::ffwhich(ind.x, !is.na(ind.x))))
    }
  }
  
  metaDataTemp <- plpData$metaData
  metaDataTemp$call$covariateSettings <- covariateSetting

  result <- list(outcomes = plpData$outcomes,
                 cohorts = plpData$cohorts,
                 covariates= ff::clone(plpData$covariates),
                 exclude= plpData$exclude,
                 covariateRef = ff::clone(plpData$covariateRef),
                 metaData= metaDataTemp)
  result$covariates <- ff::as.ffdf(plpData$covariates[ind.all[-1],])
  result$covariateRef <- plpData$covariateRef[ff::as.ff(ind.covRef),]
  
  class(result) <- 'plpData'
  
  # filters the entries to include in plpData$covariates and plpData$covariateRef
  # return the indexes
  return(result)
}
