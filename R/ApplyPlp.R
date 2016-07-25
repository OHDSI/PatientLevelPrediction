#TODO

ApplyPlp <- function(plpModel, plpData, population){
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





