#TODO

ApplyPlp <- function(plpModel, plpData, population){
  # TODO: input checks
  
  prediction <- plpModel$transform(population=population, plpData=plpData)
  
  # evaluation? - shall we add this? evaluatePlp(prediction)
  
  return(prediction)
  
}