# code to parse the modelDesign list into json

addAttributes <- function(x){
  attributeValues <- attributes(x)
  if('names' %in% names(attributeValues)){
    attributeValues$names <- NULL
  }
  
  if(length(attributeValues)>0){
    names(attributeValues) <- paste0('attr_', names(attributeValues))
    x <- c(x, attributeValues)
  }
  return(x)
}

prepareToJson <- function(md){
  md$restrictPlpDataSettings <- addAttributes(md$restrictPlpDataSettings)
  md$populationSettings <- addAttributes(md$populationSettings)
  md$preprocessSettings <- addAttributes(md$preprocessSettings)
  md$executeSettings <- addAttributes(md$executeSettings)
  
  md$modelSettings$param <- addAttributes(md$modelSettings$param)
  md$modelSettings <- addAttributes(md$modelSettings)
  
  if(class(md$covariateSettings) == 'covariateSettings'){
    md$covariateSettings <- list(md$covariateSettings)
  }
  md$covariateSettings <- lapply(md$covariateSettings, function(x) addAttributes(x)) 
  
  if(class(md$sampleSettings) == 'sampleSettings'){
    md$sampleSettings <- list(md$sampleSettings)
  }
  md$sampleSettings <- lapply(md$sampleSettings, function(x) addAttributes(x)) 
  
  if(class(md$featureEngineeringSettings) == 'featureEngineeringSettings'){
    md$featureEngineeringSettings<- list(md$featureEngineeringSettings)
  }
  md$featureEngineeringSettings <- lapply(md$featureEngineeringSettings, function(x) addAttributes(x)) 
  
  md <- addAttributes(md)
  return(md)
}



extractAttributes <- function(x){
  
  ind <- grep('attr_', names(x))
  
  if(length(ind)>0){
    attributeValues <- x[ind]
    x <- x[-ind]
    names(attributeValues) <- gsub(pattern = 'attr_',replacement = '',x = names(attributeValues))
    attributeValues$names <- names(x)
    attributes(x) <- attributeValues
  }
  
  return(x)
}

prepareToRlist <- function(md){
  md$restrictPlpDataSettings <- extractAttributes(md$restrictPlpDataSettings)
  md$populationSettings <- extractAttributes(md$populationSettings)
  md$preprocessSettings <- extractAttributes(md$preprocessSettings)
  md$executeSettings <- extractAttributes(md$executeSettings)
  
  md$modelSettings$param <- extractAttributes(md$modelSettings$param)
  md$modelSettings <- extractAttributes(md$modelSettings)
  
  md$covariateSettings <- lapply(md$covariateSettings, function(x) extractAttributes(x)) 
  
  md$sampleSettings <- lapply(md$sampleSettings, function(x) extractAttributes(x)) 
  
  md$featureEngineeringSettings <- lapply(md$featureEngineeringSettings, function(x) extractAttributes(x)) 
  
  md <- extractAttributes(md)
  return(md)
}