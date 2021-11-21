createPythonData <- function(trainData){
  
  labels <- trainData$labels %>% 
    dplyr::mutate(rowIdPython = .data$rowId-1) %>%
    dplyr::inner_join(trainData$folds, by = 'rowId')
  labels <- as.matrix(labels[,c('rowIdPython','outcomeCount','index')])
  
  x <- toSparseM(trainData, map = NULL)
  
  covariateMap <- x$map
  pythonData <- reticulate::r_to_py(x$data)
  
  return(
    list(
      pythonMatrixData = pythonData,
      pythonLabels = labels,
      covariateMap = covariateMap
    )
  )
}