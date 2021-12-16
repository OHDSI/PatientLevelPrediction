listCartesian <- function(allList){
  
  sizes <- lapply(allList, function(x) 1:length(x))
  combinations <- expand.grid(sizes)
  
  result <- list()
  length(result) <- nrow(combinations)
  
  for(i in 1:nrow(combinations)){
    tempList <- list()
    for(j in 1:ncol(combinations)){
      tempList <- c(tempList, list(allList[[j]][[combinations[i,j]]]))
    }
    names(tempList) <- names(allList)
    result[[i]] <- tempList
  }
  
  return(result)
}

  
