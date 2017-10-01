getProtocolSpecification <- function() {
  # load the plp settings
  sf <- system.file("doc", "patientLevelPredictionRun.csv", package = "PatientLevelPrediction")
  protspec <- read.csv(sf)

  entry <- list()
  length(entry) <- length(protspec$function.)
  j <- 0
  for (fun in protspec$function.) {
    j <- j + 1
    tempfun <- strsplit(as.character(fun), "::")[[1]]
    fun <- get(tempfun[2])  #, envir=paste0('namespace:',tempfun[1]))

    argsf <- args(fun)
    inputsName <- names(as.list(argsf))

    values <- as.list(argsf)
    for (i in 1:length(values)) {
      if (is.null(values[[i]])) {
        values[[i]] <- "no default"
      }
      if (class(values[[i]]) %in% c("call", "name")) {
        values[[i]] <- as.character(values[[i]])
      }
    }
    defaultValue <- unlist(lapply(values, function(x) paste(x, collapse = " ")))

    entry[[j]] <- list(functionName = protspec$function.[j],
                       description = protspec$description[j],
                       order = protspec$order[j],
                       inputName = inputsName,
                       inputDefaultValues = defaultValue)
  }

  jsontest <- jsonlite::toJSON(entry, pretty = TRUE, auto_unbox = TRUE)
  return(jsontest)
}

#
jsontest <- getProtocolSpecification()
readr::write_lines(jsontest, "export.json")


