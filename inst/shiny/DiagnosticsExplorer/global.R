# check package dependancies - will move to launcher after study-a-thon
stop <- F
for(pkg in c('shiny', 'shinydashboard', 'DT', 'plotly', 'dplyr', 'tidyr','ggplot2', 'shinycssloaders')){
  if(is.na(tryCatch({utils::packageVersion(pkg)}, error = function(e) NA))){
    warning(paste0('Package ', pkg, ' not installed - please install'))
    stop <- T
  }
}
if(stop){stop('Need to install dependancies')}

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
#library(scales)
#library(ggiraph)


rm(list=ls()[ls()%in%c('settings', 'namesdetails', 'characterization','distribution','proportion')])
source("PlotsAndTables.R")


if (!exists("shinySettings")) {
  if (file.exists("diagnostics")) {
    shinySettings <- list(dataFolder = "diagnostics")
  } else {
    shinySettings <- list(dataFolder = "./diagnostics")
  }
  
}

dataFolder <- shinySettings$dataFolder

if (file.exists(file.path(dataFolder, "PreMerged.RData"))) {
  writeLines("Using merged data detected in data folder")
  load(file.path(dataFolder, "PreMerged.RData"))
} else {
  zipFiles <- list.files(dataFolder, pattern = ".zip", full.names = TRUE)
  
  loadFile <- function(file, folder, overwrite) {
    print(file)
    tableName <- gsub(".csv$", "", file)
    camelCaseName <- SqlRender::snakeCaseToCamelCase(tableName)
    #data <- readr::read_csv(file.path(folder, file), col_types = readr::cols(), guess_max = 1e7, locale = readr::locale(encoding = "UTF-8"))
    data <- utils::read.csv(file.path(folder, file))
    
    colnames(data) <- SqlRender::snakeCaseToCamelCase(colnames(data))
    
    if (!overwrite && exists(camelCaseName, envir = .GlobalEnv)) {
      existingData <- get(camelCaseName, envir = .GlobalEnv)
      if (nrow(existingData) > 0) {
        if (nrow(data) > 0 &&
            all(colnames(existingData) %in% colnames(data)) &&
            all(colnames(data) %in% colnames(existingData))) {
          data <- data[, colnames(existingData)]
        }
        
        if (!isTRUE(all.equal(colnames(data), colnames(existingData), check.attributes = FALSE))) {
          stop("Table columns do no match previously seen columns. Columns in ", 
               file, 
               ":\n", 
               paste(colnames(data), collapse = ", "), 
               "\nPrevious columns:\n",
               paste(colnames(existingData), collapse = ", "))
        }
      }
      data <- rbind(existingData, data)
    }
    assign(camelCaseName, data, envir = .GlobalEnv)
    
    invisible(NULL)
  }
  
  for (i in 1:length(zipFiles)) {
    writeLines(paste("Processing", zipFiles[i]))
    tempFolder <- tempfile()
    dir.create(tempFolder)
    unzip(zipFiles[i], exdir = tempFolder)
    
    csvFiles <- list.files(tempFolder, pattern = ".csv")
    lapply(csvFiles, loadFile, folder = tempFolder, overwrite = (i == 1))
    
    unlink(tempFolder, recursive = TRUE)
  }
}

namesdetails$names <- as.character(namesdetails$names)
# Fixing the labels (more to add)
getNames <- function(cohortid){
  res <- namesdetails %>% filter(ids == cohortid) %>% select(names)
  res$names[1]
}
getId <- function(cohortname){
  res <- namesdetails %>% filter(names == cohortname) %>% select(ids)
  res$ids[1]
}

# Sort selectors
databases <- sort(as.list(unique(settings %>% select(cdmdatabasename)))$cdmdatabasename)

outcomeCohorts <- lapply(as.list(unique(settings %>% select(outcomeid)))$outcomeid, getNames)
targetCohorts <- lapply(as.list(unique(settings %>% select(cohortid)))$cohortid, getNames)

settings$tar <- unlist(
  lapply(1:nrow(settings), function(x) paste0(settings$startanchor[x], ' + ', settings$riskwindowstart[x], 
                                          ' days - ', settings$endanchor[x], ' + ', settings$riskwindowend[x], ' days'))
  )
tars <- as.list(unique(settings %>% select(tar)))$tar
#tars <- unique(settings %>% select(riskwindowstart,	startanchor,	riskwindowend,	endanchor))
#tars <- lapply(1:nrow(tars), function(x) paste0(tars$startanchor[x], ' + ', tars$riskwindowstart[x], 
#                                  ' days - ', tars$endanchor[x], ' + ', tars$riskwindowend[x], ' days'))

# Variable Selector
distributionVars <- c('daysFromObsStart','daysToObsEnd','daysToOutcomeAfterMin','daysToOutcomeBeforeMin')


writeLines("Data Loaded")

