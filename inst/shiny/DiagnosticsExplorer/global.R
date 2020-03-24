library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(scales)
library(ggiraph)

source("PlotsAndTables.R")


if (!exists("shinySettings")) {
  if (file.exists("data")) {
    shinySettings <- list(dataFolder = "data")
  } else {
    shinySettings <- list(dataFolder = "./data")
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
    data <- readr::read_csv(file.path(folder, file), col_types = readr::cols(), guess_max = 1e7, locale = readr::locale(encoding = "UTF-8"))
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

# Fixing the labels (more to add)
outcomeCohorts <- sort(as.list(unique(distribution %>% select(outcomeid)))$outcomeid)
targetCohorts <- sort(as.list(unique(distribution %>% select(targetid)))$targetid)

# Sort selectors
databases <- sort(as.list(unique(settings %>% select(cdmdatabasename)))$cdmdatabasename)

# Variable Selector
distributionVars <- c('daysFromObsStart','daysToObsEnd','daysToOutcomeAfterMin','daysToOutcomeBeforeMin')


writeLines("Data Loaded")

