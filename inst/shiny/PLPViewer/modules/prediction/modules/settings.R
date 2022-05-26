settingsViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
  shiny::h3('Model Settings: ',
            shiny::a("help", href="https://ohdsi.github.io/PatientLevelPrediction/reference/index.html", target="_blank")
  ),
  DT::dataTableOutput(ns('modelTable')),
  
  shiny::h3('Population Settings: ',
            shiny::a("help", href="https://ohdsi.github.io/PatientLevelPrediction/reference/createStudyPopulation.html", target="_blank")
  ),
  DT::dataTableOutput(ns('populationTable')),
  
  shiny::h3('Covariate Settings: ',
            shiny::a("help", href="http://ohdsi.github.io/FeatureExtraction/reference/createCovariateSettings.html", target="_blank")
  ),
  DT::dataTableOutput(ns('covariateTable')),
  
  shiny::h3("Hyper-parameters"),
  DT::dataTableOutput(ns('hpTable')),
  shiny::h3("Attrition"),
  DT::dataTableOutput(ns('attritionTable'))
  )
}

setingsServer <- function(
  id,
  resultTable, 
  rowId, 
  mySchema, 
  con,
  inputSingleView,
  myTableAppend, 
  targetDialect                     
  ) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      shiny::observe({
        if(!is.null(rowId()) & inputSingleView() == 'Design Settings'){

          modelDesign <- getModelDesign(
            modelDesignId = resultTable$modelDesignId[rowId()],
            mySchema, 
            con,
            myTableAppend, 
            targetDialect   
          )
          
          hyperParamSearch <- getHyperParamSearch(
            modelDesignId = resultTable$modelDesignId[rowId()],
            databaseId = resultTable$developmentDatabaseId[rowId()],
            mySchema, 
            con,
            myTableAppend, 
            targetDialect   
          ) 
          
          attrition <- getAttrition(
            performanceId = resultTable$performanceId[rowId()],
            mySchema, 
            con,
            myTableAppend, 
            targetDialect   
          ) 
          
          # input tables
          output$modelTable <- DT::renderDataTable(
            formatModSettings(modelDesign$modelSettings  )
          )
          output$covariateTable <- DT::renderDataTable(
            formatCovSettings(modelDesign$covariateSettings)
          )
          output$populationTable <- DT::renderDataTable(
            formatPopSettings(modelDesign$populationSettings)
          )
          
          output$hpTable <- DT::renderDataTable(
            DT::datatable(
              as.data.frame(
                hyperParamSearch
              ),
              options = list(scrollX = TRUE),
              colnames = 'Fold AUROC'
            )
          )
          
          output$attritionTable <- DT::renderDataTable(
            attrition
          )
        }
      })
  
    }
  )
}


# helpers


# get the data
getModelDesign <- function(
  modelDesignId = resultTable$modelDesignId[rowId()],
  mySchema, 
  con,
  myTableAppend, 
  targetDialect   
){
  if(!is.null(modelDesignId)){
  print(paste0('model design: ', modelDesignId))
  modelDesign <- list()
  
  sql <- "SELECT * FROM 
    @my_schema.@my_table_appendmodel_designs 
    WHERE model_design_id = @model_design_id;"
  
  sql <- SqlRender::render(sql = sql, 
                           my_schema = mySchema,
                           model_design_id = modelDesignId,
                           my_table_append = myTableAppend)
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  ParallelLogger::logInfo("starting population, model setting and covariate setting")
  
  ids <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(ids) <- SqlRender::snakeCaseToCamelCase(colnames(ids))
  
  ParallelLogger::logInfo("finishing getting model design setting ids")
  
  popSetId <- ids$populationSettingId
  modSetId <- ids$modelSettingId
  covSetId <- ids$covariateSettingId
  feSetId <- ids$featureEngineeringSettingId
  sampleSetId <- ids$sampleSettingId
  splitId <- ids$splitSettingId
  tId <- ids$cohortId
  oId <- ids$outcomeId
  # plpDataSettingId
  
  ParallelLogger::logInfo("start modeSet")
  sql <- "SELECT * FROM @my_schema.@my_table_appendmodel_settings WHERE model_setting_id = @model_setting_id"
  sql <- SqlRender::render(sql = sql, 
                           my_schema = mySchema,
                           model_setting_id = modSetId,
                           my_table_append = myTableAppend)
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  
  tempModSettings <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(tempModSettings) <- SqlRender::snakeCaseToCamelCase(colnames(tempModSettings))
  ParallelLogger::logInfo("end modeSet")
  
  modelDesign$modelSettings <- ParallelLogger::convertJsonToSettings(tempModSettings$modelSettingsJson)
  
  ParallelLogger::logInfo("start covSet")
  sql <- "SELECT * FROM @my_schema.@my_table_appendcovariate_settings WHERE covariate_setting_id = @setting_id"
  sql <- SqlRender::render(sql = sql, 
                           my_schema = mySchema,
                           setting_id = covSetId,
                           my_table_append = myTableAppend)
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  tempSettings <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(tempSettings) <- SqlRender::snakeCaseToCamelCase(colnames(tempSettings))
  modelDesign$covariateSettings <- ParallelLogger::convertJsonToSettings(tempSettings$covariateSettingsJson)
  ParallelLogger::logInfo("end covSet")
  
  ParallelLogger::logInfo("start popSet")
  sql <- "SELECT * FROM @my_schema.@my_table_appendpopulation_settings WHERE population_setting_id = @setting_id"
  sql <- SqlRender::render(sql = sql, 
                           my_schema = mySchema,
                           setting_id = popSetId,
                           my_table_append = myTableAppend)
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  tempSettings <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(tempSettings) <- SqlRender::snakeCaseToCamelCase(colnames(tempSettings))
  modelDesign$populationSettings <- ParallelLogger::convertJsonToSettings(tempSettings$populationSettingsJson)
  ParallelLogger::logInfo("end popSet")
  
  return(modelDesign)
  }
  return(NULL)
}


getHyperParamSearch <- function(
  modelDesignId,
  databaseId,
  mySchema, 
  con,
  myTableAppend, 
  targetDialect   
){
  
  sql <- "SELECT train_details FROM @my_schema.@my_table_appendmodels WHERE database_id = @database_id
       and model_design_id = @model_design_id"
  sql <- SqlRender::render(sql = sql, 
                           my_schema = mySchema,
                           database_id = databaseId,
                           model_design_id = modelDesignId,
                           my_table_append = myTableAppend)
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  models <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(models) <- SqlRender::snakeCaseToCamelCase(colnames(models))
  
  trainDetails <- ParallelLogger::convertJsonToSettings(models$trainDetails)
  
  return(trainDetails$hyperParamSearch)
}


getAttrition <- function(
  performanceId,
  mySchema, 
  con,
  myTableAppend, 
  targetDialect   
){
sql <- "SELECT * FROM @my_schema.@my_table_appendattrition WHERE performance_id = @performance_id"
ParallelLogger::logInfo("start attrition")
sql <- SqlRender::render(sql = sql, 
                         my_schema = mySchema,
                         performance_id = performanceId,
                         my_table_append = myTableAppend)
sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)

attrition  <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
colnames(attrition) <- SqlRender::snakeCaseToCamelCase(colnames(attrition))
ParallelLogger::logInfo("end attrition")

return(attrition)
}

# formating
formatModSettings <- function(modelSettings){
  
  modelset <- data.frame(
  paramJson = as.character(
    ParallelLogger::convertSettingsToJson(
      modelSettings$param
      )
  )
  )

  return(modelset)
}

# format covariateSettings
formatCovSettings <- function(covariateSettings){
  
  if(class(covariateSettings)=='covariateSettings'){
    covariateSettings <- list(covariateSettings)
  }
  
  #code for when multiple covariateSettings
  covariates <- c() 
  for(i in 1:length(covariateSettings)){
    covariatesTemp <- data.frame(
      fun = attr(covariateSettings[[i]],'fun'),
      setting = i,
      covariateName = names(covariateSettings[[i]]), 
      SettingValue = unlist(
        lapply(
          covariateSettings[[i]], 
          function(x) paste0(x, collapse='-')
        )
      )
    )
    covariates  <- rbind(covariates,covariatesTemp)
  }
  row.names(covariates) <- NULL
  return(covariates)
}

# format populationSettings
formatPopSettings <- function(populationSettings){
  population <- populationSettings
  population$attrition <- NULL # remove the attrition as result and not setting
  population <- data.frame(Setting = names(population), 
                           Value = unlist(lapply(population, 
                                                 function(x) paste0(x, 
                                                                    collapse='-')))
  ) 
  row.names(population) <- NULL
  return(population)
}

