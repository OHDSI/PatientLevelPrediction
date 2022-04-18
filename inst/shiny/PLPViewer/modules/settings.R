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

setingsServer <- function(id, plpResult) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # input tables
      output$modelTable <- DT::renderDataTable(
        formatModSettings(plpResult()$model$settings$modelSettings  )
        )
      output$covariateTable <- DT::renderDataTable(
        formatCovSettings(plpResult()$model$settings$covariateSettings)
        )
      output$populationTable <- DT::renderDataTable(
        formatPopSettings(plpResult()$model$settings$populationSettings)
        )
      
      output$hpTable <- DT::renderDataTable(
        DT::datatable(
          as.data.frame(
            plpResult()$model[[
              which(
                names(plpResult()$model) %in% c('validationDetails','trainDetails')
              )
            ]]$hyperParamSearch
          ),
          options = list(scrollX = TRUE),
          colnames = 'Fold AUROC'
        )
      )
      
      output$attritionTable <- DT::renderDataTable(
        plpResult()$model[[
          which(
            names(plpResult()$model) %in% c('validationDetails','trainDetails')
            )
          ]]$attrition
      )
      
  
    }
  )
}


# helpers
# format modelSettings
formatModSettings <- function(modelSettings){
  modelset <- data.frame(Setting = c(names(modelSettings$finalModelParameters)),
                         Value = c(unlist(lapply(modelSettings$finalModelParameters, 
                                                                     function(x) paste0(x, collapse=','))))
    )
  row.names(modelset) <- NULL
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

