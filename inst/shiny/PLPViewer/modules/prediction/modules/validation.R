validationViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    style = "font-size:70%",
    shiny::p('Select one or more rows to generate comparison ROC and calibration plots'),
    DT::dataTableOutput(ns('validationTable')), 
    
    shiny::fluidRow(
      shinydashboard::box(
        status = 'info',
        title = "Roc Plot",
        solidHeader = TRUE,
        shinycssloaders::withSpinner(shiny::plotOutput(ns('valRoc')))
      ),
      shinydashboard::box(
        status = 'info',
        title = "Calibration Plot",
        solidHeader = TRUE,
        side = "right",
        shinycssloaders::withSpinner(shiny::plotOutput(ns('valCal')))
      )
    )
  )
}

validationServer <- function(
  id, 
  summaryTable,
  resultRow,
  con, 
  inputSingleView,
  mySchema,
  targetDialect = NULL,
  myTableAppend = NULL
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      validationTable <- shiny::reactive({
        if(inputSingleView() == 'Validation' & !is.null(resultRow())){
          return(
            getValSummary(
              con, 
              mySchema, 
              modelDesignId = summaryTable[resultRow(), 'modelDesignId'],
              developmentDatabaseId = summaryTable[resultRow(), 'developmentDatabaseId'],
              targetDialect = targetDialect, 
              myTableAppend = myTableAppend
            )
          )
        } else{
          return(NULL)
        }
      }
      )
      
      output$validationTable <- DT::renderDataTable(
        {
        
        if(!is.null(validationTable())){
          cind <- c('modelDesignId','T','O', 'Val', 'AUROC','calibrationInLarge intercept', 'T Size', 'O Count','Val (%)')%in%colnames(validationTable())
          validationTable()[,c('modelDesignId','T','O', 'Val', 'AUROC','calibrationInLarge intercept', 'T Size', 'O Count','Val (%)')[cind]]
        } else{
          NULL
        }
      }, 
      escape = FALSE, 
      filter = 'top', 
      extensions = 'Buttons', 
      options = list(
        dom = 'Blfrtip', 
        scrollX = TRUE
      ),
      rownames= FALSE 
      ) #options = list(filter = 'top'))
        
      # need to modify this for non-database results!
      valResult <- shiny::reactive({
        
        if(is.null(validationTable())){
          return(
            list(
              thresholdSummaryList = NULL,
              calibrationSummaryList = NULL, 
              databaseName = '', 
              Ts='', 
              Os=''
            )
          )
        }
        
        valTable <- validationTable()[input$validationTable_rows_selected,,]
        if(!is.null(input$validationTable_rows_selected)){
          names <- valTable[, "Val"]
          Ts <- valTable[, "T"]
          Os <- valTable[, "O"]
          thresholdSummaryList <- list()
          calibrationSummaryList <- list()
          
          for (i in 1:nrow(valTable)){
            
            thresholdSummaryList[[i]] <- getResult(
              performanceId = valTable$performanceId[i], 
              con = con,
              tableName = paste0(myTableAppend,'threshold_summary'), 
              mySchema = mySchema, 
              targetDialect = targetDialect 
              )
            calibrationSummaryList[[i]] <- getResult(
              performanceId = valTable$performanceId[i], 
              con = con,
              tableName = paste0(myTableAppend,'calibration_summary'), 
              mySchema = mySchema, 
              targetDialect = targetDialect 
            )
          }
          list(
            thresholdSummaryList = thresholdSummaryList, 
            calibrationSummaryList = calibrationSummaryList,
            databaseName = names, 
            Ts=Ts, 
            Os=Os
          )
        }else{
          list(
            thresholdSummaryList = NULL,
            calibrationSummaryList = NULL, 
            databaseName = '', 
            Ts='', 
            Os=''
          )
        }
      })
      
      output$valRoc <- shiny::renderPlot({
        
        if(is.null(valResult()$thresholdSummaryList)){
          return(NULL)
        } else{
          plotRocs(
            thresholdSummaryList = valResult()$thresholdSummaryList, 
            modelNames = paste0(1:length(valResult()$Ts),':',substr(valResult()$Ts,1,5),'-',substr(valResult()$Os,1,5),'-', substr(valResult()$databaseName,1,5))
          )
        }
      })
      output$valCal <- shiny::renderPlot({
        
        if(is.null(valResult()$calibrationSummaryList)){
          return(NULL)
        } else{
          plotCalsSmooth(
            calibrationSummaryList = valResult()$calibrationSummary, 
            modelNames =  paste0(1:length(valResult()$Ts),':',substr(valResult()$Ts,1,5),'-',substr(valResult()$Os,1,5),'-', substr(valResult()$databaseName,1,5))
          )
        }
        
      })
      
      # add other plots?
      
    }
  )
}


getValSummary <- function(
  con, 
  mySchema, 
  modelDesignId, 
  developmentDatabaseId,
  targetDialect, 
  myTableAppend = '' 
){
  ParallelLogger::logInfo("getting Val summary")
  
  print(developmentDatabaseId)

  sql <- "SELECT 
           results.performance_id, 
           results.model_design_id, 
           --databases.database_acronym AS Dev, 
           d.database_acronym AS Val,
           targets.cohort_name AS target, 
           outcomes.cohort_name AS outcome,
           results.population_setting_id, 
           -- pop and plp_data settings?
           tars.tar_start_day, tars.tar_start_anchor, 
           tars.tar_end_day, tars.tar_end_anchor,
   ROUND(aucResult.auc, 3) as auroc,
   ROUND(auclbResult.auclb, 3) as auroclb,
   ROUND(aucubResult.aucub, 3) as aurocub,
   ROUND(auprcResult.auprc,4) as auprc,
   nResult.population_size, 
   oResult.outcome_count,
   ROUND(nTest.test_size*100.0/nResult.population_size, 1) as eval_percent,
   ROUND(oResult.outcome_count*100.0/nResult.population_size,4) as outcome_percent,
   ROUND(calibration_in_large, 3) as calibration_in_large
   
   FROM 
   
   (SELECT * FROM @my_schema.@my_table_appendperformances 
         where model_design_id = @model_design_id and 
         development_database_id = @development_database_id
   ) AS results
         
    LEFT JOIN (SELECT cohort_id, cohort_name FROM @my_schema.@my_table_appendcohorts) AS targets ON results.cohort_id = targets.cohort_id
    LEFT JOIN (SELECT cohort_id, cohort_name FROM @my_schema.@my_table_appendcohorts) AS outcomes ON results.outcome_id = outcomes.cohort_id
    LEFT JOIN @my_schema.@my_table_appenddatabase_details AS d ON results.validation_database_id = d.database_id 
    LEFT JOIN @my_schema.@my_table_appendtars AS tars ON results.tar_id = tars.tar_id
    LEFT JOIN (SELECT performance_id, value AS auc FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'AUROC' and evaluation in ('Test','Validation') ) AS aucResult ON results.performance_id = aucResult.performance_id
    LEFT JOIN (SELECT performance_id, value AS auclb FROM @my_schema.@my_table_appendevaluation_statistics where metric = '95% lower AUROC' and evaluation in ('Test','Validation') ) AS auclbResult ON results.performance_id = auclbResult.performance_id
    LEFT JOIN (SELECT performance_id, value AS aucub FROM @my_schema.@my_table_appendevaluation_statistics where metric = '95% upper AUROC' and evaluation in ('Test','Validation') ) AS aucubResult ON results.performance_id = aucubResult.performance_id
    LEFT JOIN (SELECT performance_id, value AS auprc FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'AUPRC' and evaluation in ('Test','Validation') ) AS auprcResult ON results.performance_id = auprcResult.performance_id
    
    LEFT JOIN (SELECT performance_id, value AS calibration_in_large FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'calibrationInLarge intercept' and evaluation in ('Test','Validation') ) AS CalibrationInLargeResult ON results.performance_id = CalibrationInLargeResult.performance_id

    LEFT JOIN (SELECT performance_id, sum(value) AS population_size FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'populationSize' and evaluation in ('Test','Train','Validation') group by performance_id) AS nResult ON results.performance_id = nResult.performance_id
    LEFT JOIN (SELECT performance_id, sum(value) AS outcome_count FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'outcomeCount' and evaluation in ('Test','Train','Validation') group by performance_id) AS oResult ON results.performance_id = oResult.performance_id
    LEFT JOIN (SELECT performance_id, value AS test_size FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'populationSize' and evaluation in ('Test','Validation')) AS nTest ON results.performance_id = nTest.performance_id;"
  
  sql <- SqlRender::render(sql = sql, 
                           my_schema = mySchema, 
                           model_design_id = modelDesignId,
                           development_database_id = developmentDatabaseId,
                           my_table_append = myTableAppend)
  
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  
  valTable <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(valTable) <- SqlRender::snakeCaseToCamelCase(colnames(valTable))
  
  valTable$target <- trimws(valTable$target) # not needed
  valTable$outcome <- trimws(valTable$outcome) # not needed
  
  valTable <- valTable %>% 
    dplyr::rename(`Population setting` = populationSettingId) %>%
    dplyr::rename(`T Size` = populationSize) %>% 
    dplyr::rename(`O Count` = outcomeCount) %>%
    dplyr::rename(`Val (%)` = evalPercent) %>%
    dplyr::rename(`O Incidence (%)` = outcomePercent) %>%
    dplyr::rename(`T` = target) %>%
    dplyr::rename(`O` = outcome) %>%
    dplyr::rename(`AUROC` = auroc) %>%
    dplyr::rename(`AUPRC` = auprc) %>%
    dplyr::rename(`Val` = val) %>%
    dplyr::rename(`calibrationInLarge intercept` = calibrationInLarge)
  
  valTable <- editTar(valTable) # ISSUE: function not in the module
  
  valTable$timeStamp <- 0
  valTable$AUROC <- paste0(valTable$AUROC, ' (', valTable$auroclb,'-',valTable$aurocub, ')')
  ParallelLogger::logInfo("got db summary")
  
  return(valTable[,c('Val', 'T','O', 'Population setting',
                     'TAR', 'AUROC', 'AUPRC', 'calibrationInLarge intercept',
                     'T Size', 'O Count','Val (%)', 'O Incidence (%)', 'timeStamp', 'modelDesignId', 'performanceId')])
  
}




# helper for multiple roc plots
plotRocs <- function(
  thresholdSummaryList,
  modelNames, 
  type= NULL, 
  fileName=NULL
){
  if(class(thresholdSummaryList)!='list'){
    stop('Need to enter a list')
  }
  
  
  if(missing(modelNames)){
    modelNames <- paste0('Model ', 1:length(thresholdSummaryList))
  }
  
  createSteps <- function(
    thresholdSummary, 
    type, 
    name
  ){
    
    if(is.null(type)){
      if(length(unique(thresholdSummary$evaluation)) > 1){
        ind <- thresholdSummary$evaluation%in%c('Test','validation')
        x<- thresholdSummary[ind,c('falsePositiveRate','sensitivity')]
      } else {
        x<- thresholdSummary[,c('falsePositiveRate','sensitivity')]
      }
    } else {
      ind <- thresholdSummary$evaluation == type
      x <- thresholdSummary[ind,c('falsePositiveRate','sensitivity')]
    }
    
    x <- x[order(x$falsePositiveRate, x$sensitivity),]
    
    # add the bit to get the step
    stepsExtra <- cbind(x[-1,1], x[-nrow(x),2])
    colnames(stepsExtra) <- colnames(x)
    x <- rbind(c(1,1), x, stepsExtra, c(0,0))
    x <- x[order(x$falsePositiveRate, x$sensitivity),]
    
    x$model <- name
    return(x)
  }
  
  stepVals <- lapply(
    1:length(thresholdSummaryList), 
    function(i){
      createSteps(
        thresholdSummaryList[[i]], 
        type=type[i], 
        name=modelNames[i]
        )
    }
  )
  data <- do.call(rbind, stepVals)
  
  plot <- ggplot2::ggplot(
    data = data, 
    ggplot2::aes(
      x = falsePositiveRate, 
      y = sensitivity, 
      color = model
    )
  ) +
    ggplot2::geom_polygon(
      ggplot2::aes(fill = model), 
      alpha = 0.2
    ) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_abline(
      intercept = 0, 
      slope = 1,
      linetype = 2
    ) +
    ggplot2::scale_x_continuous(
      "1 - specificity", 
      limits=c(0,1)
    ) +
    ggplot2::scale_y_continuous(
      "Sensitivity", 
      limits=c(0,1)
    ) +
    ggplot2::scale_color_discrete(name = 'Result') +
    ggplot2::scale_fill_discrete(guide = FALSE)
  
  if (!is.null(fileName)){
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
  }
  
  return(plot)
}

plotCalsSmooth <- function(
  calibrationSummaryList,
  modelNames, 
  type = NULL
  ){
  
  
  if(missing(modelNames))
    modelNames <- paste0('Model ', 1:length(calibrationSummaryList))
  
  calVal <- function(
    calibrationSummary, 
    type, 
    name
    ){
    
    if(is.null(type)){
      if(length(unique(calibrationSummary$evaluation)) > 1){
        ind <- calibrationSummary$evaluation %in% c('Test','Validation')
        data <- calibrationSummary[ind,c('averagePredictedProbability','observedIncidence','personCountAtRisk')]
      } else{
        data <- calibrationSummary[,c('averagePredictedProbability','observedIncidence','personCountAtRisk')]
      }
    } else{
      ind <- calibrationSummary$evaluation==type
      data <- calibrationSummary[ind,c('averagePredictedProbability','observedIncidence','personCountAtRisk')]
    }
    
    maxes <- max(max(data$averagePredictedProbability), max(data$observedIncidence))*1.1
    
    fit <- stats::loess(data$observedIncidence ~ data$averagePredictedProbability, degree = 1)
    smoothData <- data.frame(
      p = seq(0,maxes,0.0001), 
      y = stats::predict(fit, seq(0,maxes,0.0001)), 
      model = name
    )
    smoothData <- smoothData[!is.na(smoothData$y),]
    
    return(smoothData)
  }
  
  getVal <- function(
    calibrationSummary, 
    type, 
    name
    ){
    
    if(is.null(type)){
      if(length(unique(calibrationSummary$evaluation)) > 1){
        ind <- calibrationSummary$evaluation %in% c('Test','Validation')
        data <- calibrationSummary[ind, c('averagePredictedProbability','observedIncidence')]
      } else{
        data <- calibrationSummary[, c('averagePredictedProbability','observedIncidence')]
      }
    } else{
      ind <- calibrationSummary$evaluation == type
      data <- calibrationSummary[ind, c('averagePredictedProbability','observedIncidence')]
    }
    

    values <- data.frame(
      p = data$averagePredictedProbability, 
      y = data$observedIncidence, 
      model = name
    )
    
    values <- values[seq(1, nrow(values), 10),]
    
    return(values)
  }
  
  calVal<- lapply(
    1:length(calibrationSummaryList), 
    function(i){
      calVal(
        calibrationSummaryList[[i]], 
        type=type[i], 
        name=modelNames[i]
        )
    }
  )
  smoothData <- do.call(rbind, calVal)
  
  values <- do.call(rbind,lapply(1:length(calibrationSummaryList), function(i) getVal(calibrationSummaryList[[i]], type=type[i], name=modelNames[i])))
  
  plot <- ggplot2::ggplot(
    data = smoothData, 
    ggplot2::aes(
      x = .data$p, 
      y = .data$y, 
      color = .data$model
      )
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        linetype = "Loess"),
        size = 1,
        show.legend = T
      ) +
    ggplot2::geom_abline(
      intercept = 0, 
      slope = 1, 
      linetype = 5, 
      size=0.4,
      show.legend = TRUE, 
      color = "black"
      ) +
    ggplot2::geom_point(data = values) +
    ggplot2::labs(
      x = "Average Predicted Probability", 
      y = "Observed Fraction With Outcome"
      ) 
  
  return(plot)
}

