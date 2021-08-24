validationViewer <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(style = "font-size:70%",
             shiny::p('Select one or more rows to generate comparison ROC and calibration plots'),
    DT::dataTableOutput(ns('validationTable')), 
    
  shiny::fluidRow(
    shinydashboard::box(status = 'info',
                        title = "Roc Plot",
                        solidHeader = TRUE,
                        shinycssloaders::withSpinner(shiny::plotOutput(ns('valRoc')))),
    shinydashboard::box(status = 'info',
                        title = "Calibration Plot",
                        solidHeader = TRUE,
                        side = "right",
                        shinycssloaders::withSpinner(shiny::plotOutput(ns('valCal'))))
  )
  )
}

validationServer <- function(id, 
                             plpResult,
                             result,
                             validation,
                             inputType,
                             useDatabase,
                             summaryTable,
                             resultRow,
                             con, 
                             mySchema,
                             connectionDetails) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      if (useDatabase == F){
        validationTable <- shiny::reactive(dplyr::filter(summaryTable,
                                                         Analysis == summaryTable[resultRow(),'Analysis']))
      }
      else{
        # validationTable <- shiny::reactive(getValSummary(con, mySchema, summaryTable[filterIndex(),'Analysis'][trueRow()]))
        validationTable <- shiny::reactive(getValSummary(con, mySchema, summaryTable[resultRow(),'Analysis']))
      }
      
      #shiny::reactive({print(validationTable())})
      #output$validationTable <- DT::renderDataTable(dplyr::select(validationTable(),c(Analysis, Dev, Val, AUC)), rownames= FALSE)
      output$validationTable <- DT::renderDataTable({
        if(nrow(validationTable())>0){
          validationTable()[,c('Analysis','T','O', 'Val', 'AUC','calibrationInLarge', 'T Size', 'O Count','Val (%)')]
        } else{
          NULL
        }
      }, escape = FALSE, filter = 'top', rownames= FALSE ) #options = list(filter = 'top'))
      
      # need to modify this for non-database results!
      valtemplist <- list()
      valResult <- shiny::reactive({
        
        valTable <- validationTable()[input$validationTable_rows_selected,]
        if(nrow(valTable)>0){
          names <- valTable[, "Val"]
          Ts <- valTable[, "T"]
          Os <- valTable[, "O"]
          for (i in 1:nrow(valTable)){
            valtemplist[[i]] <- getPlpResult(result,validation,valTable, inputType, i, val = T, 
                                             mySchema = mySchema, connectionDetails = connectionDetails)
          }
          list(results = valtemplist, databaseName = names, Ts=Ts, Os=Os)
        }else{
          list(results = list(list()), databaseName = '', Ts='', Os='')
        }
      })
      
      output$valRoc <- shiny::renderPlot({
        
        if(is.null(valResult()$results[[1]]$performanceEvaluation)){
          return(NULL)
        } else{
          plotRocs(evaluationList = valResult()$results, 
                   modelNames = paste0(1:length(valResult()$Ts),':',substr(valResult()$Ts,1,5),'-',substr(valResult()$Os,1,5),'-', substr(valResult()$databaseName,1,5)))
        }
      })
      output$valCal <- shiny::renderPlot({
        
        if(is.null(valResult()$results[[1]]$performanceEvaluation)){
          return(NULL)
        } else{
          plotCals(evaluationList = valResult()$results, 
                   modelNames =  paste0(1:length(valResult()$Ts),':',substr(valResult()$Ts,1,5),'-',substr(valResult()$Os,1,5),'-', substr(valResult()$databaseName,1,5)))
        }
        
      })
      
    }
  )
}



# helper for multiple roc plots
plotRocs <- function(evaluationList,modelNames, type= NULL, fileName=NULL){
  if(class(evaluationList)!='list')
    stop('Need to enter a list')
  
  if("thresholdSummary"%in%names(evaluationList[[1]])){
    evaluationList <- evaluationList
  }else if("performanceEvaluation"%in%names(evaluationList[[1]])){
    evaluationList <- lapply(evaluationList, function(x) x$performanceEvaluation)
  } else{
    stop('Wrong evaluationList')
  }
  
  if(missing(modelNames))
    modelNames <- paste0('Model ', 1:length(evaluationList))
  
  createSteps <- function(evaluation, type, name){
    
    if(is.null(type)){
      if(length(unique(evaluation$thresholdSummary$Eval))>1){
        ind <- evaluation$thresholdSummary$Eval%in%c('test','validation')
        x<- evaluation$thresholdSummary[ind,c('falsePositiveRate','sensitivity')]} else{
          x<- evaluation$thresholdSummary[,c('falsePositiveRate','sensitivity')]
        }
    } else {
      ind <- evaluation$thresholdSummary$Eval==type
      x <- evaluation$thresholdSummary[ind,c('falsePositiveRate','sensitivity')]
    }
    
    x <- x[order(x$falsePositiveRate, x$sensitivity),]
    
    # add the bit to get the step
    stepsExtra <- cbind(x[-1,1], x[-nrow(x),2])
    colnames( stepsExtra) <- colnames(x)
    x <- rbind(c(1,1), x, stepsExtra, c(0,0))
    x <- x[order(x$falsePositiveRate, x$sensitivity),]
    
    x$model <- name
    return(x)
  }
  
  stepVals <- lapply(1:length(evaluationList), function(i) createSteps(evaluationList[[i]], type=type[i], name=modelNames[i]))
  data<- do.call(rbind, stepVals)
  
  plot <- ggplot2::ggplot(data=data, ggplot2::aes(x=falsePositiveRate, y=sensitivity, color=model)) +
    ggplot2::geom_polygon(ggplot2::aes(fill = model), alpha = 0.2) +
    ggplot2::geom_line(size=1) +
    ggplot2::geom_abline(intercept = 0, slope = 1,linetype = 2) +
    ggplot2::scale_x_continuous("1 - specificity", limits=c(0,1)) +
    ggplot2::scale_y_continuous("Sensitivity", limits=c(0,1)) +
    ggplot2::scale_color_discrete(name = 'Result')+
    ggplot2::scale_fill_discrete(guide=FALSE)
  
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
  return(plot)
}

plotCals <- function(evaluationList,modelNames, type = NULL, fileName=NULL){
  
  if("calibrationSummary"%in%names(evaluationList[[1]])){
    evaluationList <- evaluationList
  }else if("performanceEvaluation"%in%names(evaluationList[[1]])){
    evaluationList <- lapply(evaluationList, function(x) x$performanceEvaluation)
  } else{
    stop('Wrong evaluationList')
  }
  
  if(missing(modelNames))
    modelNames <- paste0('Model ', 1:length(evaluationList))
  
  calVal <- function(evaluation, type, name){
    
    if(is.null(type)){
      if(length(unique(evaluation$calibrationSummary$Eval))>1){
        ind <- evaluation$calibrationSummary$Eval%in%c('test','validation')
        x<- evaluation$calibrationSummary[ind,c('averagePredictedProbability','observedIncidence','PersonCountAtRisk')]
      } else{
        x<- evaluation$calibrationSummary[,c('averagePredictedProbability','observedIncidence','PersonCountAtRisk')]
      }
    } else{
      ind <- evaluation$calibrationSummary$Eval==type
      x<- evaluation$calibrationSummary[ind,c('averagePredictedProbability','observedIncidence','PersonCountAtRisk')]
    }
    
    cis <- apply(x, 1, function(x) binom.test(x[2]*x[3], x[3], alternative = c("two.sided"), conf.level = 0.95)$conf.int)
    x$lci <- cis[1,]
    x$uci <- cis[2,]
    x$model <- name
    return(x)
  }
  
  calVal<- lapply(1:length(evaluationList), function(i) calVal(evaluationList[[i]], type=type[i], name=modelNames[i]))
  data<- do.call(rbind, calVal)
  
  maxes <- max(max(data$averagePredictedProbability), max(data$observedIncidence))*1.1
  
  limits <- ggplot2::aes(ymax = uci, ymin= lci)
  
  plot <- ggplot2::ggplot(data=data,
                          ggplot2::aes(x=averagePredictedProbability, y=observedIncidence,
                                       color=model)) +
    ggplot2::geom_point(size=2) +
    ggplot2::geom_errorbar(limits) +
    ggplot2::geom_line() +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 5, size=0.4,
                         show.legend = TRUE) +
    ggplot2::scale_x_continuous("Average Predicted Probability") +
    ggplot2::scale_y_continuous("Observed Fraction With Outcome") +
    ggplot2::coord_cartesian(xlim = c(0, maxes), ylim=c(0,maxes)) +
    ggplot2::scale_color_discrete(name = 'Result')
  
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
  return(plot)
}