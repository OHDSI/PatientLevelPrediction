# @file ViewPlp.R
#
# Copyright 2017 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' vunPlp - Interactively view the performance and model settings
#'
#' @description
#' This is a shiny app for viewing interactive plots of the performance and the settings
#' @details
#' Either the result of runPlp and view the plots
#' @param runPlp             The output of runPlp() (an object of class 'runPlp')
#' @return
#' Opens a shiny app for interactively viewing the results
#'
#' @export

viewPlp <- function(runPlp) {
  #require(shiny)
  #require(plotly)
  shiny::shinyApp(
    # ui code =================================================================
    #==============================================================================  
    
    ui = shiny::shinyUI(
      shiny::fluidPage(
        
        #shiny::singleton(
        #  tags$head(tags$script(src = "message-handler.js"))
        #),
        
        shiny::navbarPage("PatientLevelPrediction Explorer", id='mainnav',
                          
                          shiny::tabPanel(title = "Internal Validation", value="plots",
                                          
                                          #shiny::uiOutput("resultSelect"),
                                          
                                          shiny::tabsetPanel(id ="visTabs",
                                                             shiny::tabPanel(title = "Evaluation Summary", 
                                                                             value="panel_evalSum",
                                                                             shiny::h4("Evaluation Summary"),
                                                                             DT::dataTableOutput("evalSummary")),
                                                             shiny::tabPanel(title = "Characterization", 
                                                                             value="panel_characterization",
                                                                             shiny::h4("Characterization"),
                                                                             shiny::selectInput("covSumCol", "Color:", choices=c("Included into model"='binary',
                                                                                                                                 "Record type"='type',
                                                                                                                                 "No color"='none'),
                                                                                                selected='binary'),
                                                                             shiny::selectInput("covSumSize", "Size:", choices=c("Included into model"='binary',
                                                                                                                                 "No size"='none'),
                                                                                                selected='binary'),
                                                                             plotly::plotlyOutput("characterization")),
                                                             shiny:: tabPanel(title = "ROC", value="panel_roc",
                                                                              shiny::h4("Test"),
                                                                              plotly::plotlyOutput("rocPlotTest"),
                                                                              shiny::h4("Train"),
                                                                              plotly::plotlyOutput("rocPlotTrain"),
                                                                              
                                                                              shiny::p("The ROC plot shows the general ability of the model to discriminate between people with the outcome during the time at risk and those without the outcome.  It is a plot of specificity vs 1-sensitivity at every threshold (in general you may only be considered with highly specific mdoels and therefore may only want to focus towards a part of the curve)."),
                                                                              shiny::p("The x=y line is added to the plot as this shows the performance of a model that assigns a class at random (i.e., the model can not discrimiante between the people who will and will not develop the outcome during the time at risk)")
                                                                              
                                                             ),
                                                             
                                                             shiny::tabPanel(title = "Calibration", value="panel_cal",
                                                                             shiny::h4("Test"),
                                                                             plotly::plotlyOutput("calPlotTest"),
                                                                             shiny::h4("Train"),
                                                                             plotly::plotlyOutput("calPlotTrain")),
                                                             shiny::tabPanel(title = "Demographics", value="panel_demo",
                                                                             shiny::h4("Test"),
                                                                             plotly::plotlyOutput("demoPlotTest"),
                                                                             shiny::h4("Train"),
                                                                             plotly::plotlyOutput("demoPlotTrain")),
                                                             shiny::tabPanel(title = "Preference", value="panel_pref",
                                                                             shiny::h4("Test"),
                                                                             plotly::plotlyOutput("prefPlotTest"),
                                                                             shiny::h4("Train"),
                                                                             plotly::plotlyOutput("prefPlotTrain")),
                                                             shiny::tabPanel(title = "Box Plot", value="panel_box",
                                                                             shiny::h4("Test"),
                                                                             shiny::plotOutput("boxPlotTest"),
                                                                             shiny::h4("Train"),
                                                                             shiny::plotOutput("boxPlotTrain")
                                                             ),
                                                             #========================================================
                                                             #  view settings
                                                             #========================================================
                                                             shiny::tabPanel("Settings",
                                                                             
                                                                             shiny::tabsetPanel(id ="settingsTabs",
                                                                                                shiny::tabPanel(title = "Options", value="panel_options",
                                                                                                                #shiny::h4(shiny::textOutput("modelName")),
                                                                                                                shiny::h4("Model Options"),
                                                                                                                DT::dataTableOutput("modelDetails"),
                                                                                                                DT::dataTableOutput("hyperDetails"),
                                                                                                                shiny::h4("Population Options"),
                                                                                                                DT::dataTableOutput("popDetails"),
                                                                                                                shiny::h4("Variable Options"),
                                                                                                                DT::dataTableOutput("varDetails")),
                                                                                                
                                                                                                shiny::tabPanel(title = "Attrition", value="panel_attrition",
                                                                                                                shiny::h4("Attrition"),
                                                                                                                DT::dataTableOutput("attrition"))
                                                                             )
                                                                             
                                                             ) # end of settings)
                                                             
                                          )
                          ),
                          
                          shiny::tabPanel(title = "External Validation", value="external"
                                          
                          ) # add select validation location with plots...
                          
                          
                          
        )
        
        
      )
    ), 
    
    # server code =================================================================
    #==============================================================================
    shiny::shinyServer(function(input, output) {
      
      # reactive values - contains the location of the plpResult
      ##reactVars <- shiny::reactiveValues(resultLocation=NULL,
      ##                                   plpResult= NULL)
      reactVars <- list(plpResult=runPlp,
                        covSumColor='binary',
                        covSumSize='binary')
      
      # reaction events
      shiny::observeEvent(input$covSumCol, {
        reactVars$covSumColor <- input$covSumCol
        reactVars$covSumSize <- input$covSumSize
        output$characterization <- plotly::renderPlotly({
          if(is.null(reactVars$plpResult))
            return(NULL)
          
          plotCovSummary(reactVars)
          })
      }
      )
      shiny::observeEvent(input$covSumSize, {
        reactVars$covSumSize <- input$covSumSize
        reactVars$covSumColor <- input$covSumCol
        output$characterization <- plotly::renderPlotly({
          if(is.null(reactVars$plpResult))
            return(NULL)
          plotCovSummary(reactVars)
        })
      }
      )
      
      # create ui for selecting result location
      #output$resultSelect <- shiny::renderUI(
      #  shiny::wellPanel(
      #    
      #    shiny::fluidRow(
      #      shiny::column(10, shiny::textInput("resultLocation", label ='',placeholder = 'Enter plp result directory location... (e.g.,  C:/Documents/plpResult)', width = '100%')),
      #      shiny::column(1, shiny::actionButton("resultEnter", "Select"))#,
      #      #column(2, textInput())
      #    )
      #  ))
    
      
      # when resultEnter clicked -> check file location is plpResult -> set reactVars$resultLocation
      ##shiny::observeEvent(input$resultEnter, {
      ##  reactVars$resultLocation <- input$resultLocation
      
      ##  if(dir.exists(input$resultLocation))
      ##    plpResult <- PatientLevelPrediction::loadPlpResult(input$resultLocation)
      
      ##  if('runPlp'%in%class(plpResult)){
      ##    reactVars$plpResult <- plpResult
      ##  }
      ##})
      
      
      
      # create outputs 
      output$evalSummary <- DT::renderDataTable({
        if(is.null(reactVars$plpResult))
          return(NULL)
        
        returnTab <- as.data.frame(reactVars$plpResult$performanceEvaluation$evaluationStatistics)
        returnTab <- reshape2::dcast(returnTab[,-1], Metric ~ Eval, value.var = 'Value')
        
        returnTab <-data.frame(Metric=returnTab[,colnames(returnTab)=='Metric'],
                               test=format(as.double(unlist(returnTab[,colnames(returnTab)=='test'])), digits=3, nsmall=2),
                               train=format(as.double(unlist(returnTab[,colnames(returnTab)=='train'])), digits=3, nsmall=2))
        
        #rownames(returnTab) <- 1:length(returnTab)
        
      },     escape = FALSE, selection = 'none',
      options = list(
        pageLength = 25
        #,initComplete = I("function(settings, json) {alert('Done.');}")
      ))
      
      # Covariate summary - add buttons to color by type
      output$characterization <- plotly::renderPlotly({
        if(is.null(reactVars$plpResult))
          return(NULL)
        
        plotCovSummary(reactVars)
        
        
      })
      
      # ROCs
      output$rocPlotTest <- plotly::renderPlotly({
        if(is.null(reactVars$plpResult))
          return(NULL)
        #PatientLevelPrediction::plotSparseRoc(reactVars$plpResult$performanceEvaluation, 
        #                                      type='test')
        data <- reactVars$plpResult$performanceEvaluation$thresholdSummary[reactVars$plpResult$performanceEvaluation$thresholdSummary$Eval=='test',]
        plotly::plot_ly(x = 1-c(0,data$specificity,1)) %>%
          plotly::add_lines(y = c(1,data$sensitivity,0),name = "hv", 
                            text = paste('Risk Threshold:',c(0,data$predictionThreshold,1)),
                            line = list(shape = "hv",
                                        color = 'rgb(22, 96, 167)'),
                            fill = 'tozeroy') %>%
          plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                            line = list(dash = "dash"), color = I('black'),
                            type='scatter') %>%
          layout(title = "ROC Plot",
                 xaxis = list(title = "1-specificity"),
                 yaxis = list (title = "Sensitivity"),
                 showlegend = FALSE)
      })
      output$rocPlotTrain <- plotly::renderPlotly({
        if(is.null(reactVars$plpResult))
          return(NULL)
        #PatientLevelPrediction::plotSparseRoc(reactVars$plpResult$performanceEvaluation, 
        #                                      type='train')
        data <- reactVars$plpResult$performanceEvaluation$thresholdSummary[reactVars$plpResult$performanceEvaluation$thresholdSummary$Eval=='train',]
        plotly::plot_ly(x = 1-c(0,data$specificity,1)) %>%
          plotly::add_lines(y = c(1,data$sensitivity,0),name = "hv", 
                            text = paste('Risk Threshold:',c(0,data$predictionThreshold,1)),
                            line = list(shape = "hv",
                                        color = 'rgb(22, 96, 167)'),
                            fill = 'tozeroy') %>%
          plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                            line = list(dash = "dash"), color = I('black'),
                            type='scatter') %>%
          layout(title = "ROC Plot",
                 xaxis = list(title = "1-specificity"),
                 yaxis = list (title = "Sensitivity"),
                 showlegend = FALSE)
      })
      
      # Calibration
      output$calPlotTest <- plotly::renderPlotly({
        if(is.null(reactVars$plpResult))
          return(NULL)
        #PatientLevelPrediction::plotSparseCalibration(reactVars$plpResult$performanceEvaluation, 
        #                                      type='test')
        dataval <- reactVars$plpResult$performanceEvaluation$calibrationSummary[reactVars$plpResult$performanceEvaluation$calibrationSummary$Eval=='test',]
        dataval <- dataval[, c('averagePredictedProbability','observedIncidence', 'PersonCountAtRisk')]
        cis <- apply(dataval, 1, function(x) binom.test(x[2]*x[3], x[3], alternative = c("two.sided"), conf.level = 0.95)$conf.int)
        dataval$lci <- cis[1,]  
        dataval$uci <- cis[2,]
        dataval$ci <- dataval$observedIncidence-dataval$lci
        
        plotly::plot_ly(x = dataval$averagePredictedProbability) %>%
          plotly::add_markers(y = dataval$observedIncidence,
                              error_y = list(type = "data",
                                             array = dataval$ci,
                                             color = '#000000')) %>%
          plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                            line = list(dash = "dash"), color = I('black'),
                            type='scatter') %>%
          layout(title = "Calibration Plot",
                 yaxis = list(title = "Observed Incidence",
                              range = c(0, 1.1*max(c(dataval$averagePredictedProbability,dataval$observedIncidence)))),
                 xaxis = list (title = "Mean Predicted Risk",
                               range = c(0, 1.1*max(c(dataval$averagePredictedProbability,dataval$observedIncidence)))),
                 showlegend = FALSE)
      })
      output$calPlotTrain <- plotly::renderPlotly({
        if(is.null(reactVars$plpResult))
          return(NULL)
        #PatientLevelPrediction::plotSparseCalibration(reactVars$plpResult$performanceEvaluation, 
        #                                      type='train')
        dataval <- reactVars$plpResult$performanceEvaluation$calibrationSummary[reactVars$plpResult$performanceEvaluation$calibrationSummary$Eval=='train',]
        dataval <- dataval[, c('averagePredictedProbability','observedIncidence', 'PersonCountAtRisk')]
        cis <- apply(dataval, 1, function(x) binom.test(x[2]*x[3], x[3], alternative = c("two.sided"), conf.level = 0.95)$conf.int)
        dataval$lci <- cis[1,]  
        dataval$uci <- cis[2,]
        dataval$ci <- dataval$observedIncidence-dataval$lci
        
        plotly::plot_ly(x = dataval$averagePredictedProbability) %>%
          plotly::add_markers(y = dataval$observedIncidence,
                              error_y = list(type = "data",
                                             array = dataval$ci,
                                             color = '#000000')) %>%
          plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                            line = list(dash = "dash"), color = I('black'),
                            type='scatter') %>%
          layout(title = "Calibration Plot",
                 yaxis = list(title = "Observed Incidence",
                              range = c(0, 1.1*max(c(dataval$averagePredictedProbability,dataval$observedIncidence)))),
                 xaxis = list (title = "Mean Predicted Risk",
                               range = c(0, 1.1*max(c(dataval$averagePredictedProbability,dataval$observedIncidence)))),
                 showlegend = FALSE)
      })
      
      # Pref distributions
      output$prefPlotTest <- plotly::renderPlotly({
        if(is.null(reactVars$plpResult))
          return(NULL)
        dataval <- reactVars$plpResult$performanceEvaluation$thresholdSummary[reactVars$plpResult$performanceEvaluation$thresholdSummary$Eval=='test',]
        dataval$nonout <- (dataval$falsePositiveCount-c(dataval$falsePositiveCount[-1],0))/dataval$falsePositiveCount[1]
        dataval$out <- (dataval$truePositiveCount-c(dataval$truePositiveCount[-1],0))/dataval$truePositiveCount[1]
        
        plot_ly(x = dataval$preferenceThreshold, alpha = 0.6) %>%
          add_bars(y = dataval$nonout, name = 'Non-outcome')  %>%
          add_bars(y = dataval$out, name = 'Outcome') %>%
          layout(barmode = "overlay") %>%
          layout(bargap = 0) %>%
          layout(title = 'Preference Distribution',
                 xaxis = list(title = "Predicted Preference (scaled risk)"),
                 yaxis = list(title = "Density"))
        
      })
      output$prefPlotTrain <- plotly::renderPlotly({
        if(is.null(reactVars$plpResult))
          return(NULL)
        #PatientLevelPrediction::plotPreferencePDF(reactVars$plpResult$performanceEvaluation, 
        #                                              type='train')
        dataval <- reactVars$plpResult$performanceEvaluation$thresholdSummary[reactVars$plpResult$performanceEvaluation$thresholdSummary$Eval=='train',]
        dataval$nonout <- (dataval$falsePositiveCount-c(dataval$falsePositiveCount[-1],0))/dataval$falsePositiveCount[1]
        dataval$out <- (dataval$truePositiveCount-c(dataval$truePositiveCount[-1],0))/dataval$truePositiveCount[1]
        
        plot_ly(x = dataval$preferenceThreshold, alpha = 0.6) %>%
          add_bars(y = dataval$nonout, name = 'Non-outcome')  %>%
          add_bars(y = dataval$out, name = 'Outcome') %>%
          layout(barmode = "overlay") %>%
          layout(bargap = 0) %>%
          layout(title = 'Preference Distribution',
                 xaxis = list(title = "Predicted Preference (scaled risk)"),
                 yaxis = list(title = "Density"))
      })
      
      # box plots
      output$boxPlotTest <- shiny::renderPlot({
        if(is.null(reactVars$plpResult))
          return(NULL)
        PatientLevelPrediction::plotPredictionDistribution(reactVars$plpResult$performanceEvaluation, 
                                                           type='test')
      })
      output$boxPlotTrain <- shiny::renderPlot({
        if(is.null(reactVars$plpResult))
          return(NULL)
        PatientLevelPrediction::plotPredictionDistribution(reactVars$plpResult$performanceEvaluation, 
                                                           type='train')
        
      })
      
      # demo calibration
      output$demoPlotTest <- plotly::renderPlotly({
        if(is.null(reactVars$plpResult))
          return(NULL)
        #PatientLevelPrediction::plotDemographicSummary(reactVars$plpResult$performanceEvaluation, 
        #                                                   type='test')
        dataval <- reactVars$plpResult$performanceEvaluation$demographicSummary[reactVars$plpResult$performanceEvaluation$demographicSummary$Eval=='test',]
        dataval$averagePredictedProbability[is.na(dataval$averagePredictedProbability)] <- 0
        dataval$PersonCountAtRisk[is.na(dataval$PersonCountAtRisk)] <- 0
        dataval$PersonCountWithOutcome[is.na(dataval$PersonCountWithOutcome)] <- 0
        
        dataval$ageGroup2 <-gsub('Age group: ','',dataval$ageGroup)
        if(sum(c('Male','Female')%in%dataval$genGroup)==2)
        {
          dataval$ageGroup2 <- factor(dataval$ageGroup2,
                                      levels = dataval$ageGroup2[dataval$genGroup=='Male'][order(dataval$demographicId[dataval$genGroup=='Male'])])
          
          p1 <- plot_ly(x = dataval$ageGroup2[dataval$genGroup=='Male']) %>%
            add_lines(y = dataval$averagePredictedProbability[dataval$genGroup=='Male'],
                      error_y = list(value=dataval$StDevPredictedProbability[dataval$genGroup=='Male']),
                      name='Mean Predicted Risk',
                      line = list(color = 'rgb(22, 96, 167)')) %>%
            add_lines(y = dataval$PersonCountWithOutcome[dataval$genGroup=='Male']/dataval$PersonCountAtRisk[dataval$genGroup=='Male'],
                      name='Observed Risk',
                      line = list(color = 'rgb(205, 12, 24)')) %>%
            layout(yaxis = list(range = c(0,max(c(dataval$PersonCountWithOutcome/dataval$PersonCountAtRisk,dataval$averagePredictedProbability), na.rm =T)
            )),
            showlegend = FALSE)
          
          p2 <- plot_ly(x = dataval$ageGroup2[dataval$genGroup=='Female']) %>%
            add_lines(y = dataval$averagePredictedProbability[dataval$genGroup=='Female'],#error_y = list(value=dataval$StDevPredictedProbability[dataval$genGroup=='Male']),
                      error_y = list(value=dataval$StDevPredictedProbability[dataval$genGroup=='Female'],
                                     color = 'rgb(22, 96, 167)'),
                      name='Mean Predicted Risk',
                      line = list(color = 'rgb(22, 96, 167)')) %>%
            add_lines(y = dataval$PersonCountWithOutcome[dataval$genGroup=='Female']/dataval$PersonCountAtRisk[dataval$genGroup=='Female'],
                      name='Observed Risk',
                      line = list(color = 'rgb(205, 12, 24)')) %>%
            layout(yaxis = list(range = c(0,max(c(dataval$PersonCountWithOutcome/dataval$PersonCountAtRisk,dataval$averagePredictedProbability), na.rm =T)
            )),
            showlegend = FALSE)
          
          subplot(p1, p2) %>% 
            layout(annotations = list(
              list(x = 0.2 , y = 1.05, text = "Males", showarrow = F, xref='paper', yref='paper'),
              list(x = 0.8 , y = 1.05, text = "Females", showarrow = F, xref='paper', yref='paper')),
              title = 'Demographics Plot',
              yaxis = list(title = "Fraction",
                           range = c(0,max(c(dataval$PersonCountWithOutcome/dataval$PersonCountAtRisk,dataval$averagePredictedProbability), na.rm =T)
                           ))
            )
        } else if(sum(dataval$PersonCountAtRisk, na.rm = T)!=0){
          dataval$ageGroup2 <- factor(dataval$ageGroup2,
                                      levels = dataval$ageGroup2[order(dataval$demographicId)])
          
          plot_ly(x = dataval$ageGroup2) %>%
            add_lines(y = dataval$averagePredictedProbability,
                      error_y = list(value=dataval$StDevPredictedProbability,
                                     color = 'rgb(22, 96, 167)'),
                      name='Mean Predicted Risk',
                      line = list(color = 'rgb(22, 96, 167)')) %>%
            add_lines(y = dataval$PersonCountWithOutcome/dataval$PersonCountAtRisk,
                      name='Observed Risk',
                      line = list(color = 'rgb(205, 12, 24)')) %>%
            layout(yaxis = list(title = "Fraction",
                                range = c(0,max(c(dataval$PersonCountWithOutcome/dataval$PersonCountAtRisk,dataval$averagePredictedProbability), na.rm =T)
                                )),
                   title = 'Demographics Plot (No Gender)',
                   showlegend = FALSE)
          
        } else {
          return(NULL)
        }
        
      })
      output$demoPlotTrain <- plotly::renderPlotly({
        if(is.null(reactVars$plpResult))
          return(NULL)
        #PatientLevelPrediction::plotDemographicSummary(reactVars$plpResult$performanceEvaluation, 
        #                                                   type='train')
        dataval <- reactVars$plpResult$performanceEvaluation$demographicSummary[reactVars$plpResult$performanceEvaluation$demographicSummary$Eval=='train',]
        dataval$averagePredictedProbability[is.na(dataval$averagePredictedProbability)] <- 0
        dataval$PersonCountAtRisk[is.na(dataval$PersonCountAtRisk)] <- 0
        dataval$PersonCountWithOutcome[is.na(dataval$PersonCountWithOutcome)] <- 0
        
        dataval$ageGroup2 <-gsub('Age group: ','',dataval$ageGroup)
        if(sum(c('Male','Female')%in%dataval$genGroup)==2)
        {
          dataval$ageGroup2 <- factor(dataval$ageGroup2,
                                      levels = dataval$ageGroup2[dataval$genGroup=='Male'][order(dataval$demographicId[dataval$genGroup=='Male'])])
          
          p1 <- plot_ly(x = dataval$ageGroup2[dataval$genGroup=='Male']) %>%
            add_lines(y = dataval$averagePredictedProbability[dataval$genGroup=='Male'],
                      error_y = list(value=dataval$StDevPredictedProbability[dataval$genGroup=='Male']),
                      name='Mean Predicted Risk',
                      line = list(color = 'rgb(22, 96, 167)')) %>%
            add_lines(y = dataval$PersonCountWithOutcome[dataval$genGroup=='Male']/dataval$PersonCountAtRisk[dataval$genGroup=='Male'],
                      name='Observed Risk',
                      line = list(color = 'rgb(205, 12, 24)')) %>%
            layout(yaxis = list(range = c(0,max(c(dataval$PersonCountWithOutcome/dataval$PersonCountAtRisk,dataval$averagePredictedProbability), na.rm =T)
            )),
            showlegend = FALSE)
          
          p2 <- plot_ly(x = dataval$ageGroup2[dataval$genGroup=='Female']) %>%
            add_lines(y = dataval$averagePredictedProbability[dataval$genGroup=='Female'],#error_y = list(value=dataval$StDevPredictedProbability[dataval$genGroup=='Male']),
                      error_y = list(value=dataval$StDevPredictedProbability[dataval$genGroup=='Female'],
                                     color = 'rgb(22, 96, 167)'),
                      name='Mean Predicted Risk',
                      line = list(color = 'rgb(22, 96, 167)')) %>%
            add_lines(y = dataval$PersonCountWithOutcome[dataval$genGroup=='Female']/dataval$PersonCountAtRisk[dataval$genGroup=='Female'],
                      name='Observed Risk',
                      line = list(color = 'rgb(205, 12, 24)')) %>%
            layout(yaxis = list(range = c(0,max(c(dataval$PersonCountWithOutcome/dataval$PersonCountAtRisk,dataval$averagePredictedProbability), na.rm =T)
            )),
            showlegend = FALSE)
          
          subplot(p1, p2) %>% 
            layout(annotations = list(
              list(x = 0.2 , y = 1.05, text = "Males", showarrow = F, xref='paper', yref='paper'),
              list(x = 0.8 , y = 1.05, text = "Females", showarrow = F, xref='paper', yref='paper')),
              title = 'Demographics Plot',
              yaxis = list(title = "Fraction",
                           range = c(0,max(c(dataval$PersonCountWithOutcome/dataval$PersonCountAtRisk,dataval$averagePredictedProbability), na.rm =T)
                           ))
            )
        } else if(sum(dataval$PersonCountAtRisk, na.rm = T)!=0){
          dataval$ageGroup2 <- factor(dataval$ageGroup2,
                                      levels = dataval$ageGroup2[order(dataval$demographicId)])
          
          plot_ly(x = dataval$ageGroup2) %>%
            add_lines(y = dataval$averagePredictedProbability,
                      error_y = list(value=dataval$StDevPredictedProbability,
                                     color = 'rgb(22, 96, 167)'),
                      name='Mean Predicted Risk',
                      line = list(color = 'rgb(22, 96, 167)')) %>%
            add_lines(y = dataval$PersonCountWithOutcome/dataval$PersonCountAtRisk,
                      name='Observed Risk',
                      line = list(color = 'rgb(205, 12, 24)')) %>%
            layout(yaxis = list(title = "Fraction",
                                range = c(0,max(c(dataval$PersonCountWithOutcome/dataval$PersonCountAtRisk,dataval$averagePredictedProbability), na.rm =T)
                                )),
                   title = 'Demographics Plot (No Gender)',
                   showlegend = FALSE)
          
        } else {
          return(NULL)
        }
      })
      
      
      
      
      # SETTINGS
      output$modelDetails <- DT::renderDataTable({
        if(is.null(reactVars$plpResult))
          return(NULL)
        
        returnTab <-data.frame(Model = reactVars$plpResult$inputSetting$modelSettings$name,
                               Test_Split = reactVars$plpResult$inputSetting$testSplit,
                               Test_Fraction = reactVars$plpResult$inputSetting$testFraction)
        #,nfold=reactVars$plpResult$inputSetting$nfold)
        
      },     escape = FALSE, selection = 'none',
      options = list(
        pageLength = 25
        #,initComplete = I("function(settings, json) {alert('Done.');}")
      ))
      
      output$hyperDetails <- DT::renderDataTable({
        if(is.null(reactVars$plpResult))
          return(NULL)
        
        returnTab <- as.data.frame(reactVars$plpResult$model$hyperParamSearch)
      },     escape = FALSE, selection = 'none',
      options = list(
        pageLength = 25
        #,initComplete = I("function(settings, json) {alert('Done.');}")
      ))
      
      output$popDetails <- DT::renderDataTable({
        if(is.null(reactVars$plpResult))
          return(NULL)
        
        returnTab <- data.frame(Input= names(reactVars$plpResult$inputSetting$populationSettings),
                                Value = unlist(lapply(reactVars$plpResult$inputSetting$populationSettings, 
                                                      function(x) paste(x, collapse=',', sep=','))))
        
        #rownames(returnTab) <- 1:length(returnTab) 
      },     escape = FALSE, selection = 'none',
      options = list(
        pageLength = 25
        #,initComplete = I("function(settings, json) {alert('Done.');}")
      ))
      
      output$varDetails <- DT::renderDataTable({
        if(is.null(reactVars$plpResult))
          return(NULL)
        if(is.null(reactVars$plpResult$inputSetting$dataExtrractionSettings$covariateSettings))
          return(NULL)
        
        # if custom covs get the default one:
        if('getDbDefaultCovariateData' %in% 
           unlist(lapply(reactVars$plpResult$inputSetting$dataExtrractionSettings$covariateSettings, 
                         function(x) attr(x,"fun")))){
          ind <- which(unlist(lapply(reactVars$plpResult$inputSetting$dataExtrractionSettings$covariateSettings, 
                                     function(x) attr(x,"fun")))=='getDbDefaultCovariateData')
          reactVars$plpResult$inputSetting$dataExtrractionSettings$covariateSettings <- reactVars$plpResult$inputSetting$dataExtrractionSettings$covariateSettings[[ind]]
        }
        
        
        
        returnTab <- data.frame(Input= names(reactVars$plpResult$inputSetting$dataExtrractionSettings$covariateSettings),
                                Value = unlist(reactVars$plpResult$inputSetting$dataExtrractionSettings$covariateSettings))
        
        #rownames(returnTab) <- 1:length(returnTab)
        
      },     escape = FALSE, selection = 'none',
      options = list(
        pageLength = 25
        #,initComplete = I("function(settings, json) {alert('Done.');}")
      ))
      
      output$attrition <- DT::renderDataTable({
        if(is.null(reactVars$plpResult))
          return(NULL)
        
        returnTab <- reactVars$plpResult$model$populationSettings$attrition
        #rownames(returnTab) <- 1:length(returnTab)
        
      },     escape = FALSE, selection = 'none',
      options = list(
        pageLength = 25
        #,initComplete = I("function(settings, json) {alert('Done.');}")
      ))
      
      
    })
  )
}


plotCovSummary <- function(reactVars){
  if(is.null(reactVars$plpResult))
    return(NULL)
  
  #PatientLevelPrediction::plotVariableScatterplot(reactVars$plpResult$covariateSummary)
  dataVal <- reactVars$plpResult$covariateSummary
  inc <- dataVal$covariateValue!=0 
  
  if(reactVars$covSumSize=='binary'){
    sizeBig=8
    sizeSmall=4
  }
  if(reactVars$covSumSize=='none'){
    sizeBig=6
    sizeSmall=6
  }
  
  if(reactVars$covSumColor=='binary'){
    if(sum(!inc)>0 && sum(inc)>0){
    plot_ly(x = dataVal$CovariateMeanWithNoOutcome[inc] ) %>%
      plotly::add_markers(y = dataVal$CovariateMeanWithOutcome[inc],
                          marker = list(size = sizeBig, color='blue'),
                          text = paste(dataVal$covariateName[inc])) %>%
      plotly::add_markers(y = dataVal$CovariateMeanWithOutcome[!inc],
                          x = dataVal$CovariateMeanWithNoOutcome[!inc],
                          marker = list(size = sizeSmall, color='red'),
                          text = paste(dataVal$covariateName[!inc])) %>%
      plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                        line = list(dash = "dash"), color = I('black'),
                        type='scatter') %>%
      layout(title = 'Prevalance of baseline predictors in persons with and without outcome',
             xaxis = list(title = "Prevalance in persons without outcome"),
             yaxis = list(title = "Prevalance in persons with outcome"),
             showlegend = FALSE)} else{
               plot_ly(x = dataVal$CovariateMeanWithNoOutcome ) %>%
                 plotly::add_markers(y = dataVal$CovariateMeanWithOutcome,
                                     marker = list(size = sizeBig, color='blue'),
                                     text = paste(dataVal$covariateName)) %>%
                 plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                                   line = list(dash = "dash"), color = I('black'),
                                   type='scatter') %>%
                 layout(title = 'Prevalance of baseline predictors in persons with and without outcome',
                        xaxis = list(title = "Prevalance in persons without outcome"),
                        yaxis = list(title = "Prevalance in persons with outcome"),
                        showlegend = FALSE)
             }
  } else if(reactVars$covSumColor=='type'){
    dataVal$color <- rep('other', length(dataVal$covariateName))
    for( value in c('condition','drug','measure','observation','procedure','age', 'gender')){
      cond <- grep(value, tolower(dataVal$covariateName))
      if(length(cond)>0) dataVal$color[cond] <- value
    }
    if(sum(!inc)>0 && sum(inc)>0){
    plot_ly(x = dataVal$CovariateMeanWithNoOutcome[inc] ) %>%
      plotly::add_markers(y = dataVal$CovariateMeanWithOutcome[inc],
                          marker = list(size = sizeBig, color=dataVal$color[inc]),
                          text = paste(dataVal$covariateName[inc])) %>%
      plotly::add_markers(y = dataVal$CovariateMeanWithOutcome[!inc],
                          x = dataVal$CovariateMeanWithNoOutcome[!inc],
                          marker = list(size = sizeSmall, color=dataVal$color[!inc]),
                          text = paste(dataVal$covariateName[!inc])) %>%
      plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                        line = list(dash = "dash"), color = I('black'),
                        type='scatter') %>%
      layout(title = 'Prevalance of baseline predictors in persons with and without outcome',
             xaxis = list(title = "Prevalance in persons without outcome"),
             yaxis = list(title = "Prevalance in persons with outcome"),
             showlegend = FALSE)} else {
               plot_ly(x = dataVal$CovariateMeanWithNoOutcome ) %>%
                 plotly::add_markers(y = dataVal$CovariateMeanWithOutcome,
                                     marker = list(size = sizeBig, color=dataVal$color),
                                     text = paste(dataVal$covariateName)) %>%
                 plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                                   line = list(dash = "dash"), color = I('black'),
                                   type='scatter') %>%
                 layout(title = 'Prevalance of baseline predictors in persons with and without outcome',
                        xaxis = list(title = "Prevalance in persons without outcome"),
                        yaxis = list(title = "Prevalance in persons with outcome"),
                        showlegend = FALSE) 
               
             }
    
  }else if(reactVars$covSumColor=='none'){
    if(sum(!inc)>0 && sum(inc)>0){
    plot_ly(x = dataVal$CovariateMeanWithNoOutcome[inc] ) %>%
      plotly::add_markers(y = dataVal$CovariateMeanWithOutcome[inc],
                          marker = list(size = sizeBig, color='blue'),
                          text = paste(dataVal$covariateName[inc])) %>%
      plotly::add_markers(y = dataVal$CovariateMeanWithOutcome[!inc],
                          x = dataVal$CovariateMeanWithNoOutcome[!inc],
                          marker = list(size = sizeSmall, color='blue'),
                          text = paste(dataVal$covariateName[!inc])) %>%
      plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                        line = list(dash = "dash"), color = I('black'),
                        type='scatter') %>%
      layout(title = 'Prevalance of baseline predictors in persons with and without outcome',
             xaxis = list(title = "Prevalance in persons without outcome"),
             yaxis = list(title = "Prevalance in persons with outcome"),
             showlegend = FALSE)} else {
               plot_ly(x = dataVal$CovariateMeanWithNoOutcome ) %>%
                 plotly::add_markers(y = dataVal$CovariateMeanWithOutcome,
                                     marker = list(size = sizeBig, color='blue'),
                                     text = paste(dataVal$covariateName)) %>%
                 plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                                   line = list(dash = "dash"), color = I('black'),
                                   type='scatter') %>%
                 layout(title = 'Prevalance of baseline predictors in persons with and without outcome',
                        xaxis = list(title = "Prevalance in persons without outcome"),
                        yaxis = list(title = "Prevalance in persons with outcome"),
                        showlegend = FALSE)
             }
    
  }
  
}