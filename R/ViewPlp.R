# @file ViewPlp.R
#
# Copyright 2019 Observational Health Data Sciences and Informatics
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

#' viewPlp - Interactively view the performance and model settings
#'
#' @description
#' This is a shiny app for viewing interactive plots of the performance and the settings
#' @details
#' Either the result of runPlp and view the plots
#' @param runPlp             The output of runPlp() (an object of class 'runPlp')
#' @param validatePlp  The output of externalValidatePlp (on object of class 'validatePlp')
#' @return
#' Opens a shiny app for interactively viewing the results
#'
#' @export

viewPlp <- function(runPlp, validatePlp = NULL) {
  ensure_installed("shiny")
  ensure_installed("DT")
  ensure_installed("htmlwidgets")
  
  if(missing(runPlp))
    stop('Need to input runPlp object')
  if(class(runPlp)!='runPlp'){
      stop('runPlp is not of class runPlp')
  }
  
  if(!is.null(validatePlp)){
    if(class(validatePlp)!='validatePlp')
      stop('validatePlp is not of class validatePlp')
  }
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
                                                                             
                                                               shiny::sidebarLayout(
                                                                 shiny::sidebarPanel(width=2,
                                                                                     shiny::actionButton(inputId = 'resetCharacter', 
                                                                                                         label = 'Unselect All'#, 
                                                                                                         #width = 'auto'
                                                                                                         ),
                                                                                     shiny::radioButtons(inputId = "covSumCol",
                                                                                                         label = "Color:",
                                                                                                         choiceNames = c('None','Included','Analysis'),  
                                                                                                         choiceValues = c('none','binary','type'),
                                                                                                         selected='binary'),
                                                                                     shiny::radioButtons(inputId = "covSumSize",
                                                                                                         label = "Size:",
                                                                                                         choiceNames = c('None','Included','Coefficient'),  
                                                                                                         choiceValues = c('none','binary','coef'),
                                                                                                         selected='binary')
                                                                 ),
                                                                 shiny::mainPanel(
                                                                   shiny::tabsetPanel(id ="characterisation",
                                                                                      shiny::tabPanel(
                                                                                        title = "Plot", value="character_plot",
                                                                             plotly::plotlyOutput("characterization")),
                                                                             shiny::tabPanel(
                                                                               title = "Table", value="character_table",
                                                                             DT::dataTableOutput("characterizationTab")
                                                                             )
                                                                   )
                                                                 )
                                                                 )),
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
                          
                          shiny::tabPanel(title = "External Validation", value="external",
                                          shiny::tabsetPanel(id ="valTabs",
                                                             shiny::tabPanel(title = "Evaluation Summary", 
                                                                             value="panel_evalSum2",
                                                                             shiny::h4("Evaluation Summary"),
                                                                             DT::dataTableOutput("evalSummaryVal")),
                                                             shiny::tabPanel(title = "Characterization", 
                                                                             value="panel_characterization2",
                                                                             shiny::h4("Characterization"),
                                                                             DT::dataTableOutput("characterizationTabVal")
                                                                             ),
                                                             shiny:: tabPanel(title = "ROC", value="panel_roc2",
                                                                              #shiny::h4("Internal validation"),
                                                                              #plotly::plotlyOutput("rocPlotTest"),
                                                                              shiny::h4("External Validation"),
                                                                              plotly::plotlyOutput("rocPlotVal")
                                                                              
                                                                               ),
                                                             
                                                             shiny::tabPanel(title = "Calibration", value="panel_cal2",
                                                                             #shiny::h4("Internal validation"),
                                                                             #plotly::plotlyOutput("calPlotTest"),
                                                                             shiny::h4("External validation"),
                                                                             plotly::plotlyOutput("calPlotVal")
                                                                             )
                                                             
                                                             
                                          )         
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
      reactVars <- list(plpResult=runPlp)
      
      # reset the row selection
      shiny::observeEvent(input$resetCharacter,
        {DT::selectRows(proxy=DT::dataTableProxy(outputId='characterizationTab', 
                                                deferUntilFlush=F), 
                       selected=NULL)}
      )
      
  
    
      # create outputs 
      output$evalSummary <- DT::renderDataTable({
        if(is.null(reactVars$plpResult))
          return(NULL)
        
        returnTab <- as.data.frame(reactVars$plpResult$performanceEvaluation$evaluationStatistics)
        returnTab$Metric <- gsub('.auc','',returnTab$Metric)
        returnTab <- reshape2::dcast(returnTab[,-1], Metric ~ Eval, value.var = 'Value')
        
        # adding incidence
        oc <- returnTab[returnTab[,colnames(returnTab)=='Metric']=='outcomeCount',colnames(returnTab)!='Metric']
        pop <- returnTab[returnTab[,colnames(returnTab)=='Metric']=='populationSize',colnames(returnTab)!='Metric']
        inc <- c('Incidence',as.double(oc)/as.double(pop)*100)
        returnTab <- rbind(returnTab, inc)
        
        returnTab <-data.frame(Metric=returnTab[,colnames(returnTab)=='Metric'],
                               test=format(as.double(unlist(returnTab[,colnames(returnTab)=='test'])), digits=3, nsmall=2,scientific=F),
                               train=format(as.double(unlist(returnTab[,colnames(returnTab)=='train'])), digits=3, nsmall=2,scientific=F))
        
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
        plotCovSummary(reactVars,input)
      })
      
      output$characterizationTab <- DT::renderDataTable({
        if(is.null(reactVars$plpResult))
          return(NULL)
        
        returnTab <- as.data.frame(reactVars$plpResult$covariateSummary)
        if(!is.null(returnTab$CovariateMeanWithOutcome)){
          returnTab$meanDifference <-returnTab$CovariateMeanWithOutcome- returnTab$CovariateMeanWithNoOutcome
          returnTab <- returnTab[,c('covariateName','CovariateMeanWithOutcome','CovariateMeanWithNoOutcome','meanDifference')]
        } else {
          returnTab <- returnTab[,c('covariateName','CovariateCountWithOutcome','CovariateCountWithNoOutcome')]
        }
        returnTab[,-1] <- formatC(as.double(unlist(returnTab[,-1])), digits=4,format = "f")
        returnTab
        
      },     escape = FALSE, #selection = 'none',
      options = list(
        pageLength = 25
      ))
      
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
                                Value = unlist(lapply(reactVars$plpResult$inputSetting$dataExtrractionSettings$covariateSettings, function(x) paste(x, collapse='-')))
                                  
                                  )
        
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
      
      # here 
      ## =================================================================================
      ##    EXTERNAL VALIDATION PLOTS
      ## =================================================================================
      output$characterizationTabVal <- DT::renderDataTable({
        if(is.null(validatePlp))
          return(NULL)
        
        if(length(validatePlp$validation)>1){
          valSummary <- c()
          for(i in 1:length(validatePlp$validation)){
            validatePlp$validation[[i]]$covariateSummary$meanDiff <- validatePlp$validation[[i]]$covariateSummary$CovariateMeanWithOutcome-
              validatePlp$validation[[i]]$covariateSummary$CovariateMeanWithNoOutcome
            voi <- validatePlp$validation[[i]]$covariateSummary[,c('covariateId','covariateName','meanDiff')]
            voi$database <- names(validatePlp$validation)[i]
            valSummary <- rbind(voi, valSummary)
          }
          valSummary$covariateName <- as.character(valSummary$covariateName)
          valSummary <- reshape2::dcast(valSummary, covariateId+covariateName~database, value.var = 'meanDiff')
          valSummary[,-c(1,2)] <- formatC(as.double(unlist(valSummary[,-c(1,2)])), digits=4,format = "f")
          #valSummary
          merge(reactVars$plpResult$covariateSummary[,c('covariateId','covariateValue')],valSummary, by='covariateId')
        } else {
          merge(reactVars$plpResult$covariateSummary[,c('covariateId','covariateValue')],validatePlp$validation[[1]]$covariateSummary, by='covariateId')
          
        }
      },     escape = FALSE, selection = 'none',
      options = list(
        pageLength = 25
      ))
      
      output$evalSummaryVal <- DT::renderDataTable({
        if(is.null(validatePlp))
          return(NULL)
        
       
        validatePlp$summary$Incidence <- as.double(as.character(validatePlp$summary$outcomeCount))/as.double(as.character(validatePlp$summary$populationSize))
        # format to 3dp
        for(col in colnames(validatePlp$summary)[!colnames(validatePlp$summary)%in%c('Database','outcomeCount','populationSize')])
          class(validatePlp$summary[,col]) <- 'numeric'
        is.num <- sapply(validatePlp$summary, is.numeric)
        validatePlp$summary[is.num] <- apply(validatePlp$summary[is.num],2,  round, 3)
        
        returnTab <- t(as.data.frame(validatePlp$summary))
      },     escape = FALSE, selection = 'none',
      options = list(
        pageLength = 25
        #,initComplete = I("function(settings, json) {alert('Done.');}")
      ))
      
      output$rocPlotVal <- plotly::renderPlotly({
        if(is.null(validatePlp))
          return(NULL)
        #PatientLevelPrediction::plotSparseRoc(reactVars$plpResult$performanceEvaluation, 
        #                                      type='train')
        rocPlotVal <- list()
        length(rocPlotVal) <- length(validatePlp$validation)
        for(i in 1:length(validatePlp$validation)){
        data <- validatePlp$validation[[i]]$performanceEvaluation$thresholdSummary
        rocPlotVal[[i]] <- plotly::plot_ly(x = 1-c(0,data$specificity,1)) %>%
          plotly::add_lines(y = c(1,data$sensitivity,0),name = "hv", 
                            text = paste('Risk Threshold:',c(0,data$predictionThreshold,1)),
                            line = list(shape = "hv",
                                        color = 'rgb(22, 96, 167)'),
                            fill = 'tozeroy') %>%
          plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                            line = list(dash = "dash"), color = I('black'),
                            type='scatter') %>%
          plotly::layout(annotations = list(text = names(validatePlp$validation)[i],
            xref = "paper", yref = "paper", yanchor = "bottom",xanchor = "center",
            align = "center",x = 0.5,y = 1,showarrow = FALSE))

        }
        p <- do.call(plotly::subplot, rocPlotVal)
        p %>% plotly::layout(xaxis = list(title = "1-specificity"),
                             yaxis = list (title = "Sensitivity"),
                             showlegend = FALSE)
      })
      
      output$calPlotVal <- plotly::renderPlotly({
        if(is.null(validatePlp))
          return(NULL)
        
        calPlotVal <- list()
        length(calPlotVal) <- length(validatePlp$validation)
        for(i in 1:length(validatePlp$validation)){
          data <- validatePlp$validation[[i]]$performanceEvaluation$calibrationSummary
          data <- data[, c('averagePredictedProbability','observedIncidence', 'PersonCountAtRisk')]
          cis <- apply(data, 1, function(x) binom.test(x[2]*x[3], x[3], alternative = c("two.sided"), conf.level = 0.95)$conf.int)
        data$lci <- cis[1,]  
        data$uci <- cis[2,]
        data$ci <- data$observedIncidence-data$lci
        
        calPlotVal[[i]] <- plotly::plot_ly(x = data$averagePredictedProbability) %>%
          plotly::add_markers(y = data$observedIncidence,
                              error_y = list(type = "data",
                                             array = data$ci,
                                             color = '#000000')) %>%
          plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                            line = list(dash = "dash"), color = I('black'),
                            type='scatter') %>%
          plotly::layout(annotations = list(text = names(validatePlp$validation)[i],
                                            xref = "paper", yref = "paper", yanchor = "bottom",xanchor = "center",
                                            align = "center",x = 0.5,y = 1,showarrow = FALSE),
                         yaxis = list(range = c(0, 1.1*max(c(data$averagePredictedProbability,data$observedIncidence)))),
                         xaxis = list (range = c(0, 1.1*max(c(data$averagePredictedProbability,data$observedIncidence))))
          )
          
        }
        p <- do.call(plotly::subplot, calPlotVal)
        
        p %>% plotly::layout(yaxis = list(title = "Observed Incidence"),
                             xaxis = list (title = "Mean Predicted Risk"),
                             showlegend = FALSE)
        
      })
      
    })
  )
  
  
  
}


plotCovSummary <- function(reactVars,input){
  if(is.null(reactVars$plpResult))
    return(NULL)
  
  dataVal <- reactVars$plpResult$covariateSummary
  # remove large values...
  dataVal$CovariateCountWithOutcome[is.na(dataVal$CovariateMeanWithOutcome)] <- 0
  dataVal$CovariateMeanWithOutcome[dataVal$CovariateMeanWithOutcome>1] <- 1
  dataVal$CovariateMeanWithNoOutcome[dataVal$CovariateMeanWithNoOutcome>1] <- 1
  dataVal$covariateValue[is.na(dataVal$covariateValue)] <- 0
  

  #get the size
  #====================================
  if(input$covSumSize=='binary'){
    dataVal$size <- rep(4, length(dataVal$covariateValue))
    dataVal$size[dataVal$covariateValue!=0] <- 8
  }else if(input$covSumSize=='none'){
     dataVal$size <- rep(6, length(dataVal$covariateValue))
  } else if(input$covSumSize=='coef'){
    dataVal$size <- abs(dataVal$covariateValue)
    dataVal$size[is.na(dataVal$size)] <- 0
    dataVal$size <- 10*dataVal$size/max(dataVal$size)
  }
  
  #get the included
  #====================================
  inc <- dataVal$covariateValue!=0 
  nonInc <- dataVal$covariateValue==0 
  incAnnotations <- F
  if(length(input$characterizationTab_rows_selected)>0){
    inc <- input$characterizationTab_rows_selected
    incAnnotations <- T
    writeLines(paste0(inc, collapse='-'))
  }
  
  #get the color
  #=====================================
  if(input$covSumCol=='binary'){
  dataVal$color <- rep('blue', length(dataVal$covariateName))
  dataVal$color[nonInc] <- 'red'
  } else if(input$covSumCol=='type'){
    dataVal$color <- as.factor(dataVal$analysisId)
    #rep('purple', length(dataVal$covariateName)) # need to do this...
  } else if(input$covSumCol=='none'){
    dataVal$color <- rep('black', length(dataVal$covariateName))
  }
  
  # do annotations
  dataVal$annotation <- sapply(dataVal$covariateName, getName)
    
   
  if(incAnnotations){
    plot_ly(x = dataVal$CovariateMeanWithNoOutcome[inc] ) %>%
      plotly::add_markers(y = dataVal$CovariateMeanWithOutcome[inc],
                          marker = list(size = dataVal$size[inc],#sizeBig, 
                                        color=dataVal$color[inc]),
                          text = paste(dataVal$covariateName[inc])) %>%
      plotly::add_annotations(x=dataVal$CovariateMeanWithNoOutcome[inc],
                              y=dataVal$CovariateMeanWithOutcome[inc],
                              text=paste(dataVal$annotation[inc]),
                              xref='x', yref='y', showarrow=T,arrowhead=4,
                              arrowsize=.5, ax=20,ay=-40) %>%
      plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                        line = list(dash = "dash"), color = I('black'),
                        type='scatter') %>%
      layout(title = 'Prevalance of baseline predictors in persons with and without outcome',
             xaxis = list(title = "Prevalance in persons without outcome"),
             yaxis = list(title = "Prevalance in persons with outcome"),
             showlegend = FALSE)
  } else{
    plot_ly(x = dataVal$CovariateMeanWithNoOutcome[inc] ) %>%
      plotly::add_markers(y = dataVal$CovariateMeanWithOutcome[inc],
                          marker = list(size = dataVal$size[inc],#sizeBig, 
                                        color=dataVal$color[inc]),
                          text = paste(dataVal$covariateName[inc])) %>%
      plotly::add_markers(y = dataVal$CovariateMeanWithOutcome[nonInc],
                          x = dataVal$CovariateMeanWithNoOutcome[nonInc],
                          marker = list(size = dataVal$size[nonInc],#sizeSmall, 
                                        color = dataVal$color[nonInc]),
                          text = paste(dataVal$covariateName[nonInc])) %>% 
      plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                        line = list(dash = "dash"), color = I('black'),
                        type='scatter') %>%
      layout(title = 'Prevalance of baseline predictors in persons with and without outcome',
             xaxis = list(title = "Prevalance in persons without outcome"),
             yaxis = list(title = "Prevalance in persons with outcome"),
             showlegend = FALSE)      
  }

  
}


getName <- function(x){  
  x <- as.character(x)
  if(length(grep('index month:', x))>0){
    return(x)
  } else if(length(grep('age group:', x))>0){
    return(x)
  } else if(length(grep('Concept set:', x))>0){
    x <- strsplit(paste0(x), split=':')[[1]][2]
    #x <- gsub(' ','',x)
    return(x)
  }else if(length(grep('index:', x))>0){
    x <- strsplit(paste0(x), split='index:')[[1]][2]
    #x <- gsub(' ','',x)
    return(x)
  } else {
    return(x)
  }
}
