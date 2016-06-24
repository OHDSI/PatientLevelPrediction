lr_lasso <- function(population, plpData, param,index, search='adaptive', quiet=F,
                     outcomeId, cohortId, ...){

  # check plpData is coo format:
  if(!'ffdf'%in%class(plpData$covariates) || class(plpData)=='plpData.libsvm')
    stop('Lasso Logistic regression requires plpData in coo format')
  
  metaData <- attr(population, 'metaData')
  if(!is.null(population$indexes))
    population <- population[population$indexes>0,]
  attr(population, 'metaData') <- metaData
  #TODO - how to incorporate indexes?
  val <- 0.003
  if(!is.null(param$val )) val <- param$val
  if(!quiet)
    writeLines(paste0('Training lasso logistic regression model on train set containing ', nrow(population), ' people with ',sum(population$outcomeCount>0), ' outcomes'))
  start <- Sys.time()
  modelTrained <- fitPredictiveModel(population,
                                     plpData = plpData,
                                     modelType = "logistic",
                                     prior = createPrior("laplace",exclude = c(0),useCrossValidation = TRUE),
                                     control = createControl(noiseLevel = "quiet", cvType = "auto",
                                                             startingVariance = val,
                                                             tolerance  = 2e-06,
                                                             cvRepetitions = 1, fold=ifelse(!is.null(index$index),max(index$index),1),
                                                             selectorType = "byPid",
                                                             threads=-1,
                                                             maxIterations = 3000),
                                     silent=quiet)
  
  # TODO get optimal lambda value
  
  comp <- Sys.time() - start
  if(!quiet)
    writeLines(paste0('Model Logistic Regression with Lasso regularisation trained - took:',  format(comp, digits=3)))
  
  varImp <- data.frame(covariateId=names(modelTrained$coefficients)[names(modelTrained$coefficients)!='(Intercept)'], 
                       value=modelTrained$coefficients[names(modelTrained$coefficients)!='(Intercept)'])
  if(sum(abs(varImp$value)>0)==0){
    warning('No non-zero coefficients')
    varImp <- NULL
  } else {
  varImp <- varImp[abs(varImp$value)>0,]
  varImp <- merge(varImp, ff::as.ram(plpData$covariateRef))
  varImp<-varImp[order(-abs(varImp$value)),]
  }
  
  
  result <- list(model = modelTrained,
                 modelSettings = list(model='lr_lasso', modelParameters=param), #todo get lambda as param
                 trainCVAuc = NULL,
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,# can use populationSettings$outcomeId?
                 cohortId=cohortId,
                 varImp = varImp,
                 trainingTime=comp
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'plp'
  attr(result, 'predictionType') <- 'binary'
  return(result)
}

#xgboost
gbm_xgboost <- function(population, plpData, param,index, quiet=F,
                     outcomeId, cohortId, ...){
  
  if(!quiet)
    writeLines('Training GBM model')
  
  # check plpData is coo format:
  if(!'ffdf'%in%class(plpData$covariates) || class(plpData)=='plpData.libsvm')
    stop('This algorithm requires plpData in coo format')
  
  metaData <- attr(population, 'metaData')
  if(!is.null(population$indexes))
    population <- population[population$indexes>0,]
  attr(population, 'metaData') <- metaData
  #TODO - how to incorporate indexes?
  
  # convert data into sparse Matrix:
  result <- toSparseM(plpData,population,map=NULL, silent=F)
  data <- result$data
  
  # now get population of interest
  data <- data[population$rowId,]
  
  # set test/train sets (for printing performance as it trains)
  if(!quiet)
    writeLines(paste0('Training gradient boosting machine model on train set containing ', nrow(population), ' people with ',sum(population$outcomeCount>0), ' outcomes'))
  start <- Sys.time()
  
  # pick the best hyper-params and then do final training on all data...
  writeLines('train')
  datas <- list(population=population, data=data)
  param.sel <- lapply(param, function(x) do.call(gbm_model2, c(x,datas)  ))
  writeLines('hyper')
  param.sel <- unlist(lapply(param.sel, function(x) x$auc))
  
  param <- param[[which.max(param.sel)]]
  param$final=T
  
  writeLines('final train')
  trainedModel <- do.call(gbm_model2, c(param,datas)  )$model

  comp <- Sys.time() - start
  if(!quiet)
    writeLines(paste0('Model GBM trained - took:',  format(comp, digits=3)))
  
  varImp <- xgboost::xgb.importance(model =trainedModel)
  varImp$Feature <- as.double(varImp$Feature)
  varImp<- merge(varImp, ff::as.ram(result$covariateRef), by.x='Feature', by.y='covariateId')
  varImp <- varImp[order(-varImp$Gain),]
  colnames(varImp)[colnames(varImp)=='Gain'] <- 'value'
  
  result <- list(model = trainedModel,
                 modelSettings = list(model='gbm_xgboost', modelParameters=param), #todo get lambda as param
                 trainCVAuc = NULL,
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,# can use populationSettings$outcomeId?
                 cohortId=cohortId,
                 varImp = varImp,
                 trainingTime=comp,
                 covariateMap=result$map
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'xgboost'
  attr(result, 'predictionType') <- 'binary'
  return(result)
}


#todo - create python exe for neural network/randon forest/naive bayes and other methods 
# SVM not suitable for our data

python_rf <- function(population, plpData, param, index, search='grid', quiet=F,
                      outcomeId, cohortId, ...){
  
  # check plpData is libsvm format:
  if('ffdf'%in%class(plpData$covariates) || class(plpData)!='plpData.libsvm')
    stop('Random forest requires plpData in libsvm format')
  if(!file.exists(file.path(plpData$covariates,'covariate.txt')))
    stop('Cannot find libsvm file')
  
  if(colnames(population)[ncol(population)]!='indexes'){
    warning('indexes column not present as last column - setting all index to 1')
    population$indexes <- rep(1, nrow(popualtion))
  }
    
  
  if(!quiet)
    writeLines(paste0('Training random forest model...' ))
  start <- Sys.time()
  
  # create vector of 1s and 0s indicating whether the plpData row is in the populaiton
  rowIds <- read.table(file.path(plpData$covariates,'rowId.txt'))[,1]
  rowData <- rep(0, length(rowIds))
  rowData[rowIds%in%population$rowId] <- 1
  write.table(rowData, file.path(plpData$covariates,'dataRows.txt'), col.names=F, row.names = F)
  
  # make sure populating is ordered?
  write.table(population[,c('rowId','outcomeCount','indexes')], file.path(plpData$covariates,'population.txt'), col.names=F, row.names = F)

  #do var imp
  if(param$varImp[1]==T){
    system(paste(system.file(package='PatientLevelPrediction', 'executionables',
                             'win64','python','rf_var_imp.exe'), gsub('/','\\\\',plpData$covariates), 
                 gsub('/','\\\\',plpData$covariates )  ))
    
    #load var imp and create mapping/missing
    varImp <- read.table(file.path(plpData$covariates, 'varImp.txt'))[,1]
    
    if(mean(varImp)==0)
      stop('No important variables - seems to be an issue with the data')
    
    inc <- which(varImp>mean(varImp), arr.ind=T)
    covariateRef <- ff::as.ram(plpData$covariateRef)[inc,]
    
  # save mapping, missing, indexes
  } else{
    covariateRef <- ff::as.ram(plpData$covariateRef)
    inc <- 1:ncol(covariateRef)
  }
  write.table(inc-1, file.path(plpData$covariates, 'included.txt'), row.names=F, col.names = F)
  
  # run rf_plp for each grid search:
  outLoc <- file.path(getwd(),'temp_models')
  all_auc <- c()
  for(i in 1:nrow(param)){
    system(paste(system.file(package='PatientLevelPrediction', 'executionables',
                             'win64','python','rf_train.exe'), gsub('/','\\\\',plpData$covariates),
                 param$ntrees[i],param$max_depth[i], 
                 param$mtries[i], 0,0, gsub('/','\\\\',outLoc),i )  )
    
    
    pred <- read.csv(file.path(outLoc,i,'prediction.txt'), header=F)
    colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
    attr(pred, "metaData") <- list(predictionType="binary")
    auc <- PatientLevelPrediction::computeAuc(pred)
    all_auc <- c(all_auc, auc)
    writeLines(paste0('Model with settings: ntrees:',param$ntrees[i],' max_depth: ',param$max_depth[i], 
                      'mtry: ', param$mtry[i] , ' obtained AUC of ', auc))
  }
  
  # select best model and remove the others
  modelTrained <- file.path(outLoc, which.max(all_auc)) # location - delete others
  param.best <- param[which.max(all_auc),]
  varImp <- read.csv(file.path(outLoc, which.max(all_auc),
                                 'varImp.txt'), header=F) 
  covariateRef <- cbind(covariateRef, varImp)
  write.table(covariateRef, file.path(outLoc, which.max(all_auc),'covs.txt'), row.names=F, col.names=T)
  
  # delete all other models and move model to models folder:
  #...
  
  comp <- start-Sys.time()
  
  # return model location
  result <- list(model = modelTrained,
                 trainCVAuc = all_auc[which.max(all_auc)],
                 modelSettings = list(model='randomForest_python',modelParameters=param.best),
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,
                 cohortId=cohortId,
                 varImp = covariateRef,
                 trainingTime =comp,
                 dense=0
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'python'
  attr(result, 'predictionType') <- 'binary'
  
  
  return(result)
}









python_nb <- function(population, plpData, param, index, search='grid', quiet=F,
                      outcomeId, cohortId, ...){
  
  # check plpData is libsvm format:
  if('ffdf'%in%class(plpData$covariates) || class(plpData)!='plpData.libsvm')
    stop('Random forest requires plpData in libsvm format')
  if(!file.exists(file.path(plpData$covariates,'covariate.txt')))
    stop('Cannot find libsvm file')
  
  if(colnames(population)[ncol(population)]!='indexes'){
    warning('indexes column not present as last column - setting all index to 1')
    population$indexes <- rep(1, nrow(popualtion))
  }
  
  
  if(!quiet)
    writeLines(paste0('Training naive bayes model...' ))
  start <- Sys.time()
  
  # create vector of 1s and 0s indicating whether the plpData row is in the populaiton
  rowIds <- read.table(file.path(plpData$covariates,'rowId.txt'))[,1]
  rowData <- rep(0, length(rowIds))
  rowData[rowIds%in%population$rowId] <- 1
  write.table(rowData, file.path(plpData$covariates,'dataRows.txt'), col.names=F, row.names = F)
  
  # make sure populating is ordered?
  write.table(population[,c('rowId','outcomeCount','indexes')], file.path(plpData$covariates,'population.txt'), col.names=F, row.names = F)
  
  # run model:
  outLoc <- file.path(getwd(),'temp_models')
  system(paste(system.file(package='PatientLevelPrediction', 'executionables',
                             'win64','python','naive_bayes.exe'), gsub('/','\\\\',plpData$covariates),
                  gsub('/','\\\\',outLoc),1 )  )
    
    
  pred <- read.csv(file.path(outLoc,1,'prediction.txt'), header=F)
  colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
  attr(pred, "metaData") <- list(predictionType="binary")
  pred$value <- 1-pred$value
  auc <- PatientLevelPrediction::computeAuc(pred)
  writeLines(paste0('Model obtained CV AUC of ', auc))
  
  # get the univeriate selected features (nb requires dense so need feat sel)
  varImp <- read.csv(file.path(outLoc,1, 'varImp.txt'), header=F)[,1]
  varImp[is.na(varImp)] <- 0
  if(mean(varImp)==0)
    stop('No important variables - seems to be an issue with the data')
  
  top2000 <- varImp[order(-varImp)][2000]
  inc <- which(varImp>=top2000, arr.ind=T)
  covariateRef <- ff::as.ram(plpData$covariateRef)
  covariateRef$varImp <- varImp
  
  covariateRef <- covariateRef[inc,] # this messes up order
  write.table(covariateRef, file.path(outLoc, 1,'covs.txt'), row.names=F, col.names=T)
  
  
  # select best model and remove the others
  modelTrained <- file.path(outLoc, 1) 
  param.best <- ''

  comp <- start-Sys.time()
  
  # return model location
  result <- list(model = modelTrained,
                 trainCVAuc = auc,
                 modelSettings = list(model='naiveBayes_python',modelParameters=param.best),
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,
                 cohortId=cohortId,
                 varImp = covariateRef,
                 trainingTime =comp,
                 dense=1
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'python'
  attr(result, 'predictionType') <- 'binary'
  
  
  return(result)
}


#================ H2o models ======================

randomForest_plp <- function(population, plpData, param, index, search='grid', quiet=F,
                             outcomeId, cohortId, ...){
  
  # check plpData is libsvm format:
  if('ffdf'%in%class(plpData$covariates) || class(plpData)!='plpData.libsvm')
    stop('Random forest requires plpData in libsvm format')
  if(!file.exists(file.path(plpData$covariates,'covariate.txt')))
    stop('Cannot find libsvm file')
  
  if(!quiet)
    writeLines(paste0('Training random forest model...' ))
  start <- Sys.time()
  paramInput <- param
  
  # get h2o data corresponding to popualtion with outcomeCount column 
  # and fold column if indexex column in popualtion
  h2oData <- restrictLibsvmToPopulation(plpData, population)
  
  if(!'outcomeCount'%in%h2o::colnames(h2oData))
    stop('population does not contain outcomeCount')
  
  fold_col <- "fold"
  if(!'fold'%in%h2o::colnames(h2oData)){
    warning('No indexes column in popualtion - training on all data')
    fold_col <- NULL
  } else{
    h2oData <- h2oData[as.vector(h2o::h2o.which(h2oData[,'fold']>0)),]
    writeLines(paste0('Training data has ',h2o::nrow.H2OFrame(h2oData), 
                      ' rows and ',h2o::ncol.H2OFrame(h2oData),' columns'))
  }
  
  # TODO
  # if fast varImp then do initial fast run to select features and get index
  # incInd <- ...
  incInd <- (1:h2o::ncol.H2OFrame(h2oData))[!as.vector(h2o::colnames(h2oData)%in%c('outcomeCount','fold','rowId'))] 
  
  rfTrainer <- function(sample_rate=0.5,mtries=-1, ntrees=50, bal=F,
                        nbins=20, max_depth=4, min_rows=20){
    modelTrained <- h2o::h2o.randomForest(x=incInd , y='outcomeCount',
                                          training_frame = h2oData, sample_rate=sample_rate,
                                          mtries=mtries, nbins=nbins,
                                          ntrees = ntrees, max_depth=max_depth,
                                          balance_classes = bal, fold_column = fold_col
    )
    param.string <- paste(paste0(names(as.list(match.call()) ),':',as.list(match.call()))[-1], collapse=',')
    writeLines(paste0('Random forest model with params: ',param.string,' obtained AUC: ',format(modelTrained@model$cross_validation_metrics@metrics$AUC, digits=3)))
    auc <- modelTrained@model$cross_validation_metrics@metrics$AUC
    model <- modelTrained
    varImp <- modelTrained@model$variable_importances
    return(list(auc=auc, model=model))
  }
  
  # default grid search:
  if(is.null(param))
    param <- split(expand.grid(bal=c(T,F), mtries=c(-1), ntrees=c(20,50,100)), 1:6)
  
  res <- lapply(param, function(x) do.call(rfTrainer, x ))
  modelTrained <- res[[which.max(unlist(lapply(res, function(x) x$auc)))]]$model
  param.best <- param[[which.max(unlist(lapply(res, function(x) x$auc)))]]
  comp <- Sys.time() - start
  
  
  covRef <- read.table(file.path(plpData$covariates, 'covariateRef.txt'), header = T)
  varImp <- as.data.frame(modelTrained@model$variable_importances)
  #varImp$covariateId <- as.double(gsub('C', '', as.character(varImp$variable)))-1
  varImp$covariateId <- varImp$variable
  writeLines(paste0('Test ', varImp$variable[1]))
  varImp <- varImp[varImp$relative_importance>0,]
  varImp <- merge(varImp, covRef)
  varImp<-varImp[order(-varImp$scaled_importance),]
  
  result <- list(model = modelTrained,
                 trainCVAuc = ifelse(is.null(modelTrained@model$cross_validation_metrics@metrics$AUC),
                                   modelTrained@model$training_metrics@metrics$AUC,
                                   modelTrained@model$cross_validation_metrics@metrics$AUC),
                 modelSettings = list(model='randomForest_plp',modelParameters=param.best),
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,
                 cohortId=cohortId,
                 varImp = varImp,
                 trainingTime =comp
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'h2o'
  attr(result, 'predictionType') <- 'binary'
  if(!quiet)
    writeLines(paste0('Training of Model random forest including all formating took:',  format(comp, digits=3)))
  return(result)
}






gbm_plp <- function(population,plpData, param, index, search='grid', quiet=F,
                    outcomeId, cohortId, ...){
  
    # check plpData is libsvm format:
    if('ffdf'%in%class(plpData$covariates) || class(plpData)!='plpData.libsvm')
      stop('Random forest requires plpData in libsvm format')
    if(!file.exists(file.path(plpData$covariates,'covariate.txt')))
      stop('Cannot find libsvm file')
    
    if(!quiet)
      writeLines(paste0('Training gradient boosting machine model...' ))
    start <- Sys.time()
    paramInput <- param
    
    # get h2o data corresponding to popualtion with outcomeCount column 
    # and fold column if indexex column in popualtion
    h2oData <- restrictLibsvmToPopulation(plpData, population)
    
    if(!'outcomeCount'%in%h2o::colnames(h2oData))
      stop('population does not contain outcomeCount')
    
    fold_col <- "fold"
    if(!'fold'%in%h2o::colnames(h2oData)){
      warning('No indexes column in popualtion - training on all data')
      fold_col <- NULL
    } else{
      h2oData <- h2oData[as.vector(h2o::h2o.which(h2oData[,'fold']>0)),]
      writeLines(paste0('Training data has ',h2o::nrow.H2OFrame(h2oData), 
                        ' rows and ',h2o::ncol.H2OFrame(h2oData),' columns'))
      
    }
    
    # TODO
    # if fast varImp then do initial fast run to select features and get index
    # incInd <- ...
    incInd <- (1:h2o::ncol.H2OFrame(h2oData))[!as.vector(h2o::colnames(h2oData)%in%c('outcomeCount','rowId','fold')  )] 
    outcomeNum <- which(h2o::colnames(h2oData)=='outcomeCount', arr.ind = T )

  gbmTrainer <- function(rsampRate=0.9,csampRate=1, ntrees=1, bal=F,
                         nbins=20, max_depth=4, min_rows=2, learn_rate=0.1){
    writeLines(paste0('Training gdm with fold column: ', fold_col))
    modelTrained <- h2o::h2o.gbm(x=2:50#incInd 
                                          , y='outcomeCount',
                                 training_frame = h2oData,
                                 #model_id ='gbm_plp_main',
                                 #checkpoint=NULL,
                                 distribution = "bernoulli",
                                 sample_rate = rsampRate, col_sample_rate=csampRate,
                                 balance_classes = bal, ntrees = ntrees,
                                 max_depth = max_depth, min_rows = min_rows,learn_rate = learn_rate,
                                 nbins=nbins,fold_column = fold_col)
                             #,
                             #error =function(e) writeLines(paste0('#ERROR: ', e)),
                             #warning = function(w) writeLines(paste0('@WARNING: ', w)),
                             #finally=writeLines(paste0('-- end of h2o gbm model training'))
                             #)
    param.string <- paste(paste0(names(as.list(match.call()) ),':',as.list(match.call()))[-1], collapse=',')
    writeLines(paste(class(modelTrained@model)))
    if(!is.null(fold_col)){
      writeLines(paste0('GBM model with params: ',param.string,' obtained AUC: ',format(modelTrained@model$cross_validation_metrics@metrics$AUC, digits=3)))
      auc <- modelTrained@model$cross_validation_metrics@metrics$AUC
    } else {
      writeLines(paste0('GBM model with params: ',param.string,' obtained AUC: ',format(modelTrained@model$training_metrics@metrics$AUC, digits=3)))
      auc <- modelTrained@model$training_metrics@metrics$AUC
    }
    model <- modelTrained
    return(list(auc=auc, model=model))
  }
  
  # default grid search:
  if(is.null(param))
    param <- split(expand.grid(bal=c(T,F), rsampRate=c(0.7,0.9,1), ntrees=c(20,50,100)), 1:18)
  
  if(!quiet)
    writeLines(paste0('Searching ',length(param), ' hyper-parameter combinations'))
  
  writeLines('testing start')
  writeLines(paste(class(h2oData)))
  ##res <- lapply(param, function(x) do.call(gbmTrainer, x ))
  data <- h2o::as.h2o(matrix(runif(1000),ncol=10))
  h2o::h2o.exportFile(h2oData, file.path('S:/tester/gdm'), force = FALSE)
  res = h2o::h2o.gbm(x=2:1000 #incInd 
                     #,distribution = "bernou'lli"
                      , y=1#'outcomeCount'
                     , ntrees = 10, ignore_const_cols=T,nfolds=2,
                      training_frame = h2oData)#data)
  writeLines(paste0(res@model$training_metrics@metrics$AUC))
  
  
  ##writeLines(paste(h2o::colnames(h2oData), collapse='-'))
  ##writeLines(paste(outcomeNum))
  writeLines(paste0(class(res)))
  writeLines('testing end')
  
  modelTrained <- res[[which.max(unlist(lapply(res, function(x) x$auc)))]]$model
  param.best <- param[[which.max(unlist(lapply(res, function(x) x$auc)))]]
  comp <- Sys.time() - start
  
  covRef <- read.table(file.path(plpData$covariates, 'covariateRef.txt'), header = T)
  varImp <- as.data.frame(modelTrained@model$variable_importances)
  #varImp$covariateId <- as.double(gsub('C', '', as.character(varImp$variable)))-1
  varImp$covariateId <- varImp$variable
  writeLines(paste0('Test ', varImp$variable[1]))
  varImp <- varImp[varImp$relative_importance>0,]
  varImp <- merge(varImp, covRef)
  varImp<-varImp[order(-varImp$scaled_importance),]
  
  result <- list(model = modelTrained,
                 trainCVAuc = ifelse(is.null(modelTrained@model$cross_validation_metrics@metrics$AUC),
                                   modelTrained@model$training_metrics@metrics$AUC,
                                   modelTrained@model$cross_validation_metrics@metrics$AUC),
                 modelSettings = list(model='gbm_plp',modelParameters=param.best),
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,
                 cohortId=cohortId,
                 varImp = varImp,
                 trainingTime =comp
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'h2o'
  attr(result, 'predictionType') <- 'binary'
  if(!quiet)
    writeLines(paste0('Training of Model gradient boosting machine including all formating took:',  format(comp, digits=3)))
  return(result)
}



lr_enet_plp <- function(population, plpData,index,  param, search='grid', quiet=F,
                        outcomeId, cohortId, ...){
  
  # check plpData is libsvm format:
  if('ffdf'%in%class(plpData$covariates) || class(plpData)!='plpData.libsvm')
    stop('Random forest requires plpData in libsvm format')
  if(!file.exists(file.path(plpData$covariates,'covariate.txt')))
    stop('Cannot find libsvm file')
  
  if(!quiet)
    writeLines(paste0('Training elastic new logistic regression model...' ))
  start <- Sys.time()
  paramInput <- param
  
  # get h2o data corresponding to popualtion with outcomeCount column 
  # and fold column if indexex column in popualtion
  h2oData <- restrictLibsvmToPopulation(plpData, population)
  
  if(!'outcomeCount'%in%h2o::colnames(h2oData))
    stop('population does not contain outcomeCount')
  
  fold_col <- "fold"
  if(!'fold'%in%h2o::colnames(h2oData)){
    warning('No indexes column in popualtion - training on all data')
    fold_col <- NULL
  } else{
    h2oData <- h2oData[as.vector(h2o::h2o.which(h2oData[,'fold']>0)),]
  }
  
  # TODO
  # if fast varImp then do initial fast run to select features and get index
  # incInd <- ...
  incInd <- (1:h2o::ncol.H2OFrame(h2oData))[!as.vector(h2o::colnames(h2oData)%in%c('outcomeCount', 'fold','rowId') )] 
  
  
  glmTrainer <- function(alpha=0.5, lambda=0.000001, lambda_search=T,
                         lambda_min_ratio = 0.0001,nlambdas = 100){
    writeLines(paste0('Training glm with alpha:',alpha, ' -lambda search:', lambda_search, ' feat:', length(incInd)))
    modelTrained <- h2o::h2o.glm(x=incInd , y='outcomeCount',
                                 training_frame = h2oData, family= "binomial",
                                 alpha=alpha,
                                 lambda = lambda, lambda_search = lambda_search,
                                 lambda_min_ratio = lambda_min_ratio, nlambdas = nlambdas)
    #fold_col)
    param.string <- paste(paste0(names(as.list(match.call()) ),':',as.list(match.call()))[-1], collapse=',')
    writeLines(paste0('Elastic net logistic regression model with params: ',param.string,' obtained AUC: ',format(modelTrained@model$training_metrics@metrics$AUC, digits=3)))
    auc <- modelTrained@model$training_metrics@metrics$AUC
    model <- modelTrained
    return(list(auc=auc, model=model))
  }
  

  res <- lapply(param, function(x) do.call(glmTrainer, x ))
  modelTrained <- res[[which.max(unlist(lapply(res, function(x) x$auc)))]]$model
  param.best <- param[[which.max(unlist(lapply(res, function(x) x$auc)))]]
  comp <- Sys.time() - start
  
  covRef <- read.table(file.path(plpData$covariates, 'covariateRef.txt'), header = T)
  varImp <- as.data.frame(modelTrained@model$variable_importances)
  #varImp$covariateId <- as.double(gsub('C', '', as.character(varImp$variable)))-1
  varImp$covariateId <- varImp$variable
  writeLines(paste0('Test ', varImp$variable[1]))
  varImp <- varImp[varImp$relative_importance>0,]
  varImp <- merge(varImp, covRef)
  varImp<-varImp[order(-varImp$scaled_importance),]
  
  result <- list(model = modelTrained,
                 trainCVAuc = ifelse(is.null(modelTrained@model$cross_validation_metrics@metrics$AUC),
                                   modelTrained@model$training_metrics@metrics$AUC,
                                   modelTrained@model$cross_validation_metrics@metrics$AUC),
                 modelSettings = list(model='glm_plp',modelParameters=param.best),
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,
                 cohortId=cohortId,
                 varImp = varImp,
                 trainingTime =comp
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'h2o'
  attr(result, 'predictionType') <- 'binary'
  if(!quiet)
    writeLines(paste0('Training of Model elastic net regression including all formating took:',  format(comp, digits=3)))
  return(result)
}

#========================================================

knn_plp <- function(plpData,population, index, param, quiet=T, cohortId, outcomeId, ...){
  # check plpData is coo format:
  if(!'ffdf'%in%class(plpData$covariates) || class(plpData)=='plpData.libsvm')
    stop('KNN requires plpData in coo format')
  
  metaData <- attr(population, 'metaData')
  if(!is.null(population$indexes))
    population <- population[population$indexes>0,]
  attr(population, 'metaData') <- metaData
  
  start <- Sys.time()
  k <- param$k
  if(is.null(k))
    k <- 10
  indexFolder <- param$indexFolder
  
  #clone data to prevent accidentally deleting plpData 
  covariates <- ff::clone(plpData$covariates)
  covariates <- limitCovariatesToPopulation(covariates, ff::as.ff(population$rowId))
  
  # format of knn
  #outcomes$y <- ff::as.ff(rep(1, length(unique(ff::as.ram(outcomes$rowId)))))
  # add 0 outcome:
  #ppl <- cohorts$rowId
  #new <- ppl[!ppl%in%unique(outcomes$rowId)]
  #newOut <- data.frame(rowId=new, outcomeId=-1,outcomeCount=1,timeToEvent=0,y=0)
  #outcomes <- as.ffdf(rbind(plpData$outcomes,newOut))
  population$y <- population$outcomeCount
  population$y[population$y>0] <- 1
  
  # create the model in indexFolder
  BigKnn::buildKnn(outcomes = ff::as.ffdf(population[,c('rowId','y')]),
                   covariates = ff::as.ffdf(covariates),
                   indexFolder = indexFolder)
  
  comp <- Sys.time() - start
  if(!quiet)
    writeLines(paste0('Model knn trained - took:',  format(comp, digits=3)))
  
  result <- list(model = indexFolder,
                 trainCVAuc = -1,    # did I actually save this!?
                 modelSettings = list(model='knn',
                                      modelParameters=list(k=k),
                                      indexFolder=indexFolder
                 ),
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,
                 cohortId=cohortId,
                 varImp = NULL,
                 trainingTime =comp
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'knn'
  attr(result, 'predictionType') <- 'binary'
  return(result)
}







####### HELPER FUNCTIONS #############

restrictLibsvmToPopulation <- function(plpData, population){
  
  if(missing(plpData) || is.null(plpData))
    stop('No plpData input')
  if(missing(population) || is.null(population))
    stop('No population input')
  if(!'plpData.libsvm'%in%class(plpData))
    stop('plpData is not in libsvm format - convert first')
  
  writeLines('loading libsvm data into h2o...')
  popSize = nrow(population)
  h2oData <- h2o::h2o.importFile(path = file.path(plpData$covariates,'covariate.txt'))
  rowIds <- read.table(file.path(plpData$covariates,'rowId.txt'))[,1]
  covariateIds <- read.table(file.path(plpData$covariates,'covariateRef.txt'), header=T)
  h2oData <- h2oData[,-1] # remove dummy label column that was required for format
  h2o::colnames(h2oData) =  covariateIds$covariateId
  
  # remove 0 columns
  zeroVals <- h2o::h2o.mean(h2oData)
  inds.zero <-which(zeroVals>0, arr.ind =T)
  h2oData <- h2oData[,inds.zero]
  
  h2oData$rowId <- h2o::as.h2o(rowIds)

  
  if(!is.null(population$outcomeCount)){
    if(!is.null(population$indexes)){
      writeLines('merging data with population of interest...')
      colnames(population)[colnames(population)=='indexes'] <- 'fold'
      h2oData2 <- h2o::h2o.merge(h2o::as.h2o(population[,c('rowId','fold','outcomeCount')]),
                                 h2oData)
      #h2o::colnames(h2oData)[colnames(h2oData)=='indexes'] <- 'fold'
    } else{
      h2oData2 <- h2o::h2o.merge(h2o::as.h2o(population[,c('rowId','outcomeCount')]), h2oData)
    }
    # convert label to factor
    writeLines(paste0('ncols: ', h2o::ncol.H2OFrame(h2oData2), 
                      ' nrows: ', h2o::nrow.H2OFrame(h2oData2)))
    ##writeLines(paste0('converting outcome p1...')) 
    names <- h2o::as.h2o(h2o::colnames(h2oData2))
    ##writeLines(paste0('converting outcome p2...'))
    ind <- h2o::h2o.which(names=='outcomeCount')
    ##writeLines(paste0('converting outcome p3...'))
    ind <- as.double(as.data.frame(ind))
    ##writeLines(paste0('converting outcome p4...'))
    writeLines(paste0('converting outcome (column ',ind,') to factor...')) 
    h2oData2[,ind] = h2o::as.factor(h2oData2[,ind])
    writeLines('converted...')
    
  } else {
    h2oData2 <- h2o::h2o.merge(h2o::as.h2o(population[,c('rowId','subjectId')]), h2oData)
  }
  
  if(nrow(h2oData2)!=popSize)
    warning('Some popualtion missing from plpData - not included')
  
  
  return(h2oData2)
}



toSparseM <- function(plpData,population, map=NULL, silent=T){
  cov <- ff::clone(plpData$covariates)
  covref <- ff::clone(plpData$covariateRef)

  
  writeLines(paste0('Max cov:', max(ff::as.ram(cov$covariateId))))
  
  # restrict to popualtion for speed
  if(!silent)
    writeLines('restricting to population for speed...')
  idx <- ffbase::ffmatch(x = cov$rowId, table = ff::as.ff(population$rowId))
  idx <- ffbase::ffwhich(idx, !is.na(idx))
  cov <- cov[idx, ]
  
  if(!silent)
    writeLines('Now converting covariateId...')
  oldIds <- as.double(ff::as.ram(plpData$covariateRef$covariateId))
  newIds <- 1:nrow(plpData$covariateRef)
  
  if(!is.null(map)){
    writeLines('restricting to model variables...')
    writeLines(paste0('oldIds: ',length(map[,'oldIds'])))
    writeLines(paste0('newIds:', max(as.double(map[,'newIds']))))
    ind <- ffbase::ffmatch(x=covref$covariateId, table=ff::as.ff(as.double(map[,'oldIds'])))
    ind <- ffbase::ffwhich(ind, !is.na(ind))
    covref <- covref[ind,]
    
    ind <- ffbase::ffmatch(x=cov$covariateId, table=ff::as.ff(as.double(map[,'oldIds'])))
    ind <- ffbase::ffwhich(ind, !is.na(ind))
    cov <- cov[ind,]
  }
  if(is.null(map))
    map <- data.frame(oldIds=oldIds, newIds=newIds)
  

  
  for (i in bit::chunk(covref$covariateId)) {
    ids <- covref$covariateId[i[1]:i[2]]
    ids <- plyr::mapvalues(ids, as.double(map$oldIds), as.double(map$newIds), warn_missing = FALSE)
    covref$covariateId[i[1]:i[2]] <- ids
    # tested and working
  }
  for (i in bit::chunk(cov$covariateId)) {
    ids <- cov$covariateId[i[1]:i[2]]
    ids <- plyr::mapvalues(ids, as.double(map$oldIds), as.double(map$newIds), warn_missing = FALSE)
    cov$covariateId[i[1]:i[2]] <- ids
  }
  writeLines(paste0('Max ',ffbase::max.ff(cov$covariateId)))
  if(!silent)
    writeLines('Finished - Converting covariateIds to actual column numbers')
  
  #convert into sparseM
  if(!silent){
    writeLines(paste0('# cols: ', nrow(covref)))
    writeLines(paste0('Max rowId: ', ffbase::max.ff(cov$rowId)))
  }
  
  # chunk then add
  
  data <- Matrix::sparseMatrix(i=1,
                               j=1,
                               x=0,
                               dims=c(ffbase::max.ff(cov$rowId), max(map$newIds))) # edit this to max(map$newIds)
  for (ind in bit::chunk(cov$covariateId)) {
    writeLines(paste0('start:', ind[1],'- end:',ind[2]))
    temp <- tryCatch(Matrix::sparseMatrix(i=ff::as.ram(cov$rowId[ind]),
                                          j=ff::as.ram(cov$covariateId[ind]),
                                          x=ff::as.ram(cov$covariateValue[ind]),
                                          dims=c(ffbase::max.ff(cov$rowId), max(map$newIds))),
                     warning = function(w) writeLines(paste(w)),
                     error = function(e) writeLines(paste(e))
    )
    data <- data+ temp
  }
  if(!silent)
    writeLines(paste0('Sparse matrix with dimensionality: ', dim(data)))
  
  result <- list(data=data,
                 covariateRef=covref,
                 map=map)
  return(result)
  
}



#====================== helpers

gbm_model2 <- function(data, population,
                       max.depth=6, min_child_weight=20, nthread=20,
                       nround=100, eta=0.1, final=F, ...){
  
  writeLines(paste('Training GBM with ',length(unique(population$indexes)),' fold CV'))
  if(!is.null(population$indexes) && final==F){
    index_vect <- unique(population$indexes)
    perform <- c()
    for(index in 1:length(index_vect )){
      writeLines(paste('Fold ',index, ' -- with ', sum(population$index!=index),'train rows'))
      train <- xgboost::xgb.DMatrix(data = data[population$index!=index,], label=population$outcomeCount[population$index!=index])
      test <- xgboost::xgb.DMatrix(data = data[population$index==index,], label=population$outcomeCount[population$index==index])
      watchlist <- list(train=train, test=test)
      
      model <- xgboost::xgb.train(data = train, 
                                  max.depth = max.depth, eta = eta, nthread = nthread, 
                                  min_child_weight = min_child_weight,
                                  nround = nround,
                                  watchlist = watchlist,
                                  objective = "binary:logistic",
                                  eval.metric = "logloss", eval.metric = "auc",
                                  print.every.n=10)
      
      pred <- xgboost::predict(model, data[population$index==index,])
      prediction <- population[population$index==index,]
      prediction$value <- pred
      attr(prediction, "metaData") <- list(predictionType = "binary") 
      perform <- c(perform,computeAuc(prediction))
    }
    auc <- mean(perform)
    
  } else {
    train <- xgboost::xgb.DMatrix(data = data, label=population$outcomeCount)
    model <- xgboost::xgb.train(data = train, 
                                max.depth = max.depth, eta = eta, nthread = nthread, 
                                min_child_weight = min_child_weight,
                                nround = nround,
                                objective = "binary:logistic",
                                eval.metric = "logloss", eval.metric = "auc",
                                print.every.n=10)
    
    pred <- xgboost::predict(model, data)
    prediction <- population
    prediction$value <- pred
    attr(prediction, "metaData") <- list(predictionType = "binary") 
    auc <-computeAuc(prediction)
  }
  param.val <- paste0('max depth: ',max.depth,'-- min_child_weight: ', min_child_weight, 
                      '-- nthread: ', nthread, ' nround: ',nround, '-- eta: ', eta)
  writeLines('==========================================')
  writeLines(paste0('GMB with parameters:', param.val,' obtained an AUC of ',auc))
  writeLines('==========================================')
  
  result <- list(model=model,
                 auc=auc)
  return(result)
}