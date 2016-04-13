lr_lasso <- function(population, plpData, param,index, search='adaptive', quiet=F,...){
  #trainInd <- index$rowId[index$index>0]
  #plpData <- subsetPlpdata(plpData, trainInd)
  #if(nrow(plpData$cohorts)!=length(trainInd)) writeLines('dimension mismatch')
  
  population <- population[population$indexes>0,]
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
                                                             tolerance  = 2e-07,
                                                             cvRepetitions = 1, fold=max(index$index),
                                                             selectorType = "byPid",
                                                             threads=-1),
                                     silent=quiet)
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
                 trainAuc = NULL,
                 trainCalibration=NULL,
                 modelSettings = list(model='lr_lasso', modelParameters=param),
                 metaData = plpData$metaData,
                 varImp = varImp,
                 trainingTime=comp
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'plp'
  attr(result, 'predictionType') <- 'binary'
  return(result)
}


nnet_plp <- function(plpData, param,index, search='grid',quiet=F,... ){
  trainInd <- index$rowId[index$index>0]
  plpData <- subsetPlpdata(plpData, trainInd)
  if(!quiet)
    writeLines('Training neural network')
  start <- Sys.time()
  plpMat <- cov_to_mat(plpData, quiet=F)
  #writeLines(paste0(sum(is.na(plpMat))))
  #writeLines(paste0(plpMat[1,colnames(plpMat)%in%'outcomeCount']))
  labs <- rep('no', nrow(plpMat))
  labs[plpMat[,colnames(plpMat)%in%'outcomeCount']==1] <- 'yes'
  colnames(plpMat)[!colnames(plpMat)%in%c('rowId','outcomeCount')] <- paste0('X',colnames(plpMat)[!colnames(plpMat)%in%c('rowId','outcomeCount')])
  
  size <- c(2,ifelse(nrow(plpMat)>=50, 25, floor(nrow(plpMat)/2)) ,ifelse(nrow(plpMat)>=50, 50, nrow(plpMat)) )
  if(!is.null(param$size))
    size <- param$size
  decay <- c(0,0.1, 0.05)
  if(!is.null(param$size))
    decay <- param$decay
  
  maxits<- 500
  maxwts <- 20000
  if(!is.null(param$maxwts))
    maxwts <- param$maxwts
  if(!is.null(param$maxits))
    maxits <- param$maxits
  
  weights <- rep(1, nrow(plpMat))
  weights[labs=='yes'] <- sum(labs=='no')/sum(labs=='yes')
  
  tuneGrid <- expand.grid(size=size, decay=decay)
  fitControl <- caret::trainControl(method = "repeatedcv", number = 3,repeats = 1,
                                    verboseIter = FALSE,classProbs = TRUE,
                                    summaryFunction=caret::twoClassSummary)
  
  model1 <- caret::train(x=plpMat[,!colnames(plpMat)%in%c('outcomeCount','rowId')],
                         y=as.factor(labs),
                         method = "nnet",
                         #preProcess = NULL,
                         weights = weights,
                         metric = 'ROC',
                         maximize = TRUE,
                         trControl = fitControl,
                         tuneGrid = tuneGrid,
                         maxit=maxits,MaxNWts=maxwts)
  
  param.string <- paste(paste0(c('size','decay'),':',model1$results[which.max(model1$results$ROC),c('size','decay')]), collapse=',')
  if(!quiet)
    writeLines(paste0('Neural Network with parameters ',param.string,' obtained AUC: ', format(model1$results$ROC[which.max(model1$results$ROC)], digits=3)))
  
  param.best <- model1$results[which.max(model1$results$ROC),c('size','decay')]
  
  comp <- Sys.time() - start
  
  metaData <- list(param.search=param)
  plpData$metaData$modelSearch <- metaData
  
  result <- list(model = model1,
                 trainAuc = model1$results$ROC[which.max(model1$results$ROC)],
                 trainCalibration= NULL,
                 modelSettings = list(model='nnet',modelParameters=param.best),
                 metaData = plpData$metaData,
                 covariateRef = plpData$covariateRef,
                 trainingTime =comp
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'caret'
  attr(result, 'predictionType') <- 'binary'
  return(result)
}



svmRadial_plp <- function(plpData, param, index, search='grid', quiet=F,...){
  trainInd <- index$rowId[index$index>0]
  plpData <- subsetPlpdata(plpData, trainInd)
  if(!quiet)
    writeLines('Training svmRadial model')
  start <- Sys.time()
  plpMat <- cov_to_mat(plpData, quiet=F)
  labs <- rep('no', nrow(plpMat))
  labs[plpMat[,colnames(plpMat)%in%'outcomeCount']==1] <- 'yes'
  colnames(plpMat)[!colnames(plpMat)%in%c('rowId','outcomeCount')] <- paste0('X',colnames(plpMat)[!colnames(plpMat)%in%c('rowId','outcomeCount')])
  
  sigma <- c(0.001,0.1,1,10)
  if(!is.null(param$sigma))
    sigma <- param$sigma
  C <- seq(1,10,2)
  if(!is.null(param$C))
    C <- param$C
  tuneGrid <- expand.grid(sigma=sigma, C=C)
  
  
  if(!is.null(plpMat)){
    fitControl <- caret::trainControl(method = "repeatedcv", number = 3,repeats = 1,
                                      verboseIter = FALSE,returnResamp = "all",classProbs = TRUE,
                                      summaryFunction=caret::twoClassSummary)
    
    model1 <- caret::train(x=plpMat[,!colnames(plpMat)%in%c('outcomeCount','rowId')],
                           y=as.factor(labs),
                           method = "svmRadial",
                           preProcess = NULL,
                           weights = NULL,
                           metric = 'ROC',
                           maximize = TRUE,
                           tuneGrid = tuneGrid,
                           trControl = fitControl)
    param.string <- paste(paste0(c('sigma','C'),':',model1$results[which.max(model1$results$ROC),c('sigma','C')]), collapse=',')
    if(!quiet)
      writeLines(paste0('svmRadial with parameters ',param.string,' obtained AUC: ', format(model1$results$ROC[which.max(model1$results$ROC)], digits=3)))
    
    param.best <- model1$results[which.max(model1$results$ROC),c('sigma','C')]
    
    comp <- Sys.time() - start
    
    metaData <- list(param.search=param)
    plpData$metaData$modelSearch <- metaData
    
    result <- list(model = model1,
                   trainAuc = model1$results$ROC[which.max(model1$results$ROC)],
                   trainCalibration= NULL,
                   modelSettings = list(model='svmRadial',modelParameters=param.best),
                   metaData = plpData$metaData,
                   trainingTime =comp
    )
    class(result) <- 'plpModel'
    attr(result, 'type') <- 'caret'
    attr(result, 'predictionType') <- 'binary'
  }
  return(result)
}

#================ H2o models ======================

randomForest_plp <- function(population, plpData, param, dirPath, index, search='grid', quiet=F,...){
  
  if(!quiet)
    writeLines(paste0('Training random forest model...' ))
  start <- Sys.time()
  paramInput <- param
  
  h2oData <- h2o.importFile(path = file.path(dirPath,'libSVM','plpData.txt'))
  rowIds <- read.table(file.path(dirPath,'libSVM','rowId.txt'))[,1]
  ind <- (1:nrow(h2oData))[rowIds%in%population$rowId[population$indexes>0]]
  
  h2oData <- h2oData[ind,]
  writeLines(paste0('1: ',sum(h2oData[,1]>0), ' - 0:', sum(h2oData[,1]==0)))

  # convert label to factor
  h2oData[,1] <- h2o::as.factor(1*(h2oData[,1]>0))
  # add fold column:
  h2oData$fold <- h2o::as.h2o(index$index[index$index>0])
  
  # TODO
  # if fast varImp then do initial fast run to select features and get index
  # incInd <- ...
  incInd <- 2:(ncol(h2oData)-1)
  
  rfTrainer <- function(sample_rate=0.5,mtries=-1, ntrees=50, bal=F,
                        nbins=20, max_depth=4, min_rows=20){
    modelTrained <- h2o::h2o.randomForest(x=incInd , y=1,
                                          training_frame = h2oData, sample_rate=sample_rate,
                                          mtries=mtries, nbins=nbins,
                                          ntrees = ntrees, max_depth=max_depth,
                                          balance_classes = bal, fold_column = "fold"
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
  
  
  covRef <- read.table(file.path(dirPath,'libSVM', 'covariateRef.txt'), header = T)
  varImp <- as.data.frame(modelTrained@model$variable_importances)
  varImp$covariateId <- as.double(gsub('C', '', as.character(varImp$variable)))-1
  varImp <- varImp[varImp$relative_importance>0,]
  varImp <- merge(varImp, covRef)
  varImp<-varImp[order(-varImp$scaled_importance),]
  
  result <- list(model = modelTrained,
                 trainAuc = ifelse(is.null(modelTrained@model$cross_validation_metrics@metrics$AUC),
                                   modelTrained@model$training_metrics@metrics$AUC,
                                   modelTrained@model$cross_validation_metrics@metrics$AUC),
                 trainCalibration= NULL,
                 modelSettings = list(model='randomForest_plp',modelParameters=param.best),
                 metaData = plpData$metaData,
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






gbm_plp <- function(population,plpData, param, dirPath, index, search='grid', quiet=F,...){
  if(!quiet)
    writeLines(paste0('Training gradient boosting machine model...' ))
  start <- Sys.time()
  paramInput <- param
  
  h2oData <- h2o::h2o.importFile(path = file.path(dirPath,'libSVM','plpData.txt'))
  rowIds <- read.table(file.path(dirPath,'libSVM','rowId.txt'))[,1]
  ind <- (1:nrow(h2oData))[rowIds%in%population$rowId[population$indexes>0]]
  
  h2oData <- h2oData[ind,]
  writeLines(paste0('1: ',sum(h2oData[,1]>0), ' - 0:', sum(h2oData[,1]==0)))
  
  # convert label to factor
  h2oData[,1] <- h2o::as.factor(1*(h2oData[,1]>0))
  # add fold column:
  h2oData$fold <- h2o::as.h2o(index$index[index$index>0])

  gbmTrainer <- function(rsampRate=0.9,csampRate=1, ntrees=1, bal=F,
                         nbins=20, max_depth=4, min_rows=2, learn_rate=0.1){
    modelTrained <- h2o::h2o.gbm(x=2:(ncol(h2oData)-1) , y=1,
                                 training_frame = h2oData,distribution = "bernoulli",
                                 sample_rate = rsampRate, col_sample_rate=csampRate,
                                 balance_classes = bal, ntrees = ntrees,
                                 max_depth = max_depth, min_rows = min_rows,learn_rate = learn_rate,
                                 nbins=nbins,fold_column = "fold")
    param.string <- paste(paste0(names(as.list(match.call()) ),':',as.list(match.call()))[-1], collapse=',')
    writeLines(paste0('GBM model with params: ',param.string,' obtained AUC: ',format(modelTrained@model$cross_validation_metrics@metrics$AUC, digits=3)))
    auc <- modelTrained@model$cross_validation_metrics@metrics$AUC
    model <- modelTrained
    return(list(auc=auc, model=model))
  }
  
  # default grid search:
  if(is.null(param))
    param <- split(expand.grid(bal=c(T,F), rsampRate=c(0.7,0.9,1), ntrees=c(20,50,100)), 1:18)
  
  res <- lapply(param, function(x) do.call(gbmTrainer, x ))
  modelTrained <- res[[which.max(unlist(lapply(res, function(x) x$auc)))]]$model
  param.best <- param[[which.max(unlist(lapply(res, function(x) x$auc)))]]
  comp <- Sys.time() - start
  
  covRef <- read.table(file.path(dirPath,'libSVM', 'covariateRef.txt'), header = T)
  varImp <- as.data.frame(modelTrained@model$variable_importances)
  varImp$covariateId <- as.double(gsub('C', '', as.character(varImp$variable)))-1
  varImp <- varImp[varImp$relative_importance>0,]
  varImp <- merge(varImp, covRef)
  varImp<-varImp[order(-varImp$scaled_importance),]
  
  result <- list(model = modelTrained,
                 trainAuc = ifelse(is.null(modelTrained@model$cross_validation_metrics@metrics$AUC),
                                   modelTrained@model$training_metrics@metrics$AUC,
                                   modelTrained@model$cross_validation_metrics@metrics$AUC),
                 trainCalibration= NULL,
                 modelSettings = list(model='gbm_plp',modelParameters=param.best),
                 metaData = plpData$metaData,
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



lr_enet_plp <- function(population, plpData,dirPath,index,  param, search='grid', quiet=F,...){
  trainInd <- index$rowId[index$index>0]
  if(!quiet)
    writeLines(paste0('Training logistic regression with elastic net model...' ))
  start <- Sys.time()
  
  h2oData <- h2o.importFile(path = file.path(dirPath,'libSVM','plpData.txt'))
  rowIds <- read.table(file.path(dirPath,'libSVM','rowId.txt'))[,1]
  ind <- (1:nrow(h2oData))[rowIds%in%population$rowId[population$indexes>0]]
  
  h2oData <- h2oData[ind,]
  writeLines(paste0('1: ',sum(h2oData[,1]>0), ' - 0:', sum(h2oData[,1]==0)))
  
  # convert label to factor
  h2oData[,1] <- h2o::as.factor(1*(h2oData[,1]>0))
  # add fold column:
  h2oData$fold <- h2o::as.h2o(index$index[index$index>0])
  
  glmTrainer <- function(alpha=0.5, lambda=0.000001, lambda_search=T,
                         lambda_min_ratio = 1/1000,nlambdas = 100){
    modelTrained <- h2o::h2o.glm(x=2:(ncol(h2oData)-1) , y=1,
                                 training_frame = h2oData, family= "binomial",
                                 alpha=alpha,
                                 lambda = lambda, lambda_search = lambda_search,
                                 lambda_min_ratio = lambda_min_ratio, nlambdas = nlambdas,
                                 fold_column= "fold")
    param.string <- paste(paste0(names(as.list(match.call()) ),':',as.list(match.call()))[-1], collapse=',')
    writeLines(paste0('Elastic net logistic regression model with params: ',param.string,' obtained AUC: ',format(modelTrained@model$training_metrics@metrics$AUC, digits=3)))
    auc <- modelTrained@model$training_metrics@metrics$AUC
    model <- modelTrained
    return(list(auc=auc, model=model))
  }
  
  # default grid search:
  if(is.null(param))
    param <- split(expand.grid(lamba=c(0.000001), alpha=c(0,0.2,0.5)), 1:3)
  
  
  res <- lapply(param, function(x) do.call(glmTrainer, x ))
  modelTrained <- res[[which.max(unlist(lapply(res, function(x) x$auc)))]]$model
  param.best <- param[[which.max(unlist(lapply(res, function(x) x$auc)))]]
  comp <- Sys.time() - start
  
  covRef <- read.table(file.path(dirPath,'libSVM', 'covariateRef.txt'), header = T)
  varImp <- as.data.frame(modelTrained@model$variable_importances)
  varImp$covariateId <- as.double(gsub('C', '', as.character(varImp$variable)))-1
  varImp <- varImp[varImp$relative_importance>0,]
  varImp <- merge(varImp, covRef)
  varImp<-varImp[order(-varImp$scaled_importance),]
  
  result <- list(model = modelTrained,
                 trainAuc = ifelse(is.null(modelTrained@model$cross_validation_metrics@metrics$AUC),
                                   modelTrained@model$training_metrics@metrics$AUC,
                                   modelTrained@model$cross_validation_metrics@metrics$AUC),
                 trainCalibration= NULL,
                 modelSettings = list(model='glm_plp',modelParameters=param.best),
                 metaData = plpData$metaData,
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

knn_plp <- function(plpData, index, param, quiet=T, ...){
  trainInd <- index$rowId[index$index>0]
  plpData <- subsetPlpdata(plpData, trainInd)
  start <- Sys.time()
  k <- param$k
  if(is.null(k))
    k <- 10
  indexFolder <- param$indexFolder
  
  #clone data to prevent accidentally deleting plpData 
  covariates <- ff::clone(plpData$covariates)
  
  # format of knn
  outcomes$y <- ff::as.ff(rep(1, length(unique(ff::as.ram(outcomes$rowId)))))
  
  # add 0 outcome:
  ppl <- cohorts$rowId
  new <- ppl[!ppl%in%unique(outcomes$rowId)]
  newOut <- data.frame(rowId=new, outcomeId=-1,outcomeCount=1,timeToEvent=0,y=0)
  outcomes <- as.ffdf(rbind(plpData$outcomes,newOut))
  
  # create the model in indexFolder
  BigKnn::buildKnn(outcomes = ff::as.ffdf(outcomes),
                   covariates = ff::as.ffdf(covariates),
                   indexFolder = indexFolder)
  
  comp <- Sys.time() - start
  if(!quiet)
    writeLines(paste0('Model knn trained - took:',  format(comp, digits=3)))
  
  result <- list(model = indexFolder,
                 modelLoc = indexFolder,    # did I actually save this!?
                 trainAuc = NULL,
                 trainCalibration=NULL,
                 modelSettings = list(model='knn',
                                      modelParameters=list(k=k),
                                      indexFolder=indexFolder
                 ),
                 metaData = plpData$metaData,
                 trainingTime =comp
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'knn'
  attr(result, 'predictionType') <- 'binary'
  return(result)
}







####### HELPER FUNCTIONS #############

cov_to_mat <- function(plpData, quiet=T){
  if(is.null(plpData$covariates))
    return(NULL)
  # now convert into h2o matrix
  if(!quiet)
    writeLines('Converting sparse data into matrix...')
  start <- Sys.time()
  cov <- reshape2::dcast(ff::as.ram(plpData$covariates), rowId~covariateId, value.var='covariateValue', fill=0)
  
  # add people with no covarites:
  ppl <- plpData$cohorts$rowId
  miss.ppl <- ppl[!ppl%in%cov$rowId]
  if(length(miss.ppl)>0){
    cov.add <- matrix(rep(0, (ncol(cov)-1)*length(miss.ppl)   ), ncol=(ncol(cov)-1))
    cov.add <- data.frame(miss.ppl,cov.add)
    colnames(cov.add) <- colnames(cov)
    cov<-  rbind(cov, cov.add)
  }
  
  
  allData <- merge(cov, plpData$outcomes[,c('rowId','outcomeCount')], by='rowId', all.x=T)
  allData$outcomeCount[is.na(allData$outcomeCount)] <- 0
  
  if(!quiet)
    writeLines(paste0('Conversion tooK:', format(Sys.time()-start, digits=3)))
  
  return(allData)
}