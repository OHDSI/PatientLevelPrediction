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

#' ensemble - Create an ensembling model using different models
#'
#' @description
#' #' 
#' @details
#' 
#' 
#' @param population                      The population created using createStudyPopulation() who will be used to develop the model
#' @param dataList                        An list of object of type \code{plpData} - the patient level prediction
#'                                        data extracted from the CDM.
#' @param modelList                       An list of type of base model created using one of the function in final ensembling model:
#'                                         \itemize{
#'                                         \item{LRTorch()}{ A logistic regression model}
#'                                         \item{MLPTorch()}{ A neural network model}
#'                                         \item{CNNTorch()}{ A convolutional neural network model}
#'                                         }
#' @param testSplit                       Either 'person' or 'time' specifying the type of evaluation used.
#'                                         'time' find the date where testFraction of patients had an index after the date and assigns patients with an index prior to this date into the training set and post the date into the test set
#'                                         'person' splits the data into test (1-testFraction of the data) and
#'                                         train (validationFraction of the data) sets.  The split is stratified by the class label.
#' @param testFraction                    The fraction of the data to be used as the test set in the patient
#'                                         split evaluation.
#' @param splitSeed                       The seed used to split the test/train set when using a person type testSplit                  
#' @param nfold                           The number of folds used in the cross validation (default 3)
#' @param analysisId                      The analysis ID
#' @param ensembleStrategy                The strategy used for ensembling the outputs from different models, it can be 'mean', 'product',  
#'                                        'weighted' and 'stacked'
#'                                         'mean'     the average probability from differnt models
#'                                         'product'  the product rule
#'                                         'weighted' the weighted average probability from different models using train AUC as weights.
#'                                         'stacked'  the stakced ensemble trains a logistics regression on different models. 
#'
#' @export
runEnsembleModel <- function(population, dataList, modelList,
                    testSplit = 'time', testFraction=0.25, splitSeed=NULL, nfold=3, analysisId=NULL, ensembleStrategy = 'mean'){
  start.all <- Sys.time()
  if(is.null(analysisId))
    analysisId <- gsub(':','',gsub('-','',gsub(' ','',start.all)))

  trainAUCs <- c()
  pred_probas <- matrix(nrow=length(population$subjectId), ncol =0)
  #run the models in list one by one.
  for (Index in seq_along(modelList)) {
    results <- PatientLevelPrediction::runPlp(population, dataList[[Index]], 
                                              modelSettings = modelList[[Index]],
                                              testSplit=testSplit,
                                              testFraction=testFraction,
                                              nfold=nfold, splitSeed = splitSeed)
    trainAUCs <- c(trainAUCs, as.numeric(results$performanceEvaluation$evaluationStatistics[3, 4])) 
  	prob <- results$prediction
  	if (Index == 1){
      prediction <- prob
	   }
    pred_probas <- cbind(pred_probas, prob[ncol(prob)]) 
  }

  if (ensembleStrategy == 'mean') {
    ensem_proba = rowMeans(pred_probas)
  } else if (ensembleStrategy == 'product') {
    ensem_proba = apply(pred_probas, 1, prod)
    ensem_proba <- ensem_proba^(1/length(modelList))
  } else if (ensembleStrategy == 'weighted') {
    trainAUCs <- trainAUCs/sum(trainAUCs) 
    ensem_proba = rowSums(t(t(as.matrix(pred_probas)) * trainAUCs))
  } else if (ensembleStrategy == 'stacked') {
    train_index = prediction$indexes>0
	  test_index = prediction$indexes<0
	  #add columne name to the matrix.
    for (ind in seq(1, Index)){
      colnames(pred_probas)[ind] <- paste('col', ind, sep="")
    }
	train_prob = pred_probas[train_index,]
	train_y = as.matrix(prediction$outcomeCount)[train_index]  
	lr_model <- glm(train_y ~. , data = train_prob, family = binomial(link = "logit"))
	ensem_proba <- predict(lr_model, newdata = pred_probas, type= "response")
  } else {
	  stop('ensembleStrategy must be mean, product, weighted and stacked')
  }
  
  prediction[ncol(prediction)] <- ensem_proba
  attr(prediction, "metaData")$analysisId <- analysisId

  flog.info('Train set evaluation')
  performance.train <- evaluatePlp(prediction[prediction$indexes>0,], dataList[[1]], model = modelList[[1]]$model)
  flog.trace('Done.')
  flog.info('Test set evaluation')
  performance.test <- evaluatePlp(prediction[prediction$indexes<0,], dataList[[1]], model = modelList[[1]]$model)
  flog.trace('Done.')
  performance <- reformatPerformance(train=performance.train, test=performance.test, analysisId, dataList[[1]])
    
  return(performance)
}
