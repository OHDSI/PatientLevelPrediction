# @file createSettings.R
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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

#' Create setting for lasso logistic regression
#'
#' @param variance   a single value or vector of values to be used to train multiple models and the model with the
#'                   best performance on the cross validation set is choosen
#'
#' @examples
#' model.lr <- logisticRegressionModel()
#' @export
logisticRegressionModel <- function(variance=0.01){
  result <- list(model='lr_lasso', param=list(val=variance))
  class(result) <- 'modelSettings' 
  attr(result, 'libSVM') <- F
  
  return(result)
}

#' Create setting for gradient boosting machine model
#'
#' @param rsampRate  The fraction of rows to include in each tree during training
#' @param csampRate  The fraction of features to include in each tree during training 
#' @param ntrees     The number of trees to build 
#' @param bal        Whether to balance the training set classes
#' @param nbins      Number of bins used in continuous variables?
#' @param max_depth  Maximum number of interactions - a large value will lead to slow model training
#' @param min_rows   The minimum number of rows required at each end node of the tree
#' @param learn_rate The boosting learn rate
#'
#' @examples
#' model.gbm <- GBMclassifier(rsampRate=c(0.5,0.9,1),csampRate=1, ntrees=c(10,100), bal=c(F,T),
#'                            max_depth=c(4,5), learn_rate=c(0.1,0.01))
#'
#' @export
GBMclassifier <- function(rsampRate=0.9,csampRate=1, ntrees=c(10,100), bal=F,
                          nbins=20, max_depth=4, min_rows=2, learn_rate=0.1){
  
  result <- list(model='gbm_plp', param= split(expand.grid(bal=bal, rsampRate=rsampRate, ntrees=ntrees, csampRate=csampRate,
                                                           nbins=nbins, max_depth=max_depth, min_rows=min_rows, learn_rate=learn_rate),
                                               1:(length(rsampRate)*length(csampRate)*length(bal)*length(ntrees)*length(nbins)*length(max_depth)*length(min_rows)*length(learn_rate)  ))
  )
  class(result) <- 'modelSettings' 
  attr(result, 'libSVM') <- T
  
  return(result)
}


#' Create setting for gradient boosting machine model using gbm_xgboost implementation
#'
#' @param ntrees     The number of trees to build 
#' @param nthreads   The number of computer threads to (how many cores do you have?)
#' @param max_depth  Maximum number of interactions - a large value will lead to slow model training
#' @param min_rows   The minimum number of rows required at each end node of the tree
#' @param learn_rate The boosting learn rate
#'
#' @examples
#' model.gbm <- GBMclassifier_xgboost(ntrees=c(10,100), nthread=20,
#'                            max_depth=c(4,6), learn_rate=c(0.1,0.3))
#'
#' @export
GBMclassifier_xgboost <- function(ntrees=c(10,100), nthread=20,
                                 max_depth=6, min_rows=20, learn_rate=0.1){
  
  if(length(nthread)>1)
    error(paste('nthreads must be length 1'))
  
  result <- list(model='gbm_xgboost', 
                 param= split(expand.grid(nround=ntrees, 
                              max.depth=max_depth, min_child_weight=min_rows, 
                              eta=learn_rate, nthread=nthread),
                              1:(length(ntrees)*length(max_depth)*length(min_rows)*length(learn_rate)  ))
)
  class(result) <- 'modelSettings' 
  attr(result, 'libSVM') <- F
  
  return(result)
}


#' Create setting for random forest model
#'
#' @param mtries     The number of features to include in each tree (-1 defaults to square root of total features)
#' @param rsampRate  The fraction of rows to include in each tree during training
#' @param csampRate  The fraction of features to include in each tree during training 
#' @param ntrees     The number of trees to build 
#' @param bal        Whether to balance the training set classes
#' @param nbins      Number of bins used in continuous variables?
#' @param max_depth  Maximum number of interactions - a large value will lead to slow model training
#' @param min_rows   The minimum number of rows required at each end node of the tree
#'
#' @examples
#' model.rf <- RFclassifier(mtries=c(-1,5,20), rsampRate=c(0.5,0.9,1),csampRate=1, ntrees=c(10,100), bal=c(F,T),
#'                            max_depth=c(5,20))
#' @export
RFclassifier <- function(mtries=-1,ntrees=c(10,500), rsampRate=0.9,csampRate=1, bal=F,
                          nbins=20, max_depth=17, min_rows=2){

  result <- list(model='randomForest_plp', param= split(expand.grid(bal=bal, sample_rate=rsampRate, ntrees=ntrees, mtries=mtries,
                                                                    nbins=nbins, max_depth=max_depth, min_rows=min_rows),
                                                        1:(length(rsampRate)*length(mtries)*length(bal)*length(ntrees)*length(nbins)*length(max_depth)*length(min_rows)  ))
  )
  class(result) <- 'modelSettings' 
  attr(result, 'libSVM') <- T
  
  return(result)
}



#' Create setting for random forest model with python (very fast)
#'
#' @param mtries     The number of features to include in each tree (-1 defaults to square root of total features)
#' @param ntrees     The number of trees to build 
#' @param max_depth  Maximum number of interactions - a large value will lead to slow model training
#'
#' @examples
#' model.rf <- RFclassifier(mtries=c(-1,5,20),  ntrees=c(10,100), 
#'                            max_depth=c(5,20))
#' @export
RFclassifier_python <- function(mtries=-1,ntrees=c(10,500),max_depth=17, varImp=T){
  
  result <- list(model='python_rf', param= expand.grid(ntrees=ntrees, mtries=mtries,
                                                       max_depth=max_depth, varImp=varImp))
  class(result) <- 'modelSettings' 
  attr(result, 'libSVM') <- T
  
  return(result)
}


#' Create setting for naive bayes model with python 
#'
#' @examples
#' model.nb <- NBclassifie_pythonr()
#' @export
NBclassifier_python <- function(){
  
  result <- list(model='python_nb', param= '')
  class(result) <- 'modelSettings' 
  attr(result, 'libSVM') <- T
  
  return(result)
}

#' Create setting for generalised linear model with elastic-new regularisation
#'
#' @param alpha   ...
#' @param lambda  ...
#' @param lambda_search ...
#' @param nlambdas  ...
#' @param lambda_min_ratio ...
#' @examples
#' model.glm <- GLMclassifier(alpha=c(0.5,0.1,0.9))

#' @export
GLMclassifier <- function(alpha=0.5, lambda=0.000001, lambda_search=T, nlambdas=100,
                          lambda_min_ratio=0.00001){
  
  result <- list(model='lr_enet_plp', param=split(expand.grid(alpha=alpha, lambda=lambda, lambda_search=lambda_search,
                                                              nlambdas=nlambdas, lambda_min_ratio=lambda_min_ratio),
                                                  1:(length(alpha)*length(lambda)*length(lambda_search)*length(nlambdas)*length(lambda_min_ratio)  ))
  )
  class(result) <- 'modelSettings' 
  attr(result, 'libSVM') <- T
  
  return(result)
}

#' Create setting for knn model
#'
#' @param k         The number of neighbors to consider 
#' @param indexFolder The directory where the results and intermediate steps are output
#'
#' @examples
#' model.knn <- KNNclassifier(k=c(3,100,1000))
#' @export
KNNclassifier <- function(k=1000, indexFolder=getwd()){
  
  result <- list(model='knn_plp', param=list(k=k, indexFolder=indexFolder)
                 )
  class(result) <- 'modelSettings' 
  attr(result, 'libSVM') <- F
  
  return(result)
}







variableImportance <- function(){
  
  
  
}


