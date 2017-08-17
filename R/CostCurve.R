# @file CostCurve.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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

#' Create setting for DecisionTree with python 
#' @param max_depth    The maximum depth of the tree
#' @param min_samples_split    The minimum samples per split
#' @param min_samples_leaf    The minimum number of samples per leaf
#' @param min_impurity_split  Threshold for early stopping in tree growth. A node will split if its impurity is above the threshold, otherwise it is a leaf. 
#' @param class_weight        Balance or None
#' @param seed                The random state seed
#'
#' @examples
#' \dontrun{
#' model.decisionTree <- setDecisionTree(max_depth=10,min_samples_leaf=10, seed=NULL )
#' }
#' @export
createCostCurve <- function(population, plpData, modeltype = 'MLPTorch',
                                testSplit = 'time', testFraction=0.25, factors = c(1,2,4,6,8,10,12,14,16,20,30,40,50), splitSeed=NULL, nfold=3, indexes=NULL,
                                save=NULL, saveModel=T,verbosity=futile.logger::INFO, timeStamp=FALSE, analysisId=NULL, saveFig = FALSE){
  nrRuns <- length(factors);
  costCurve <- data.frame(x = numeric(nrRuns),
                              trainAUC = integer(nrRuns),
                              testAUC = integer(nrRuns))
  run<-1
  for (factor in factors) {
    if (modeltype == 'MLPTorch'){
    modelSettings <- setMLPTorch(class_weight = factor)
    } else if (modeltype == 'LRTorch'){
      modelSettings <- setLRTorch(class_weight = factor)
    } else if (modeltype == 'CNN'){
      modelSettings <- setCNNTorch(class_weight = factor)
    } else {
      stop('Cost-senenstive leanring is not supported for this type of model')
    }
    #model <- setLRTorch(class_weight = 1)
    results <- PatientLevelPrediction::runPlp(population, plpData, 
                                              modelSettings = modelSettings,
                                              testSplit=testSplit,
                                              testFraction=testFraction,
                                              nfold=nfold, splitSeed = splitSeed)
    costCurve$x[run]<- factor
    costCurve$trainAUC[run] <- as.numeric(results$performance$evaluationStatistics[3,4]) # do not like this!
    costCurve$testAUC[run] <- as.numeric(results$performance$evaluationStatistics[12, 4])
    run <- run + 1
  }
  return(costCurve)
}