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

#' costCurve - Create a cost curve for weighting classes using different weights
#'
#' @description
#' #' 
#' @details
#' 
#' 
#' @param population                       The population created using createStudyPopulation() who will be used to develop the model
#' @param plpData                          An object of type \code{plpData} - the patient level prediction
#'                                         data extracted from the CDM.
#' @param modeltype                        An type of model created using one of the function:
#'                                         \itemize{
#'                                         \item{LRTorch()}{ A logistic regression model}
#'                                         \item{MLPTorch()}{ A neural network model}
#'                                         \item{CNNTorch()}{ A convolutional neural network model}
#'                                         }
#' @param testSplit                        Either 'person' or 'time' specifying the type of evaluation used.
#'                                         'time' find the date where testFraction of patients had an index after the date and assigns patients with an index prior to this date into the training set and post the date into the test set
#'                                         'person' splits the data into test (1-testFraction of the data) and
#'                                         train (validationFraction of the data) sets.  The split is stratified by the class label.
#' @param testFraction                     The fraction of the data to be used as the test set in the patient
#'                                         split evaluation.
#' @param splitSeed                        The seed used to split the test/train set when using a person type testSplit                  
#' @param nfold                            The number of folds used in the cross validation (default 3)
#' @param factors                          The list of ratios between positive and negative samples
#'
#'
#'
#' @export
createCostCurve <- function(population, plpData, modeltype = 'MLPTorch',
                                testSplit = 'time', testFraction=0.25, factors = c(1,2,4,6,8,10,12,14,16,20,30,40,50), splitSeed=NULL, nfold=3){
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