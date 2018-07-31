# @file AdaBoost.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
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

#' Create setting for AdaBoost with python
#' @param nEstimators    The maximum number of estimators at which boosting is terminated
#' @param learningRate   Learning rate shrinks the contribution of each classifier by learningRate.
#'                       There is a trade-off between learningRate and nEstimators .
#' @param seed           A seed for the model
#'
#' @examples
#' \dontrun{
#' model.adaBoost <- setAdaBoost(size = 4, alpha = 1e-05, seed = NULL)
#' }
#' @export
setAdaBoost <- function(nEstimators = 50, learningRate = 1, seed = NULL) {

  if (!class(seed) %in% c("numeric", "NULL"))
    stop("Invalid seed")
  if (class(nEstimators) != "numeric")
    stop("nEstimators must be a numeric value >0 ")
  if (nEstimators < 1)
    stop("nEstimators must be greater that 0 or -1")
  if (class(learningRate) != "numeric")
    stop("learningRate must be a numeric value >0 and <=1")
  if (learningRate > 1)
    stop("learningRate must be less that or equal to 1")
  if (learningRate < 0)
    stop("learningRate must be a numeric value >0")



  # test python is available and the required dependancies are there:
  if (!PythonInR::pyIsConnected()) {
    tryCatch({
      python.test <- PythonInR::autodetectPython(pythonExePath = NULL)
    }, error = function(err) {
      stop("Python was not found on your system. See the vignette for instructions.")
    })
  }
  result <- list(model = "fitAdaBoost",
                 param = split(expand.grid(nEstimators = nEstimators,
                                           learningRate = learningRate,
                                           seed = ifelse(is.null(seed), "NULL", seed)), 1:(length(nEstimators) * length(learningRate))),
                 name = "AdaBoost")
  class(result) <- "modelSettings"

  return(result)
}

fitAdaBoost <- function(population,
                        plpData,
                        param,
                        search = "grid",
                        quiet = F,
                        outcomeId,
                        cohortId,
                        ...) {

  # check plpData is libsvm format or convert if needed
  if (!"ffdf" %in% class(plpData$covariates))
    stop("Needs plpData")

  if (colnames(population)[ncol(population)] != "indexes") {
    warning("indexes column not present as last column - setting all index to 1")
    population$indexes <- rep(1, nrow(population))
  }

  # connect to python if not connected
  if (!PythonInR::pyIsConnected()) {
    PythonInR::pyConnect()
    PythonInR::pyOptions("numpyAlias", "np")
    PythonInR::pyOptions("useNumpy", TRUE)
    PythonInR::pyImport("numpy", as = "np")
  }


  # return error if we can't connect to python
  if (!PythonInR::pyIsConnected())
    stop("Python not connect error")

  start <- Sys.time()

  population$rowIdPython <- population$rowId - 1  # -1 to account for python/r index difference
  PythonInR::pySet("population",
                   as.matrix(population[, c("rowIdPython", "outcomeCount", "indexes")]))

  # convert plpData in coo to python:
  x <- toSparsePython(plpData, population, map = NULL)

  # save the model to outLoc TODO: make this an input or temp location?
  outLoc <- file.path(getwd(), "python_models")
  # clear the existing model pickles
  for (file in dir(outLoc)) file.remove(file.path(outLoc, file))

  # run model:
  outLoc <- file.path(getwd(), "python_models")
  PythonInR::pySet("modelOutput", outLoc)


  # do cross validation to find hyperParameter
  hyperParamSel <- lapply(param, function(x) do.call(trainAdaBoost, c(x, train = TRUE)))


  hyperSummary <- cbind(do.call(rbind, param), unlist(hyperParamSel))

  # now train the final model and return coef
  bestInd <- which.max(abs(unlist(hyperParamSel) - 0.5))[1]
  finalModel <- do.call(trainAdaBoost, c(param[[bestInd]], train = FALSE))

  # get the coefs and do a basic variable importance:
  varImp <- PythonInR::pyGet("adab.feature_importances_", simplify = F)[, 1]
  varImp[is.na(varImp)] <- 0
  # varImp <- PythonInR::pyGet('mlp.coefs_[0]', simplify = F)[,1] varImp[is.na(varImp)] <- 0

  covariateRef <- ff::as.ram(plpData$covariateRef)
  incs <- rep(1, nrow(covariateRef))
  covariateRef$included <- incs
  covariateRef$value <- unlist(varImp)


  # select best model and remove the others (!!!NEED TO EDIT THIS)
  modelTrained <- file.path(outLoc)
  param.best <- param[[bestInd]]

  comp <- start - Sys.time()

  # return model location (!!!NEED TO ADD CV RESULTS HERE)
  result <- list(model = modelTrained,
                 trainCVAuc = hyperParamSel,
                 hyperParamSearch = hyperSummary,
                 modelSettings = list(model = "fitAdaBoost", modelParameters = param.best),
                 metaData = plpData$metaData,
                 populationSettings = attr(population, "metaData"),
                 outcomeId = outcomeId,
                 cohortId = cohortId,
                 varImp = covariateRef,
                 trainingTime = comp,
                 dense = 0)
  class(result) <- "plpModel"
  attr(result, "type") <- "python"
  attr(result, "predictionType") <- "binary"


  return(result)
}


trainAdaBoost <- function(nEstimators = 50, learningRate = 1, seed = NULL, train = TRUE) {
  # PythonInR::pySet('size', as.matrix(size) ) PythonInR::pySet('alpha', as.matrix(alpha) )
  PythonInR::pyExec(paste0("n_estimators = ", nEstimators))
  PythonInR::pyExec(paste0("learning_rate = ", learningRate))
  PythonInR::pyExec(paste0("seed = ", ifelse(is.null(seed), "None", seed)))
  if (train)
    PythonInR::pyExec("train = True")
  if (!train)
    PythonInR::pyExec("train = False")

  # then run standard python code
  PythonInR::pyExecfile(system.file(package = "PatientLevelPrediction", "python", "adaBoost.py"))

  if (train) {
    # then get the prediction
    pred <- PythonInR::pyGet("prediction", simplify = FALSE)
    pred <- apply(pred, 1, unlist)
    pred <- t(pred)
    colnames(pred) <- c("rowId", "outcomeCount", "indexes", "value")
    pred <- as.data.frame(pred)
    attr(pred, "metaData") <- list(predictionType = "binary")

    pred$value <- 1 - pred$value
    auc <- PatientLevelPrediction::computeAuc(pred)
    writeLines(paste0("Model obtained CV AUC of ", auc))
    return(auc)
  }

  return(T)

}
