# @file formatting.R
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

#' Convert the plpData in COO format into a sparse R matrix
#'
#' @description
#' Converts the standard plpData to a sparse matrix
#'
#' @details
#' This function converts the covariate file from ffdf in COO format into a sparse matrix from
#' the package Matrix
#' @param plpData                       An object of type \code{plpData} with covariate in coo format - the patient level prediction
#'                                      data extracted from the CDM.
#' @param population                    The population to include in the matrix
#' @param map                           A covariate map (telling us the column number for covariates)
#' @param temporal                      Whether you want to convert temporal data
#' @examples
#' #TODO
#'
#' @return
#' Returns a list, containing the data as a sparse matrix, the plpData covariateRef
#' and a data.frame named map that tells us what covariate corresponds to each column
#' This object is a list with the following components: \describe{
#' \item{data}{A sparse matrix with the rows corresponding to each person in the plpData and the columns corresponding to the covariates.}
#' \item{covariateRef}{The plpData covariateRef.}
#' \item{map}{A data.frame containing the data column ids and the corresponding covariateId from covariateRef.}
#' }
#'
#' @export
toSparseM <- function(plpData,population, map=NULL, temporal=F){
  cov <- plpData$covariates #ff::clone(plpData$covariates)
  covref <- plpData$covariateRef#ff::clone(plpData$covariateRef)

  plpData.mapped <- MapCovariates(covariates=cov, covariateRef=ff::clone(covref),
                                  population, map)

  for (i in bit::chunk(plpData.mapped$covariateRef$covariateId)) {
    ids <- plpData.mapped$covariateRef$covariateId[i[1]:i[2]]
    ids <- plyr::mapvalues(ids, as.double(plpData.mapped$map$oldIds), as.double(plpData.mapped$map$newIds), warn_missing = FALSE)
    plpData.mapped$covariateRef$covariateId[i[1]:i[2]] <- ids
    # tested and working
  }
  for (i in bit::chunk(plpData.mapped$covariates$covariateId)) {
    ids <- plpData.mapped$covariates$covariateId[i[1]:i[2]]
    ids <- plyr::mapvalues(ids, as.double(plpData.mapped$map$oldIds), as.double(plpData.mapped$map$newIds), warn_missing = FALSE)
    plpData.mapped$covariates$covariateId[i[1]:i[2]] <- ids
  }
  futile.logger::flog.debug(paste0('Max ',ffbase::max.ff(plpData.mapped$covariates$covariateId)))

    #convert into sparseM
  futile.logger::flog.debug(paste0('# cols: ', nrow(plpData.mapped$covariateRef)))
  futile.logger::flog.debug(paste0('Max rowId: ', ffbase::max.ff(plpData.mapped$covariates$rowId)))

  # chunk then add
  if(!temporal){
  data <- Matrix::sparseMatrix(i=1,
                               j=1,
                               x=0,
                               dims=c(max(population$rowId), max(plpData.mapped$map$newIds))) # edit this to max(map$newIds)
  for (ind in bit::chunk(plpData.mapped$covariates$covariateId)) {
    futile.logger::flog.debug(paste0('start:', ind[1],'- end:',ind[2]))
    temp <- ftry(Matrix::sparseMatrix(i=ff::as.ram(plpData.mapped$covariates$rowId[ind]),
                                          j=ff::as.ram(plpData.mapped$covariates$covariateId[ind]),
                                          x=ff::as.ram(plpData.mapped$covariates$covariateValue[ind]),
                                          dims=c(max(population$rowId), max(plpData.mapped$map$newIds)))
    )
    data <- data+ temp
  }
  } else {
    for(i in min(cov$timeId):max(cov$timeId)){
      plpData.mapped$temp_covariates<-plpData.mapped$covariates[plpData.mapped$covariates$timeId==i]
      data <- Matrix::sparseMatrix(i=1,
                                   j=1,
                                   x=0,
                                   dims=c(max(population$rowId), max(plpData.mapped$map$newIds))) # edit this to max(map$newIds)
      for (ind in bit::chunk(plpData.mapped$temp_covariates$covariateId)) {
        futile.logger::flog.debug(paste0('start:', ind[1],'- end:',ind[2]))
        temp <- futile.logger::ftry(Matrix::sparseMatrix(i=ff::as.ram(plpData.mapped$temp_covariates$rowId[ind]),
                                                         j=ff::as.ram(plpData.mapped$temp_covariates$covariateId[ind]),
                                                         x=ff::as.ram(plpData.mapped$temp_covariates$covariateValue[ind]),
                                                         dims=c(max(population$rowId), max(plpData.mapped$map$newIds)))
        )
        data <- data+temp
      }
      data_array<-slam::as.simple_sparse_array(data)
      #extending one more dimesion to the array
      data_array<-slam::extend_simple_sparse_array(data_array,c(1L))
      #binding arrays along the dimesion
      if(i==min(cov$timeId)) {result_array<-data_array
      }else{
        result_array<-slam::abind_simple_sparse_array(result_array,data_array,MARGIN=2L)
      }
    }
    data <- result_array
  }
  
  futile.logger::flog.debug(paste0('Sparse matrix with dimensionality: ', paste(dim(data), collapse=',')  ))

  result <- list(data=data,
                 covariateRef=plpData.mapped$covariateRef,
                 map=plpData.mapped$map)
  return(result)

}

# restricts to pop and saves/creates mapping
MapCovariates <- function(covariates, covariateRef, population, map){

  # restrict to population for speed
  futile.logger::flog.trace('restricting to population for speed...')
  idx <- ffbase::ffmatch(x = covariates$rowId, table = ff::as.ff(population$rowId))
  idx <- ffbase::ffwhich(idx, !is.na(idx))
  covariates <- covariates[idx, ]

  futile.logger::flog.trace('Now converting covariateId...')
  oldIds <- as.double(ff::as.ram(covariateRef$covariateId))
  newIds <- 1:nrow(covariateRef)

  if(!is.null(map)){
    futile.logger::flog.trace('restricting to model variables...')
    futile.logger::flog.trace(paste0('oldIds: ',length(map[,'oldIds'])))
    futile.logger::flog.trace(paste0('newIds:', max(as.double(map[,'newIds']))))
    ind <- ffbase::ffmatch(x=covariateRef$covariateId, table=ff::as.ff(as.double(map[,'oldIds'])))
    ind <- ffbase::ffwhich(ind, !is.na(ind))
    covariateRef <- covariateRef[ind,]

    ind <- ffbase::ffmatch(x=covariates$covariateId, table=ff::as.ff(as.double(map[,'oldIds'])))
    ind <- ffbase::ffwhich(ind, !is.na(ind))
    covariates <- covariates[ind,]
  }
  if(is.null(map))
    map <- data.frame(oldIds=oldIds, newIds=newIds)

  return(list(covariates=covariates,
              covariateRef=covariateRef,
              map=map))
}


#' Convert the plpData in COO format into a sparse python matrix
#'
#' @description
#' Converts the standard plpData to a sparse matrix firectly into python
#'
#' @details
#' This function converts the covariate file from ffdf in COO format into a sparse matrix from
#' the package Matrix
#' @param plpData                       An object of type \code{plpData} with covariate in coo format - the patient level prediction
#'                                      data extracted from the CDM.
#' @param population                    The population to include in the matrix
#' @param map                           A covariate map (telling us the column number for covariates)
#' @param temporal                      Whether to include timeId into tensor
#' @param pythonExePath                 Location of python exe you want to use
#' @examples
#' #TODO
#'
#' @return
#' Returns a list, containing the python object name of the sparse matrix, the plpData covariateRef
#' and a data.frame named map that tells us what covariate corresponds to each column
#' This object is a list with the following components: \describe{
#' \item{data}{The python object name containing a sparse matrix with the rows corresponding to each person in the plpData and the columns corresponding to the covariates.}
#' \item{covariateRef}{The plpData covariateRef.}
#' \item{map}{A data.frame containing the data column ids and the corresponding covariateId from covariateRef.}
#' }
#'
#' @export
toSparsePython <- function(plpData,population, map=NULL, temporal=F, pythonExePath=NULL){
  # test python is available and the required dependancies are there:
  if ( !PythonInR::pyIsConnected() ){
    python.test <- PythonInR::autodetectPython(pythonExePath = pythonExePath)

    if(is.null(python.test$pythonExePath))
      stop('You need to install python for this method - please see ...')
  }
  if ( !PythonInR::pyIsConnected() ){
    PythonInR::pyConnect(pythonExePath = pythonExePath)
    PythonInR::pyOptions("numpyAlias", "np")
    PythonInR::pyOptions("useNumpy", TRUE)
    PythonInR::pyImport("numpy", as='np')
  }
  
  if(temporal){
    PythonInR::pyExec("import tensorflow as tf")
  }

  # return error if we can't connect to python
  if ( !PythonInR::pyIsConnected() )
    stop('Python not connect error')

  PythonInR::pyExec('import numpy as np')
  PythonInR::pyOptions("useNumpy", TRUE)

  cov <- plpData$covariates #ff::clone(plpData$covariates)
  covref <- plpData$covariateRef #ff::clone(plpData$covariateRef)

  plpData.mapped <- MapCovariates(covariates=cov, covariateRef=ff::clone(covref),
                                  population, map=map)

  for (i in bit::chunk(plpData.mapped$covariateRef$covariateId)) {
    ids <- plpData.mapped$covariateRef$covariateId[i[1]:i[2]]
    ids <- plyr::mapvalues(ids, as.double(plpData.mapped$map$oldIds), as.double(plpData.mapped$map$newIds), warn_missing = FALSE)
    plpData.mapped$covariateRef$covariateId[i[1]:i[2]] <- ids
    # tested and working
  }
  for (i in bit::chunk(plpData.mapped$covariates$covariateId)) {
    ids <- plpData.mapped$covariates$covariateId[i[1]:i[2]]
    ids <- plyr::mapvalues(ids, as.double(plpData.mapped$map$oldIds), as.double(plpData.mapped$map$newIds), warn_missing = FALSE)
    plpData.mapped$covariates$covariateId[i[1]:i[2]] <- ids
  }
  futile.logger::flog.debug(paste0('Converting data into python sparse matrix...'))

  #convert into sparseM
  futile.logger::flog.debug(paste0('# cols: ', as.double(max(plpData.mapped$map$newIds)))) #nrow(plpData.mapped$covariateRef)))
  futile.logger::flog.debug(paste0('Max rowId: ', ffbase::max.ff(plpData.mapped$covariates$rowId)))

  if(temporal){
    futile.logger::flog.debug(paste0('Max timeId: ', ffbase::max.ff(plpData.mapped$covariates$timeId)))
  }
  
  # chunk then add

  # now load each part of the coo data into python as 3 vectors
  # containing row, column and value
  # create the sparse python matrix and then add to it
  PythonInR::pySet('xmax',as.integer(max(population$rowId)))
  PythonInR::pySet('ymax',as.integer(max(plpData.mapped$map$newIds)))
  if(temporal){
    PythonInR::pySet('tmax',as.integer(max(plpData.mapped$covariates$timeId)))
  }
  
  if(!temporal){
  PythonInR::pyExec('from scipy.sparse import coo_matrix')
  PythonInR::pyExec("plpData = coo_matrix((np.array([0]), (np.array([0]), np.array([0]) )), shape=(xmax, ymax))")
  ##for (ind in bit::chunk(plpData.mapped$covariates$covariateId)) {
  for (ind in bit::chunk(plpData.mapped$covariates)) {
    futile.logger::flog.debug(paste0('start:', ind[1],'- end:',ind[2]))
    # then load in the three vectors based on ram limits and add to the current matrix
    ## old slower code =====
    ##PythonInR::pySet('data', as.matrix(ff::as.ram(plpData.mapped$covariates$covariateValue[ind])))
    ##PythonInR::pySet('x', as.matrix(ff::as.ram(plpData.mapped$covariates$rowId[ind])-1))
    ##PythonInR::pySet('y', as.matrix(ff::as.ram(plpData.mapped$covariates$covariateId[ind])-1))
    ##PythonInR::pyExec("tempData = coo_matrix((data[:,0], (x[:,0], y[:,0])), shape=(xmax, ymax))")
    ##======
    PythonInR::pySet('dataall', as.matrix(ff::as.ram(plpData.mapped$covariates[ind,c('rowId','covariateId','covariateValue')])))
    PythonInR::pyExec("tempData = coo_matrix((dataall[:,2], (dataall[:,0]-1, dataall[:,1]-1)), shape=(xmax, ymax))")
    PythonInR::pyExec("plpData = plpData+tempData")
  }
  futile.logger::flog.debug(paste0('Sparse python matrix done '))
  futile.logger::flog.debug(PythonInR::pyExec("print  'dataset has %s rows and %s columns' %(plpData.shape[0],plpData.shape[1])")
  )
  } else{
    # do the sparse tensor in tensorflow
    # initiate empty sparsetensor:
    PythonInR::pyExec("plpData = tf.SparseTensor(indices=[[0,0,0]], 
                  values=np.float64([0]), dense_shape=[xmax, ymax, tmax])")
    
    for (ind in bit::chunk(plpData.mapped$covariates)) {
      futile.logger::flog.debug(paste0('start:', ind[1],'- end:',ind[2]))
      # subtract 1 as python index starts at 0:
      PythonInR::pySet('datas', as.matrix(ff::as.ram(plpData.mapped$covariates[ind,c('rowId','covariateId','timeId', 'covariateValue')])))
      PythonInR::pyExec("indexes= tf.convert_to_tensor(datas[:,0:3]-1, dtype=tf.int64)")
      PythonInR::pyExec("tempData = tf.SparseTensor(indices=indexes, 
                  values=datas[:,3], dense_shape=[xmax, ymax, tmax])")
      # add tensors:
      PythonInR::pyExec("plpData = tf.sparse_add(plpData, tempData)")
    }
   
    futile.logger::flog.debug(paste0('Sparse python tensor converted'))                            
  }
  result <- list(data='plpData',
                 covariateRef=plpData.mapped$covariateRef,
                 map=plpData.mapped$map)
  return(result)

}



# reformat the evaluation
reformatPerformance <- function(train, test, analysisId){

  nr1 <- length(unlist(train$evaluationStatistics[-1]))
  nr2 <- length(unlist(test$evaluationStatistics[-1]))
  evaluationStatistics <- cbind(analysisId=rep(analysisId,nr1+nr2),
                                Eval=c(rep('train', nr1),rep('test', nr2)),
                                Metric = names(c(unlist(train$evaluationStatistics[-1]),
                                                 unlist(test$evaluationStatistics[-1]))),
                                Value = c(unlist(train$evaluationStatistics[-1]),
                                      unlist(test$evaluationStatistics[-1]))
                                )

  nr1 <- nrow(train$thresholdSummary)
  nr2 <- nrow(test$thresholdSummary)
  thresholdSummary <- rbind(cbind(analysisId=rep(analysisId,nr1),Eval=rep('train', nr1),
                                      train$thresholdSummary),
                                cbind(analysisId=rep(analysisId,nr2),Eval=rep('test', nr2),
                                      test$thresholdSummary))

  if(!is.null(train$demographicSummary)){
    nr1 <- nrow(train$demographicSummary)
    nr2 <- nrow(test$demographicSummary)
    demographicSummary <- rbind(cbind(analysisId=rep(analysisId,nr1),Eval=rep('train', nr1),
                                      train$demographicSummary),
                                cbind(analysisId=rep(analysisId,nr2),Eval=rep('test', nr2),
                                      test$demographicSummary))
  } else{
    demographicSummary <- NULL
  }

  nr1 <- nrow(train$calibrationSummary)
  nr2 <- nrow(test$calibrationSummary)
  calibrationSummary <- rbind(cbind(analysisId=rep(analysisId,nr1),Eval=rep('train', nr1),
                                    train$calibrationSummary),
                              cbind(analysisId=rep(analysisId,nr2),Eval=rep('test', nr2),
                                    test$calibrationSummary))

  nr1 <- nrow(train$predictionDistribution)
  nr2 <- nrow(test$predictionDistribution)
  predictionDistribution <- rbind(cbind(analysisId=rep(analysisId,nr1),Eval=rep('train', nr1),
                                    train$predictionDistribution),
                              cbind(analysisId=rep(analysisId,nr2),Eval=rep('test', nr2),
                                    test$predictionDistribution))


  result <- list(evaluationStatistics=evaluationStatistics,
                 thresholdSummary=thresholdSummary,
                 demographicSummary =demographicSummary,
                 calibrationSummary=calibrationSummary,
                 predictionDistribution=predictionDistribution)

  return(result)
}

