#  apply random forest model on new data
#===============================================================
# INPUT:
# 1) location of new data
# 2) location of model
#
# OUTPUT:
# it returns a file with indexes merged with prediction for test index  - named new_pred
#================================================================
import numpy as np
from collections import OrderedDict
import os
import sys
import timeit
import math
from scipy.sparse import coo_matrix,csr_matrix,vstack,hstack
from joblib import Memory
import joblib

def batch(tensor, batch_size = 50):
    """ It is used to create batch samples, each batch has batch_size samples"""
    tensor_list = []
    length = tensor.shape[0]
    i = 0
    while True:
        if (i+1) * batch_size >= length:
            tensor_list.append(tensor[i * batch_size: length])
            return tensor_list
        tensor_list.append(tensor[i * batch_size: (i+1) * batch_size])
        i += 1
#================================================================
###########################################################################
def python_predict_temporal(population, plpData, model_loc, dense, autoencoder):
  print("Applying Python Model") 
  print("Loading Data...")
  # load data + train,test indexes + validation index
  #y=population[:,1]
  
  ###########################################################################	
  # uf dense convert 
  if dense==1:
    print("converting to dense data...")
    X = plpData.to_dense().numpy()
  if dense==0:
    print("keeping data sparse...")
    X = plpData.numpy()
  ###########################################################################	
  
  # order the data
  X = X[np.int64(population[:, 0]), :]
  # load index file
  print("population loaded- %s rows and %s columns" %(np.shape(population)[0], np.shape(population)[1]))
  print("Dataset has %s rows and %s columns" %(X.shape[0], X.shape[1]))
  print("Data ready for model has %s features" %(np.shape(X)[1]))

  # load model
  print("Loading model...")
  if autoencoder:
    autoencoder_model = joblib.load(os.path.join(model_loc, 'autoencoder_model.pkl'))
    X = autoencoder_model.get_encode_features(X)
  modelTrained = joblib.load(os.path.join(model_loc,"model.pkl")) 
  print("Calculating predictions on population...")
  test_batch = batch(X, batch_size = 32)
  test_pred = []
  for test in test_batch:
    pred_test1 = modelTrained.predict_proba(test)[:, 1]
    test_pred = np.concatenate((test_pred , pred_test1), axis = 0)
  print("Prediction complete: %s rows" %(np.shape(test_pred)[0]))
  print("Mean: %s prediction value" %(np.mean(test_pred)))
  # merge pred with population
  test_pred.shape = (population.shape[0], 1)
  prediction = np.append(population,test_pred, axis=1)
  return prediction
  
def python_predict(population, plpData, model_loc, dense, autoencoder):
  print("Applying Python Model") 
  print("Loading Data...")
  # load data + train,test indexes + validation index
  #y=population[:,1]
  X = plpData[population[:,0].astype(int),:]
  # load index file
  print("population loaded- %s rows and %s columns" %(np.shape(population)[0], np.shape(population)[1]))
  print("Dataset has %s rows and %s columns" %(X.shape[0], X.shape[1]))
  print("Data ready for model has %s features" %(np.shape(X)[1]))
  ###########################################################################	
  # uf dense convert 
  if dense==1:
    print("converting to dense data...")
    X=X.toarray()
  ###########################################################################	
  # load model
  print("Loading model...")
  if autoencoder:
    autoencoder_model = joblib.load(os.path.join(model_loc, 'autoencoder_model.pkl'))
    X = autoencoder_model.get_encode_features(X)
  modelTrained = joblib.load(os.path.join(model_loc,"model.pkl")) 
  print("Calculating predictions on population...")
  test_pred = modelTrained.predict_proba(X)[:, 1]
  print("Prediction complete: %s rows" %(np.shape(test_pred)[0]))
  print("Mean: %s prediction value" %(np.mean(test_pred)))
  # merge pred with population
  test_pred.shape = (population.shape[0], 1)
  prediction = np.append(population,test_pred, axis=1)
  return prediction


def python_predict_survival(population, plpData, model_loc):
  print("Applying Python Model") 
  print("Loading Data...")
  # load data + train,test indexes + validation index
  X = plpData[population[:,0].astype(int),:]
  # load index file
  print("population loaded- %s rows and %s columns" %(np.shape(population)[0], np.shape(population)[1]))
  print("Dataset has %s rows and %s columns" %(X.shape[0], X.shape[1]))
  print("Data ready for model has %s features" %(np.shape(X)[1]))
  ###########################################################################	
  # load model
  print("Loading model...")
  modelTrained = joblib.load(os.path.join(model_loc,"model.pkl")) 
  print("Calculating predictions on population...")
  test_pred = modelTrained.predict(X.toarray())
  test_pred = test_pred.flatten()
  rowCount = population.shape[0]
  test_pred = test_pred[0:(rowCount)]
  print("Prediction complete: %s rows" %(np.shape(test_pred)[0]))
  print("Mean: %s prediction value" %(np.mean(test_pred)))
  # merge pred with population
  test_pred.shape = (population.shape[0], 1)
  prediction = np.append(population,test_pred, axis=1)
  return prediction
  
def python_predict_garden(population, plpData, model_loc,quantile=None):
  print("Applying Python Model") 
  print("Loading Data...")
  # load data + train,test indexes + validation index
  #y=population[:,1]
  X = plpData[population[:,0].astype(int),:]
  # load index file
  print("population loaded- %s rows and %s columns" %(np.shape(population)[0], np.shape(population)[1]))
  print("Dataset has %s rows and %s columns" %(X.shape[0], X.shape[1]))
  print("Data ready for model has %s features" %(np.shape(X)[1]))
  ###########################################################################	
  # load model
  print("Loading model...")
  modelTrained = joblib.load(os.path.join(model_loc,"model.pkl")) 
  print("Calculating predictions on population...")
  test_pred = modelTrained.predict(X,quantile=quantile)[:, 0]
  print("Prediction complete: %s rows" %(np.shape(test_pred)[0]))
  print("Mean: %s prediction value" %(np.mean(test_pred)))
  # merge pred with population
  test_pred.shape = (population.shape[0], 1)
  prediction = np.append(population,test_pred, axis=1)
  return prediction
