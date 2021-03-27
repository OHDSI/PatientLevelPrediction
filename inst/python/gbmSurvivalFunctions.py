#  Gradient boosting survial model with n fold cross validation
#===============================================================
# uses scikit-survival python library 
# to install python package: conda install -c sebp scikit-survival
#================================================================
import numpy as np
import os
import sys
import timeit
import math
from sksurv.ensemble import GradientBoostingSurvivalAnalysis
from scipy.sparse import coo_matrix,csr_matrix,vstack,hstack
from joblib import Memory
import joblib

#================================================================
def train_gbmsurv(population = None, plpData= None, train = True, modelOutput =None, loss='coxph', learning_rate=0.1, n_estimators=100, criterion='friedman_mse', min_samples_split=2, min_samples_leaf=1, min_weight_fraction_leaf=0.0, max_depth=3, min_impurity_split=None, min_impurity_decrease=0.0, max_features=None, max_leaf_nodes=None, subsample=1.0, dropout_rate=0.0, verbose=0, seed = 1, quiet = True):
  print("Training python scikit-survial GradientBoostingSurvivalAnalysis model" )
  ytype=np.dtype([('outcome', '?'), ('surv', 'i')])
  y=np.empty(len(population[:,1]),dtype=ytype)
  y['outcome']= population[:,1]>0
  y['surv']= population[:,2]
  X = plpData[population[:,0],:]
  trainInds =population[:,population.shape[1]-1] >0
  print("Dataset has %s rows and %s columns" %(X.shape[0], X.shape[1]))
  print("population loaded- %s rows and %s columns" %(np.shape(population)[0], np.shape(population)[1]))
  ###########################################################################
  if train:
    pred_size = int(np.sum(population[:,population.shape[1]-1] > 0))
    print("Calculating prediction for train set of size %s" %(pred_size))
    test_pred = np.zeros(pred_size)# zeros length sum(population[:,population.size[1]] ==i)
    for i in range(1, int(np.max(population[:,population.shape[1]-1])+1), 1):
      testInd =population[population[:,population.shape[1]-1] > 0,population.shape[1]-1] ==i
      trainInd = (population[population[:,population.shape[1]-1] > 0,population.shape[1]-1] !=i)
      train_x = X[trainInds,:][trainInd,:]
      train_y = y[trainInds,][trainInd,]
      test_x = X[trainInds,:][testInd,:]	
      print("Fold %s split %s in train set and %s in test set" %(i, train_x.shape[0], test_x.shape[0]))
      print("Train set contains %s outcomes " %(np.sum(train_y['outcome'])))
      print("Training fold %s" %(i))
      start_time = timeit.default_timer()	
      gbmsurv = GradientBoostingSurvivalAnalysis(loss=loss, learning_rate=learning_rate, n_estimators=n_estimators, criterion=criterion, min_samples_split=min_samples_split, min_samples_leaf=min_samples_leaf, min_weight_fraction_leaf=min_weight_fraction_leaf, max_depth=max_depth, min_impurity_split=min_impurity_split, min_impurity_decrease=min_impurity_decrease, random_state=seed, max_features=max_features, max_leaf_nodes=max_leaf_nodes, subsample=subsample, dropout_rate=dropout_rate, verbose=verbose)
      gbmsurv = gbmsurv.fit(X=train_x, y=train_y)
      end_time = timeit.default_timer()
      print("Training fold took: %.2f s" %(end_time-start_time))
      print("Calculating predictions on left out fold set...")
      ind = (population[:,population.shape[1]-1] > 0)
      ind = population[ind,population.shape[1]-1]==i
      rowCount = np.sum(ind)
      temp_pred = gbmsurv.predict(test_x.toarray())
      temp_pred = temp_pred.flatten()
      temp_pred = temp_pred[0:(rowCount)]
      test_pred[ind] = temp_pred
      print("Prediction complete: %s rows " %(np.shape(test_pred[ind])[0]))
      print("Mean: %s prediction value" %(np.mean(test_pred[ind])))
    # merge pred with indexes[testInd,:]
    test_pred.shape = (population[population[:,population.shape[1]-1] > 0,:].shape[0], 1)
    prediction = np.append(population[population[:,population.shape[1]-1] > 0,:],test_pred, axis=1)
    return prediction;
  # train final:
  else:
    print("Training final python scikit-survial GradientBoostingSurvivalAnalysis model on all train data...")
    print("X- %s rows and Y %s length" %(X[trainInds,:].shape[0], y[trainInds].shape[0]))
    start_time = timeit.default_timer()	
    gbmsurv = GradientBoostingSurvivalAnalysis(loss=loss, learning_rate=learning_rate, n_estimators=n_estimators, criterion=criterion, min_samples_split=min_samples_split, min_samples_leaf=min_samples_leaf, min_weight_fraction_leaf=min_weight_fraction_leaf, max_depth=max_depth, min_impurity_split=min_impurity_split, min_impurity_decrease=min_impurity_decrease, random_state=seed, max_features=max_features, max_leaf_nodes=max_leaf_nodes, subsample=subsample, dropout_rate=dropout_rate, verbose=verbose)
    gbmsurv = gbmsurv.fit(X[trainInds,:], y[trainInds])
    end_time = timeit.default_timer()
    print("Training final took: %.2f s" %(end_time-start_time))
    # save the model:
    if not os.path.exists(modelOutput):
      os.makedirs(modelOutput)
    print("Model saved to: %s" %(modelOutput)	)
    joblib.dump(gbmsurv, os.path.join(modelOutput,"model.pkl"), compress = True) 
    pred = gbmsurv.predict(X[trainInds,:].toarray())
    pred = pred.flatten()
    rowCount = np.sum(trainInds)
    pred = pred[0:(rowCount)]
    pred.shape = (population[population[:,population.shape[1]-1] > 0,:].shape[0], 1)
    prediction = np.append(population[population[:,population.shape[1]-1] > 0,:],pred, axis=1)
    return prediction, gbmsurv.feature_importances_;
