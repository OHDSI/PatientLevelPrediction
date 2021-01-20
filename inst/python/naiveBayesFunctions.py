#  Naive bayes classification with n fold cross validation
#===============================================================
# INPUT:
# 1) location of files: libsvm file + indexes file (rowId, index)
# 2) 
#
# OUTPUT:
# it returns a file with indexes merged with prediction for test index  
#================================================================
import numpy as np
#from collections import OrderedDict
import os
import sys
import timeit
import math
from sklearn.naive_bayes import GaussianNB #BernoulliNB
from scipy.sparse import coo_matrix,csr_matrix,vstack,hstack
#from sklearn.feature_selection import SelectFromModel#from sklearn.cross_validation import PredefinedSplit
from joblib import Memory
#from sklearn.datasets import load_svmlight_file
import joblib
from sklearn.feature_selection import SelectKBest
from sklearn.feature_selection import chi2

#================================================================
def train_naive_bayes(population, plpData, modelOutput, variableNumber, quiet):
  print("Training Naive Bayes model " )
  y = population[:,1]
  X = plpData[population[:,0].astype(int),:]
  print("population loaded- %s rows and %s columns" %(np.shape(population)[0], np.shape(population)[1]))
  print("Dataset has %s rows and %s columns" %(X.shape[0], X.shape[1]))
  ###########################################################################
  featnum = min(variableNumber,X.shape[1])
  print("Applying univariate feature selection to select %s features as naive bayes reqires non-sparse data " %(featnum))
  kbest = SelectKBest(chi2, k=featnum).fit(X[population[:,population.shape[1]-1] > 0,:], y[population[:,population.shape[1]-1] > 0])
  kbest.scores_ = np.nan_to_num(kbest.scores_)
  print("Test kbest length: %s non-zero: %s" %(kbest.scores_.shape[0], np.sum(kbest.scores_!=0)))
  threshold = -np.sort(-kbest.scores_)[featnum-1]
  print("Threshold varImp set at %s" %(threshold))
  X = X[population[:,population.shape[1]-1] > 0, :]
  print("Test X dim: %s , %s" %(X.shape[0], X.shape[1]) )
  X = X[:,kbest.scores_ >=threshold]
  print("Test X dim: %s , %s" %(X.shape[0], X.shape[1]) )
  X = X.toarray()
  y = y[population[:,population.shape[1]-1] > 0]
  pred_size = int(np.sum(population[:,population.shape[1]-1] > 0))
  print("Calculating prediction for train set of size %s" %(pred_size))
  test_pred = np.zeros(pred_size)# zeros length sum(population[:,population.size[1]] ==i)
  for i in range(1, int(np.max(population[:,population.shape[1]-1])+1), 1):
    testInd = population[population[:,population.shape[1]-1] > 0,population.shape[1]-1] ==i
    trainInd = (population[population[:,population.shape[1]-1] > 0,population.shape[1]-1] !=i)
    train_x = X[trainInd,:]
    train_y = y[trainInd]
    test_x = X[testInd,:]	
    print("Fold %s split %s in train set and %s in test set" %(i, train_x.shape[0], test_x.shape[0]))
    print("Train set contains %s outcomes " %(np.sum(train_y)))
    # train on full test data using hyper parameters
    print("Training fold %s" %(i))
    start_time = timeit.default_timer()	
    gnb = GaussianNB()
    gnb = gnb.fit(train_x, train_y)
    end_time = timeit.default_timer()
    print("Training fold took: %.2f s" %(end_time-start_time))
    print("Calculating predictions on left out fold set...")
    ind = (population[:,population.shape[1]-1] > 0)
    ind = population[ind,population.shape[1]-1]==i
    test_pred[ind] = gnb.predict_proba(test_x)[:,1]
    print("Prediction complete: %s rows " %(np.shape(test_pred[ind])[0]))
    print("Mean: %s prediction value" %(np.mean(test_pred[ind])))
  # cv pred
  test_pred.shape = (population[population[:,population.shape[1]-1] > 0,:].shape[0], 1)
  predictioncv = np.append(population[population[:,population.shape[1]-1] > 0,:],test_pred, axis=1)
	# train final:
  print("Training final naive bayes model on all train data...")
  start_time = timeit.default_timer()	
  gnb = GaussianNB()
  gnb = gnb.fit(X, y)
  end_time = timeit.default_timer()
  print("Training final took: %.2f s" %(end_time-start_time))
	# save the model:
  if not os.path.exists(modelOutput):
    os.makedirs(modelOutput)
  print("Model saved to: %s" %(modelOutput)	)
  joblib.dump(gnb, os.path.join(modelOutput,"model.pkl"), compress = True)  
  # merge pred with indexes[testInd,:]
  test_pred = gnb.predict_proba(X)[:,1]
  test_pred.shape = (population[population[:,population.shape[1]-1] > 0,:].shape[0], 1)
  prediction = np.append(population[population[:,population.shape[1]-1] > 0,:],test_pred, axis=1)
  return prediction, predictioncv, kbest.scores_;
