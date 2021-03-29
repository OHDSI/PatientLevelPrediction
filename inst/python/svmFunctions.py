#  SVM classification with n fold cross validation
#===============================================================
# INPUT:
# 1) location of files: libsvm file + indexes file (rowId, index)
# 2) 
#
# OUTPUT:
# it returns a file with indexes merged with prediction for test index  
#================================================================
import numpy as np
import os
import sys
import timeit
import math
from sklearn.svm import SVC
from sklearn.inspection import permutation_importance
from scipy.sparse import coo_matrix,csr_matrix,vstack,hstack
from joblib import Memory
import joblib

#================================================================
def train_svm(population, plpData, train, kernel, C, degree, gamma , shrinking, coef0, classWeight, modelOutput, seed, varImp, quiet):
  print("Training SVM model " )
  y = population[:,1]
  X = plpData[population[:,0].astype(int),:]
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
      train_y = y[trainInds][trainInd]
      test_x = X[trainInds,:][testInd,:]	
      print("Fold %s split %s in train set and %s in test set" %(i, train_x.shape[0], test_x.shape[0]))
      print("Train set contains %s outcomes " %(np.sum(train_y)))
      print("Training fold %s" %(i))
      start_time = timeit.default_timer()	
      if classWeight == 'balanced':
        adab = SVC(kernel = kernel, C = C, degree = degree, gamma =gamma, shrinking = shrinking, coef0 = coef0, probability=True, tol=0.0001, class_weight = classWeight)
      else:
        adab = SVC(kernel = kernel, C = C, degree = degree, gamma =gamma, shrinking = shrinking, coef0 = coef0, probability=True, tol=0.0001, class_weight = None)
      adab = adab.fit(X=train_x, y=train_y)
      end_time = timeit.default_timer()
      print("Training fold took: %.2f s" %(end_time-start_time))
      print("Calculating predictions on left out fold set...")
      ind = (population[:,population.shape[1]-1] > 0)
      ind = population[ind,population.shape[1]-1]==i
      test_pred[ind] = adab.predict_proba(test_x)[:,1]
      print("Prediction complete: %s rows " %(np.shape(test_pred[ind])[0]))
      print("Mean: %s prediction value" %(np.mean(test_pred[ind])))
    # merge pred with indexes[testInd,:]
    test_pred.shape = (population[population[:,population.shape[1]-1] > 0,:].shape[0], 1)
    prediction = np.append(population[population[:,population.shape[1]-1] > 0,:],test_pred, axis=1)
    return prediction;
  # train final:
  else:
    print("Training final SVM model on all train data...")
    print("X- %s rows and Y %s length" %(X[trainInds,:].shape[0], y[trainInds].shape[0]))
    start_time = timeit.default_timer()	
    if classWeight == 'balanced':
      adab = SVC(kernel = kernel, C = C, degree = degree, gamma =gamma, shrinking = shrinking, coef0 = coef0, probability=True, tol=0.0001, class_weight = classWeight)
    else:
      adab = SVC(kernel = kernel, C = C, degree = degree, gamma =gamma, shrinking = shrinking, coef0 = coef0, probability=True, tol=0.0001, class_weight = None)
    adab = adab.fit(X = X[trainInds,:], y = y[trainInds])
    end_time = timeit.default_timer()
    print("Training final took: %.2f s" %(end_time-start_time))
    # save the model:
    if not os.path.exists(modelOutput):
      os.makedirs(modelOutput)
    print("Model saved to: %s" %(modelOutput)	)
    joblib.dump(adab, os.path.join(modelOutput,"model.pkl"), compress = True) 
    pred = adab.predict_proba(X[trainInds,:])[:,1]
    pred.shape = (population[population[:,population.shape[1]-1] > 0,:].shape[0], 1)
    prediction = np.append(population[population[:,population.shape[1]-1] > 0,:],pred, axis=1)
    if varImp:
      maxSize = 10000
      if X.shape[0] < 10000:
        maxSize = X.shape[0]
      viInd = np.random.choice(X.shape[0], maxSize, replace=False)
      varImp = permutation_importance(adab, X[viInd,:].toarray(), y[viInd], n_repeats=1, random_state=0)
      return prediction, varImp.importances_mean;
    else:
      return prediction  
