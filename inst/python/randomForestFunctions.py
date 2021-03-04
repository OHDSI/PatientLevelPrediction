#  random forest classification with n fold cross validation
#===============================================================
# INPUT:
# 1) location of files: libsvm file + indexes file (rowId, index)
# 2) ntree, max_depth, mtry, var_imp
#
# OUTPUT:
# it returns a file with indexes merged with prediction for test index  
#================================================================
import numpy as np
from collections import OrderedDict
import os
import sys
import timeit
import math
from sklearn.ensemble import RandomForestClassifier
from scipy.sparse import coo_matrix,csr_matrix,vstack,hstack
#from sklearn.feature_selection import SelectFromModel
#from sklearn.cross_validation import PredefinedSplit
#from sklearn.externals.joblib import Memory
#from sklearn.datasets import load_svmlight_file
import joblib
#================================================================
def train_rf(population, plpData, ntrees, max_depth, mtry, included, seed, quiet):
  if quiet==False: 
    print("Training Random Forest with: mtry: %s - max_depth: %s - ntrees: %s " %(mtry, max_depth, ntrees))
  y = population[:,1]
  X = plpData[population[:,0].astype(int),:]
  X = X[:,included.flatten().astype(int)]
  if (mtry==-1):
    mtry =int(np.round(np.sqrt(X.shape[1])))
  if quiet==False:
    print("population loaded- %s rows and %s columns" %(np.shape(population)[0], np.shape(population)[1]))
  ###########################################################################
  pred_size = int(np.sum(population[:,population.shape[1]-1] > 0))
  if quiet==False:
    print("Calculating prediction for train set of size %s" %(pred_size))
  test_pred = np.zeros(pred_size)# zeros length sum(population[:,population.size[1]] ==i)
  for i in range(1, int(np.max(population[:,population.shape[1]-1])+1), 1):
    testInd =population[:,population.shape[1]-1] ==i
    trainInd = (population[:,population.shape[1]-1] !=i) & (population[:,population.shape[1]-1] > 0)
    train_x = X[trainInd,:]
    train_y = y[trainInd]
    test_x = X[testInd,:]	
    
    if quiet==False:
      print("Fold %s split %s in train set and %s in test set" %(i, train_x.shape[0], test_x.shape[0]))
      print("Train set contains %s outcomes " %(np.sum(train_y)))
      # train on full test data using hyper parameters
      print("Training user specified random forest...")
    start_time = timeit.default_timer()	
    rf = RandomForestClassifier(max_features=int(mtry), n_estimators=ntrees,max_depth=max_depth,min_samples_split=5,  n_jobs=-1, bootstrap=False, random_state=seed)#, class_weight="balanced_subsample")
    #rf = RandomForestClassifier(max_features=mtry, n_estimators=500,max_depth=4,min_samples_split=5, random_state=0, n_jobs=-1, bootstrap=False)#, class_weight="balanced_subsample")
    rf = rf.fit(train_x, train_y)
    end_time = timeit.default_timer()
  
    if quiet==False:
      print("Training fold took: %.2f s" %(end_time-start_time))
      print("Calculating predictions on left out fold set...")
    ind = (population[:,population.shape[1]-1] > 0)
    ind = population[ind,population.shape[1]-1]==i
    test_pred[ind] = rf.predict_proba(test_x)[:,1]
  
    if quiet==False:
      print("Prediction complete: %s rows " %(np.shape(test_pred[ind])[0]))
      print("Mean: %s prediction value" %(np.mean(test_pred[ind])))
  
  # get the CV performance
  trainInd = population[:,population.shape[1]-1] > 0
  test_pred.shape = (population[trainInd,:].shape[0], 1)
  prediction = np.append(population[trainInd,:],test_pred, axis=1)
  
  return prediction;
  
  
  
def final_rf(population, plpData, ntrees, max_depth, mtry, included, modelOutput, seed, quiet):
  if quiet==False:
    print("Training final model with best investigated hyper-parameters..." )
  y = population[:,1]
  X = plpData[population[:,0].astype(int),:]
  X = X[:,included.flatten().astype(int)]
  trainInd =population[:,population.shape[1]-1] >0
  train_x = X[trainInd,:]
  train_y = y[trainInd]
  if (mtry==-1):
    mtry =int(np.round(np.sqrt(X.shape[1])))

  start_time = timeit.default_timer()	
  rf = RandomForestClassifier(max_features=int(mtry), n_estimators=ntrees,max_depth=max_depth,min_samples_split=5,  n_jobs=-1, bootstrap=False, random_state=seed)#, class_weight="balanced_subsample")
  rf = rf.fit(train_x, train_y)
  end_time = timeit.default_timer()
  
  if quiet==False:
    print("Training final took: %.2f s" %(end_time-start_time))
  # save the model:
  if not os.path.exists(modelOutput):
    os.makedirs(modelOutput)
  if quiet==False:  
    print("Model saved to: %s" %(modelOutput)	)
  
  joblib.dump(rf, os.path.join(modelOutput,"model.pkl"), compress = True)
  
  # merge pred with indexes[testInd,:]
  train_pred = rf.predict_proba(train_x)[:,1]
  train_pred.shape = (population[trainInd,:].shape[0], 1)
  prediction = np.append(population[trainInd,:],train_pred, axis=1)
  varImp = rf.feature_importances_
  return prediction, varImp;
  
def rf_var_imp(population, plpData, quiet='False'):
  print("Using Random Forest to select features") 
  
  ntrees = 2000
  max_depth = 17
  y = population[:,1]
  X = plpData[population[:,0].astype(int),:]
  print("population loaded- %s rows and %s columns" %(np.shape(population)[0], np.shape(population)[1]))
  
  trainInd =population[:,population.shape[1]-1] >0
  #testInd = population[:,population.shape[1]-1] < 0
  
  ###########################################################################
  train_x = X[trainInd,:]
  train_y = y[trainInd]
  
  if quiet==False:
    print("Train set contains %s outcomes " %(np.sum(train_y)))
  
  mtry = int(np.round(np.sqrt(train_x.shape[1])))
  if quiet==False:
    print("Training random forest with mtry= %s" %(mtry))
    # feature selection
    print("Applying variable importance feature selection...")
  rf = RandomForestClassifier(max_features=mtry, n_estimators=ntrees,max_depth=max_depth,min_samples_split=2, random_state=0, n_jobs=-1, bootstrap=False)
  rf = rf.fit(train_x, train_y)
  #feat_sel = SelectFromModel(rf,threshold='mean', prefit=True)
  #train_x = feat_sel.transform(train_x)
  
  if quiet==False:
    print("Selected %s number of features" %(train_x.shape[1]))
  return rf.feature_importances_;
