#  DecisionTree classification with n fold cross validation
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
from sklearn import tree 
from sklearn.tree import DecisionTreeClassifier
from scipy.sparse import coo_matrix,csr_matrix,vstack,hstack
#from sklearn.feature_selection import SelectFromModel
from joblib import Memory
#from sklearn.datasets import load_svmlight_file
import joblib

#================================================================
def train_decision_tree(population, train, plpData, plot, max_depth, min_samples_split, min_samples_leaf, min_impurity_decrease, class_weight, seed, quiet, varNames, modelOutput):
  if plot:
    import pydotplus  # this is for plotting the tree (make optional)
  if quiet==False:
    print("Training Decision Tree model " )
  y = population[:,1]
  X = plpData[population[:,0].astype(int),:]
  trainInds =population[:,population.shape[1]-1] >0
  if quiet==False:
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
      if quiet==False:
        print("Fold %s split %s in train set and %s in test set" %(i, train_x.shape[0], test_x.shape[0]))
        print("Train set contains %s outcomes " %(np.sum(train_y)))
        # train on fold
        print("Training fold %s" %(i))
      start_time = timeit.default_timer()	
      if class_weight=='None':
        dt = DecisionTreeClassifier(criterion='gini', splitter='best', max_depth=max_depth, min_samples_split=min_samples_split, min_samples_leaf=min_samples_leaf, min_weight_fraction_leaf=0.0, max_features=None, random_state=seed, max_leaf_nodes=None, min_impurity_decrease=min_impurity_decrease, class_weight=None)
      else:
        dt = DecisionTreeClassifier(criterion='gini', splitter='best', max_depth=max_depth, min_samples_split=min_samples_split, min_samples_leaf=min_samples_leaf, min_weight_fraction_leaf=0.0, max_features=None, random_state=seed, max_leaf_nodes=None, min_impurity_decrease=min_impurity_decrease, class_weight=class_weight)
      dt = dt.fit(train_x, train_y)
      end_time = timeit.default_timer()
      if quiet==False:
        print("Training fold took: %.2f s" %(end_time-start_time))
        print("Calculating predictions on left out fold set...")
      ind = (population[:,population.shape[1]-1] > 0)
      ind = population[ind,population.shape[1]-1]==i
      test_pred[ind] = dt.predict_proba(test_x)[:,1]
      if quiet==False:
        print("Prediction complete: %s rows " %(np.shape(test_pred[ind])[0]))
        print("Mean: %s prediction value" %(np.mean(test_pred[ind])))
    # merge pred with indexes[testInd,:]
    test_pred.shape = (population[population[:,population.shape[1]-1] > 0,:].shape[0], 1)
    prediction = np.append(population[population[:,population.shape[1]-1] > 0,:],test_pred, axis=1)
    return prediction;
  # train final:
  else:
    if quiet==False:
      print("Training final decision tree model on all train data...")
      print("X- %s rows and Y %s length" %(X[trainInds,:].shape[0], y[trainInds].shape[0]))
    start_time = timeit.default_timer()	
    if class_weight=='None':
      dt = DecisionTreeClassifier(criterion='gini', splitter='best', max_depth=max_depth, min_samples_split=min_samples_split, min_samples_leaf=min_samples_leaf, min_weight_fraction_leaf=0.0, max_features=None, random_state=seed, max_leaf_nodes=None, min_impurity_decrease=min_impurity_decrease, class_weight=None)
    else:
      dt = DecisionTreeClassifier(criterion='gini', splitter='best', max_depth=max_depth, min_samples_split=min_samples_split, min_samples_leaf=min_samples_leaf, min_weight_fraction_leaf=0.0, max_features=None, random_state=seed, max_leaf_nodes=None, min_impurity_decrease=min_impurity_decrease, class_weight=class_weight)
    dt = dt.fit(X[trainInds,:], y[trainInds])
    end_time = timeit.default_timer()
    if quiet==False:
      print("Training final took: %.2f s" %(end_time-start_time))
    # save the model:
    if not os.path.exists(modelOutput):
      os.makedirs(modelOutput)
    if quiet==False:
      print("Model saved to: %s" %(modelOutput)	)
    joblib.dump(dt, os.path.join(modelOutput,"model.pkl"), compress = True)
    test_pred = dt.predict_proba(X[trainInds,:])[:,1]
    test_pred.shape = (population[trainInds,:].shape[0], 1)
    prediction = np.append(population[trainInds,:],test_pred, axis=1)
    if plot:
      plotfile = modelOutput+"\\tree_plot.dot"
      tree.export_graphviz(dt, out_file=plotfile, feature_names=varnames[:,0])#X.dtype.names ) #variables[:,0])
      #graph = pydotplus.graph_from_dot_file(plotfile) 
      #plotfile = modelOutput+"\\tree_plot.pdf"
      #graph.write_pdf(plotfile ) 
    # manually create the pdf 
    ##commandLine = "dot -Tpdf "+inLoc +"-o"+ outLoc
    ##os.system(commandLine)
    return prediction, dt.feature_importances_;
