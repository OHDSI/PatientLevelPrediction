#  Neural network classification with n fold cross validation
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
from sklearn.neural_network import MLPClassifier
from scipy.sparse import coo_matrix,csr_matrix,vstack,hstack
#from sklearn.feature_selection import SelectFromModel
from sklearn.externals.joblib import Memory
from sklearn.datasets import load_svmlight_file
from sklearn.externals import joblib

#================================================================
print "Training Neural Network model " 

print "Loading Data..."
# load data + train,test indexes + validation index
##mem = Memory("./mycache")

##@mem.cache
def get_data():
    data = load_svmlight_file(dataLocation+"\covariate.txt")
    return data[0], data[1]

X, y = get_data()
# only get the population data (dataRows is a binary vector with 0 representing a non-training 
# data point and 1 representing a data point ot be used for training)
dataRows = np.loadtxt(dataLocation+'\dataRows.txt', delimiter=' ')
X = X[dataRows>0,:]

print "Dataset has %s rows and %s columns" %(X.shape[0], X.shape[1])

# load index file
population = np.loadtxt(dataLocation+'\population.txt', delimiter=' ')
y = population[:,1]
# reduce y to the popualtion with an index value >0 (to be used in training set)
y = y[population[:,population.shape[1]-1] > 0]
X = X[population[:,population.shape[1]-1] > 0,:]
print "population loaded- %s rows and %s columns" %(np.shape(population)[0], np.shape(population)[1])
###########################################################################


pred_size = int(np.sum(population[:,population.shape[1]-1] > 0))
print "Calculating prediction for train set of size %s" %(pred_size)
test_pred = np.zeros(pred_size)# zeros length sum(population[:,population.size[1]] ==i)
for i in range(1, int(np.max(population[:,population.shape[1]-1])+1), 1):
  testInd =population[population[:,population.shape[1]-1] > 0,population.shape[1]-1] ==i
  trainInd = (population[population[:,population.shape[1]-1] > 0,population.shape[1]-1] !=i)
  train_x = X[trainInd,:]
  train_y = y[trainInd]
  
  test_x = X[testInd,:]	
  print "Fold %s split %s in train set and %s in test set" %(i, train_x.shape[0], test_x.shape[0])
  print "Train set contains %s outcomes " %(np.sum(train_y))

  # train on fold
  print "Training fold %s" %(i)
  start_time = timeit.default_timer()	
  mlp = MLPClassifier(algorithm='l-bfgs', alpha=1e-5, hidden_layer_sizes=(5, 2), random_state=1)
  mlp = mlp.fit(train_x, train_y)
  end_time = timeit.default_timer()
  print "Training fold took: %.2f s" %(end_time-start_time)
  print "Calculating predictions on left out fold set..."
  ind = (population[:,population.shape[1]-1] > 0)
  ind = population[ind,population.shape[1]-1]==i
  test_pred[ind] = mlp.predict_proba(test_x)[:,1]
  print "Prediction complete: %s rows " %(np.shape(test_pred[ind])[0])
  print "Mean: %s prediction value" %(np.mean(test_pred[ind]))

	
# train final:
print "Training final neural network model on all train data..."
print "X- %s rows and Y %s columns" %(X.shape[0], y.shape[0])

start_time = timeit.default_timer()	
mlp = MLPClassifier(algorithm='l-bfgs', alpha=1e-5, hidden_layer_sizes=(5, 2), random_state=1)
mlp = mlp.fit(X, y)
end_time = timeit.default_timer()
print "Training final took: %.2f s" %(end_time-start_time)
	
# save the model:
if not os.path.exists(modelOutput):
    os.makedirs(modelOutput)
print "Model saved to: %s" %(modelOutput)	
	
joblib.dump(mlp, modelOutput+'\\model.pkl') 

# merge pred with indexes[testInd,:]
test_pred.shape = (population[population[:,population.shape[1]-1] > 0,:].shape[0], 1)
prediction = np.append(population[population[:,population.shape[1]-1] > 0,:],test_pred, axis=1)
