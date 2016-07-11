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
from sklearn.ensemble import RandomForestClassifier
from sklearn.naive_bayes import GaussianNB
from scipy.sparse import coo_matrix,csr_matrix,vstack,hstack
from sklearn.feature_selection import SelectFromModel
#from sklearn.cross_validation import PredefinedSplit
from sklearn.externals.joblib import Memory
from sklearn.datasets import load_svmlight_file
from sklearn.externals import joblib


#================================================================


print "Applying Python Model" 

###########################################################################	

print "Loading Data..."
# load data + train,test indexes + validation index
mem = Memory("./mycache")

@mem.cache
def get_data():
    data = load_svmlight_file(data_loc+"\covariate.txt")
    return data[0], data[1]

X, y = get_data()

# load index file
pop = np.loadtxt(data_loc+'\population.txt', delimiter=' ')
print "population loaded- %s rows and %s columns" %(np.shape(pop)[0], np.shape(pop)[1])
#dataRows = np.loadtxt(data_loc+'\dataRows.txt', delimiter=' ')
X = X[dataRows[:,0]>0,:]
print "Dataset has %s rows and %s columns" %(X.shape[0], X.shape[1])

###########################################################################	

# remap column in new data to model columns:
# load mapping/missing and format:
##mapping = np.loadtxt(data_loc+'\mapping.txt', delimiter=' ')
old = mapping[:,1] 
new = mapping[:,0]
 
# add column of zeros for each missing column
# how to test missing exists?
print "Checking model feature "
vc1 = old[(old>=0) & (new>=0)]
vc2 = old[(new<0) & (old>=0)]

miss_col = np.zeros((X.shape[0], vc2.shape[0]))
print "Found %s features in new data but missing %s features" %(vc1.shape[0], vc2.shape[0])
if vc2.shape[0]>0:
  X = np.append((X[:,(old>=0) & (new>=0)],miss_col), axis=1)
if vc2.shape[0]==0:
  X = X[:,(old>=0) & (new>=0)]
# reorder:
ind = np.concatenate((vc1, vc2), axis=0) 
X = X[:,np.argsort(ind)]
print "Data ready for model has %s features" %(np.shape(X)[1])


# uf dense convert 
if dense==1:
  print "converting to dense data..."
  X=X.toarray()

###########################################################################	

# load model
print "Loading model..."
rf = joblib.load(model_loc+'\\model.pkl') 

print "Calculating predictions on population..."
test_pred = rf.predict_proba(X)[:,1]
print "Prediction complete: %s rows" %(np.shape(test_pred)[0])
print "Mean: %s prediction value" %(np.mean(test_pred))

# merge pred with population
test_pred.shape = (pop.shape[0], 1)
prediction = np.append(pop,test_pred, axis=1)
