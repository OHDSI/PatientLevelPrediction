#  random forest classification variable importance
#===============================================================
# INPUT:
# 1) location of files: libsvm file + indexes file (rowId, index)
#
# OUTPUT:
# it returns a file with selected variable index  
#================================================================
import numpy as np
import os
import sys
import timeit
import math
from sklearn.ensemble import RandomForestClassifier
from scipy.sparse import coo_matrix,csr_matrix,vstack,hstack
from sklearn.feature_selection import SelectFromModel
from sklearn.externals.joblib import Memory
from sklearn.datasets import load_svmlight_file


#================================================================
ntrees = 2000
max_depth = 17

print "Using Random Forest to select features" 

##print "Loading Data..."
# load data + train,test indexes + validation index
##mem = Memory("./mycache")
##@mem.cache

##def get_data():
##    data = load_svmlight_file(dataLocation+"\covariate.txt")
##    return data[0], data[1]

##X, y = get_data()

# load index file
y = population[:,1]
X = plpData[population[:,0],:]
print "population loaded- %s rows and %s columns" %(np.shape(population)[0], np.shape(population)[1])

trainInd =population[:,population.shape[1]-1] >0
testInd = population[:,population.shape[1]-1] < 0

###########################################################################
train_x = X[trainInd,:]
train_y = y[trainInd]
print "Train set contains %s outcomes " %(np.sum(train_y))

mtry = int(np.round(np.sqrt(train_x.shape[1])))
print "Training random forest with mtry= %s" %(mtry)
# feature selection
print "Applying variable importance feature selection..."
rf = RandomForestClassifier(max_features=mtry, n_estimators=ntrees,max_depth=max_depth,min_samples_split=2, random_state=0, n_jobs=-1, bootstrap=False)
rf = rf.fit(train_x, train_y)
feat_sel = SelectFromModel(rf,threshold='mean', prefit=True)
train_x = feat_sel.transform(train_x)
print "Selected %s number of features" %(train_x.shape[1])
	


