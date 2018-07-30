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
#from sklearn.ensemble import RandomForestClassifier
#from sklearn.naive_bayes import GaussianNB
from scipy.sparse import coo_matrix,csr_matrix,vstack,hstack
#from sklearn.feature_selection import SelectFromModel
#from sklearn.cross_validation import PredefinedSplit
from sklearn.externals.joblib import Memory
#from sklearn.datasets import load_svmlight_file
from sklearn.externals import joblib


#================================================================


print("Applying Python Model" )

###########################################################################	

print("Loading Data...")
# load data + train,test indexes + validation index

y=population[:,1]
X = plpData[population[:,0],:]
X = X[:,included.flatten()]

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
modelTrained = joblib.load(os.path.join(model_loc,"model.pkl")) 

print("Calculating predictions on population...")
test_pred = modelTrained.predict_proba(X)[:,1]
print("Prediction complete: %s rows" %(np.shape(test_pred)[0]))
print("Mean: %s prediction value" %(np.mean(test_pred)))

# merge pred with population
test_pred.shape = (population.shape[0], 1)
prediction = np.append(population,test_pred, axis=1)
