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
if "python_dir" in globals():
    sys.path.insert(0, python_dir)
    import TorchUtils as tu
#================================================================


print("Applying Python Model") 

###########################################################################
	
def get_temproal_data(covariates, population):
    p_ids_in_cov = set(covariates[:, 0])
    timeid_len = len(set(covariates[:, -2]))
    full_covariates = np.array([]).reshape(0,4)
    default_covid = covariates[0, 1]
    for p_id in  population[:, 0]:
        if p_id not in p_ids_in_cov:
            tmp_x = np.array([p_id, default_covid, 1, 0]).reshape(1,4) #default cov id, timeid=1
            full_covariates = np.concatenate((full_covariates, tmp_x), axis=0)
        else:
            tmp_x = covariates[covariates[:, 0] == p_id, :]
            #print tmp_x.shape, X.shape
            full_covariates = np.concatenate((full_covariates, tmp_x), axis=0)

    X, patient_keys = tu.convert_to_temporal_format(full_covariates, timeid_len  = timeid_len, predict = True)
    return X


print("Loading Data...")
# load data + train,test indexes + validation index

y=population[:,1]
#print covariates.shape

if modeltype == 'temporal':
    X = plpData.to_dense().numpy()
    X = X[np.int64(population[:, 0]), :]
	#X = get_temproal_data(covariates, population)
    dense = 0
else:
    #print included
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

print(X.shape)
print("Calculating predictions on population...")

if autoencoder:
    autoencoder_model = joblib.load(os.path.join(model_loc, 'autoencoder_model.pkl'))
    X = autoencoder_model.get_encode_features(X)

if modeltype == 'temporal':
    test_batch = tu.batch(X, batch_size = 32)
    test_pred = []
    for test in test_batch:
        pred_test1 = modelTrained.predict_proba(test)[:, 1]
        test_pred = np.concatenate((test_pred , pred_test1), axis = 0)
else:
    test_pred = modelTrained.predict_proba(X)[:, 1]


if test_pred.ndim != 1:
    test_pred = test_pred[:,1]
  

print("Prediction complete: %s rows" %(np.shape(test_pred)[0]))
print("Mean: %s prediction value" %(np.mean(test_pred)))

# merge pred with population
test_pred.shape = (population.shape[0], 1)
prediction = np.append(population,test_pred, axis=1)
