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


print "Applying Python Model" 

###########################################################################
def convert_format2(covriate_ids, patient_dict, y_dict = None, time_window = 1, save = True):
    D = len(covriate_ids)
    N = len(patient_dict)
    if 365%time_window == 0:
        T = 365/time_window
    else:
        T = 365/time_window + 1
        
    print D,N,T
    concept_list =list(covriate_ids)
    concept_list.sort()
    x_raw = np.zeros((N, D, T), dtype=float)
    #y = np.zeros((O,N,T), dtype=int)
    patient_ind = 0
    p_ids = []
    patient_keys = patient_dict.keys()
    for kk in patient_keys:
        #print('-------------------')
        vals = patient_dict[kk] 
        p_ids.append(int(kk))
        for timeid, meas in vals.iteritems():
            int_time = int(timeid) - 1
            for val in meas:
                if not len(val):
                    continue
                cov_id, cov_val = val
                lab_ind = concept_list.index(cov_id)
                x_raw[patient_ind][lab_ind][int_time] = float(cov_val)
    
        patient_ind = patient_ind + 1

    return x_raw, patient_keys

def convert_2_cnn_format(covariates, time_window = 12):
    covariate_ids = set()
    time_ids = set()
    patient_dict = OrderedDict()
    #print covariates.shape
    #pdb.set_trace()
    for row in covariates:
        #print columns
        p_id, cov_id, time_id, cov_val = row[0], row[1], row[2], row[3]
        
        if p_id not in patient_dict:
            patient_dict[p_id] = {time_id: [(cov_id, cov_val)]}
        else:
            if time_id not in patient_dict[p_id]:
                patient_dict[p_id][time_id] = [(cov_id, cov_val)]
            else:
                patient_dict[p_id][time_id].append((cov_id, cov_val))
        covariate_ids.add(cov_id)
    #T = 365/time_window
    x, patient_keys = convert_format2(covariate_ids, patient_dict, time_window = time_window)
    
    return x, patient_keys
	
def get_temproal_data(covariates, population, time_window = 12):
    p_ids_in_cov = set(covariates[:, 0])
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

    X, patient_keys = convert_2_cnn_format(full_covariates)
    return X

def batch(tensor, batch_size = 100):
    tensor_list = []
    length = tensor.shape[0]
    i = 0
    while True:
        if (i+1) * batch_size >= length:
            tensor_list.append(tensor[i * batch_size: length])
            return tensor_list
        tensor_list.append(tensor[i * batch_size: (i+1) * batch_size])
        i += 1

print "Loading Data..."
# load data + train,test indexes + validation index

y=population[:,1]
#print covariates.shape

print 'population size', len(y)
if modeltype == 'temporal':
	X = get_temproal_data(covariates, population)
	dense = 0
else:
	X = plpData[population[:,0],:]
	X = X[:,included.flatten()]

# load index file
print "population loaded- %s rows and %s columns" %(np.shape(population)[0], np.shape(population)[1])
print "Dataset has %s rows and %s columns" %(X.shape[0], X.shape[1])
print "Data ready for model has %s features" %(np.shape(X)[1])

###########################################################################	
# uf dense convert 
if dense==1:
  print "converting to dense data..."
  X=X.toarray()
###########################################################################	

# load model
print "Loading model..."
modelTrained = joblib.load(os.path.join(model_loc,'model.pkl')) 

print X.shape
print "Calculating predictions on population..."
if modeltype == 'temporal':
    test_batch = batch(X, batch_size = 50)
    test_pred = []
    for test in test_batch:
        pred_test1 = model.predict_proba(test)[:, 1]
        test_pred = np.concatenate((test_pred , pred_test1), axis = 0)
else:
    test_pred = modelTrained.predict_proba(X)

if test_pred.ndim != 1:
    test_pred = test_pred[:,1]
  

print "Prediction complete: %s rows" %(np.shape(test_pred)[0])
print "Mean: %s prediction value" %(np.mean(test_pred))

# merge pred with population
test_pred.shape = (population.shape[0], 1)
prediction = np.append(population,test_pred, axis=1)
