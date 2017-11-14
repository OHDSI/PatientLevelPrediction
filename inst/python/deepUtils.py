"""
 deepUtils.py

 Copyright 2016 Observational Health Data Sciences and Informatics

 This file is part of PatientLevelPrediction

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
"""

import sys
import os
import cPickle
import pdb
import random
import numpy as np
from sklearn.externals import joblib
from collections import OrderedDict
output_dir = 'data'


def convert_format2(covriate_ids, patient_dict, y_dict=None, time_window=1):
    """
    create matrix for temporal models.

    :param covriate_ids: the covariate ids in the whole data
    :param patient_dict: the dictionary contains the data for each patient
    :param y_dict: if the output labels is known, it contains the labels for patients
    :param time_window: the number of days as a window when extracting temporal data
    :return: return the raw data in 3-D format, patients x covariates x number of windows
    """
    D = len(covriate_ids)
    N = len(patient_dict)
    if 365 % time_window == 0:
        T = 365 / time_window
    else:
        T = 365 / time_window + 1

    print D, N, T
    concept_list = list(covriate_ids)
    concept_list.sort()
    x_raw = np.zeros((N, D, T), dtype=float)
    # y = np.zeros((O,N,T), dtype=int)
    patient_ind = 0
    p_ids = []
    patient_keys = patient_dict.keys()
    for kk in patient_keys:
        # print('-------------------')
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


def convert_2_cnn_format(covariates, time_window=12):
    covariate_ids = set()
    patient_dict = OrderedDict()
    # print covariates.shape
    # pdb.set_trace()
    for row in covariates:
        # print columns
        p_id, cov_id, time_id, cov_val = row[0], row[1], row[2], row[3]

        if p_id not in patient_dict:
            patient_dict[p_id] = {time_id: [(cov_id, cov_val)]}
        else:
            if time_id not in patient_dict[p_id]:
                patient_dict[p_id][time_id] = [(cov_id, cov_val)]
            else:
                patient_dict[p_id][time_id].append((cov_id, cov_val))
        covariate_ids.add(cov_id)
    # T = 365/time_window
    x, patient_keys = convert_format2(covariate_ids, patient_dict, time_window=time_window)

    return x, patient_keys

def read_data(filename):
    covriate_ids = set()
    patient_dict = {}
    head = True
    with open(filename) as fp:
        for lines in fp:
            if head:
                head = False
                continue
            lines = lines.strip('\n').strip('\r').split(',')
            #print(lines, type(lines), lines[0])
            try:
                p_id, cov_id, time_id, cov_val =  lines[1], lines[2], lines[3], lines[4]
            except:
                pdb.set_trace()
            print(p_id, cov_id, time_id)
            if p_id not in patient_dict:
                patient_dict[p_id] = {}
            else:
                if time_id not in patient_dict[p_id]:
                    patient_dict[p_id][time_id] = []
                else:
                    patient_dict[p_id][time_id].append((cov_id, cov_val))
            covriate_ids.add(cov_id)
    #time_window  = 1
    patient_dict = {k: v for k, v in patient_dict.iteritems() if v} #remove empty patients
    #(15, 2000, 60) 20000 patients, 15 lab tests,
    return covriate_ids, patient_dict

def split_training_validation(classes, validation_size=0.2, shuffle=False):
    """split sampels based on balnace classes"""
    num_samples = len(classes)
    classes = np.array(classes)
    classes_unique = np.unique(classes)
    num_classes = len(classes_unique)
    indices = np.arange(num_samples)
    # indices_folds=np.zeros([num_samples],dtype=int)
    training_indice = []
    training_label = []
    validation_indice = []
    validation_label = []
    for cl in classes_unique:
        indices_cl = indices[classes == cl]
        num_samples_cl = len(indices_cl)

        # split this class into k parts
        if shuffle:
            random.shuffle(indices_cl)  # in-place shuffle

        # module and residual
        num_samples_each_split = int(num_samples_cl * validation_size)
        res = num_samples_cl - num_samples_each_split

        training_indice = training_indice + [val for val in indices_cl[num_samples_each_split:]]
        training_label = training_label + [cl] * res

        validation_indice = validation_indice + [val for val in indices_cl[:num_samples_each_split]]
        validation_label = validation_label + [cl] * num_samples_each_split

    training_index = np.arange(len(training_label))
    random.shuffle(training_index)
    training_indice = np.array(training_indice)[training_index]
    training_label = np.array(training_label)[training_index]

    validation_index = np.arange(len(validation_label))
    random.shuffle(validation_index)
    validation_indice = np.array(validation_indice)[validation_index]
    validation_label = np.array(validation_label)[validation_index]

    return training_indice, training_label, validation_indice, validation_label


if __name__ == "__main__":
    filename = sys.argv[1]
    population = joblib.load('/data/share/plp/SYNPUF/population.pkl')
    # y = population[:, 1]
    covriate_ids, patient_dict = read_data(filename)
    # y_ids = np.array([int(val) for val in patient_dict.keys()])
    # Y = []
    y_dict = dict(zip(population[:, 0], population[:, 1]))
    # for val in y_ids:
    #    Y.append(y_dict[y_ids]) 
    x_train, x_valid, x_test, Y_train, Y_valid, Y_test = convert_format2(covriate_ids, patient_dict, y_dict)
