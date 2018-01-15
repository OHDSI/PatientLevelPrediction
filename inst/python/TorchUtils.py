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
import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.autograd import Variable
from torch.utils.data import DataLoader, TensorDataset
from sklearn.externals import joblib
from sklearn.metrics import roc_auc_score
from collections import OrderedDict
output_dir = 'data'


class FocalLoss(nn.Module):
    """
    Method to handle data imbalance based on paper (arXiv:1708.02002) entitled
    Focal loss for dense object detection.
    Loss(x, class) = - (1-softmax(x)[class])^gamma \log(softmax(x)[class])

    """

    def __init__(self, gamma=5, eps=1e-7, size_average=False):
        super(FocalLoss, self).__init__()
        self.gamma = gamma
        self.eps = eps
        self.size_average = size_average

    def forward(self, input, target):
        y = self.one_hot(target, input.size(-1))
        logit = F.softmax(input)
        logit = logit.clamp(self.eps, 1. - self.eps)

        loss = -1 * y * torch.log(logit)  # cross entropy
        loss = loss * (1 - logit) ** self.gamma  # focal loss

        if self.size_average:
            loss = loss.mean()
        else:
            loss = loss.sum()
        return loss

    def one_hot(self, index, classes):
        """

        :param index: is the labels
        :param classes: number if classes
        :return:
        """
        size = index.size() + (classes,)
        view = index.size() + (1,)

        mask = torch.Tensor(*size).fill_(0)
        index = index.view(*view)
        ones = 1.

        if isinstance(index, Variable):
            ones = Variable(torch.Tensor(index.size()).fill_(1))
            mask = Variable(mask, volatile=index.volatile)
            if torch.cuda.is_available():
                ones = ones.cuda()
                mask = mask.cuda()

        return mask.scatter_(1, index, ones)


class Estimator(object):
    """
    It is used for training different deep models in the same interface.
    """

    def __init__(self, model):
        self.model = model

    def compile(self, optimizer, loss):
        self.optimizer = optimizer
        self.loss_f = loss

    def _fit(self, train_loader, l1regularization=False, autoencoder=False):
        """
        train one epoch

        :param train_loader: The data loaded using DataLoader
        :param l1regularization: default False
        :return: the return fitted loss and accuracy
        """
        loss_list = []
        acc_list = []
        for idx, (X, y) in enumerate(train_loader):
            X_v = Variable(X)
            y_v = Variable(y)
            if torch.cuda.is_available():
                X_v = X_v.cuda()
                y_v = y_v.cuda()
                # print 'GPU id', torch.cuda.current_device()
            self.optimizer.zero_grad()
            # the below comemnted lines are used for multiple GPU training
            # if torch.cuda.device_count() > 1:
            # net = torch.nn.DataParallel(self.model, device_ids = range(torch.cuda.device_count()))
            # if cuda:
            #	net = net.cuda()
            # y_pred = net(X_v)
            y_pred = self.model(X_v)
            if autoencoder:
                loss = self.loss_f(y_pred, X_v)
            else:
                loss = self.loss_f(y_pred, y_v)
            if l1regularization:
                l1_crit = nn.L1Loss(size_average=False)
                reg_loss = 0
                for param in self.model.parameters():
                    target = Variable(torch.from_numpy(np.zeros(param.size()).astype(np.float32)))
                    if torch.cuda.is_available():
                        target = target.cuda()
                    reg_loss += l1_crit(param, target)

                factor = 0.0005
                loss += factor * reg_loss

            loss.backward()
            self.optimizer.step()
            loss_list.append(loss.data[0])
            if autoencoder:
                acc_list.append(0)
            else:
                classes = torch.topk(y_pred, 1)[1].data.cpu().numpy().flatten()
                acc = self._accuracy(classes, y_v.data.cpu().numpy().flatten())
                acc_list.append(acc)
            del loss
            del y_pred

        return sum(loss_list) / len(loss_list), sum(acc_list) / len(acc_list)

    def fit(self, X, y, batch_size=32, nb_epoch=10, validation_data=(), l1regularization=False, autoencoder =False):
        train_set = TensorDataset(torch.from_numpy(X.astype(np.float32)),
                                  torch.from_numpy(y.astype(np.float32)).long().view(-1))
        train_loader = DataLoader(dataset=train_set, batch_size=batch_size, shuffle=True)
        self.model.train()
        for t in range(nb_epoch):
            loss, acc = self._fit(train_loader, l1regularization=l1regularization, autoencoder = autoencoder)
            val_log = ''
            if validation_data and not autoencoder:
                val_loss, auc = self.evaluate(validation_data[0], validation_data[1], batch_size)
                val_log = "- val_loss: %06.4f - auc: %6.4f" % (val_loss, auc)
                print val_log
                # print("Epoch %s/%s loss: %06.4f - acc: %06.4f %s" % (t, nb_epoch, loss, acc, val_log))

    def evaluate(self, X, y, batch_size=32):
        y_pred = self.predict(X)
        y_v = Variable(torch.from_numpy(y).long(), requires_grad=False)
        if torch.cuda.is_available():
            y_v = y_v.cuda()
        loss = self.loss_f(y_pred, y_v)
        predict = y_pred.data.cpu().numpy()[:, 1].flatten()
        auc = roc_auc_score(y, predict)
        # lasses = torch.topk(y_pred, 1)[1].data.numpy().flatten()
        # #cc = self._accuracy(classes, y)
        return loss.data[0], auc

    def _accuracy(self, y_pred, y):
        return float(sum(y_pred == y)) / y.shape[0]

    def predict(self, X):
        X = Variable(torch.from_numpy(X.astype(np.float32)))
        if torch.cuda.is_available():
            X = X.cuda()
        y_pred = self.model(X)
        return y_pred

    def predict_proba(self, X):
        self.model.eval()
        return self.model.predict_proba(X)

def batch(tensor, batch_size = 50):
    """ It is used to create batch samples, each batch has batch_size samples"""
    tensor_list = []
    length = tensor.shape[0]
    i = 0
    while True:
        if (i+1) * batch_size >= length:
            tensor_list.append(tensor[i * batch_size: length])
            return tensor_list
        tensor_list.append(tensor[i * batch_size: (i+1) * batch_size])
        i += 1


class selu(nn.Module):
    def __init__(self):
        super(selu, self).__init__()
        self.alpha = 1.6732632423543772848170429916717
        self.scale = 1.0507009873554804934193349852946

    def forward(self, x):
        temp1 = self.scale * F.relu(x)
        temp2 = self.scale * self.alpha * (F.elu(-1 * F.relu(-1 * x)))
        return temp1 + temp2


class alpha_drop(nn.Module):
    def __init__(self, p=0.05, alpha=-1.7580993408473766, fixedPointMean=0, fixedPointVar=1):
        super(alpha_drop, self).__init__()
        keep_prob = 1 - p
        self.a = np.sqrt(
            fixedPointVar / (keep_prob * ((1 - keep_prob) * pow(alpha - fixedPointMean, 2) + fixedPointVar)))
        self.b = fixedPointMean - self.a * (keep_prob * fixedPointMean + (1 - keep_prob) * alpha)
        self.alpha = alpha
        self.keep_prob = 1 - p
        self.drop_prob = p

    def forward(self, x):
        if self.keep_prob == 1 or not self.training:
            # print("testing mode, direct return")
            return x
        else:
            random_tensor = self.keep_prob + torch.rand(x.size())

            binary_tensor = Variable(torch.floor(random_tensor))

            if torch.cuda.is_available():
                binary_tensor = binary_tensor.cuda()

            x = x.mul(binary_tensor)
            ret = x + self.alpha * (1 - binary_tensor)
            ret.mul_(self.a).add_(self.b)
            return ret

def convert_to_3d_matrix(covariate_ids, patient_dict, y_dict = None, timeid_len = 31, cov_mean_dict = None):
    """
    create matrix for temporal models.

    :param covariate_ids: the covariate ids in the whole data
    :param patient_dict: the dictionary contains the data for each patient
    :param y_dict: if the output labels is known, it contains the labels for patients
    :param timeid_len: the total number time window gaps when extracting temporal data
    :return: return the raw data in 3-D format, patients x covariates x number of windows, and the patients ids
    """
    D = len(covariate_ids)
    N = len(patient_dict)
    #if 365%time_window == 0:
    #    T = 365/time_window
    #else:
    #    T = 365/time_window + 1
    T = timeid_len   
    print D,N,T
    concept_list =list(covariate_ids)
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
                if cov_id not in covariate_ids:
                    continue
                lab_ind = concept_list.index(cov_id)
                if cov_mean_dict is None:
                    x_raw[patient_ind][lab_ind][int_time] = float(cov_val)
                else:
                    mean_std = cov_mean_dict[cov_id]
                    x_raw[patient_ind][lab_ind][int_time] = (float(cov_val) - mean_std[0])/mean_std[1]
    
        patient_ind = patient_ind + 1

    return x_raw, patient_keys

def convert_to_temporal_format(covariates, timeid_len= 31, normalize = False):
    """
    It reads the data from covariates extracted by FeatureExtraction package and convert it to temporal data matrix

    :param covariates: covariates extracted by FeatureExtraction package
    :param timeid_len: the total number of window gaps when extracting temporal data
    :return: return the raw data in 3-D format, patients x covariates x number of windows, and the patients ids
    """
    covariate_ids = set()
    patient_dict = OrderedDict()
    print 'Loading temporal data'
    #pdb.set_trace()
    cov_vals_dict = {}
    for row in covariates:
        #print columns
        p_id, cov_id, time_id, cov_val = row[0], row[1], row[2], row[3]
        cov_vals_dict.setdefault(cov_id, []).append(float(cov_val))
        if p_id not in patient_dict:
            patient_dict[p_id] = {time_id: [(cov_id, cov_val)]}
        else:
            if time_id not in patient_dict[p_id]:
                patient_dict[p_id][time_id] = [(cov_id, cov_val)]
            else:
                patient_dict[p_id][time_id].append((cov_id, cov_val))
        #covariate_ids.add(cov_id)
    #T = 365/time_window
    cov_mean_dict = {}

    for key, val in cov_vals_dict.iteritems():
        mean_val = np.mean(val)
        std_val = np.std(val)
        # Remove those covariates with few occurrence (<5)
        if len(val) >= 5:
            covariate_ids.add(key)

        cov_mean_dict[key] = (mean_val, std_val)

    if normalize:
        x, patient_keys = convert_to_3d_matrix(covariate_ids, patient_dict, timeid_len = timeid_len, cov_mean_dict = cov_mean_dict)
    else:
        x, patient_keys = convert_to_3d_matrix(covariate_ids, patient_dict, timeid_len=timeid_len)
    
    return x, patient_keys



def read_data(filename):
    covariate_ids = set()
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
                    covariate_ids.add(cov_id)
    #time_window  = 1
    patient_dict = {k: v for k, v in patient_dict.iteritems() if v} #remove empty patients
    #(15, 2000, 60) 20000 patients, 15 lab tests,
    return covariate_ids, patient_dict

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
    #x, patient_keys = convert_2_cnn_format(covariates, timeid_len = 31)
    # for val in y_ids:
    #    Y.append(y_dict[y_ids]) 
    x_train, x_valid, x_test, Y_train, Y_valid, Y_test = convert_to_temporal_format(covriate_ids, patient_dict, y_dict)
