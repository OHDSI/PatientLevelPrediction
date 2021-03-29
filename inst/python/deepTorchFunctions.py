"""
 deepTorch.py: It implements different deep learning classifiers

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
import pdb
import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.autograd import Variable
from torch.utils.data import DataLoader, TensorDataset
from torch.nn.utils.rnn import pack_padded_sequence
from torch.nn.utils.rnn import pad_packed_sequence
from collections import OrderedDict
import timeit
import joblib
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_auc_score
import numpy as np

import warnings
warnings.filterwarnings("ignore")

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

def loss_function(recon_x, x, mu, logvar):
    """Loss function for varational autoencoder VAE"""
    BCE = F.binary_cross_entropy(recon_x, x, size_average=False)

    # 0.5 * sum(1 + log(sigma^2) - mu^2 - sigma^2)
    KLD = -0.5 * torch.sum(1 + logvar - mu.pow(2) - logvar.exp())

    return BCE + KLD


def mixup_data(x, y, alpha=1.0):

    '''Compute the mixup data. Return mixed inputs, pairs of targets, and lambda
    Data Augmentation method based on paper (arXiv:1710.09412) entitled
    mixup: Beyond empirical risk minimization.
    '''
    if alpha > 0.:
        lam = np.random.beta(alpha, alpha)
    else:
        lam = 1.
    batch_size = x.size()[0]
    if torch.cuda.is_available():
        index = torch.randperm(batch_size).cuda()
    else:
        index = torch.randperm(batch_size)

    mixed_x = lam * x + (1 - lam) * x[index,:]
    y_a, y_b = y, y[index]
    return mixed_x, y_a, y_b, lam

def mixup_criterion(y_a, y_b, lam):
    return lambda criterion, pred: lam * criterion(pred, y_a) + (1 - lam) * criterion(pred, y_b)

def early_stop(metrics_hist, patience = 3):
    if not np.all(np.isnan(metrics_hist)):
        return np.nanargmin(metrics_hist) > len(metrics_hist) - patience
    else:
        #keep training if criterion results have all been nan so far
        return False


class Estimator(object):
    """
    It is used for training different deep models in the same interface.
    """

    def __init__(self, model):
        self.model = model

    def compile(self, optimizer, loss):
        self.optimizer = optimizer
        self.loss_f = loss

    def _fit(self, train_loader, l1regularization=False, autoencoder=False, mixup=False, vae = False):
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

            if mixup:
                X_v, y_v_a, y_v_b, lam = mixup_data(X_v, y_v)
                X_v, y_v_a, y_v_b = Variable(X_v), Variable(y_v_a), Variable(y_v_b)

                # print 'GPU id', torch.cuda.current_device()
            self.optimizer.zero_grad()
            # the below comemnted lines are used for multiple GPU training
            # if torch.cuda.device_count() > 1:
            # net = torch.nn.DataParallel(self.model, device_ids = range(torch.cuda.device_count()))
            # if cuda:
            #	net = net.cuda()
            # y_pred = net(X_v)
            if autoencoder:
                if vae:
                    y_pred, mu, logvar = self.model(X_v)
                    loss = loss_function(y_pred, X_v, mu, logvar)
                else:
                    y_pred = self.model(X_v)
                    loss = self.loss_f(y_pred, X_v)
            else:
                y_pred = self.model(X_v)

                loss = self.loss_f(y_pred, y_v)

                if mixup:
                    loss_func = mixup_criterion(y_v_a, y_v_b, lam)
                    loss = loss_func(self.loss_f, y_pred)

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
            loss_list.append(loss.item())
            if autoencoder:
                acc_list.append(0)
            else:
                classes = torch.topk(y_pred, 1)[1].data.cpu().numpy().flatten()
                acc = self._accuracy(classes, y_v.data.cpu().numpy().flatten())
                acc_list.append(acc)
            del loss
            del y_pred

        return sum(loss_list) / len(loss_list), sum(acc_list) / len(acc_list)

    def fit(self, X, y, batch_size=32, nb_epoch=10, validation_data=(), l1regularization=False, autoencoder =False, vae = False):
        train_set = TensorDataset(torch.from_numpy(X.astype(np.float32)),
                                  torch.from_numpy(y.astype(np.float32)).long().view(-1))
        train_loader = DataLoader(dataset=train_set, batch_size=batch_size, shuffle=True)
        self.model.train()
        for t in range(nb_epoch):
            loss, acc = self._fit(train_loader, l1regularization=l1regularization, autoencoder = autoencoder, vae = vae)
            #print loss
            val_log = ''
            if validation_data and not autoencoder:
                val_loss, auc = self.evaluate(validation_data[0], validation_data[1], batch_size)

                val_log = "- val_loss: %06.4f - auc: %6.4f" % (val_loss, auc)
                print(val_log)
                # print("Epoch %s/%s loss: %06.4f - acc: %06.4f %s" % (t, nb_epoch, loss, acc, val_log))

    def evaluate(self, X, y, batch_size=32):
        y_pred = self.predict(X)
        y_v = Variable(torch.from_numpy(y).long(), requires_grad=False)
        if torch.cuda.is_available():
            y_v = y_v.cuda()
        loss = self.loss_f(y_pred, y_v)
        predict = y_pred.data.cpu().numpy()[:, 1].flatten()
        auc = roc_auc_score(y, predict)
        return loss.item(), auc

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

class EarlyStopping(object): # pylint: disable=R0902
    """
    Gives a criterion to stop training when a given metric is not
    improving anymore
    Args:
        mode (str): One of `min`, `max`. In `min` mode, training will
            be stopped when the quantity monitored has stopped
            decreasing; in `max` mode it will be stopped when the
            quantity monitored has stopped increasing. Default: 'min'.
        patience (int): Number of epochs with no improvement after
            which training is stopped. For example, if
            `patience = 2`, then we will ignore the first 2 epochs
            with no improvement, and will only stop learning after the
            3rd epoch if the loss still hasn't improved then.
            Default: 10.
        threshold (float): Threshold for measuring the new optimum,
            to only focus on significant changes. Default: 1e-4.
        threshold_mode (str): One of `rel`, `abs`. In `rel` mode,
            dynamic_threshold = best * ( 1 + threshold ) in 'max'
            mode or best * ( 1 - threshold ) in `min` mode.
            In `abs` mode, dynamic_threshold = best + threshold in
            `max` mode or best - threshold in `min` mode. Default: 'rel'.
    """

    def __init__(self, mode='min', patience=3, threshold=1e-4, threshold_mode='rel'):
        self.patience = patience
        self.mode = mode
        self.threshold = threshold
        self.threshold_mode = threshold_mode
        self.best = None
        self.num_bad_epochs = None
        self.mode_worse = None  # the worse value for the chosen mode
        self.is_better = None
        self.last_epoch = -1
        self._init_is_better(mode=mode, threshold=threshold,
                             threshold_mode=threshold_mode)
        self._reset()

    def _reset(self):
        """Resets num_bad_epochs counter and cooldown counter."""
        self.best = self.mode_worse
        self.num_bad_epochs = 0

    def step(self, metrics, epoch=None):
        """ Updates early stopping state """
        current = metrics
        if epoch is None:
            epoch = self.last_epoch = self.last_epoch + 1
        self.last_epoch = epoch

        if self.is_better(current, self.best):
            self.best = current
            self.num_bad_epochs = 0
        else:
            self.num_bad_epochs += 1

    @property
    def stop(self):
        """ Should we stop learning? """
        return self.num_bad_epochs > self.patience

    def _cmp(self, mode, threshold_mode, threshold, a, best): # pylint: disable=R0913, R0201
        if mode == 'min' and threshold_mode == 'rel':
            rel_epsilon = 1. - threshold
            return a < best * rel_epsilon

        elif mode == 'min' and threshold_mode == 'abs':
            return a < best - threshold

        elif mode == 'max' and threshold_mode == 'rel':
            rel_epsilon = threshold + 1.
            return a > best * rel_epsilon

        return a > best + threshold

    def _init_is_better(self, mode, threshold, threshold_mode):
        if mode not in {'min', 'max'}:
            raise ValueError('mode ' + mode + ' is unknown!')
        if threshold_mode not in {'rel', 'abs'}:
            raise ValueError('threshold mode ' + threshold_mode + ' is unknown!')

        if mode == 'min':
            self.mode_worse = float('inf')
        else:  # mode == 'max':
            self.mode_worse = (-float('inf'))

        self.is_better = partial(self._cmp, mode, threshold_mode, threshold)

    def state_dict(self):
        """ Returns early stopping state """
        return {key: value for key, value in self.__dict__.items() if key != 'is_better'}

    def load_state_dict(self, state_dict):
        """ Loads early stopping state """
        self.__dict__.update(state_dict)
        self._init_is_better(mode=self.mode, threshold=self.threshold,
                             threshold_mode=self.threshold_mode)

def adjust_learning_rate(learning_rate, optimizer, epoch):
    """Sets the learning rate to the initial LR decayed by 10 every 30 epochs"""

    lr = learning_rate * (0.1 ** (epoch // 10))

    for param_group in optimizer.param_groups:
        param_group['lr'] = lr
    return lr

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
    T = timeid_len   
    concept_list =list(covariate_ids)
    concept_list.sort()
    x_raw = np.zeros((N, D, T), dtype=float)
    patient_ind = 0
    p_ids = []
    patient_keys = patient_dict.keys()
    #print covariate_ids
    for kk in patient_keys:
        #print('-------------------')
        vals = patient_dict[kk]
        #sorted(vals)
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
                    if mean_std[1]:
                        x_raw[patient_ind][lab_ind][int_time] = (float(cov_val) - mean_std[0])/mean_std[1]
                    else:
                        x_raw[patient_ind][lab_ind][int_time] = float(cov_val)

    
        patient_ind = patient_ind + 1

    #impute the data using the value of previous timestamp
    #fw = open('patient_var.txt', 'w')
    for i in xrange(N):
        for j in xrange(D):
            temp = x_raw[i][j]
            nonzero_inds = np.nonzero(temp)[0]
            count_nonzeros =  len(nonzero_inds)
            #fw.write(str(i) + '\t' + str(count_nonzeros) + '\n')
            if count_nonzeros == 1:
                ind = nonzero_inds[0]
                for k in xrange(ind + 1, T):
                    x_raw[i][j][k] = x_raw[i][j][ind]
            elif count_nonzeros > 1:
                for ind in xrange(1, count_nonzeros):
                    for k in xrange(nonzero_inds[ind -1] + 1, nonzero_inds[ind]):
                        x_raw[i][j][k] = x_raw[i][j][nonzero_inds[ind - 1]]
                # For last nonzeros.
                for k in xrange(nonzero_inds[-1] + 1, T):
                    x_raw[i][j][k] = x_raw[i][j][nonzero_inds[-1]]

    #fw.close()

    return x_raw, patient_keys

def forward_impute_missing_value(x_raw):
    N = x_raw.shape[0]
    D = x_raw.shape[1]
    T = x_raw.shape[2]
    for i in xrange(N):
        for j in xrange(D):
            temp = x_raw[i][j]
            nonzero_inds = np.nonzero(temp)[0]
            count_nonzeros =  len(nonzero_inds)
            #fw.write(str(i) + '\t' + str(count_nonzeros) + '\n')
            if count_nonzeros == 1:
                ind = nonzero_inds[0]
                for k in xrange(ind + 1, T):
                    x_raw[i][j][k] = x_raw[i][j][ind]
            elif count_nonzeros > 1:
                for ind in xrange(1, count_nonzeros):
                    for k in xrange(nonzero_inds[ind -1] + 1, nonzero_inds[ind]):
                        x_raw[i][j][k] = x_raw[i][j][nonzero_inds[ind - 1]]
                # For last nonzeros.
                for k in xrange(nonzero_inds[-1] + 1, T):
                    x_raw[i][j][k] = x_raw[i][j][nonzero_inds[-1]]


def convert_to_temporal_format(covariates, timeid_len= 31, normalize = True, predict = False):
    """
    It reads the data from covariates extracted by FeatureExtraction package and convert it to temporal data matrix

    :param covariates: covariates extracted by FeatureExtraction package
    :param timeid_len: the total number of window gaps when extracting temporal data
    :return: return the raw data in 3-D format, patients x covariates x number of windows, and the patients ids
    """
    patient_dict = OrderedDict()
    print('Loading temporal data')
    cov_vals_dict = {}
    for row in covariates:
        p_id, cov_id, time_id, cov_val = row[0], row[1], row[2], row[3]
        cov_id = np.int64(cov_id)
        #time_id = int(time_id)
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
    covariate_ids = set()
    cov_mean_dict = {}
    if not predict:
        fw = open('covariate_mean_std.csv', 'w')
        for key, val in cov_vals_dict.iteritems():
            mean_val = np.mean(val)
            std_val = np.std(val)

            # Remove those covariates with few occurrence (<5)
            if len(val) >= 5:
                covariate_ids.add(key)
                cov_mean_dict[key] = (mean_val, std_val)
                fw.write(str(key) + ',' + str(mean_val) + ',' + str(std_val) + '\n')
        fw.close()
    else:
        fp = open('covariate_mean_std.csv', 'r')
        for line in fp:
            values = line.rstrip().split(',')
            key = np.int64(values[0])
            covariate_ids.add(key)
            cov_mean_dict[key] = (float(values[1]), float(values[2]))
        fp.close()


    if normalize:
        x, patient_keys = convert_to_3d_matrix(covariate_ids, patient_dict, timeid_len = timeid_len, cov_mean_dict = cov_mean_dict)
    else:
        x, patient_keys = convert_to_3d_matrix(covariate_ids, patient_dict, timeid_len=timeid_len)
    
    return x, patient_keys


def read_covariates(covariate_file):
    patient_dict = {}
    head = True
    with open(covariate_file, 'r') as fp:
        for line in fp:
            if head:
                head = False
                continue
            values = line.rstrip().split(',')
            patient_id = values[1]
            cov_id = values[2]
            #time_id = int(values[-1])
            # covariates in one patient has time order
            patient_dict.setdefault(patient_id, []).append((cov_id))
    new_patient = []
    for key in patient_dict.keys():
        #patient_dict[key].sort()
        sort_vals = []
        for val in patient_dict[key]:
            if val[1] not in sort_vals:
                sort_vals.append(val)
        new_patient.append(sort_vals)

    return new_patient


def word_embeddings(covariate_file, embedding_size=50):
    import gensim.models.word2vec as w2v
    modelname = "processed_%s.w2v" % ('heartfailure')
    sentences = read_covariates(covariate_file)
    model = w2v.Word2Vec(size=embedding_size, min_count=3, workers=4, iter=10, sg=1)
    print("building word2vec vocab on %s..." % (covariate_file))

    model.build_vocab(sentences)
    print("training...")
    model.train(sentences, total_examples=model.corpus_count, epochs=model.iter)
    out_file = modelname
    print("writing embeddings to %s" % (out_file))
    model.save(out_file)
    return out_file

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


class LogisticRegression(nn.Module):
    """
    Train a logistic regression model using pytorch
    """
    def __init__(self, input_size, num_classes = 2):
        super(LogisticRegression, self).__init__()
        self.linear = nn.Linear(input_size, num_classes)

    def forward(self, x):
        out = self.linear(x)
        out = torch.sigmoid(out)
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        with torch.no_grad():
            x = Variable(x)
            if torch.cuda.is_available():
                x = x.cuda()
            y = self.forward(x)
            temp = y.data.cpu().numpy()
            return temp


class MLP(nn.Module):
    """
    Train a multiple-layer perceptron with one hideen layer
    """
    def __init__(self, input_dim, hidden_size, num_classes = 2):
        super(MLP, self).__init__()
        self.fc1 = nn.Linear(input_dim, hidden_size)
        self.fc2 = nn.Linear(hidden_size, num_classes)

    def forward(self, x):
        x = F.relu(self.fc1(x))
        x = F.dropout(x, p =0.5, training=self.training)
        x = self.fc2(x)
        x = torch.sigmoid(x)
        return x

    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        with torch.no_grad():
            x = Variable(x)
            if torch.cuda.is_available():
                x = x.cuda()
            y = self.forward(x)
            temp = y.data.cpu().numpy()
            return temp


class SNN(nn.Module):
    """
    Train a multiple-layer self normalizing neural network, ref arXiv:1706.02515
    """

    def __init__(self, input_dim, hidden_size, num_classes=2):
        super(SNN, self).__init__()
        self.fc1 = nn.Linear(input_dim, hidden_size)
        self.fc2 = selu()
        self.ad1 = alpha_drop()
        self.fc4 = nn.Linear(hidden_size, num_classes)

    def forward(self, x):
        x = F.relu(self.fc1(x))
        x = F.dropout(x, p=0.5, training=self.training)
        x = self.fc2(x)
        x = self.ad1(x)
        x = self.fc4(x)
        x = torch.sigmoid(x)
        return x

    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        with torch.no_grad():
            x = Variable(x)
            if torch.cuda.is_available():
                x = x.cuda()
            y = self.forward(x)
            temp = y.data.cpu().numpy()
            return temp

class AutoEncoder(nn.Module):
    """
    A stacked autoencoder with 2 hiddden layers and need be adapted for EHR data.
    """
    def __init__(self, input_size, encoding_size):
        super(AutoEncoder, self).__init__()

        self.encoder = nn.Sequential(
            nn.Linear(input_size, input_size/2),
            nn.ReLU(True),
            nn.Linear(input_size/2, input_size/4),
            nn.ReLU(True),
            nn.Linear(input_size/4, encoding_size),
            nn.ReLU(True)
        )
        self.decoder = nn.Sequential(
            nn.Linear(encoding_size, input_size/4),
            nn.ReLU(True),
            nn.Linear(input_size/4, input_size/2),
            nn.ReLU(True),
            nn.Linear(input_size/2, input_size)
        )

    def forward(self, x):
        if torch.cuda.is_available():
            x = x.cuda()
        encoded = self.encoder(x)
        decoded = self.decoder(encoded)
        return decoded

    def get_encode_features(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        with torch.no_grad():
            x = Variable(x)
            if torch.cuda.is_available():
                x = x.cuda()
            encoded = self.encoder(x)
            encoded = encoded.data.cpu().numpy()
            return encoded

class VAE(nn.Module):
    """
    A stacked variational autoencoder with 2 hiddden layers and need be adapted for EHR data.
    """
    def __init__(self, input_size, encoding_size):
        super(VAE, self).__init__()

        self.fc1 = nn.Linear(input_size, input_size/2)
        self.fc21 = nn.Linear(input_size/2, encoding_size)
        self.fc22 = nn.Linear(input_size/2, encoding_size)
        self.fc3 = nn.Linear(encoding_size, input_size/2)
        self.fc4 = nn.Linear(input_size/2, input_size)

        self.relu = nn.ReLU()
        self.sigmoid = nn.Sigmoid()

    def encode(self, x):
        h1 = self.relu(self.fc1(x))
        return self.fc21(h1), self.fc22(h1)

    def reparameterize(self, mu, logvar):
        if self.training:
            std = logvar.mul(0.5).exp_()
            eps = Variable(std.data.new(std.size()).normal_())
            return eps.mul(std).add_(mu)
        else:
            return mu

    def decode(self, z):
        h3 = self.relu(self.fc3(z))
        return self.sigmoid(self.fc4(h3))

    def forward(self, x):
        if torch.cuda.is_available():
            x = x.cuda()
        mu, logvar = self.encode(x)
        z = self.reparameterize(mu, logvar)
        return self.decode(z), mu, logvar

    def get_encode_features(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        with torch.no_grad():
            x = Variable(x)
            if torch.cuda.is_available():
                x = x.cuda()
            mu, logvar = self.encode(x)
            encoded = self.reparameterize(mu, logvar)
            encoded = encoded.data.cpu().numpy()
            return encoded

class Decoder(nn.Module):
    """ VAE decoder input_size = original inputsize/16*256"""
    def __init__(self, latent_size, input_size, img_channels = 1, kernel_size=(1, 4), stride=(1, 2), padding=(0, 1)):
        super(Decoder, self).__init__()
        self.latent_size = latent_size
        self.img_channels = img_channels

        self.fc1 = nn.Linear(latent_size, input_size)
        self.deconv1 = nn.ConvTranspose2d(input_size, 128, kernel_size, stride=stride, padding = padding)
        self.deconv2 = nn.ConvTranspose2d(128, 64, kernel_size, stride=stride, padding = padding)
        self.deconv3 = nn.ConvTranspose2d(64, 32, kernel_size, stride=stride, padding = padding)
        self.deconv4 = nn.ConvTranspose2d(32, img_channels, kernel_size, stride=stride, padding = padding)

    def forward(self, x): # pylint: disable=arguments-differ
        x = F.relu(self.fc1(x))
        x = x.unsqueeze(-1).unsqueeze(-1)
        x = F.relu(self.deconv1(x))
        x = F.relu(self.deconv2(x))
        x = F.relu(self.deconv3(x))
        reconstruction = torch.sigmoid(self.deconv4(x))
        return reconstruction

class Encoder(nn.Module): # pylint: disable=too-many-instance-attributes
    """ VAE encoder """
    def __init__(self, latent_size, input_size, img_channels = 1, kernel_size=(1, 4), stride=(1, 2), padding=(0, 1)):
        super(Encoder, self).__init__()
        self.latent_size = latent_size
        #self.img_size = img_size
        self.img_channels = img_channels

        self.conv1 = nn.Conv2d(img_channels, 32, kernel_size, stride=stride, padding = padding)
        self.conv2 = nn.Conv2d(32, 64, kernel_size, stride=stride, padding = padding)
        self.conv3 = nn.Conv2d(64, 128, kernel_size, stride=stride, padding = padding)
        self.conv4 = nn.Conv2d(128, 256, kernel_size, stride=stride, padding = padding)
        out_size = input_size / 16
        self.fc_mu = nn.Linear(out_size, latent_size)
        self.fc_logsigma = nn.Linear(out_size, latent_size)


    def forward(self, x): # pylint: disable=arguments-differ
        x = F.relu(self.conv1(x))
        x = F.relu(self.conv2(x))
        x = F.relu(self.conv3(x))
        x = F.relu(self.conv4(x))
        x = x.view(x.size(0), -1)

        mu = self.fc_mu(x)
        logsigma = self.fc_logsigma(x)

        return mu, logsigma

class VAE_CNN(nn.Module):
    """ Variational Autoencoder """
    def __init__(self, latent_size, input_size):
        super(VAE, self).__init__()
        self.encoder = Encoder(latent_size, input_size)
        input_size = input_size/16
        self.decoder = Decoder(latent_size, input_size)

    def forward(self, x): # pylint: disable=arguments-differ
        mu, logsigma = self.encoder(x)
        sigma = logsigma.exp()
        eps = torch.randn_like(sigma)
        z = eps.mul(sigma).add_(mu)

        recon_x = self.decoder(z)
        return recon_x, mu, logsigma

    def get_encode_features(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        with torch.no_grad():
            x = Variable(x)
            if torch.cuda.is_available():
                x = x.cuda()
            mu, logvar = self.encoder(x)
            encoded = mu .data.cpu().numpy()
            return encoded

class CNN(nn.Module):
    def __init__(self, nb_filter, num_classes = 2, kernel_size = (1, 5), pool_size = (1, 3), labcounts = 32, window_size = 12, hidden_size = 200, stride = (1, 1), padding = 0):
        super(CNN, self).__init__()
        self.layer1 = nn.Sequential(
            nn.Conv2d(1, nb_filter, kernel_size, stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU(),
            nn.MaxPool2d(pool_size, stride = stride))
        out1_size = (window_size + 2*padding - (kernel_size[1] - 1) - 1)/stride[1] + 1
        maxpool_size = (out1_size + 2*padding - (pool_size[1] - 1) - 1)/stride[1] + 1
        self.layer2 = nn.Sequential(
            nn.Conv2d(nb_filter, nb_filter, kernel_size, stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU(),
            nn.MaxPool2d(pool_size, stride = stride))
        out2_size = (maxpool_size + 2*padding - (kernel_size[1] - 1) - 1)/stride[1] + 1
        maxpool_size = (out2_size + 2*padding - (pool_size[1] - 1) - 1)/stride[1] + 1
        self.drop1 = nn.Dropout(p=0.5)
        self.fc1 = nn.Linear(int(maxpool_size*labcounts*nb_filter), hidden_size)
        self.bn = nn.BatchNorm1d(hidden_size)
        self.drop2 = nn.Dropout(p=0.5)
        self.relu1 = nn.ReLU()
        self.fc2 = nn.Linear(hidden_size, num_classes)
        
    def forward(self, x):
        x = x.view(x.size(0), 1, x.size(1), x.size(2))
        out = self.layer1(x)
        out = self.layer2(out)
        out = out.view(out.size(0), -1)
        out = self.drop1(out)
        out = self.fc1(out)
        out = self.drop2(out)
        out = self.bn(out)
        out = self.relu1(out)
        out = self.fc2(out)
        out = torch.sigmoid(out)
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        with torch.no_grad():
            x = Variable(x)
            if torch.cuda.is_available():
                x = x.cuda()
            y = self.forward(x)
            temp = y.data.cpu().numpy()
            return temp


#allow multiple kernel with differnt kernel size
class CNN_MLF(nn.Module):
    """
    It is a deep CNNs with three different kernel size, the outputs from the three CNNs are concatenated to fed into two fully connected layers.
    """
    def __init__(self, nb_filter, num_classes = 2, kernel_size = (1, 5), pool_size = (1, 3), labcounts = 32, window_size = 12, hidden_size = 200, stride = (1, 1), padding = 0):
        super(CNN_MLF, self).__init__()
        self.layer1 = nn.Sequential(
            nn.Conv2d(1, nb_filter, kernel_size = (1, 3), stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU(),
            nn.MaxPool2d(pool_size, stride = stride))
        out1_size = (window_size + 2*padding - (3 - 1) - 1)/stride[1] + 1
        maxpool1_size = (out1_size + 2*padding - (pool_size[1] - 1) - 1)/stride[1] + 1
        self.layer2 = nn.Sequential(
            nn.Conv2d(1, nb_filter, kernel_size = (1, 4), stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU(),
            nn.MaxPool2d(pool_size, stride = stride))
        out2_size = (window_size + 2*padding - (4 - 1) - 1)/stride[1] + 1 #4 is the convolve filter size
        maxpool2_size = (out2_size + 2*padding - (pool_size[1] - 1) - 1)/stride[1] + 1
        self.layer3 = nn.Sequential(
            nn.Conv2d(1, nb_filter, kernel_size = (1, 5), stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU(),
            nn.MaxPool2d(pool_size, stride = stride))
        out3_size = (window_size + 2*padding - (5 - 1) - 1)/stride[1] + 1
        maxpool3_size = (out3_size + 2*padding - (pool_size[1] - 1) - 1)/stride[1] + 1
        conv_outsize = 	maxpool1_size + maxpool2_size +maxpool3_size
        self.drop1 = nn.Dropout(p=0.5)
        self.fc1 = nn.Linear(conv_outsize*labcounts*nb_filter, hidden_size)
        self.drop2 = nn.Dropout(p=0.5)
        self.relu1 = nn.ReLU()
        self.fc2 = nn.Linear(hidden_size, num_classes)
        
    def forward(self, x):
        x = x.view(x.size(0), 1, x.size(1), x.size(2))
        out1 = self.layer1(x)
        out2 = self.layer2(x)
        out3 = self.layer3(x)
        out = torch.cat((out1.view(out1.size(0), -1), out2.view(out2.size(0), -1), out3.view(out2.size(0), -1)), 1)
        out = self.drop1(out)
        out = self.fc1(out)
        out = self.drop2(out)
        out = self.relu1(out)
        out = self.fc2(out)
        out = torch.sigmoid(out)
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        with torch.no_grad():
            x = Variable(x)
            if torch.cuda.is_available():
                x = x.cuda()
            y = self.forward(x)
            temp = y.data.cpu().numpy()
            return temp


class CNN_LSTM(nn.Module):
    """
    It is a deep network with two layer CNN, followed by LSTM layer, which further fed into two fully connected layers.
    """
    def __init__(self, nb_filter, num_classes = 2, kernel_size = (1, 5), pool_size = (1, 3), labcounts = 32, window_size = 12, hidden_size = 100, stride = (1, 1), padding = 0, num_layers = 2):
        super(CNN_LSTM, self).__init__()
        self.layer1 = nn.Sequential(
            nn.Conv2d(1, nb_filter, kernel_size, stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU(),
            nn.MaxPool2d(pool_size, stride = stride))
        self.num_layers = num_layers
        self.hidden_size = hidden_size
        out1_size = (window_size + 2*padding - (kernel_size[1] - 1) - 1)/stride[1] + 1
        maxpool_size = (out1_size + 2*padding - (pool_size[1] - 1) - 1)/stride[1] + 1
        self.downsample = nn.Conv2d(nb_filter, 1, kernel_size, stride = stride, padding = padding)
        input_size = (maxpool_size + 2*padding - (kernel_size[1] - 1) - 1)/stride[1] + 1
        self.layer2 = nn.LSTM(input_size, hidden_size, num_layers, batch_first = True)
        self.drop1 = nn.Dropout(p=0.5)
        self.fc1 = nn.Linear(hidden_size, hidden_size)
        self.drop2 = nn.Dropout(p=0.5)
        self.relu1 = nn.ReLU()
        self.fc2 = nn.Linear(hidden_size, num_classes)
        
    def forward(self, x):
        x = x.view(x.size(0), 1, x.size(1), x.size(2))
        out = self.layer1(x)
        out = self.downsample(out)
        out = torch.squeeze(out, 1)
        if torch.cuda.is_available():
            x = x.cuda()
            h0 = Variable(torch.zeros(self.num_layers, out.size(0), self.hidden_size)).cuda() 
            c0 = Variable(torch.zeros(self.num_layers, out.size(0), self.hidden_size)).cuda()
        else:
            h0 = Variable(torch.zeros(self.num_layers, out.size(0), self.hidden_size)) 
            c0 = Variable(torch.zeros(self.num_layers, out.size(0), self.hidden_size))
        out, hn  = self.layer2(out, (h0, c0))
        out = hn[0][-1]
        out = self.drop1(out)
        out = self.fc1(out)
        out = self.drop2(out)
        out = self.relu1(out)
        out = self.fc2(out)
        out = torch.sigmoid(out)
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        with torch.no_grad():
            x = Variable(x)
            if torch.cuda.is_available():
                x = x.cuda()
            y = self.forward(x)
            temp = y.data.cpu().numpy()
            return temp


class CNN_MIX(nn.Module):
    """
    It is a deep network with 2 layers CNN, which works on input and time dimension, respectively, more details refer to deepDianosis in github.
    """
    def __init__(self, nb_filter, num_classes = 2, kernel_size = (1, 5), pool_size = (1, 3), labcounts = 32, window_size = 12, hidden_size = 100, stride = (1, 1), padding = 0):
        super(CNN_MIX, self).__init__()
        self.layer1 = nn.Sequential(
            nn.Conv2d(1, nb_filter, kernel_size = (labcounts, 1), stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU())
        self.layer2 = nn.Sequential(
            nn.Conv2d(1, nb_filter, kernel_size = (nb_filter, 1), stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU(),
            nn.MaxPool2d(pool_size))
        out1_size = int(np.ceil(float(window_size)/pool_size[1]))
        self.layer3 = nn.Sequential(
            nn.Conv2d(1, nb_filter, kernel_size = kernel_size, stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU())
        
        out2_size = (out1_size + 2*padding - (kernel_size[1] - 1) - 1)/stride[1] + 1
        self.drop1 = nn.Dropout(p=0.5)
        self.fc1 = nn.Linear(out2_size*nb_filter*nb_filter, hidden_size)
        self.drop2 = nn.Dropout(p=0.5)
        self.relu1 = nn.ReLU()
        self.fc2 = nn.Linear(hidden_size, num_classes)
        
    def forward(self, x):
        x = x.view(x.size(0), 1, x.size(1), x.size(2))
        out = self.layer1(x)
        out = out.view(out.size(0), out.size(2), out.size(1), out.size(3))
        out = self.layer2(out)
        out = out.view(out.size(0), out.size(2), out.size(1), out.size(3))
        out = self.layer3(out)
        out = out.view(out.size(0), -1)
        out = self.drop1(out)
        out = self.fc1(out)
        out = self.drop2(out)
        out = self.relu1(out)
        out = self.fc2(out)
        out = torch.sigmoid(out)
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        with torch.no_grad():
            x = Variable(x)
            if torch.cuda.is_available():
                x = x.cuda()
            y = self.forward(x)
            temp = y.data.cpu().numpy()
            return temp


class CNN_MULTI(nn.Module):
    """
    It is a deep network with multiple resolution, more details refer to multiresconvnet of deepDianosis in github.
    """
    def __init__(self, nb_filter, num_classes = 2, kernel_size = (1, 5), pool_size = (1, 2), labcounts = 32, window_size = 12, hidden_size = 100, stride = (1, 1), padding = 0):
        super(CNN_MULTI, self).__init__()
        # resolution 1
        self.pool1_1 = nn.MaxPool2d(pool_size, stride = pool_size)
        maxpool_size = (window_size + 2*padding - (pool_size[1] - 1) - 1)/pool_size[1] + 1
        self.pool1_2 = nn.MaxPool2d(pool_size, stride = pool_size)
        maxpool1_2_size = (maxpool_size + 2*padding - (pool_size[1] - 1) - 1)/pool_size[1] + 1
              
        self.layer1 = nn.Sequential(
            nn.Conv2d(1, nb_filter, kernel_size = kernel_size, stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU())
        cnn1_size = (maxpool1_2_size + 2*padding - (kernel_size[1] - 1) - 1)/stride[1] + 1
        #resolution 2
        self.pool2_1 = nn.MaxPool2d(pool_size, stride = pool_size)
        maxpool2_1_size = (window_size + 2*padding - (pool_size[1] - 1) - 1)/pool_size[1] + 1
              
        self.layer2 = nn.Sequential(
            nn.Conv2d(1, nb_filter, kernel_size = kernel_size, stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU())
        cnn2_size = (maxpool2_1_size + 2*padding - (kernel_size[1] - 1) - 1)/stride[1] + 1
        self.layer3 = nn.Sequential(
            nn.Conv2d(1, nb_filter, kernel_size = kernel_size, stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU(),
            nn.MaxPool2d(pool_size))
        cnn3_size = (window_size + 2*padding - (kernel_size[1] - 1) - 1)/stride[1] + 1
        maxpool3_size = (cnn3_size + 2*padding - (pool_size[1] - 1) - 1)/pool_size[1] + 1
        self.layer4 = nn.Sequential(
            nn.Conv2d(nb_filter, nb_filter, kernel_size = kernel_size, stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU())
        cnn4_size = (maxpool3_size + 2*padding - (kernel_size[1] - 1) - 1)/stride[1] + 1
        merge_size = cnn1_size + cnn2_size + cnn4_size
        self.drop1 = nn.Dropout(p=0.5)
        self.fc1 = nn.Linear(labcounts*nb_filter*merge_size, hidden_size)
        self.drop2 = nn.Dropout(p=0.5)
        self.relu1 = nn.ReLU()
        self.fc2 = nn.Linear(hidden_size, num_classes)
        
    def forward(self, x):
        x = x.view(x.size(0), 1, x.size(1), x.size(2))
        out = self.pool1_1(x)
        out = self.pool1_2(out)
        out1 = self.layer1(out)
        out = self.pool2_1(x)
        out2 = self.layer2(out)
        out = self.layer3(x)
        out3 = self.layer4(out)
        out = torch.cat((out1.view(out1.size(0), -1), out2.view(out2.size(0), -1), out3.view(out3.size(0), -1)), 1)
        out = self.drop1(out)
        out = self.fc1(out)
        out = self.drop2(out)
        out = self.relu1(out)
        out = self.fc2(out)
        out = torch.sigmoid(out)
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        with torch.no_grad():
            x = Variable(x)
            if torch.cuda.is_available():
                x = x.cuda()
            y = self.forward(x)
            temp = y.data.cpu().numpy()
            return temp


# 1x3 Convolution
def convR(in_channels, out_channels, kernel_size, stride=1, padding = (0, 1)):
    return nn.Conv2d(in_channels, out_channels, kernel_size=kernel_size, 
                     padding=padding, stride=stride, bias=False)


# Residual Block
class ResidualBlock(nn.Module):
    def __init__(self, in_channel, nb_filter = 16, kernel_size = (1, 3), stride=1, downsample=None):
        super(ResidualBlock, self).__init__()
        self.conv1 = convR(in_channel, nb_filter, kernel_size = kernel_size, stride = stride)
        self.bn1 = nn.BatchNorm2d(nb_filter)
        self.relu = nn.ReLU(inplace=True)
        self.conv2 = convR(nb_filter, nb_filter, kernel_size = kernel_size, stride = stride)
        self.bn2 = nn.BatchNorm2d(nb_filter)
        self.downsample = downsample
        
    def forward(self, x):
        residual = x
        out = self.conv1(x)
        out = self.bn1(out)
        out = self.relu(out)
        out = self.conv2(out)
        out = self.bn2(out)
        if self.downsample:
            residual = self.downsample(x)
        out += residual
        out = self.relu(out)
        return out


# ResNet Module
class ResNet(nn.Module):
    def __init__(self, block, layers, nb_filter = 16, labcounts = 12, window_size = 36, kernel_size = (1, 3), pool_size = (1, 3), num_classes=2, hidden_size = 100):
        super(ResNet, self).__init__()
        self.in_channels = 1
        self.conv = convR(self.in_channels, nb_filter, kernel_size = kernel_size)
        self.bn = nn.BatchNorm2d(nb_filter)
        self.relu = nn.ReLU(inplace=True)
        self.layer1 = self.make_layer(block, nb_filter, layers[0],  kernel_size = kernel_size)
        self.layer2 = self.make_layer(block, nb_filter*2, layers[1], 1, kernel_size = kernel_size, in_channels = nb_filter)
        self.layer3 = self.make_layer(block, nb_filter*4, layers[2], 1, kernel_size = kernel_size, in_channels = 2*nb_filter)
        self.avg_pool = nn.AvgPool2d(pool_size)
        avgpool2_1_size = (window_size - (pool_size[1] - 1) - 1)/pool_size[1] + 1
        last_layer_size = nb_filter*4*labcounts*avgpool2_1_size
        self.fc = nn.Linear(last_layer_size, hidden_size)
        self.drop2 = nn.Dropout(p=0.5)
        self.relu1 = nn.ReLU()
        self.fc2 = nn.Linear(hidden_size, num_classes)
        
    def make_layer(self, block, out_channels, blocks, stride=1,  kernel_size = (1, 3), in_channels = 16):
        downsample = None
        if (stride != 1) or (self.in_channels != out_channels):
            downsample = nn.Sequential(
                convR(in_channels, out_channels, kernel_size = kernel_size, stride=stride),
                nn.BatchNorm2d(out_channels))
        layers = []
        layers.append(block(in_channels, out_channels, kernel_size = kernel_size, stride = stride, downsample = downsample))
        self.in_channels = out_channels
        for i in range(1, blocks):
            layers.append(block(out_channels, out_channels, kernel_size = kernel_size))
        return nn.Sequential(*layers)
     
    def forward(self, x):
        x = x.view(x.size(0), 1, x.size(1), x.size(2))
        out = self.conv(x)
        out = self.bn(out)
        out = self.relu(out)
        out = self.layer1(out)
        out = self.layer2(out)
        out = self.layer3(out)
        out = self.avg_pool(out)
        out = out.view(out.size(0), -1)
        out = self.fc(out)
        out = self.drop2(out)
        out = self.relu1(out)
        out = self.fc2(out)
        out = torch.sigmoid(out)
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        with torch.no_grad():
            x = Variable(x)
            if torch.cuda.is_available():
                x = x.cuda()
            y = self.forward(x)
            temp = y.data.cpu().numpy()
            return temp

        
class GRU(nn.Module):
    """
    It is a deep network with one GRU layer, which are further fed into one fully connected layers.
    """
    def __init__(self, input_size, hidden_size, num_layers, num_classes = 2, dropout = 0.5):
        super(GRU, self).__init__()

        self.hidden_size = hidden_size
        self.num_layers = num_layers
        self.gru = nn.GRU(input_size, hidden_size, num_layers, batch_first = True, dropout = dropout)
        self.linear = nn.Linear(hidden_size, num_classes)

    def forward(self, x):
        if torch.cuda.is_available():
            x = x.cuda()
            h0 = Variable(torch.zeros(self.num_layers, x.size(0), self.hidden_size)).cuda() # 2 for bidirection
        else:
            h0 = Variable(torch.zeros(self.num_layers, x.size(0), self.hidden_size)) # 2 for bidirection
        self.gru.flatten_parameters()
        out, hn = self.gru(x, h0)

        rearranged = hn[-1]
        out = self.linear(rearranged)
        out = torch.sigmoid(out)
        return out

    def initHidden(self, N):
        return Variable(torch.randn(1, N, self.hidden_size))
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        with torch.no_grad():
            x = Variable(x)
            if torch.cuda.is_available():
                x = x.cuda()
            y = self.forward(x)
            temp = y.data.cpu().numpy()
            return temp


class RNN(nn.Module):
    """
    It is a deep network with one LSTM layer, which are further fed into one fully connected layer.
    """
    def __init__(self, input_size, hidden_size, num_layers, num_classes = 2, dropout = 0.5):
        super(RNN, self).__init__()
        self.hidden_size = hidden_size
        self.num_layers = num_layers
        self.lstm = nn.LSTM(input_size, hidden_size, num_layers, batch_first = True, dropout = dropout)
        self.fc = nn.Linear(hidden_size, num_classes)
    
    def forward(self, x):
        if torch.cuda.is_available():
            x = x.cuda()
            h0 = Variable(torch.zeros(self.num_layers, x.size(0), self.hidden_size)).cuda() 
            c0 = Variable(torch.zeros(self.num_layers, x.size(0), self.hidden_size)).cuda()
        else:
            h0 = Variable(torch.zeros(self.num_layers, x.size(0), self.hidden_size)) 
            c0 = Variable(torch.zeros(self.num_layers, x.size(0), self.hidden_size))
        self.lstm.flatten_parameters()
        out, hn = self.lstm(x, (h0, c0))
        rearranged = hn[0][-1]
        # Decode hidden state of last time step
        out = self.fc(rearranged)
        out = torch.sigmoid(out)
        return out

    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        with torch.no_grad():
            x = Variable(x)
            if torch.cuda.is_available():
                x = x.cuda()
            y = self.forward(x)
            temp = y.data.cpu().numpy()
            return temp


class BiRNN(nn.Module):
    """
    It is a deep network with one bidirectional LSTM layer, which are further fed into one fully connected layer.
    """
    def __init__(self, input_size, hidden_size, num_layers, num_classes = 2, dropout = 0.5):
        super(BiRNN, self).__init__()
        self.hidden_size = hidden_size
        self.num_layers = num_layers
        self.lstm = nn.LSTM(input_size, hidden_size, num_layers, 
                            batch_first = True, dropout = dropout, bidirectional=True)
        self.fc = nn.Linear(hidden_size*2, num_classes)  # 2 for bidirection 
    
    def forward(self, x):
        if torch.cuda.is_available():
            x = x.cuda()
            h0 = Variable(torch.zeros(self.num_layers*2, x.size(0), self.hidden_size)).cuda() # 2 for bidirection 
            c0 = Variable(torch.zeros(self.num_layers*2, x.size(0), self.hidden_size)).cuda()
        else:
            h0 = Variable(torch.zeros(self.num_layers*2, x.size(0), self.hidden_size)) # 2 for bidirection 
            c0 = Variable(torch.zeros(self.num_layers*2, x.size(0), self.hidden_size))
        self.lstm.flatten_parameters()
        out, hn = self.lstm(x, (h0, c0))
        hn = hn[0]

        rearranged = hn[-2:].view(x.size(0), -1)
        # Decode hidden state of last time step
        out = self.fc(rearranged)
        out = torch.sigmoid(out)
        return out

    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        with torch.no_grad():
            x = Variable(x)
            if torch.cuda.is_available():
                x = x.cuda()
            y = self.forward(x)
            temp = y.data.cpu().numpy()
            return temp


# select model
def train_deeptorch(population, plpData, train = True, model_type = 'LogisticRegression', class_weight =0,  autoencoder = True, w_decay =0.9, epochs = 1, vae = False, size = 100, loss = 'LogSoftmax', nbfilters = 4, learning_rate = 0.0001, hidden_size = 100, modelOutput = 'C:/deeptorch', seed = 1, quiet = False):
  if model_type in ['LogisticRegression', 'MLP', 'SNN']:
    y = population[:, 1]
    X = plpData[population[:, 0], :]
    trainInds = population[:, population.shape[1] - 1] > 0
    if class_weight == -1:
      loss = FocalLoss(gamma = 5)
    else:
      if class_weight == 0:
        weights = float(np.count_nonzero(y))/y.shape[0]
        class_weight = [1 - weights, weights]
      else:
        class_weight = [class_weight, 1]
      class_weight = 1/torch.Tensor(class_weight)
      if torch.cuda.is_available():
        class_weight = class_weight.cuda()
      loss=nn.CrossEntropyLoss(weight = class_weight)
    
    print("Dataset has %s rows and %s columns" % (X.shape[0], X.shape[1]))
    print("population loaded- %s rows and %s columns" % (np.shape(population)[0], np.shape(population)[1]))
    ###########################################################################
    l1regularization = False
    if train:
      pred_size = int(np.sum(population[:, population.shape[1] - 1] > 0))
      print("Calculating prediction for train set of size %s" % (pred_size))
      test_pred = np.zeros(pred_size)  # zeros length sum(population[:,population.size[1]] ==i)
      for i in range(1, int(np.max(population[:, population.shape[1] - 1]) + 1), 1):
        testInd = population[population[:, population.shape[1] - 1] > 0, population.shape[1] - 1] == i
        trainInd = (population[population[:, population.shape[1] - 1] > 0, population.shape[1] - 1] != i)
        train_x = X[trainInds, :][trainInd, :]
        train_y = y[trainInds][trainInd]
        test_x = X[trainInds, :][testInd, :]
        print("Fold %s split %s in train set and %s in test set" % (i, train_x.shape[0], test_x.shape[0]))
        print("Train set contains %s outcomes " % (np.sum(train_y)))
        train_x = train_x.toarray()
        test_x = test_x.toarray()
        if autoencoder:
          print('first train stakced autoencoder')
          encoding_size = 256
          if vae:
            auto_model = VAE(input_size=train_x.shape[1], encoding_size=encoding_size)
          else:
            auto_model = AutoEncoder(input_size=train_x.shape[1], encoding_size=encoding_size)
          if torch.cuda.is_available():
            auto_model = auto_model.cuda()
          clf = Estimator(auto_model)
          clf.compile(optimizer=torch.optim.Adam(auto_model.parameters(), lr=1e-3, weight_decay = w_decay),
                                loss=nn.MSELoss())
          clf.fit(train_x, train_y, batch_size=32, nb_epoch=epochs, autoencoder = autoencoder, vae = vae)
          #split to batch for large dataset
          train_batch = batch(train_x, batch_size=32)
          train_x = np.array([]).reshape(0, encoding_size)
          for train in train_batch:
            encode_train = auto_model.get_encode_features(train)
            train_x = np.concatenate((train_x, encode_train), axis=0)
          test_batch = batch(test_x, batch_size=32)
          test_x = np.array([]).reshape(0, encoding_size)
          for test in test_batch:
            encode_Test = auto_model.get_encode_features(test)
            test_x = np.concatenate((test_x, encode_Test), axis=0)
          del auto_model
          del clf
        # train on fold
        print("Training fold %s" % (i))
        start_time = timeit.default_timer()
        if model_type == 'LogisticRegression':
          model = LogisticRegression(train_x.shape[1])
          l1regularization = True
        elif model_type == 'SNN':
          model = SNN(train_x.shape[1], size)
        else:
          model = MLP(train_x.shape[1], size)
        
        if torch.cuda.is_available():
          model = model.cuda()
        clf = Estimator(model)
        clf.compile(optimizer=torch.optim.Adam(model.parameters(), lr=1e-3, weight_decay = w_decay),
                            loss=loss)
        clf.fit(train_x, train_y, batch_size=32, nb_epoch=epochs, l1regularization = l1regularization)
        
        ind = (population[:, population.shape[1] - 1] > 0)
        ind = population[ind, population.shape[1] - 1] == i
        test_input_var = torch.from_numpy(test_x.astype(np.float32))
        test_batch = batch(test_x, batch_size = 32)
        temp = []
        for test in test_batch:
          pred_test1 = model.predict_proba(test)[:, 1]
          temp = np.concatenate((temp, pred_test1), axis = 0)
        test_pred[ind] = temp
        print("Prediction complete: %s rows " % (np.shape(test_pred[ind])[0]))
        print("Mean: %s prediction value" % (np.mean(test_pred[ind])))

      # RETURN CV PREDICTION WHEN TRAIN == T
      test_pred.shape = (population[population[:, population.shape[1] - 1] > 0, :].shape[0], 1)
      prediction = np.append(population[population[:, population.shape[1] - 1] > 0, :], test_pred, axis=1)
      return prediction;
      
    # train final:
    else:
      print("Training final neural network model on all train data...")
      print("X- %s rows and Y %s length" % (X[trainInds, :].shape[0], y[trainInds].shape[0]))
      start_time = timeit.default_timer()
      train_x = X[trainInds, :]
      train_x = train_x.toarray()
      train_y = y[trainInds]
      if not os.path.exists(modelOutput):
        os.makedirs(modelOutput)
      if autoencoder:
        encoding_size = 256
        if vae:
          auto_model = VAE(input_size=train_x.shape[1], encoding_size=encoding_size)
        else:
          auto_model = AutoEncoder(input_size=train_x.shape[1], encoding_size=encoding_size)
        if torch.cuda.is_available():
          auto_model = auto_model.cuda()
        clf = Estimator(auto_model)
        clf.compile(optimizer=torch.optim.Adam(auto_model.parameters(), lr=1e-3, weight_decay=w_decay),
                            loss=nn.MSELoss())
        clf.fit(train_x, train_y, batch_size=32, nb_epoch=epochs, autoencoder=autoencoder, vae = vae)
        train_batch = batch(train_x, batch_size=32)
        train_x = np.array([]).reshape(0, encoding_size)
        for train in train_batch:
          encode_train = auto_model.get_encode_features(train)
          train_x = np.concatenate((train_x, encode_train), axis=0)
        joblib.dump(auto_model, os.path.join(modelOutput, 'autoencoder_model.pkl'))
        del auto_model
        del clf
      print('the final parameter epochs %.2f weight_decay %.2f' %(epochs,w_decay))
      if model_type == 'LogisticRegression':
        model = LogisticRegression(train_x.shape[1])
        l1regularization = True
      elif model_type == 'SNN':
        model = SNN(train_x.shape[1], size)
      else:
        model = MLP(train_x.shape[1], size)
      if torch.cuda.is_available():
        model = model.cuda()
      clf = Estimator(model)
      clf.compile(optimizer=torch.optim.Adam(model.parameters(), lr=1e-3, weight_decay = w_decay),
                        loss=loss)
      clf.fit(train_x, train_y, batch_size=32, nb_epoch=epochs, l1regularization = l1regularization)
      end_time = timeit.default_timer()
      print("Training final took: %.2f s" % (end_time - start_time))
      print("Model saved to: %s" % (modelOutput))
      joblib.dump(model, os.path.join(modelOutput,'model.pkl'))
      # DO PREDICTION ON TRAIN:
      train_batch = batch(train_x, batch_size = 32)
      train_pred = []
      for train in train_batch:
        preds = model.predict_proba(train)[:, 1]
        train_pred = np.concatenate((train_pred, preds), axis = 0)
      train_pred.shape = (population[population[:, population.shape[1] - 1] > 0, :].shape[0], 1)
      prediction = np.append(population[population[:, population.shape[1] - 1] > 0, :], train_pred, axis=1)
      # RETURN TRAIN PREDICTION WHEN TRAIN == F
      return prediction;
  
  elif model_type in ['CNN', 'RNN', 'CNN_LSTM', 'CNN_MLF', 'CNN_MIX', 'GRU', 'BiRNN', 'CNN_MULTI', 'ResNet']:
    y = population[:, 1]
    X = plpData.to_dense().numpy()
    X = X[np.int64(population[:, 0]), :]
    trainInds = population[:, population.shape[1] - 1] > 0
    
    if class_weight == -1:
      loss = FocalLoss(gamma = 3)
    else:
      if class_weight == 0:
        weights = float(np.count_nonzero(y))/y.shape[0]
        class_weight = [1 - weights, weights]
      else:
        class_weight = [class_weight, 1]
      class_weight = 1/torch.Tensor(class_weight)
      if torch.cuda.is_available():
        class_weight = class_weight.cuda()
      loss=nn.CrossEntropyLoss(weight = class_weight)
    
    if train:
      test_pred = np.zeros(population[population[:, population.shape[1] - 1] > 0, :].shape[0])  # zeros length sum(population[:,population.size[1]] ==i)
      for i in range(1, int(np.max(population[:, population.shape[1] - 1]) + 1), 1):
        testInd = population[population[:, population.shape[1] - 1] > 0, population.shape[1] - 1] == i
        trainInd = (population[population[:, population.shape[1] - 1] > 0, population.shape[1] - 1] != i)
        train_x = X[trainInds, :][trainInd, :]
        train_y = y[trainInds][trainInd]
        test_x = X[trainInds, :][testInd, :]
        print("Fold %s split %s in train set and %s in test set" % (i, train_x.shape[0], test_x.shape[0]))
        print("Train set contains %s outcomes " % (np.sum(train_y)))
        # train on fold
        learning_rate = 0.001
        print("Training fold %s" % (i))
        start_time = timeit.default_timer()
        if model_type == 'CNN':
          model = CNN(nb_filter = nbfilters, labcounts = train_x.shape[1], window_size = train_x.shape[2])
        elif model_type == 'CNN_LSTM':
          model = CNN_LSTM(nb_filter = nbfilters, labcounts=train_x.shape[1], window_size=train_x.shape[2])
        elif model_type == 'CNN_MLF': # multiple kernels with different size
          model = CNN_MLF(nb_filter = nbfilters, labcounts = train_x.shape[1], window_size = train_x.shape[2])
        elif model_type == 'CNN_MIX': # mixed model from deepDiagnosis
          model = CNN_MIX(nb_filter = nbfilters, labcounts = train_x.shape[1], window_size = train_x.shape[2])
        elif model_type == 'CNN_MULTI': # multiple resolution model from deepDiagnosis
          model = CNN_MULTI(nb_filter = nbfilters, labcounts = train_x.shape[1], window_size = train_x.shape[2])
        elif model_type == 'ResNet':
          print('train ResNet')
          model = ResNet(ResidualBlock, [3, 3, 3], nb_filter=nbfilters, labcounts=train_x.shape[1], window_size=train_x.shape[2])
        elif model_type == 'RNN':
          model = RNN(train_x.shape[2], hidden_size, 2, 2)
        elif model_type == 'BiRNN':
          model = BiRNN(train_x.shape[2], hidden_size, 2, 2)
        elif model_type == 'GRU':
          model = GRU(train_x.shape[2], hidden_size, 2, 2)
        else:
          print('temproal data not supported by this model')
          
        if torch.cuda.is_available():
          model = model.cuda()
        clf = Estimator(model)
        clf.compile(optimizer=torch.optim.Adam(model.parameters(), lr=learning_rate, weight_decay = 0.0001),
                            loss=loss)
        clf.fit(train_x, train_y, batch_size=32, nb_epoch=epochs)
        ind = (population[:, population.shape[1] - 1] > 0)
        ind = population[ind, population.shape[1] - 1] == i
        test_batch = batch(test_x, batch_size = 32)
        temp = []
        for test in test_batch:
          pred_test1 = model.predict_proba(test)[:, 1]
          temp = np.concatenate((temp, pred_test1), axis = 0)
        test_pred[ind] = temp
        del model
        print("Prediction complete: %s rows " % (np.shape(test_pred[ind])[0]))
        print("Mean: %s prediction value" % (np.mean(test_pred[ind])))
      # RETURN CV PREDICTION
      test_pred.shape = (population[population[:, population.shape[1] - 1] > 0, :].shape[0], 1)
      prediction = np.append(population[population[:, population.shape[1] - 1] > 0, :], test_pred, axis=1)
      return prediction;
      
    # train final:
    else:
      print("Training final neural network model on all train data...")
      print("X- %s rows and Y %s length" % (X[trainInds, :].shape[0], y[trainInds].shape[0]))
      start_time = timeit.default_timer()
      train_x = X[trainInds, :]
      train_y = y[trainInds]
      learning_rate = 0.001
      if model_type == 'CNN':
        model = CNN(nb_filter = nbfilters, labcounts = train_x.shape[1], window_size = train_x.shape[2])
      elif model_type == 'CNN_LSTM':
        model = CNN_LSTM(nb_filter=nbfilters, labcounts=train_x.shape[1], window_size=train_x.shape[2])
      elif model_type == 'CNN_MLF': # multiple kernels with different size
        model = CNN_MLF(nb_filter = nbfilters, labcounts = train_x.shape[1], window_size = train_x.shape[2])
      elif model_type == 'CNN_MIX': #mixed model from deepDiagnosis
        model = CNN_MIX(nb_filter = nbfilters, labcounts = train_x.shape[1], window_size = train_x.shape[2])
      elif model_type == 'CNN_MULTI': # multi resolution model from deepDiagnosis
        model = CNN_MULTI(nb_filter = nbfilters, labcounts = train_x.shape[1], window_size = train_x.shape[2])
      elif model_type == 'ResNet':
        model = ResNet(ResidualBlock, [3, 3, 3], nb_filter=nbfilters, labcounts=train_x.shape[1], window_size=train_x.shape[2])
      elif model_type == 'RNN':
        model = RNN(train_x.shape[2], hidden_size, 2, 2)
      elif model_type == 'BiRNN':
        model = BiRNN(train_x.shape[2], hidden_size, 2, 2)
      elif model_type == 'GRU':
        model = GRU(train_x.shape[2], hidden_size, 2, 2)
      else:
        print('temproal data not supported by this model')
      
      if torch.cuda.is_available():
        model = model.cuda()
      clf = Estimator(model)
      clf.compile(optimizer=torch.optim.Adam(model.parameters(), lr=learning_rate, weight_decay = 0.0001),
                        loss=loss)
      clf.fit(train_x, train_y, batch_size=32, nb_epoch=epochs)
      end_time = timeit.default_timer()
      print("Training final took: %.2f s" % (end_time - start_time))
      # save the model:
      if not os.path.exists(modelOutput):
        os.makedirs(modelOutput)
      print("Model saved to: %s" % (modelOutput))
      
      joblib.dump(model, os.path.join(modelOutput,'model.pkl'))
      # prediction on train:
      test_batch = batch(train_x, batch_size = 32)
      test_pred = []
      for test in test_batch:
        pred_test1 = model.predict_proba(test)[:, 1]
        test_pred = np.concatenate((test_pred, pred_test1), axis = 0)
      test_pred.shape = (population[population[:, population.shape[1] - 1] > 0, :].shape[0], 1)
      prediction = np.append(population[population[:, population.shape[1] - 1] > 0, :], test_pred, axis=1)
      return prediction;
