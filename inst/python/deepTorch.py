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

if "python_dir" in globals():
    #print python_dir
    sys.path.insert(0, python_dir)

import TorchUtils as tu


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
        self.fc2 = tu.selu()
        self.ad1 = tu.alpha_drop()
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
if __name__ == "__main__":
    if model_type in ['LogisticRegression', 'MLP', 'SNN']:
        y = population[:, 1]
        X = plpData[population[:, 0], :]
        trainInds = population[:, population.shape[1] - 1] > 0
        if class_weight == -1:
            loss = tu.FocalLoss(gamma = 5)
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
                    clf = tu.Estimator(auto_model)
                    clf.compile(optimizer=torch.optim.Adam(auto_model.parameters(), lr=1e-3, weight_decay = w_decay),
                                loss=nn.MSELoss())
                    clf.fit(train_x, train_y, batch_size=32, nb_epoch=epochs, autoencoder = autoencoder, vae = vae)
                    #split to batch for large dataset
                    train_batch = tu.batch(train_x, batch_size=32)
                    train_x = np.array([]).reshape(0, encoding_size)
                    for train in train_batch:
                        encode_train = auto_model.get_encode_features(train)
                        train_x = np.concatenate((train_x, encode_train), axis=0)
                    #train_x = auto_model.get_encode_features(train_x.toarray())
                    #test_x = auto_model.get_encode_features(test_x.toarray())
                    test_batch = tu.batch(test_x, batch_size=32)
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
                clf = tu.Estimator(model)
                clf.compile(optimizer=torch.optim.Adam(model.parameters(), lr=1e-3, weight_decay = w_decay),
                            loss=loss)
                #if not autoencoder:
                #    train_x = train_x.toarray()
                #    test_x = test_x.toarray()

                clf.fit(train_x, train_y, batch_size=32, nb_epoch=epochs, l1regularization = l1regularization)

                ind = (population[:, population.shape[1] - 1] > 0)
                ind = population[ind, population.shape[1] - 1] == i

                test_input_var = torch.from_numpy(test_x.astype(np.float32))

                test_batch = tu.batch(test_x, batch_size = 32)
                temp = []
                for test in test_batch:
                    pred_test1 = model.predict_proba(test)[:, 1]
                    temp = np.concatenate((temp, pred_test1), axis = 0)
                #temp = model.predict_proba(test_input_var)[:, 1]
                #temp = preds.data.cpu().numpy().flatten()
                #print temp
                test_pred[ind] = temp
                print("Prediction complete: %s rows " % (np.shape(test_pred[ind])[0]))
                print("Mean: %s prediction value" % (np.mean(test_pred[ind])))

            # merge pred with indexes[testInd,:]
            test_pred.shape = (population[population[:, population.shape[1] - 1] > 0, :].shape[0], 1)
            prediction = np.append(population[population[:, population.shape[1] - 1] > 0, :], test_pred, axis=1)

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
                clf = tu.Estimator(auto_model)
                clf.compile(optimizer=torch.optim.Adam(auto_model.parameters(), lr=1e-3, weight_decay=w_decay),
                            loss=nn.MSELoss())

                clf.fit(train_x, train_y, batch_size=32, nb_epoch=epochs, autoencoder=autoencoder, vae = vae)
                #train_x = auto_model.get_encode_features(train_x.toarray())
                train_batch = tu.batch(train_x, batch_size=32)
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

            #if not autoencoder:
            #    train_x = train_x.toarray()

            if torch.cuda.is_available():
                model = model.cuda()
            clf = tu.Estimator(model)
            clf.compile(optimizer=torch.optim.Adam(model.parameters(), lr=1e-3, weight_decay = w_decay),
                        loss=loss)
            clf.fit(train_x, train_y, batch_size=32, nb_epoch=epochs, l1regularization = l1regularization)

            end_time = timeit.default_timer()
            print("Training final took: %.2f s" % (end_time - start_time))

            # save the model:
            print("Model saved to: %s" % (modelOutput))

            joblib.dump(model, os.path.join(modelOutput,'model.pkl'))


    elif model_type in ['CNN', 'RNN', 'CNN_LSTM', 'CNN_MLF', 'CNN_MIX', 'GRU', 'BiRNN', 'CNN_MULTI', 'ResNet']:
        #print 'running model', model_type
        y = population[:, 1]
        #plpData = plpData[population[:, 0], :]
        #config = tf.ConfigProto()
        #config.gpu_options.allow_growth = True
        #with tf.Session(config=config) as sess:
        #    X = tf.sparse_reorder(plpData)
        #    X = tf.sparse_tensor_to_dense(X)
        #    X = sess.run(X)
            #tu.forward_impute_missing_value(X)
        X = plpData.to_dense().numpy()
        X = X[np.int64(population[:, 0]), :]
        '''
        p_ids_in_cov = set(covariates[:, 0])
        full_covariates = np.array([]).reshape(0,4)
        default_covid = covariates[0, 1]
        timeid_len = len(set(covariates[:, -2]))
        for p_id in  population[:, 0]:
            if p_id not in p_ids_in_cov:
                tmp_x = np.array([p_id, default_covid, 1, 0]).reshape(1,4) #default cov id, timeid=1
                full_covariates = np.concatenate((full_covariates, tmp_x), axis=0)
            else:
                tmp_x = covariates[covariates[:, 0] == p_id, :]
                full_covariates = np.concatenate((full_covariates, tmp_x), axis=0)

        trainInds = population[:, population.shape[1] - 1] > 0
        X, patient_keys = tu.convert_to_temporal_format(full_covariates, timeid_len= timeid_len)
        full_covariates = []
        '''
        if class_weight == -1:
            loss = tu.FocalLoss(gamma = 3)
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
        trainInds = population[:, population.shape[1] - 1] > 0
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
                clf = tu.Estimator(model)
                clf.compile(optimizer=torch.optim.Adam(model.parameters(), lr=learning_rate, weight_decay = 0.0001),
                            loss=loss)

                clf.fit(train_x, train_y, batch_size=32, nb_epoch=epochs)

                ind = (population[:, population.shape[1] - 1] > 0)
                ind = population[ind, population.shape[1] - 1] == i

                test_batch = tu.batch(test_x, batch_size = 32)
                temp = []
                for test in test_batch:
                    pred_test1 = model.predict_proba(test)[:, 1]
                    temp = np.concatenate((temp, pred_test1), axis = 0)

                test_pred[ind] = temp
                del model
                print("Prediction complete: %s rows " % (np.shape(test_pred[ind])[0]))
                print("Mean: %s prediction value" % (np.mean(test_pred[ind])))

            # merge pred with indexes[testInd,:]
            test_pred.shape = (population[population[:, population.shape[1] - 1] > 0, :].shape[0], 1)
            prediction = np.append(population[population[:, population.shape[1] - 1] > 0, :], test_pred, axis=1)

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
            clf = tu.Estimator(model)
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
            test_batch = tu.batch(train_x, batch_size = 32)
            test_pred = []
            for test in test_batch:
                pred_test1 = model.predict_proba(test)[:, 1]
                test_pred = np.concatenate((test_pred, pred_test1), axis = 0)
            test_pred.shape = (population.shape[0], 1)
            prediction = np.append(population, test_pred, axis=1)

'''
if __name__ == "__main__":
    DATA_SIZE = 1000
    INPUT_SIZE = 36
    HIDDEN_SIZE = 100
    class_size = 2
    #X = np.random.randn(DATA_SIZE * class_size, 18, INPUT_SIZE)
    X = np.random.randn(DATA_SIZE * class_size, INPUT_SIZE)
    y = np.array([i for i in range(class_size) for _ in range(DATA_SIZE)])
    
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=.2)
    model = LogisticRegression(X_train.shape[1])
    l1regularization = True
    #model = CNN_LSTM(nb_filter = 16, labcounts = X.shape[1], window_size = X.shape[2]) 
    #model = ResNet(ResidualBlock, [3, 3, 3], nb_filter = 16, labcounts = X.shape[1], window_size = X.shape[2])
    #model = RNN(INPUT_SIZE, HIDDEN_SIZE, 2, class_size)
    #pdb.set_trace()
    if cuda:
        model = model.cuda()
    clf = Estimator(model)
    clf.compile(optimizer=torch.optim.Adam(model.parameters(), lr=1e-4),
                loss=nn.CrossEntropyLoss())
    clf.fit(X_train, y_train, batch_size=64, nb_epoch=10,
            validation_data=(X_test, y_test), l1regularization = l1regularization)
    score, auc = clf.evaluate(X_test, y_test)
    
    print('Test score: %s' %(auc))
'''
