import sys
import os
import pdb

import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.autograd import Variable
from torch.utils.data import DataLoader, TensorDataset

import timeit
from sklearn.externals import joblib
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_auc_score
import numpy as np

if torch.cuda.is_available():
        cuda = True
        #torch.cuda.set_device(1)
        print('===> Using GPU')
else:
        cuda = False
        print('===> Using CPU')

def batch(tensor, batch_size):
    tensor_list = []
    length = tensor.shape[0]
    i = 0
    while True:
        if (i+1) * batch_size >= length:
            tensor_list.append(tensor[i * batch_size: length])
            return tensor_list
        tensor_list.append(tensor[i * batch_size: (i+1) * batch_size])
        i += 1

class Estimator(object):

	def __init__(self, model):
		self.model = model

	def compile(self, optimizer, loss):
		self.optimizer = optimizer
		self.loss_f = loss

	def _fit(self, train_loader):
		"""
		train one epoch
		"""
		loss_list = []
		acc_list = []
		for idx, (X, y) in enumerate(train_loader):
			X_v = Variable(X)
			y_v = Variable(y)
			if cuda:
				X_v = X_v.cuda()
				y_v = y_v.cuda()
                
			self.optimizer.zero_grad()
			y_pred = self.model(X_v)
			loss = self.loss_f(y_pred, y_v)
			loss.backward()
			self.optimizer.step()

			## for log
			loss_list.append(loss.data[0])
			classes = torch.topk(y_pred, 1)[1].data.cpu().numpy().flatten()
			acc = self._accuracy(classes, y_v.data.cpu().numpy().flatten())
			acc_list.append(acc)

		return sum(loss_list) / len(loss_list) , sum(acc_list) / len(acc_list)

	def fit(self, X, y, batch_size=32, nb_epoch=10, validation_data=()):
		#X_list = batch(X, batch_size)
		#y_list = batch(y, batch_size)
		#pdb.set_trace()
		train_set = TensorDataset(torch.from_numpy(X.astype(np.float32)),
                              torch.from_numpy(y.astype(np.float32)).long().view(-1))
		train_loader = DataLoader(dataset=train_set, batch_size=batch_size, shuffle=True)
		self.model.train()
		for t in range(nb_epoch):
			loss, acc = self._fit(train_loader)
			val_log = ''
			if validation_data:
				val_loss, auc = self.evaluate(validation_data[0], validation_data[1], batch_size)
				val_log = "- val_loss: %06.4f - auc: %6.4f" % (val_loss, auc)
				print val_log
		self.model.eval()
			#rint("Epoch %s/%s loss: %06.4f - acc: %06.4f %s" % (t, nb_epoch, loss, acc, val_log))

	def evaluate(self, X, y, batch_size=32):
		y_pred = self.predict(X)

		y_v = Variable(torch.from_numpy(y).long(), requires_grad=False)
		if cuda:
			y_v = y_v.cuda()
		loss = self.loss_f(y_pred, y_v)
		predict = y_pred.data.cpu().numpy()[:, 1].flatten()
		auc = roc_auc_score(y, predict)
		#lasses = torch.topk(y_pred, 1)[1].data.numpy().flatten()
		#cc = self._accuracy(classes, y)
		return loss.data[0], auc

	def _accuracy(self, y_pred, y):
		return float(sum(y_pred == y)) / y.shape[0]

	def predict(self, X):
		X = Variable(torch.from_numpy(X.astype(np.float32)))
		if cuda:
			X= X.cuda()		
		y_pred = self.model(X)
		return y_pred		

	def predict_proba( X):
		return self.model.predict_proba(X)
		#eturn torch.topk(self.predict(X), 1)[1].data.numpy().flatten()


class LogisticRegression(nn.Module):
    def __init__(self, input_size, num_classes = 2):
        super(LogisticRegression, self).__init__()
        self.linear = nn.Linear(input_size, num_classes)

    def forward(self, x):
        out = self.linear(x)
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x)
        if cuda:
            x = x.cuda()
        y = self.forward(x)
        temp = y.data.cpu().numpy()
        return temp
    
class MLP(nn.Module):
    def __init__(self, input_dim, hidden_size, num_classes = 2):
        super(MLP, self).__init__()
        self.fc1 = nn.Linear(input_dim, hidden_size)
                
        #self.fc2 = = nn.BatchNorm1d(hidden_size)
        self.fc2 = nn.Linear(hidden_size, num_classes)

    def forward(self, x):
        x = F.relu(self.fc1(x))
        x = F.dropout(x, training=self.training)
        #x = F.relu(self.fc2(x))
        #x = F.dropout(x, p=0.2, training=self.training)

        x = self.fc2(x)
                #x = F.dropout(x, training=self.training)
        #x = F.tanh(self.fc2(x))
        x = F.sigmoid(x)
        return x

    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x)
        if cuda:
            x = x.cuda()
        y = self.forward(x)
        temp = y.data.cpu().numpy()
        return temp
            
class CNN(nn.Module):
    def __init__(self, nb_filter, num_classes = 2, kernel_size = (1, 5), pool_size = (1, 3), labcounts = 32, window_size = 12, hidden_size = 100, stride = (1, 1), padding = 0):
        super(CNN, self).__init__()
        self.layer1 = nn.Sequential(
            nn.Conv2d(1, nb_filter, kernel_size, stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU(),
            nn.MaxPool2d(pool_size, stride = stride))
        out1_size = (window_size + 2*padding - (kernel_size[1] - 1) - 1)/stride[1] + 1
        maxpool_size = (out1_size + 2*padding - (pool_size[1] - 1) - 1)/stride[1] + 1
        self.layer2 = nn.Sequential(
            nn.Conv2d(nb_filter, 2*nb_filter, kernel_size, stride = stride, padding = padding),
            nn.BatchNorm2d(2*nb_filter),
            nn.ReLU(),
            nn.MaxPool2d(pool_size, stride = stride))
        out2_size = (maxpool_size + 2*padding - (kernel_size[1] - 1) - 1)/stride[1] + 1
        maxpool2_size = (out2_size + 2*padding - (pool_size[1] - 1) - 1)/stride[1] + 1
        self.drop1 = nn.Dropout(p=0.5)
        self.fc1 = nn.Linear(maxpool2_size*labcounts*2*nb_filter, hidden_size)
        self.drop2 = nn.Dropout(p=0.5)
        self.relu1 = nn.ReLU()
        self.fc2 = nn.Linear(hidden_size, num_classes)
        
    def forward(self, x):
        #x = np.expand_dims(x.data.cpu().numpy(), axis=1)
        #if cuda:
        #    x= Variable(torch.from_numpy(x.astype(np.float32))).cuda()
        x = x.view(x.size(0), 1, x.size(1), x.size(2))
        out = self.layer1(x)
        out = self.layer2(out)
        out = out.view(out.size(0), -1)
        out = self.drop1(out)
        out = self.fc1(out)
        out = self.drop2(out)
        out = self.relu1(out)
        out = self.fc2(out)
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x)
        if cuda:
            x = x.cuda()
        y = self.forward(x)
        temp = y.data.cpu().numpy()
        return temp

class CNN_LSTM(nn.Module):
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
        #x = np.expand_dims(x.data.cpu().numpy(), axis=1)
        #if cuda:
        #    x= Variable(torch.from_numpy(x.astype(np.float32))).cuda()
        x = x.view(x.size(0), 1, x.size(1), x.size(2))
        out = self.layer1(x)
        out = self.downsample(out)
        out = torch.squeeze(out, 1)
        #pdb.set_trace()
        if cuda:
            x = x.cuda()
            h0 = Variable(torch.zeros(self.num_layers, out.size(0), self.hidden_size)).cuda() 
            c0 = Variable(torch.zeros(self.num_layers, out.size(0), self.hidden_size)).cuda()
        else:
            h0 = Variable(torch.zeros(self.num_layers, out.size(0), self.hidden_size)) 
            c0 = Variable(torch.zeros(self.num_layers, out.size(0), self.hidden_size))
        out, _  = self.layer2(out, (h0, c0))
        out = out[:, -1, :]
        #pdb.set_trace()
        out = self.drop1(out)
        out = self.fc1(out)
        out = self.drop2(out)
        out = self.relu1(out)
        out = self.fc2(out)
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x)
        if cuda:
            x = x.cuda()
        y = self.forward(x)
        temp = y.data.cpu().numpy()
        return temp
 
class CNN_MIX(nn.Module):
    def __init__(self, nb_filter, num_classes = 2, kernel_size = (1, 5), pool_size = (1, 3), labcounts = 32, window_size = 12, hidden_size = 100, stride = (1, 1), padding = 0):
        super(CNN_MIX, self).__init__()
        self.layer1 = nn.Sequential(
            nn.Conv2d(1, nb_filter, kernel_size = (labcounts, 1), stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU())
        #self.reshape1 = nn.Reshape(1, nb_filter, window_size)
        #out1_size = (labcounts + 2*padding - (kernel_size[1] - 1) - 1)/stride[0] + 1
        #maxpool_size = (out1_size + 2*padding - (pool_size[1] - 1) - 1)/stride[0] + 1
        
        self.layer2 = nn.Sequential(
            nn.Conv2d(1, nb_filter, kernel_size = (nb_filter, 1), stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU(),
            nn.MaxPool2d(pool_size))
        #self.reshape2 = nn.Reshape(1, nb_filter, window_size)
        out1_size = int(np.ceil(float(window_size)/pool_size[1]))
        #print out1_size
        self.layer3 = nn.Sequential(
            nn.Conv2d(1, nb_filter, kernel_size = kernel_size, stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU())
        
        out2_size = (out1_size + 2*padding - (kernel_size[1] - 1) - 1)/stride[1] + 1
        #print out2_size
        self.drop1 = nn.Dropout(p=0.5)
        self.fc1 = nn.Linear(out2_size*nb_filter*nb_filter, hidden_size)
        self.drop2 = nn.Dropout(p=0.5)
        self.relu1 = nn.ReLU()
        self.fc2 = nn.Linear(hidden_size, num_classes)
        
    def forward(self, x):
        #x = np.expand_dims(x.data.cpu().numpy(), axis=1)
        #if cuda:
        #    x= Variable(torch.from_numpy(x.astype(np.float32))).cuda()
        x = x.view(x.size(0), 1, x.size(1), x.size(2))
        out = self.layer1(x)
        #x = x.view(x.size(0), 1, x.size(1), x.size(2))
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
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x)
        if cuda:
            x = x.cuda()
        y = self.forward(x)
        temp = y.data.cpu().numpy()
        return temp

class CNN_MULTI(nn.Module):
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
        #self.reshape1 = nn.Reshape(1, nb_filter, window_size)
        cnn1_size = (maxpool1_2_size + 2*padding - (kernel_size[1] - 1) - 1)/stride[1] + 1
        #maxpool_size = (out1_size + 2*padding - (pool_size[1] - 1) - 1)/stride[0] + 1
        #resolution 2
        self.pool2_1 = nn.MaxPool2d(pool_size, stride = pool_size)
        maxpool2_1_size = (window_size + 2*padding - (pool_size[1] - 1) - 1)/pool_size[1] + 1
              
        self.layer2 = nn.Sequential(
            nn.Conv2d(1, nb_filter, kernel_size = kernel_size, stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU())
        #self.reshape1 = nn.Reshape(1, nb_filter, window_size)
        cnn2_size = (maxpool2_1_size + 2*padding - (kernel_size[1] - 1) - 1)/stride[1] + 1
        #maxpool_size = (out1_size + 2*padding - (pool_size[1] - 1) - 1)/stride[0] + 1
        #resolution 3
        self.layer3 = nn.Sequential(
            nn.Conv2d(1, nb_filter, kernel_size = kernel_size, stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU(),
            nn.MaxPool2d(pool_size))
        cnn3_size = (window_size + 2*padding - (kernel_size[1] - 1) - 1)/stride[1] + 1
        maxpool3_size = (cnn3_size + 2*padding - (pool_size[1] - 1) - 1)/pool_size[1] + 1
        #print out1_size
        self.layer4 = nn.Sequential(
            nn.Conv2d(nb_filter, nb_filter, kernel_size = kernel_size, stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU())
        cnn4_size = (maxpool3_size + 2*padding - (kernel_size[1] - 1) - 1)/stride[1] + 1
        #print out2_size
        merge_size = cnn1_size + cnn2_size + cnn4_size
        self.drop1 = nn.Dropout(p=0.5)
        self.fc1 = nn.Linear(labcounts*nb_filter*merge_size, hidden_size)
        self.drop2 = nn.Dropout(p=0.5)
        self.relu1 = nn.ReLU()
        self.fc2 = nn.Linear(hidden_size, num_classes)
        
    def forward(self, x):
        #x = np.expand_dims(x.data.cpu().numpy(), axis=1)
        #if cuda:
        #    x= Variable(torch.from_numpy(x.astype(np.float32))).cuda()
        #pdb.set_trace()
        x = x.view(x.size(0), 1, x.size(1), x.size(2))
        out = self.pool1_1(x)
        out = self.pool1_2(out)
        out1 = self.layer1(out)
        #pdb.set_trace()
        out = self.pool2_1(x)
        out2 = self.layer2(out)
        #out = out.view(out.size(0), out.size(2), out.size(1), out.size(3))
        out = self.layer3(x)
        out3 = self.layer4(out)
        out = torch.cat((out1.view(out1.size(0), -1), out2.view(out2.size(0), -1), out3.view(out3.size(0), -1)), 1) 
        #x = x.view(-1, 50 * 3)
        #out = out.view(out.size(0), -1)
        out = self.drop1(out)
        out = self.fc1(out)
        out = self.drop2(out)
        out = self.relu1(out)
        out = self.fc2(out)
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x)
        if cuda:
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
        #print x.data.cpu().numpy().shape
        x = x.view(x.size(0), 1, x.size(1), x.size(2))
        out = self.conv(x)
        out = self.bn(out)
        out = self.relu(out)
        out = self.layer1(out)
        #pdb.set_trace()
        #print self.layer2
        out = self.layer2(out)
        out = self.layer3(out)
        out = self.avg_pool(out)
        #pdb.set_trace()
        out = out.view(out.size(0), -1)
        out = self.fc(out)
        out = self.drop2(out)
        out = self.relu1(out)
        out = self.fc2(out)
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x)
        if cuda:
            x = x.cuda()
        y = self.forward(x)
        temp = y.data.cpu().numpy()
        return temp    
#resnet = ResNet(ResidualBlock, [3, 3, 3], nb_filter = 16)
        
class GRU(nn.Module):
    def __init__(self, input_size, hidden_size, num_layers, num_classes = 2, dropout = 0.5):
        super(GRU, self).__init__()

        self.hidden_size = hidden_size
        self.num_layers = num_layers
        self.gru = nn.GRU(input_size, hidden_size, num_layers, batch_first = True, dropout = dropout)
        self.linear = nn.Linear(hidden_size, num_classes)

    def forward(self, x):
        #x = Variable(torch.from_numpy(np.swapaxes(x.data.cpu().numpy(),0,1).astype(np.float32)))
        if cuda:
            x = x.cuda()
            h0 = Variable(torch.zeros(self.num_layers, x.size(0), self.hidden_size)).cuda() # 2 for bidirection 
            #c0 = Variable(torch.zeros(self.num_layers*2, x.size(0), self.hidden_size)).cuda()
        else:
            h0 = Variable(torch.zeros(self.num_layers, x.size(0), self.hidden_size)) # 2 for bidirection 
            #c0 = Variable(torch.zeros(self.num_layers*2, x.size(0), self.hidden_size))
        '''if cuda:
            x = Variable(torch.from_numpy(np.swapaxes(x.data.cpu().numpy(),0,1).astype(np.float32))).cuda()
        else:
            x = Variable(torch.from_numpy(np.swapaxes(x.data.cpu().numpy(),0,1).astype(np.float32)))
        '''
        out, hn = self.gru(x, h0)
        ## from (1, N, hidden) to (N, hidden)
        rearranged = out[:, -1, :]
        out = self.linear(rearranged)
        return out

    def initHidden(self, N):
        return Variable(torch.randn(1, N, self.hidden_size))
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x)
        if cuda:
            x = x.cuda()
        y = self.forward(x)
        temp = y.data.cpu().numpy()
        return temp

class RNN(nn.Module):
    def __init__(self, input_size, hidden_size, num_layers, num_classes = 2, dropout = 0.5):
        super(RNN, self).__init__()
        self.hidden_size = hidden_size
        self.num_layers = num_layers
        self.lstm = nn.LSTM(input_size, hidden_size, num_layers, batch_first = True, dropout = dropout)
        self.fc = nn.Linear(hidden_size, num_classes)
    
    def forward(self, x):
        if cuda:
            x = x.cuda()
            h0 = Variable(torch.zeros(self.num_layers, x.size(0), self.hidden_size)).cuda() 
            c0 = Variable(torch.zeros(self.num_layers, x.size(0), self.hidden_size)).cuda()
        else:
            h0 = Variable(torch.zeros(self.num_layers, x.size(0), self.hidden_size)) 
            c0 = Variable(torch.zeros(self.num_layers, x.size(0), self.hidden_size))
        
        # Forward propagate RNN
        #pdb.set_trace()
        '''
        if cuda:
            x = Variable(torch.from_numpy(np.swapaxes(x.data.cpu().numpy(),0,1).astype(np.float32))).cuda()
        else:
            x = Variable(torch.from_numpy(np.swapaxes(x.data.cpu().numpy(),0,1).astype(np.float32)))
        '''    
        out, _ = self.lstm(x, (h0, c0))  
        
        # Decode hidden state of last time step
        out = self.fc(out[:, -1, :])  
        return out

    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x)
        if cuda:
            x = x.cuda()
        y = self.forward(x)
        temp = y.data.cpu().numpy()
        return temp

class BiRNN(nn.Module):
    def __init__(self, input_size, hidden_size, num_layers, num_classes = 2, dropout = 0.5):
        super(BiRNN, self).__init__()
        self.hidden_size = hidden_size
        self.num_layers = num_layers
        self.lstm = nn.LSTM(input_size, hidden_size, num_layers, 
                            batch_first = True, dropout = dropout, bidirectional=True)
        self.fc = nn.Linear(hidden_size*2, num_classes)  # 2 for bidirection 
    
    def forward(self, x):
        if cuda:
            x = x.cuda()
            h0 = Variable(torch.zeros(self.num_layers*2, x.size(0), self.hidden_size)).cuda() # 2 for bidirection 
            c0 = Variable(torch.zeros(self.num_layers*2, x.size(0), self.hidden_size)).cuda()
        else:
            h0 = Variable(torch.zeros(self.num_layers*2, x.size(0), self.hidden_size)) # 2 for bidirection 
            c0 = Variable(torch.zeros(self.num_layers*2, x.size(0), self.hidden_size))
        # Forward propagate RNN
	'''
        if cuda:
            x = Variable(torch.from_numpy(np.swapaxes(x.data.cpu().numpy(),0,1).astype(np.float32))).cuda()
        else:
            x = Variable(torch.from_numpy(np.swapaxes(x.data.cpu().numpy(),0,1).astype(np.float32)))
        '''
        out, _ = self.lstm(x, (h0, c0))
        
        # Decode hidden state of last time step
        out = self.fc(out[:, -1, :])
        return out

    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x)
        if cuda:
            x = x.cuda()
        y = self.forward(x)
        temp = y.data.cpu().numpy()
        return temp


# select model
if model_type in ['LogisticRegression', 'MLP']:
    y = population[:, 1]
    X = plpData[population[:, 0], :]
    trainInds = population[:, population.shape[1] - 1] > 0
    
    print "Dataset has %s rows and %s columns" % (X.shape[0], X.shape[1])
    print "population loaded- %s rows and %s columns" % (np.shape(population)[0], np.shape(population)[1])
    ###########################################################################
    
    if train:
        pred_size = int(np.sum(population[:, population.shape[1] - 1] > 0))
        print "Calculating prediction for train set of size %s" % (pred_size)
        test_pred = np.zeros(pred_size)  # zeros length sum(population[:,population.size[1]] ==i)
        for i in range(1, int(np.max(population[:, population.shape[1] - 1]) + 1), 1):
            testInd = population[population[:, population.shape[1] - 1] > 0, population.shape[1] - 1] == i
            trainInd = (population[population[:, population.shape[1] - 1] > 0, population.shape[1] - 1] != i)
            train_x = X[trainInds, :][trainInd, :]
            train_y = y[trainInds][trainInd]
    
            test_x = X[trainInds, :][testInd, :]
            print "Fold %s split %s in train set and %s in test set" % (i, train_x.shape[0], test_x.shape[0])
            print "Train set contains %s outcomes " % (np.sum(train_y))
    
            # train on fold
            print "Training fold %s" % (i)
            start_time = timeit.default_timer()
            if model_type == 'LogisticRegression':
                model = LogisticRegression(train_x.shape[1])
                
            #model = ResNet(ResidualBlock, [3, 3, 3], nb_filter = 16, labcounts = X.shape[1], window_size = X.shape[2])
            #model = RNN(INPUT_SIZE, HIDDEN_SIZE, 2, class_size)
            #pdb.set_trace()
            if cuda:
                model = model.cuda()
            clf = Estimator(model)
            clf.compile(optimizer=torch.optim.Adam(model.parameters(), lr=1e-4, weight_decay = w_decay),
                        loss=nn.CrossEntropyLoss())
            clf.fit(train_x.toarray(), train_y, batch_size=64, nb_epoch=epochs)
            
            ind = (population[:, population.shape[1] - 1] > 0)
            ind = population[ind, population.shape[1] - 1] == i
            
            test_input_var = torch.from_numpy(test_x.toarray().astype(np.float32))
            #if cuda:
            #    test_input_var = test_input_var.cuda()
    
            temp = model.predict_proba(test_input_var)[:, 1]
            #temp = preds.data.cpu().numpy().flatten()
    
            test_pred[ind] = temp
            print "Prediction complete: %s rows " % (np.shape(test_pred[ind])[0])
            print "Mean: %s prediction value" % (np.mean(test_pred[ind]))
    
        # merge pred with indexes[testInd,:]
        test_pred.shape = (population[population[:, population.shape[1] - 1] > 0, :].shape[0], 1)
        prediction = np.append(population[population[:, population.shape[1] - 1] > 0, :], test_pred, axis=1)
    
    # train final:
    else:
        print "Training final neural network model on all train data..."
        print "X- %s rows and Y %s length" % (X[trainInds, :].shape[0], y[trainInds].shape[0])
    
        start_time = timeit.default_timer()
    
        train_x = X[trainInds, :]
        train_y = y[trainInds]
        print 'the final parameter epochs', epochs, 'weight_decay', w_decay
        if model_type == 'LogisticRegression':
            model = LogisticRegression(train_x.shape[1])
        #model = ResNet(ResidualBlock, [3, 3, 3], nb_filter = 16, labcounts = X.shape[1], window_size = X.shape[2])
        #model = RNN(INPUT_SIZE, HIDDEN_SIZE, 2, class_size)
        #pdb.set_trace()
        if cuda:
            model = model.cuda()
        clf = Estimator(model)
        clf.compile(optimizer=torch.optim.Adam(model.parameters(), lr=1e-4, weight_decay = w_decay),
                    loss=nn.CrossEntropyLoss())
        clf.fit(train_x.toarray(), train_y, batch_size=64, nb_epoch=epochs)

        end_time = timeit.default_timer()
        print "Training final took: %.2f s" % (end_time - start_time)
    
        # save the model:
        if not os.path.exists(modelOutput):
            os.makedirs(modelOutput)
        print "Model saved to: %s" % (modelOutput)
    
        joblib.dump(model, os.path.join(modelOutput,'model.pkl'))

if __name__ == "__main__":
    DATA_SIZE = 1000
    INPUT_SIZE = 36
    HIDDEN_SIZE = 100
    class_size = 2
    X = np.random.randn(DATA_SIZE * class_size, 18, INPUT_SIZE)
    y = np.array([i for i in range(class_size) for _ in range(DATA_SIZE)])
    
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=.2)
    model = CNN_LSTM(nb_filter = 16, labcounts = X.shape[1], window_size = X.shape[2]) 
    #model = ResNet(ResidualBlock, [3, 3, 3], nb_filter = 16, labcounts = X.shape[1], window_size = X.shape[2])
    #model = RNN(INPUT_SIZE, HIDDEN_SIZE, 2, class_size)
    #pdb.set_trace()
    if cuda:
        model = model.cuda()
    clf = Estimator(model)
    clf.compile(optimizer=torch.optim.Adam(model.parameters(), lr=1e-4),
                loss=nn.CrossEntropyLoss())
    clf.fit(X_train, y_train, batch_size=64, nb_epoch=10,
            validation_data=(X_test, y_test))
    score, auc = clf.evaluate(X_test, y_test)
    
    print('Test score:', auc)
