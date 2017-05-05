import sys
import pdb

import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.autograd import Variable
from torch.utils.data import DataLoader, TensorDataset

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
		model.train()
		for t in range(nb_epoch):
			loss, acc = self._fit(train_loader)
			val_log = ''
			if validation_data:
				val_loss, auc = self.evaluate(validation_data[0], validation_data[1], batch_size)
				val_log = "- val_loss: %06.4f - auc: %6.4f" % (val_loss, auc)
				print val_log
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
        temp = y.data.cpu().numpy().flatten()
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
        temp = y.data.cpu().numpy().flatten()
        return temp
            
class CNN(nn.Module):
    def __init__(self, nb_filter, num_classes = 2, kernel_size = (1, 5), pool_size = (1, 3), labcounts = 32, window_size = 12, hidden_size = 100, stride = (1, 1), padding = 0):
        super(CNN, self).__init__()
        self.layer1 = nn.Sequential(
            nn.Conv2d(1, nb_filter, kernel_size, stride = stride, padding = padding),
            nn.BatchNorm2d(nb_filter),
            nn.ReLU(),
            nn.MaxPool2d(pool_size, stride = stride))
        out1_size = (window_size + 2*padding - (kernel_size[1] - 1) - 1)/stride[0] + 1
        maxpool_size = (out1_size + 2*padding - (pool_size[1] - 1) - 1)/stride[0] + 1
        self.layer2 = nn.Sequential(
            nn.Conv2d(nb_filter, 2*nb_filter, kernel_size, stride = stride, padding = padding),
            nn.BatchNorm2d(2*nb_filter),
            nn.ReLU(),
            nn.MaxPool2d(pool_size, stride = stride))
        out2_size = (maxpool_size + 2*padding - (kernel_size[1] - 1) - 1)/stride[0] + 1
        maxpool2_size = (out2_size + 2*padding - (pool_size[1] - 1) - 1)/stride[0] + 1
        self.drop1 = nn.Dropout(p=0.5)
        self.fc1 = nn.Linear(maxpool2_size*labcounts*2*nb_filter, hidden_size)
        self.drop2 = nn.Dropout(p=0.5)
        self.relu1 = nn.ReLU()
        self.fc2 = nn.Linear(hidden_size, num_classes)
        
    def forward(self, x):
        x = np.expand_dims(x.data.cpu().numpy(), axis=1)
        if cuda:
            x= Variable(torch.from_numpy(x.astype(np.float32))).cuda()
        #pdb.set_trace()
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
        temp = y.data.cpu().numpy().flatten()
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
        
        out2_size = (out1_size + 2*padding - (kernel_size[1] - 1) - 1)/stride[0] + 1
        #print out2_size
        self.drop1 = nn.Dropout(p=0.5)
        self.fc1 = nn.Linear(out2_size*nb_filter*nb_filter, hidden_size)
        self.drop2 = nn.Dropout(p=0.5)
        self.relu1 = nn.ReLU()
        self.fc2 = nn.Linear(hidden_size, num_classes)
        
    def forward(self, x):
        x = np.expand_dims(x.data.cpu().numpy(), axis=1)
        if cuda:
            x= Variable(torch.from_numpy(x.astype(np.float32))).cuda()
        out = self.layer1(x)
        #pdb.set_trace()
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
        temp = y.data.cpu().numpy().flatten()
        return temp
    
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
        temp = y.data.cpu().numpy().flatten()
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
        temp = y.data.cpu().numpy().flatten()
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
        temp = y.data.cpu().numpy().flatten()
        return temp

if __name__ == "__main__":
    DATA_SIZE = 1000
    INPUT_SIZE = 36
    HIDDEN_SIZE = 100
    class_size = 2
    X = np.random.randn(DATA_SIZE * class_size, 18, INPUT_SIZE)
    y = np.array([i for i in range(class_size) for _ in range(DATA_SIZE)])
    
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=.2)
    model = CNN_MIX(nb_filter = 16, labcounts = X.shape[1], window_size = X.shape[2]) 
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
