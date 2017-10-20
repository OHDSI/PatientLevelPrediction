import sys
import os
import pdb
#os.environ["CUDA_VISIBLE_DEVICES"] = "2"
import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.autograd import Variable
from torch.utils.data import DataLoader, TensorDataset
from collections import OrderedDict
import timeit
from sklearn.externals import joblib
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_auc_score
import numpy as np

if torch.cuda.is_available():
        cuda = True
        torch.cuda.set_device(0)
        print('===> Using GPU')
        torch.backends.cudnn.enabled = True
        torch.backends.cudnn.benchmark = True
else:
        cuda = False
        print('===> Using CPU')
print 'GPU id', torch.cuda.current_device()

def batch(tensor, batch_size = 50):
    tensor_list = []
    length = tensor.shape[0]
    i = 0
    while True:
        if (i+1) * batch_size >= length:
            tensor_list.append(tensor[i * batch_size: length])
            return tensor_list
        tensor_list.append(tensor[i * batch_size: (i+1) * batch_size])
        i += 1

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

class FocalLoss1(nn.Module):
    def __init__(self, class_num = 2, alpha=None, gamma=2, size_average=True):
        super(FocalLoss1, self).__init__()
        if alpha is None:
            self.alpha = Variable(torch.ones(class_num, 1))
        else:
            if isinstance(alpha, Variable):
                self.alpha = alpha
            else:
                self.alpha = Variable(alpha)
        self.gamma = gamma
        self.class_num = class_num
        self.size_average = size_average

    def forward(self, inputs, targets):
        N = inputs.size(0)
        print(N)
        C = inputs.size(1)
        P = F.softmax(inputs)

        class_mask = inputs.data.new(N, C).fill_(0)
        class_mask = Variable(class_mask)
        ones = Variable(torch.Tensor(targets.size()).fill_(1))
        if cuda:
            class_mask = class_mask.cuda()
            ones = ones.cuda()
        ids = targets.view(-1, 1)
        class_mask.scatter_(1, ids, ones)

        if cuda:
            self.alpha = self.alpha.cuda()
        alpha = self.alpha[ids.data.view(-1)]
        
        probs = (P*class_mask).sum(1).view(-1,1)

        log_p = probs.log()

        batch_loss = -alpha*(torch.pow((1-probs), self.gamma))*log_p 
       
        if self.size_average:
            loss = batch_loss.mean()
        else:
            loss = batch_loss.sum()
        return loss

def one_hot(index, classes):
    size = index.size() + (classes,)
    view = index.size() + (1,)

    mask = torch.Tensor(*size).fill_(0)
    index = index.view(*view)
    ones = 1.

    if isinstance(index, Variable):
        ones = Variable(torch.Tensor(index.size()).fill_(1))
        mask = Variable(mask, volatile=index.volatile)
        if cuda:
               ones = ones.cuda()
               mask = mask.cuda()

    return mask.scatter_(1, index, ones)


class FocalLoss(nn.Module):

    def __init__(self, gamma=2, eps=1e-7):
        super(FocalLoss, self).__init__()
        self.gamma = gamma
        self.eps = eps

    def forward(self, input, target):
        y = one_hot(target, input.size(-1))
        logit = F.softmax(input)
        logit = logit.clamp(self.eps, 1. - self.eps)

        loss = -1 * y * torch.log(logit) # cross entropy
        loss = loss * (1 - logit) ** self.gamma # focal loss

        return loss.sum()  

class Estimator(object):

	def __init__(self, model):
		self.model = model

	def compile(self, optimizer, loss):
		self.optimizer = optimizer
		self.loss_f = loss

	def _fit(self, train_loader, l1regularization = False):
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
                        #print 'GPU id', torch.cuda.current_device()
			self.optimizer.zero_grad()
			#if torch.cuda.device_count() > 1:
			#net = torch.nn.DataParallel(self.model, device_ids = range(torch.cuda.device_count()))
			#if cuda:
			#	net = net.cuda()
			#y_pred = net(X_v)
			y_pred = self.model(X_v)
			loss = self.loss_f(y_pred, y_v)
			if l1regularization:
				l1_crit = nn.L1Loss(size_average=False)
				reg_loss = 0
				for param in self.model.parameters():
					target = Variable(torch.from_numpy(np.zeros(param.size()).astype(np.float32)))
					if cuda:
						target = target.cuda()
				reg_loss += l1_crit(param, target)
                        
				factor = 0.0005
				loss += factor * reg_loss
                
			loss.backward()
			self.optimizer.step()

			## for log
			loss_list.append(loss.data[0])
			classes = torch.topk(y_pred, 1)[1].data.cpu().numpy().flatten()
			acc = self._accuracy(classes, y_v.data.cpu().numpy().flatten())
			acc_list.append(acc)
			del loss
			del y_pred

		return sum(loss_list) / len(loss_list) , sum(acc_list) / len(acc_list)

	def fit(self, X, y, batch_size=32, nb_epoch=10, validation_data=(), l1regularization = False):
		train_set = TensorDataset(torch.from_numpy(X.astype(np.float32)),
                              torch.from_numpy(y.astype(np.float32)).long().view(-1))
		train_loader = DataLoader(dataset=train_set, batch_size=batch_size, shuffle=True)
		self.model.train()
		for t in range(nb_epoch):
			loss, acc = self._fit(train_loader, l1regularization= l1regularization)
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
		self.model.eval()
		return self.model.predict_proba(X)
		#eturn torch.topk(self.predict(X), 1)[1].data.numpy().flatten()


class LogisticRegression(nn.Module):
    def __init__(self, input_size, num_classes = 2):
        super(LogisticRegression, self).__init__()
        self.linear = nn.Linear(input_size, num_classes)

    def forward(self, x):
        out = self.linear(x)
        out = F.sigmoid(out)
        #out = F.softmax(out)
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x, volatile=True)
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
        x = Variable(x, volatile=True)
        if cuda:
            x = x.cuda()
        y = self.forward(x)
        temp = y.data.cpu().numpy()
        return temp

class AutoEncoder(nn.Module):
    def __init__(self, labcounts =  24, windows = 31):
        super(AutoEncoder, self).__init__()

        self.encoder = nn.Sequential(
            nn.Linear(labcounts*windows, 256),
            nn.Tanh(),
            nn.Linear(256, 128),
            nn.Tanh(),
            nn.Linear(128, 64),
            nn.Tanh(),
            nn.Linear(64, 32),  
        )
        self.decoder = nn.Sequential(
            nn.Linear(32, 64),
            nn.Tanh(),
            nn.Linear(64, 128),
            nn.Tanh(),
            nn.Linear(128, 256),
            nn.Tanh(),
            nn.Linear(256, labcounts*windows),
            nn.Sigmoid(),      
        )

    def forward(self, x):
        if cuda:
            x = x.cuda()
        encoded = self.encoder(x)
        decoded = self.decoder(encoded)
        return encoded, decoded
            
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
        out = F.sigmoid(out)
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x, volatile=True)
        if cuda:
            x = x.cuda()
        y = self.forward(x)
        temp = y.data.cpu().numpy()
        return temp
#allow multiple kernel with differnt kernel size
class CNN_MLF(nn.Module):
    def __init__(self, nb_filter, num_classes = 2, kernel_size = (1, 5), pool_size = (1, 3), labcounts = 32, window_size = 12, hidden_size = 100, stride = (1, 1), padding = 0):
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
        #x = np.expand_dims(x.data.cpu().numpy(), axis=1)
        #if cuda:
        #    x= Variable(torch.from_numpy(x.astype(np.float32))).cuda()
        x = x.view(x.size(0), 1, x.size(1), x.size(2))
	#pdb.set_trace()
        out1 = self.layer1(x)
        out2 = self.layer2(x)
        out3 = self.layer3(x)		
    	out = torch.cat((out1.view(out1.size(0), -1), out2.view(out2.size(0), -1), out3.view(out2.size(0), -1)), 1) 
        #out = out.view(out.size(0), -1)
        out = self.drop1(out)
        out = self.fc1(out)
        out = self.drop2(out)
        out = self.relu1(out)
        out = self.fc2(out)
        out = F.sigmoid(out)
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x, volatile=True)
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
        out = out[:, -1, :].squeeze()
        #pdb.set_trace()
        out = self.drop1(out)
        out = self.fc1(out)
        out = self.drop2(out)
        out = self.relu1(out)
        out = self.fc2(out)
        out = F.sigmoid(out)
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x, volatile=True)
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
        out = F.sigmoid(out)
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x, volatile=True)
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
        out = F.sigmoid(out)
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x, volatile=True)
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
        out = F.sigmoid(out)
        return out
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x, volatile=True)
        if cuda:
            x = x.cuda()
        y = self.forward(x)
        temp = y.data.cpu().numpy()
        return temp    
#resnet = ResNet(ResidualBlock, [3, 3, 3], nb_filter = 16)


def use_pretrained_embedding():
    embed = nn.Embedding(num_embeddings, embedding_dim)
    # pretrained_weight is a numpy matrix of shape (num_embeddings, embedding_dim)
    embed.weight.data.copy_(torch.from_numpy(pretrained_weight))
    return embed

class EMBED(nn.Module):

    def __init__(self, vocab_size, embedding_dim, context_size = 2):
        super(NGramLanguageModeler, self).__init__()
        self.embeddings = nn.Embedding(vocab_size, embedding_dim)
        self.linear1 = nn.Linear(context_size * embedding_dim, 128)
        self.linear2 = nn.Linear(128, vocab_size)

    def forward(self, inputs):
        embeds = self.embeddings(inputs).view((1, -1))
        out = F.relu(self.linear1(embeds))
        out = self.linear2(out)
        log_probs = F.log_softmax(out)
        return log_probs
        
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
        rearranged = out[:, -1, :].squeeze()
        out = self.linear(rearranged)
        out = F.sigmoid(out)
        return out

    def initHidden(self, N):
        return Variable(torch.randn(1, N, self.hidden_size))
    
    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x, volatile=True)
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
        out = self.fc(out[:, -1, :].squeeze())
        out = F.sigmoid(out)
        return out

    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x, volatile=True)
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
        out = self.fc(out[:, -1, :].squeeze())
        out = F.sigmoid(out)
        return out

    def predict_proba(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x, volatile=True)
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
    if class_weight == 0:
    	weights = float(np.count_nonzero(y))/y.shape[0]
    	class_weight = [1 - weights, weights]
    else:
    	class_weight = [class_weight, 1]
    class_weight = 1/torch.Tensor(class_weight)
    if cuda:
    	class_weight = class_weight.cuda()
    loss=nn.CrossEntropyLoss(weight = class_weight)
    if class_weight == -1:
    	loss = FocalLoss(gamma = 2)
    print "Dataset has %s rows and %s columns" % (X.shape[0], X.shape[1])
    print "population loaded- %s rows and %s columns" % (np.shape(population)[0], np.shape(population)[1])
    ###########################################################################
    l1regularization = False
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
                l1regularization = True
            else:
                model = MLP(train_x.shape[1], size)
                
            #model = ResNet(ResidualBlock, [3, 3, 3], nb_filter = 16, labcounts = X.shape[1], window_size = X.shape[2])
            #model = RNN(INPUT_SIZE, HIDDEN_SIZE, 2, class_size)
            #pdb.set_trace()
            if cuda:
                model = model.cuda()
            clf = Estimator(model)
            clf.compile(optimizer=torch.optim.Adam(model.parameters(), lr=1e-4, weight_decay = w_decay),
                        loss=loss)
            
            clf.fit(train_x.toarray(), train_y, batch_size=64, nb_epoch=epochs, l1regularization = l1regularization)
            
            ind = (population[:, population.shape[1] - 1] > 0)
            ind = population[ind, population.shape[1] - 1] == i
            
            test_input_var = torch.from_numpy(test_x.toarray().astype(np.float32))
            #if cuda:
            #    test_input_var = test_input_var.cuda()
    
            temp = model.predict_proba(test_input_var)[:, 1]
            #temp = preds.data.cpu().numpy().flatten()
            #print temp
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
            l1regularization = True
        else:
            model = MLP(train_x.shape[1], size)
        #model = ResNet(ResidualBlock, [3, 3, 3], nb_filter = 16, labcounts = X.shape[1], window_size = X.shape[2])
        #model = RNN(INPUT_SIZE, HIDDEN_SIZE, 2, class_size)
        #pdb.set_trace()
        if cuda:
            model = model.cuda()
        clf = Estimator(model)
        clf.compile(optimizer=torch.optim.Adam(model.parameters(), lr=1e-4, weight_decay = w_decay),
                    loss=loss)
        clf.fit(train_x.toarray(), train_y, batch_size=64, nb_epoch=epochs, l1regularization = l1regularization)

        end_time = timeit.default_timer()
        print "Training final took: %.2f s" % (end_time - start_time)
    
        # save the model:
        if not os.path.exists(modelOutput):
            os.makedirs(modelOutput)
        print "Model saved to: %s" % (modelOutput)
    
        joblib.dump(model, os.path.join(modelOutput,'model.pkl'))

elif model_type in ['CNN', 'RNN', 'CNN_MLF']:
    y = population[:, 1]
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

    #print full_covariates[:100], y[:100]
    trainInds = population[:, population.shape[1] - 1] > 0
    #print covariates
    print 'time_window', time_window
    X, patient_keys = convert_2_cnn_format(full_covariates, time_window = time_window)
    full_covariates = []
    print 'total patient', X.shape
    if class_weight == 0:
        weights = float(np.count_nonzero(y))/y.shape[0]
        class_weight = [1 - weights, weights]
    else:
        class_weight = [class_weight, 1]
    class_weight = 1/torch.Tensor(class_weight)
    if cuda:
    	class_weight = class_weight.cuda()
    loss=nn.CrossEntropyLoss(weight = class_weight)
    if class_weight == -1:
    	loss = FocalLoss(gamma = 2)
    trainInds = population[:, population.shape[1] - 1] > 0    
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
            print train_x.shape
            start_time = timeit.default_timer()
            if model_type == 'CNN':
                model = CNN(nb_filter = nbfilters, labcounts = train_x.shape[1], window_size = train_x.shape[2])
    	    elif model_type == 'CNN_MLF': # multiple kernels with different size
                model = CNN_MLF(nb_filter = nbfilters, labcounts = train_x.shape[1], window_size = train_x.shape[2])
    	    elif model_type == 'CNN_MIX': # mixed model from deepDiagnosis
                model = CNN_MIX(nb_filter = nbfilters, labcounts = train_x.shape[1], window_size = train_x.shape[2])
    	    elif model_type == 'CNN_MULTI': # multiple resolution model from deepDiagnosis 
                model = CNN_MULTI(nb_filter = nbfilters, labcounts = train_x.shape[1], window_size = train_x.shape[2])
            elif model_type == 'RNN':
                model = RNN(train_x.shape[2], hidden_size, 2, 2)
            elif model_type == 'GRU':
                model = GRU(train_x.shape[2], hidden_size, 2, 2)

            if cuda:
                model = model.cuda()
            clf = Estimator(model)
            clf.compile(optimizer=torch.optim.Adam(model.parameters(), lr=1e-4, weight_decay = 0.005),
                        loss=loss)
            
            clf.fit(train_x, train_y, batch_size=64, nb_epoch=epochs)
            
            ind = (population[:, population.shape[1] - 1] > 0)
            ind = population[ind, population.shape[1] - 1] == i
            
            test_batch = batch(test_x, batch_size = 50)
            temp = []
            for test in test_batch:
                pred_test1 = model.predict_proba(test)[:, 1]
                temp = np.concatenate((temp, pred_test1), axis = 0)
            #print ind, N, temp.shape, test_pred.shape
            #test_input_var = torch.from_numpy(test_x.astype(np.float32))
            #if cuda:
            #    test_input_var = test_input_var.cuda()
    
            #temp = model.predict_proba(test_input_var)[:, 1]
            #temp = preds.data.cpu().numpy().flatten()
    
            test_pred[ind] = temp
            del model
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
        #print 'the final parameter epochs', epochs, 'weight_decay', w_decay
        if model_type == 'CNN':
                model = CNN(nb_filter = nbfilters, labcounts = train_x.shape[1], window_size = train_x.shape[2])
    	elif model_type == 'CNN_MLF': # multiple kernels with different size
    		model = CNN_MLF(nb_filter = nbfilters, labcounts = train_x.shape[1], window_size = train_x.shape[2])
    	elif model_type == 'CNN_MIX': #mixed model from deepDiagnosis 
                model = CNN_MIX(nb_filter = nbfilters, labcounts = train_x.shape[1], window_size = train_x.shape[2])
    	elif model_type == 'CNN_MULTI': # multi resolution model from deepDiagnosis
                model = CNN_MULTI(nb_filter = nbfilters, labcounts = train_x.shape[1], window_size = train_x.shape[2])
        elif model_type == 'RNN':
                model = RNN(train_x.shape[2], hidden_size, 2, 2)
        elif model_type == 'GRU':
                model = GRU(train_x.shape[2], hidden_size, 2, 2)
        #model = ResNet(ResidualBlock, [3, 3, 3], nb_filter = 16, labcounts = X.shape[1], window_size = X.shape[2])
        #model = RNN(INPUT_SIZE, HIDDEN_SIZE, 2, class_size)
        #pdb.set_trace()
        if cuda:
            model = model.cuda()
        clf = Estimator(model)
        clf.compile(optimizer=torch.optim.Adam(model.parameters(), lr=1e-4, weight_decay = 0.005),
                    loss=loss)
        clf.fit(train_x, train_y, batch_size=64, nb_epoch=epochs)

        end_time = timeit.default_timer()
        print "Training final took: %.2f s" % (end_time - start_time)
    
        # save the model:
        if not os.path.exists(modelOutput):
            os.makedirs(modelOutput)
        print "Model saved to: %s" % (modelOutput)
    
        joblib.dump(model, os.path.join(modelOutput,'model.pkl'))
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
    
    print('Test score:', auc)
'''
