import sys
import pdb

import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.autograd import Variable

if torch.cuda.is_available():
        cuda = True
        #torch.cuda.set_device(1)
        print('===> Using GPU')
else:
        cuda = False
        print('===> Using CPU')

class MLP(nn.Module):
    def __init__(self, input_dim, hidden_size):
        super(MLP, self).__init__()
        self.fc1 = nn.Linear(input_dim, hidden_size)
                
                #self.fc2 = = nn.BatchNorm1d(hidden_size)
        self.fc2 = nn.Linear(hidden_size, 1)

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
    def __init__(self, nb_filter, kernel_size = 5, pool_size = 3, padding=2, labcounts = 32, window_size = 12, hidden_szie = 100):
        super(CNN, self).__init__()
        self.layer1 = nn.Sequential(
            nn.Conv1d(1, nb_filter, kernel_size, padding),
            nn.BatchNorm1d(nb_filter),
            nn.ReLU(),
            nn.MaxPool1d(pool_size))
        self.layer2 = nn.Sequential(
            nn.Conv1d(nb_filter, 2*nb_filter, kernel_size, padding),
            nn.BatchNorm1d(2*nb_filter),
            nn.ReLU(),
            nn.MaxPool1d(pool_size))
        self.drop1 = nn.Dropout(p=0.5)
        self.fc1 = nn.Linear(window_size*labcounts*32, hidden_szie)
        self.drop2 = nn.Dropout(p=0.5)
        self.relu1 = nn.ReLU()
        self.fc2 = nn.Linear(hidden_szie, 1)
        
    def forward(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x)
        if cuda:
            x = x.cuda()
        out = self.layer1(x)
        out = self.layer2(out)
        out = out.view(out.size(0), -1)
        out = self.drop1(out)
        out = self.fc1(out)
        out = self.drop2(out)
        out = self.relu1(out)
        out = self.fc2(out)
        out = out.data.cpu().numpy().flatten()
        return out

class GRU(nn.Module):
    def __init__(self, input_size, hidden_size, output_size):
        super(GRU, self).__init__()

        self.hidden_size = hidden_size

        self.gru = nn.GRU(input_size, hidden_size)
        self.linear = nn.Linear(hidden_size, output_size)

    def forward(self, x, hidden):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x)
        if cuda:
            x = x.cuda()

        _, hn = self.gru(x, hidden)
        ## from (1, N, hidden) to (N, hidden)
        rearranged = hn.view(hn.size()[1], hn.size(2))
        out = self.linear(rearranged)
        out = out.data.cpu().numpy().flatten()
        return out

    def initHidden(self, N):
        return Variable(torch.randn(1, N, self.hidden_size))

class RNN(nn.Module):
    def __init__(self, input_size, hidden_size, num_layers, num_classes):
        super(RNN, self).__init__()
        self.hidden_size = hidden_size
        self.num_layers = num_layers
        self.lstm = nn.LSTM(input_size, hidden_size, num_layers, batch_first=True)
        self.fc = nn.Linear(hidden_size, num_classes)
    
    def forward(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x)
        if cuda:
            x = x.cuda()

        # Set initial states 
        h0 = Variable(torch.zeros(self.num_layers, x.size(0), self.hidden_size)) 
        c0 = Variable(torch.zeros(self.num_layers, x.size(0), self.hidden_size))
        
        # Forward propagate RNN
        out, _ = self.lstm(x, (h0, c0))  
        
        # Decode hidden state of last time step
        out = self.fc(out[:, -1, :])  
        out = out.data.cpu().numpy().flatten()
        return out

class BiRNN(nn.Module):
    def __init__(self, input_size, hidden_size, num_layers, num_classes):
        super(BiRNN, self).__init__()
        self.hidden_size = hidden_size
        self.num_layers = num_layers
        self.lstm = nn.LSTM(input_size, hidden_size, num_layers, 
                            batch_first=True, bidirectional=True)
        self.fc = nn.Linear(hidden_size*2, num_classes)  # 2 for bidirection 
    
    def forward(self, x):
        if type(x) is np.ndarray:
            x = torch.from_numpy(x.astype(np.float32))
        x = Variable(x)
        if cuda:
            x = x.cuda()

        # Set initial states
        h0 = Variable(torch.zeros(self.num_layers*2, x.size(0), self.hidden_size)) # 2 for bidirection 
        c0 = Variable(torch.zeros(self.num_layers*2, x.size(0), self.hidden_size))
        
        # Forward propagate RNN
        out, _ = self.lstm(x, (h0, c0))
        
        # Decode hidden state of last time step
        out = self.fc(out[:, -1, :])
        out = out.data.cpu().numpy().flatten()
        return out



