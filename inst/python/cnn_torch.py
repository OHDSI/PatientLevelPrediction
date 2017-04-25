#  Neural network classification with n fold cross validation
# ===============================================================
# INPUT:
# 1) location of files: libsvm file + indexes file (rowId, index)
# 2)
#
# OUTPUT:
# it returns a file with indexes merged with prediction for test index
# ================================================================
import numpy as np
# from collections import OrderedDict
import os
import timeit
from sklearn.externals import joblib
#import pdb
import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.autograd import Variable
from torch.utils.data import DataLoader, TensorDataset

if torch.cuda.is_available():
        cuda = True
        print('===> Using GPU')
else:
        cuda = False
        print('===> Using CPU')


class CNN(nn.Module):
    def __init__(self, nb_filter, kernel_size = (1, 5), pool_size = (1, 3), labcounts = 32, window_size = 12, hidden_szie = 100, stride = 1, padding = 0):
        super(CNN, self).__init__()
        self.layer1 = nn.Sequential(
            nn.Conv2d(1, nb_filter, kernel_size, padding),
            nn.BatchNorm1d(nb_filter),
            nn.ReLU(),
            nn.MaxPool2d(pool_size))
        out1_size = (window_size + 2*padding - (kernel_size[1] - 1) - 1)/stride + 1
        maxpool_size = (out1_size + 2*padding - (pool_size[1] - 1) - 1)/stride + 1
        self.layer2 = nn.Sequential(
            nn.Conv2d(nb_filter, 2*nb_filter, kernel_size, padding),
            nn.BatchNorm1d(2*nb_filter),
            nn.ReLU(),
            nn.MaxPool2d(pool_size))
        out2_size = (maxpool_size + 2*padding - (kernel_size[1] - 1) - 1)/stride + 1
        maxpool2_size = (out2_size + 2*padding - (pool_size[1] - 1) - 1)/stride + 1
        self.drop1 = nn.Dropout(p=0.5)
        self.fc1 = nn.Linear(maxpool2_size*labcounts*2*nb_filter, hidden_szie)
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

# ================================================================
print "Training CNN with PyTorch"

#population = joblib.load('/data/plp/SYNPUF/population.pkl')
#plpData = joblib.load('/data/plp/SYNPUF/plpData.pkl')
#train = False 
#modelOutput = '/data/home/xpan/PatientLevelPrediction/model/'
#seed = 0
#size = 200
#epochs = 1
'''
if torch.cuda.is_available():
	cuda = True
	print('===> Using GPU')
else:
	cuda = False
	print('===> Using CPU')
#cuda = False
'''
np.random.seed(seed)
torch.manual_seed(seed)
if cuda:
	torch.cuda.manual_seed(seed)


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

		model = CNN(nbfilter)
		if cuda:
			model.cuda()
		#criterion = nn.BCELoss(size_average=True)
		#optimizer = torch.optim.Adam(model.parameters(), weight_decay = w_decay)
        criterion = nn.CrossEntropyLoss()
        optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)

                #optimizer = torch.optim.SGD(model.parameters(), lr=0.1, momentum = 0.9, weight_decay = 0.003)
		train_set = TensorDataset(torch.from_numpy(train_x.toarray().astype(np.float32)),
								  torch.from_numpy(train_y.astype(np.float32)).view(-1, 1))
		train_loader = DataLoader(dataset=train_set, batch_size=64, shuffle=True)

		model.train()
		for epoch in np.arange(epochs):
			for idx, (input, target) in enumerate(train_loader):
				input_var = Variable(input)
				target_var = Variable(target)

				if cuda:
					input_var = input_var.cuda()
					target_var = target_var.cuda()

				optimizer.zero_grad()
				pred = model(input_var)
				loss = criterion(pred, target_var)
				loss.backward()
				optimizer.step()

		end_time = timeit.default_timer()
		print "Training fold took: %.2f s" % (end_time - start_time)
		print "Calculating predictions on left out fold set..."
		ind = (population[:, population.shape[1] - 1] > 0)
		ind = population[ind, population.shape[1] - 1] == i

		model.eval()

		test_input_var = torch.from_numpy(test_x.toarray().astype(np.float32))
		#if cuda:
		#	test_input_var = test_input_var.cuda()

		temp = model.predict_proba(test_input_var)
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
	print 'the final parameter size', size, 'epochs', epochs, 'weight_decay', w_decay
	model = CNN(nbfilter)
	if cuda:
		model.cuda()

	criterion = nn.BCELoss(size_average=True)
	optimizer = torch.optim.Adam(model.parameters(), weight_decay = w_decay)
        #optimizer = torch.optim.SGD(model.parameters(), lr=0.1, momentum = 0.9, weight_decay = 0.003)
        #optimizer = torch.optim.Adam(model.parameters(), weight_decay = 0.003)

	train_set = TensorDataset(torch.from_numpy(train_x.toarray().astype(np.float32)),
							  torch.from_numpy(train_y.astype(np.float32)).view(-1, 1))
	train_loader = DataLoader(dataset=train_set, batch_size=64, shuffle=True)

	model.train()
	for epoch in np.arange(epochs):
		for idx, (input, target) in enumerate(train_loader):
			input_var = Variable(input)
			target_var = Variable(target)
			if cuda:
				input_var = input_var.cuda()
				target_var = target_var.cuda()

			optimizer.zero_grad()
			pred = model(input_var)
			loss = criterion(pred, target_var)
			loss.backward()
			optimizer.step()

	end_time = timeit.default_timer()
	print "Training final took: %.2f s" % (end_time - start_time)

	# save the model:
	if not os.path.exists(modelOutput):
		os.makedirs(modelOutput)
	print "Model saved to: %s" % (modelOutput)

	joblib.dump(model, os.path.join(modelOutput,'model.pkl'))
