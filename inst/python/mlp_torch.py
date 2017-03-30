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

class MLP(nn.Module):
	def __init__(self, input_dim, hidden_size):
		super(MLP, self).__init__()
		self.fc1 = nn.Linear(input_dim, hidden_size)
		self.fc2 = nn.Linear(hidden_size, 1)

	def forward(self, x):
		x = F.tanh(self.fc1(x))
		x = F.tanh(self.fc2(x))
		x = F.sigmoid(x)
		return x

	def predict_proba(self, x):
		if type(x) is np.ndarray:
			x = torch.from_numpy(x.astype(np.float32))
		x = Variable(x)
		if cuda:
			x = x.cuda()
		return self.forward(x)


# ================================================================
print "Training MLP with PyTorch"

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

		model = MLP(train_x.shape[1], size)
		if cuda:
			model.cuda()
		criterion = nn.BCELoss(size_average=True)
		optimizer = torch.optim.Adam(model.parameters())

		train_set = TensorDataset(torch.from_numpy(train_x.toarray().astype(np.float32)),
								  torch.from_numpy(train_y.astype(np.float32)).view(-1, 1))
		train_loader = DataLoader(dataset=train_set, batch_size=100, shuffle=True)

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
		if cuda:
			test_input_var = test_input_var.cuda()

		preds = model.predict_proba(test_input_var)
		temp = preds.data.cpu().numpy().flatten()

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

	model = MLP(train_x.shape[1], size)
	if cuda:
		model.cuda()

	criterion = nn.BCELoss(size_average=True)
	optimizer = torch.optim.Adam(model.parameters())

	train_set = TensorDataset(torch.from_numpy(train_x.toarray().astype(np.float32)),
							  torch.from_numpy(train_y.astype(np.float32)).view(-1, 1))
	train_loader = DataLoader(dataset=train_set, batch_size=100, shuffle=True)

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

	joblib.dump(model, modelOutput + '/model.pkl')
