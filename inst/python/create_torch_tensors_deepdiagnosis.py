###############################################################################
# If using this code or its variant please cite:
# Narges Razavian, David Sontag, "Temporal Convolutional Neural Networks 
# for Diagnosis from Lab Tests", ICLR 2016 Workshop track.
# Link: http://arxiv.org/abs/1511.07938 
# For questions about the code contact Narges (narges.sharif@gmail.com)
###############################################################################

import numpy as np
import cPickle
import os
import sys

def tofile(fname, x):
    x.tofile(fname)
    open(fname + '.type', 'w').write(str(x.dtype))
    open(fname + '.dim', 'w').write('\n'.join(map(str, x.shape)))

def usage():
	print 'use the following options and go to  for more information: '
	print '--x  is file name corresponding to input x (either numpy or cPickled numpy array)'
	print '--y  is file name corresponding to input y (either numpy or cPickled numpy array)'
	print '--outdir  is directory in which will have torch sub directories'
	print '--task  is either train|test|valid, corresponding to what x and y are refering to'
	print '--ths  is optional. It refers to a thresold for standard deviation. If an input dimension has standard deviation above this threshold, we filter that dimension to avoid issues with Stochastic Gradient Descent later.'

def create_torch_tensor(input_x, input_y, output_dir, task = 'train', threshold = 1000):
	#args = sys.argv[:]
	try: 
		x = cPickle.load(open(input_x, 'rb'))
		y = cPickle.load(open(input_y, 'rb'))		
	except:
		try:
			x = np.load(input_x)
			y = np.load(input_y)
		except:
			print 'error', input_x, ' could not be loaded either as pickle or numpy format'
			exit()

	if not os.path.exists(output_dir):
		os.makedirs(output_dir)
	if not os.path.exists(output_dir+'/' + task ):
		os.makedirs(output_dir+'/' + task)

	print 'finished loading the data of size:', x.shape, y.shape
	print 'Normalizing the x now. Please wait...'

	threshold = 1000
	x[x==0] = np.nan
	x1 = x.reshape((x.shape[0], x.shape[1]*x.shape[2], 1))
	meanarray = np.nanmean(x1, axis=1)
	# print meanarray
	stdarray = np.nanstd(x1, axis=1)
	stdarray[np.isnan(stdarray)] = 1.0
	meanarray[np.isnan(meanarray)] = 1.0
	high_range_lab_ix = (stdarray > threshold).nonzero()[0]

	print 'Checking and filtering dimensions with standard deviation above', threshold, ' for now. Stochastic gradient descent doesn\'t deal with those dimensions very well right now.'
	print 'Currently the following input dimensions are truely out of range with std>', threshold,' : indexes are:[', high_range_lab_ix, ']'

	x[high_range_lab_ix, :, :] = 0.0
	meanarray[high_range_lab_ix, :] = 0.0
	meanarrayexpanded = np.tile(meanarray.reshape(meanarray.shape[0],1,1), (1, x.shape[1], x.shape[2]))
	stdarrayexpanded = np.tile(stdarray.reshape(stdarray.shape[0],1,1), (1, x.shape[1], x.shape[2]))
	x = (x - meanarrayexpanded) / (stdarrayexpanded * 1.0)
	# print np.nanmean(x, axis=1)
	x[np.isnan(x)] = 0


	print 'Done!'

	tofile( output_dir + '/' + task + '/x'+ task +'_normalized.bin', x.astype('float32'))
	tofile( output_dir + '/' + task + '/y'+ task +'_outcomes_binary.bin', y.astype('int32'))

if __name__=='__main__':
        args = sys.argv[:]
        args.pop(0)
        threshold = 1000

        while args:
                arg = args.pop(0)
                if arg == '-h':
                        usage()
                elif arg=='--x':
                        input_x = args.pop(0)
                elif arg=='--y':
                        input_y = args.pop(0)
                elif arg=='--outdir':
                        output_dir = args.pop(0)
                elif arg=='--task':
                        task = args.pop(0)
                elif arg=='--ths':
                        threshold = args.pop(0)
	create_torch_tensor(input_x, input_y, output_dir, task , threshold)
