import sagemaker
from sagemaker.transformer import Transformer
import numpy as np
import scipy.sparse
import joblib
from joblib import Memory
import StringIO
import boto3
import os
#role = sagemaker.get_execution_role()

#================================================================
def train_sagemaker(population, plpData, classifier, hyperParameters, container, bucket, s3_output, role_arn, prefix, job_name, modelOutput):
  print("Training Sagemaker model " )
  s3 = boto3.resource('s3')
  sess = sagemaker.Session()
  
  y = population[:,1]
  X = plpData[population[:,0].astype(int),:]
  trainInds =population[:,population.shape[1]-1] >0

  print("Dataset has %s rows and %s columns" %(X.shape[0], X.shape[1]))
  print("population loaded- %s rows and %s columns" %(np.shape(population)[0], np.shape(population)[1]))
  ###########################################################################

  np.savetxt('train.csv', scipy.sparse.hstack((y[trainInds][:,None],X[trainInds,:])).todense(), delimiter=',',fmt='%i')
  np.savetxt('test.csv', X[trainInds,:].todense(), delimiter=',', fmt='%i')
  
  train_s3 = sess.upload_data(path='train.csv', bucket=bucket, key_prefix=prefix)
  test_s3 = sess.upload_data(path='test.csv', bucket=bucket, key_prefix=prefix)
  
  train_s3 = sagemaker.s3_input(s3_data = train_s3, content_type = 'text/csv')
  test_s3 = sagemaker.s3_input(s3_data = test_s3, content_type = 'text/csv')
  
  estimator = sagemaker.estimator.Estimator(image_name = container,   
                                            role = role_arn,
                                            train_instance_count = 1L,
                                            train_instance_type = 'ml.m5.large',
                                            train_volume_size = 30L,
                                            train_max_run = 3600L,
                                            input_mode = 'File',
                                            output_path = s3_output)
  if classifier == 'linear-learner':
    estimator.set_hyperparameters(feature_dim = X[trainInds,:].shape[1], predictor_type = 'binary_classifier', mini_batch_size = 100)
  if classifier == 'xgboost': 
    estimator.set_hyperparameters(num_round = 10L)
  if classifier == 'knn': 
    k = 1000
    if hyperParameters is not None:
      if hyperParameters["k"] is not None:
        k = int(hyperParameters["k"])
    estimator.set_hyperparameters(feature_dim = X[trainInds,:].shape[1], predictor_type = 'classifier', k = k, sample_size=X[trainInds,:].shape[0])
  
  input_data = {"train": train_s3}
  estimator.fit(inputs = input_data, job_name = job_name)

  transformer = estimator.transformer(instance_count=1, instance_type='ml.m4.xlarge', strategy='MultiRecord',
                                      assemble_with='Line', output_path='s3://{}/prediction'.format(bucket))
  transformer.transform('s3://{}/data/test.csv'.format(bucket), content_type='text/csv', split_type='Line')
  transformer.wait()
  
  # save the model:
  if not os.path.exists(modelOutput):
    os.makedirs(modelOutput)
  print("Model saved to: %s" %(modelOutput)	)
  
  modelkey = os.path.join('output',job_name,'output/model.tar.gz')
  s3.Bucket(bucket).download_file(modelkey, os.path.join(modelOutput,'model.tar.gz'))
  
  return True
