import numpy as np
import os
import sys
import timeit
import math
import sagemaker
from sagemaker.transformer import Transformer
import numpy as np
import scipy.sparse
import boto3
import mxnet as mx

from scipy.sparse import coo_matrix,csr_matrix,vstack,hstack
from joblib import Memory
import joblib

from sagemaker.amazon.amazon_estimator import get_image_uri
from sagemaker import get_execution_role
#================================================================

def sagemaker_predict(population, plpData, bucket, prefix, container, role_arn, model_name ):
  s3 = boto3.resource('s3')
  sess = sagemaker.Session()
  print("Applying Python Model") 
  ###########################################################################
  print("Loading Data...")
  # load data + train,test indexes + validation index
  X = plpData[population[:,0].astype(int),:]
  
  # load index file
  print("population loaded- %s rows and %s columns" %(np.shape(population)[0], np.shape(population)[1]))
  print("Dataset has %s rows and %s columns" %(X.shape[0], X.shape[1]))
  print("Data ready for model has %s features" %(np.shape(X)[1]))
  # load model
  print("loading model...")
  model_url = 's3://{}/plpModel/model.tar.gz'.format(bucket)
  
  #role = sagemaker.get_execution_role()
  role = role_arn
  container = container 
  sm_client = boto3.client('sagemaker')
 
  primary_container = {
      'Image': container,
      'ModelDataUrl': model_url
  }
  
  create_model_response = sm_client.create_model(
      ModelName = model_name,
      ExecutionRoleArn = role,
      PrimaryContainer = primary_container)
    
  print(X.shape)
  print("Calculating predictions on population...")
  np.savetxt('pred.csv', X.todense(), delimiter=',', fmt='%i')
  
  pred_s3 = sess.upload_data('pred.csv',bucket=bucket, key_prefix=prefix)
  pred_s3 = sagemaker.s3_input(s3_data = pred_s3, content_type = 'text/csv')

  transformer = sagemaker.transformer.Transformer(
    base_transform_job_name='Batch-Transform',
    model_name=model_name,
    instance_count=1,
    instance_type='ml.m4.xlarge',
    output_path='s3://{}/{}'.format(bucket, prefix)
  )
  transformer.transform('s3://{}/{}/pred.csv'.format(bucket, prefix), content_type='text/csv', split_type='Line')
  transformer.wait()
  
  return 
