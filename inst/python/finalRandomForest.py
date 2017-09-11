if quiet==False:
  print "Training final model with best investigated hyper-parameters..." 

if (mtry==-1):
  mtry =int(np.round(np.sqrt(X.shape[1])))

# train final:
trainInd =population[:,population.shape[1]-1] >0
train_x = X[trainInd,:]
train_y = y[trainInd]

start_time = timeit.default_timer()	
rf = RandomForestClassifier(max_features=int(mtry), n_estimators=ntrees,max_depth=max_depth,min_samples_split=5,  n_jobs=-1, bootstrap=False, random_state=seed)#, class_weight="balanced_subsample")
rf = rf.fit(train_x, train_y)
end_time = timeit.default_timer()
if quiet==False:
  print "Training final took: %.2f s" %(end_time-start_time)

# save the model:
if not os.path.exists(modelOutput):
  os.makedirs(modelOutput)
if quiet==False:  
  print "Model saved to: %s" %(modelOutput)	

joblib.dump(rf, modelOutput+"\\model.pkl") 
##np.savetxt(output+'\\'+id+'\\varImp.txt',rf.feature_importances_, fmt='%.18e', delimiter=',', newline='\n')

# merge pred with indexes[testInd,:]
# save 
test_pred.shape = (population[trainInd,:].shape[0], 1)
prediction = np.append(population[trainInd,:],test_pred, axis=1)
#print "%s - %s" %(prediction.shape[0], prediction.shape[1])
##np.savetxt(output+'\\'+id+'\\prediction.txt', prediction, fmt='%.18e', delimiter=',', newline='\n')
