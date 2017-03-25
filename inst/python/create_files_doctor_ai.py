import sys
import os
import cPickle
import pdb
import random
#from sklearn.cross_validation import train_test_split

# for each rowId find on the same day covariateIds and group them
# create a list of [covariateIDs]

filename = sys.argv[1]

def get_key(key):
    try:
        return int(key)
    except ValueError:
        return key

covriate_ids = set()
patient_dict = {}
with open(filename) as fp:
    for lines in fp:
        lines = lines.strip('\n').strip('\r').split(',')
        p_id, cov_id, time_id =  lines[1], lines[2], lines[3]
        print(p_id, cov_id, time_id)
        #covriate_ids.add(cov_id)
        if p_id not in patient_dict:
            patient_dict[p_id] = {time_id: [cov_id]}
        else:
            if time_id not in patient_dict[p_id]:
                patient_dict[p_id][time_id] = [cov_id]
            else:
                patient_dict[p_id][time_id].append(cov_id)
        covriate_ids.add(cov_id)

print('{} Patients'.format(len(patient_dict)))
print(patient_dict)

#pdb.set_trace()

covs_list = []
time_list = []
concept_list =list(covriate_ids)
print 'concept #', len(concept_list)

for patient, time_cov_dict in patient_dict.items():
    patient_cov_list = []
    patient_time_diffs  = []
    sorted_times = sorted(time_cov_dict, key=get_key)
    prev_time = sorted_times[0]
    for time in sorted_times:
        patient_time_diffs.append(int(time) - int(prev_time))
        cov_index_list = [concept_list.index(cov) for cov in time_cov_dict[time]]
        patient_cov_list.append(cov_index_list)
        prev_time = time
    if len(patient_time_diffs):
        time_list.append(patient_time_diffs)
        covs_list.append(patient_cov_list)

#print(covs_list, len(covs_list),count)
#print(time_list)
#pdb.set_trace()
# split the data to train, val, test by ratio 4:1:1

data_ind = range(len(covs_list))
random.shuffle(data_ind)
val_num = len(data_ind)/6
test_num = val_num
train_num =  len(data_ind) - test_num - val_num
x_train = []
x_val = []
x_test = []
time_train =[]
time_val =[]
time_test =[]
for ind in data_ind:
    if ind in data_ind[:train_num]:
        x_train.append(covs_list[ind])
        time_train.append(time_list[ind])
    elif  ind in data_ind[train_num:train_num + val_num]:
        x_val.append(covs_list[ind])
        time_val.append(time_list[ind])
    else:
        x_test.append(covs_list[ind])
        time_test.append(time_list[ind])
#y = range(len(covs_list))
#x_train, x_test, y_train, y_test = train_test_split(covs_list, y, test_size=0.2)
#x_test, x_val, y_test, y_val = train_test_split(x_test, y_test, test_size=0.5)
#pdb.set_trace()
cPickle.dump(x_train, open('visit.train', 'wb'))
cPickle.dump(x_val, open('visit.valid', 'wb'))
cPickle.dump(x_test, open('visit.test', 'wb'))
cPickle.dump(time_train, open('time.train', 'wb'))
cPickle.dump(time_val, open('time.valid', 'wb'))
cPickle.dump(time_test, open('time.test', 'wb'))

#cPickle.dump(covs_list, open('visit.p', 'wb'))
#cPickle.dump(time_list,  open('time.p',  'wb'))
