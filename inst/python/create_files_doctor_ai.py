import sys
import os
import cPickle
import pdb
import random
#from sklearn.cross_validation import train_test_split
# for each rowId find on the same day covarteIds and group them
# create a list of [covairateIDs] 
filename = sys.argv[1]

def get_key(key):
    try:
        return int(key)
    except ValueError:
        return key
covriate_ids = set()
patient_dict = {}
head = True
with open(filename) as fp:
    for lines in fp:
        if head:
            head = False
            continue
        lines = lines.strip('\n').strip('\r').split(',')
#        print(lines, type(lines), lines[0])
          
        p_id, cov_id, time_id =  lines[1], lines[2], lines[3]
        print(p_id, cov_id, time_id)
        #covriate_ids.add(cov_id)
        if p_id not in patient_dict:
            patient_dict[p_id] = {}
        else:
            if time_id not in patient_dict[p_id]:
                patient_dict[p_id][time_id] = []
            else:
                patient_dict[p_id][time_id].append(cov_id)
        covriate_ids.add(cov_id)
print(patient_dict)
#pdb.set_trace()
final_list = []
time_list = []
count = 0
concept_list =list(covriate_ids)
print 'concept #', len(concept_list)

for kk in patient_dict.keys():
    count +=1
    print('-------------------')
    print(kk, patient_dict[kk])
    temp_list = []
    time_list_temp  = []
    vv = 1000
    init = 0
    for vv in sorted(patient_dict[kk], key=get_key, reverse=True):
        if init==0:
            diff = 0
            prev_vv = vv
        else:
            diff = -1*(int(vv)-int(prev_vv))
        init=1
        print(diff)
        if len(patient_dict[kk][vv]) == 0:
            continue
        time_list_temp.append(diff)
        tmp = []
        for vc in patient_dict[kk][vv]:
            tmp.append(concept_list.index(vc))
        temp_list.append(tmp)
        #temp_list.append(patient_dict[kk][vv])
    
    #print(temp_list)
    if len(time_list_temp):
        time_list.append(time_list_temp)
        final_list.append(temp_list)
#print(final_list, len(final_list),count)
#print(time_list)
#pdb.set_trace()
# split the data to train, val, test by ratio 4:1:1

data_ind = range(len(final_list))
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
        x_train.append(final_list[ind])
        time_train.append(time_list[ind])
    elif  ind in data_ind[train_num:train_num + val_num]:
        x_val.append(final_list[ind])
        time_val.append(time_list[ind])
    else:
        x_test.append(final_list[ind]) 
        time_test.append(time_list[ind])
#y = range(len(final_list))
#x_train, x_test, y_train, y_test = train_test_split(final_list, y, test_size=0.2)
#x_test, x_val, y_test, y_val = train_test_split(x_test, y_test, test_size=0.5)
#pdb.set_trace()
cPickle.dump(x_train, open('visit.train', 'wb'))
cPickle.dump(x_val, open('visit.valid', 'wb'))
cPickle.dump(x_test, open('visit.test', 'wb'))
cPickle.dump(time_train, open('time.train', 'wb'))
cPickle.dump(time_val, open('time.valid', 'wb'))
cPickle.dump(time_test, open('time.test', 'wb'))

#cPickle.dump(final_list, open('visit.p', 'wb')) 
#cPickle.dump(time_list,  open('time.p',  'wb'))

