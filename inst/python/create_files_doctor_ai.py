import sys
import os
import cPickle

# for each rowId find on the same day covarteIds and group them
# create a list of [covairateIDs] 
filename = sys.argv[1]

def get_key(key):
    try:
        return int(key)
    except ValueError:
        return key

patient_dict = {}
with open(filename) as fp:
    for lines in fp:
        lines = lines.strip('\n').strip('\r').split(',')
#        print(lines, type(lines), lines[0])
          
        p_id, cov_id, time_id =  lines[1], lines[2], lines[3]
        print(p_id, cov_id, time_id)
        if p_id not in patient_dict:
            patient_dict[p_id] = {}
        else:
            if time_id not in patient_dict[p_id]:
                patient_dict[p_id][time_id] = []
            else:
                patient_dict[p_id][time_id].append(cov_id)
print(patient_dict)

final_list = []
time_list = []
count = 0

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
        time_list_temp.append(diff)
        temp_list.append(patient_dict[kk][vv])
    
    print(temp_list)
    time_list.append(time_list_temp)
    final_list.append(temp_list)
#print(final_list, len(final_list),count)
print(time_list)

cPickle.dump(final_list, open('visit.p', 'wb')) 
cPickle.dump(time_list,  open('time.p',  'wb'))

