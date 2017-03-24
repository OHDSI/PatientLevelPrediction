import sys
import os
import cPickle

# for each rowId find on the same day covariateIds and group them
# create a list of [covariateIDs]
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
        p_id, cov_id, time_id =  lines[1], lines[2], lines[3]
        if p_id not in patient_dict:
            patient_dict[p_id] = {time_id: [cov_id]}
        else:
            if time_id not in patient_dict[p_id]:
                patient_dict[p_id][time_id] = [cov_id]
            else:
                patient_dict[p_id][time_id].append(cov_id)
print('{} Patients'.format(len(patient_dict)))

covs_list = []
time_list = []

for patient, time_cov_dict in patient_dict.items():
    patient_cov_list = []
    patient_time_diffs  = []
    sorted_times = sorted(time_cov_dict, key=get_key)
    prev_time = sorted_times[0]
    for time in sorted_times:
        patient_time_diffs.append(int(time) - int(prev_time))
        patient_cov_list.append(time_cov_dict[time]) # all the covariates for that time
        prev_time = time
    time_list.append(patient_time_diffs)
    covs_list.append(patient_cov_list)

cPickle.dump(covs_list, open('visit.p', 'wb'))
cPickle.dump(time_list,  open('time.p',  'wb'))
print('DONE')
