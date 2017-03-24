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
        # print(p_id, cov_id, time_id)
        if p_id not in patient_dict:
            patient_dict[p_id] = {time_id: [cov_id]}
        else:
            if time_id not in patient_dict[p_id]:
                patient_dict[p_id][time_id] = [cov_id]
            else:
                patient_dict[p_id][time_id].append(cov_id)
# print(patient_dict)
print('{} Patients'.format(len(patient_dict)))

covs_list = []
time_list = []

for patient, time_cov_dict in patient_dict.items():
    # print('-------------------')
    # print(patient, time_cov_dict)
    patient_cov_list = []
    patient_time_diffs  = []
    sorted_times = sorted(time_cov_dict, key=get_key)
    prev_time = sorted_times[0]
    for time in sorted_times:
        diff = int(time) - int(prev_time)
        prev_time = time
        # print(diff)
        patient_time_diffs.append(diff)
        patient_cov_list.append(time_cov_dict[time])

    # print(patient_cov_list)
    time_list.append(patient_time_diffs)
    covs_list.append(patient_cov_list)
# print(time_list)

print('DONE')
# print(covs_list)
# print(time_list)
cPickle.dump(covs_list, open('visit.p', 'wb'))
cPickle.dump(time_list,  open('time.p',  'wb'))
