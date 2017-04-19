import sys
import os
import cPickle
import pdb
import numpy as np

output_dir = 'data'

def convert_format2(covriate_ids, patient_dict, Y = None, time_window = 1):
    D = len(covriate_ids)
    N = len(patient_dict)
    T = 365
    concept_list =list(covriate_ids)
    x_raw = np.zeros((D,N,T), dtype=float)
    #y = np.zeros((O,N,T), dtype=int)
    patient_ind = 0
    for kk in patient_dict.keys():
        print('-------------------')
        vals = patient_dict[kk] 
        for timeid, meas in vals.iteritems():
            int_time = int(timeid) - 1
            for val in meas:
                if not len(val):
                    continue
                cov_id, cov_val = val
                lab_ind = concept_list.index(cov_id)
                x_raw[lab_ind][patient_ind][int_time] = float(cov_val)
    
        patient_ind = patient_ind + 1
    

    T = 365/time_window
    extr = 365%time_window
    x = np.zeros((D,N,T), dtype=float)
    #convert to time window
    for lab in range(D):
        for patient in range(N):
            times =  x_raw[lab][patient]
            for ti in range(T):
                if ti == 0:
                    end = time_window + extr
                    win_vals = times[:end]
                else:
                    start = end
                    end = end + time_window
                    win_vals = times[start:end]
                nonzero = [val for val in  win_vals if val]
                if len(nonzero):
                    nonzero = [0] 
                x[lab][patient][ti] = np.mean(nonzero)
    
    
    
    ix_all = np.arange(N)
    np.random.shuffle(ix_all)
    ix_test = ix_all[0:N/6]
    ix_valid = ix_all[N/6:(2*N)/6]
    ix_train = ix_all[(2*N)/6:]
    
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    cPickle.dump(x[:, ix_train , :], open(output_dir+'/xtrain.pkl', 'wb'), -1)
    #cPickle.dump(y[:, ix_train , :], open(output_dir+'/ytrain.pkl', 'wb'), -1)
    cPickle.dump(x[:, ix_valid, :], open(output_dir+'/xvalid.pkl', 'wb'), -1)
    #cPickle.dump(y[:, ix_valid, :], open(output_dir+'/yvalid.pkl', 'wb'), -1)    
    cPickle.dump(x[:, ix_test, :], open(output_dir+'/xtest.pkl', 'wb'), -1)
    #cPickle.dump(y[:, ix_test, :], open(output_dir+'/ytest.pkl', 'wb'), -1) 
    
    if Y is not None:
        cPickle.dump(Y[ix_train], open(output_dir+'/ytrain.pkl', 'wb'), -1)
        cPickle.dump(Y[ix_valid], open(output_dir+'/yvalid.pkl', 'wb'), -1)   
        cPickle.dump(Y[ix_test], open(output_dir+'/ytest.pkl', 'wb'), -1)

def read_data(filename):
    covriate_ids = set()
    patient_dict = {}
    head = True
    with open(filename) as fp:
        for lines in fp:
            if head:
                head = False
                continue
            lines = lines.strip('\n').strip('\r').split(',')
            #print(lines, type(lines), lines[0])
            try:  
                p_id, cov_id, time_id, cov_val =  lines[1], lines[2], lines[3], lines[4]
            except:
                pdb.set_trace()
            print(p_id, cov_id, time_id)
            if p_id not in patient_dict:
                patient_dict[p_id] = {}
            else:
                if time_id not in patient_dict[p_id]:
                    patient_dict[p_id][time_id] = []
                else:
                    patient_dict[p_id][time_id].append((cov_id, cov_val))
            covriate_ids.add(cov_id)
    #time_window  = 1
    patient_dict = {k: v for k, v in patient_dict.iteritems() if v} #remove empty patients
    #(15, 2000, 60) 20000 patients, 15 lab tests, 
    return covriate_ids, patient_dict


__name__ == "__main__":
    filename = sys.argv[1]
    population = joblib.load('/data/plp/SYNPUF/population.pkl')
    Y = population[:, 1]
    covriate_ids, patient_dict = read_data(filename)
    convert_format2(covriate_ids, patient_dict, Y)

