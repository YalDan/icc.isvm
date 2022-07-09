import numpy as np
from python.MFCF_new.MFCF import g_functions as gf
from python.MFCF_new.MFCF import MFCF
from scipy.spatial import distance
import pandas as pd

## information theory filtering network for sparse covariance estimation (to filter out insignificant components in the covariance matrix)
def MFCF_J(X):
    '''
    sparse J
    '''
    C = np.corrcoef(X, rowvar=False)

    ctl = MFCF.mfcf_control()
    ctl['threshold'] = 0
    ctl['drop_sep'] = True
    ctl['min_clique_size'] = 2
    ctl['max_clique_size'] = 4
    gain_function = gf.sumsquares_gen

    cliques, separators, peo, gt = MFCF.mfcf(C, ctl, gain_function)

    J = MFCF.logo(C, cliques, separators)

    return J

def ICC(train_l, K = 2, gamma = 0, delta = 0, w_max = 5, DEBUG = 0, distance_function = "mahalanobis", V = 2.3, random_seed = 123):
    # train_l = time series in numpy (column = assets, row = time steps), # K = number of clusters
    # gamma = switching penalty, # delta = experimental penalty, don't use
    
    accepted_distance_functions = {"euclidean","loglik","mahalanobis"}
    
    if distance_function not in accepted_distance_functions:
        return "Error: wrong distance function. Maybe a typo?"
    
    def clustering(train_l, K=K, gamma=0, delta=0):
        # kmeans = KMeans(n_clusters=2, random_state=0).fit(train_l)
        # cK=kmeans.labels_
        
        np.random.seed(random_seed)
        cK = np.random.choice(list(range(K)), size=len(train_l),
                              p=list(np.repeat(1/K, K, axis=0)))  # random initialization, assigns points to clusters randomly
        # Q for time series, P for all stock
        X = np.array(train_l)
        Q, P = X.shape
        w = 0
        while w < w_max:  # w_max = number of iterations
            try:
                r_v = np.zeros([Q, K])  # distance of each point to the center of the cluster
                miu_r_l = np.zeros([K, P])  # center (mean) of the cluster
                J_r_l = np.zeros([K, P, P])  # inverse covariance matrix of each cluster distribution
                w += 1

                ## calculate the mean and covariance of each cluster
                if DEBUG:
                    print('round' + str(w))
                for k in range(0, K):
                    if sum(cK == k) > 10:
                        k_index = [i for i, x in enumerate(cK == k) if x]
                        miu_r = np.mean(X[k_index, :], axis=0)
                        miu_r_l[k, :] = miu_r
                        J_r = MFCF_J(X[k_index, :])
                        J_r_l[k, :, :] = J_r

                ## assign points to each cluster according to distance measure
                for q in range(0, Q):
                    for k in range(0, K):
                        if sum(cK == k) > 10:
                            # r = distance.mahalanobis(X[q, :], miu_r_l[k, :], J_r_l[k, :, :])  # mahalanobis distance
                            # r=distance.euclidean(X[q,:],miu_r_l[k,:]) # euclidean distance
                            sign, logdet = np.linalg.slogdet(
                                np.linalg.inv(J_r_l[k, :, :]))  # likelihood distance (student t)
                            r = -sign * logdet - (V + P) * np.log(
                                1 + (distance.mahalanobis(X[q, :], miu_r_l[k, :], J_r_l[k, :, :])) ** 2 / (V - 2))
                            r_v[q, k] = r

                ## penalization

                # recalculate distance by adding the penalty
                cK = ViterbiUpdate(r_v, switch_penalty=gamma, clusters_penalty=(0, 0, delta)).reshape(-1)
                # SimpleUpdate for Euclidean distance
                # cK=SimpleUpdate(r_v).reshape(-1)

                if DEBUG:
                    print(r_v)

                ## calculate the average size of the cluster
                for i, s in enumerate(cK):
                    if i == 0:
                        count = 1
                        ls_0 = []
                        ls_1 = []
                        last_s = s
                    else:
                        if last_s == s:
                            count += 1
                        else:
                            if last_s == 0:
                                ls_0.append(count)
                            if last_s == 1:
                                ls_1.append(count)
                            count = 1
                            last_s = s
                size_0 = sum(ls_0) / len(ls_0)
                size_1 = sum(ls_1) / len(ls_1)
            except:
                
                if DEBUG:
                    print("Error: too many iterations. Stopped at w = " + str(w - 1))
                
                w = w - 1
                break
            if DEBUG:
                print(size_0, size_1)
                print(list(cK).count(0), list(cK).count(1))
                print('*' * 50)

        # single last update for Euclidean
        if distance_function == "euclidean":
            cK=ViterbiUpdate(r_v,switch_penalty=gamma).reshape(-1)

        # fixed states
        count_last_state_0 = list(cK)[-15:].count(0)
        count_last_state_1 = list(cK)[-15:].count(1)
        if DEBUG:
            print()
            print(cK)
        if count_last_state_0 >= count_last_state_1:
            pass
        else:
            if DEBUG:
                print('state fixing')
            cK = np.array([i ^ 1 for i in cK])
        if DEBUG:
            print(cK)
        return cK, w

    def ViterbiUpdate(gain, switch_penalty=0, clusters_penalty=(0, 0, 0)):
        '''
        switch_penalty: integer
        cluster penalty: tuple, [0] is size of cluster 0, [1] is size of cluster 1, [2] is delta (scaling factor)
        '''

        T, num_clusters = gain.shape
        future_cost_vals = np.zeros((T, num_clusters))

        # initialization of first location
        for i in reversed(range(0, T - 1)):
            j = i + 1
            future_costs = future_cost_vals[j]
            lle_vals = gain[j]

            for cluster in range(0, num_clusters):
                total_vals = [x + y + switch_penalty for x, y in zip(future_costs, lle_vals)]
                total_vals[cluster] = total_vals[cluster] - switch_penalty
                future_cost_vals[i][cluster] = min(total_vals)

        path = np.ones((T, 1), dtype=int)
        c_g = [x + y for x, y in zip(future_cost_vals[0], gain[0])]
        dummy, curr_location = min((val, idx) for (idx, val) in enumerate(c_g))
        path[0] = curr_location

        # compute best path
        for i in range(0, T - 1):
            j = i + 1
            future_costs = future_cost_vals[j]
            lle_vals = gain[j]
            total_vals = [x + y + switch_penalty for x, y in zip(future_costs, lle_vals)]
            total_vals[path[i][0]] = total_vals[path[i][0]] - switch_penalty
            total_vals[0] = total_vals[0] + clusters_penalty[0] * clusters_penalty[2]
            total_vals[1] = total_vals[1] + clusters_penalty[1] * clusters_penalty[2]
            dummy, path[i + 1] = min((val, idx) for (idx, val) in enumerate(total_vals))

        return path

    def SimpleUpdate(gain):
        T, num_clusters = gain.shape
        path = np.ones((T, 1), dtype=int)
        for i in range(T):
            dummy, path[i] = min((val, idx) for (idx, val) in enumerate(gain[i]))
        return path

    # alpha: return
    # beta: volatility
    # gamma: persistence
    cK, interval = clustering(train_l, K = K, gamma = gamma, delta = delta)
    return cK, interval
