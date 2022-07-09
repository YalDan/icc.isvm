# -*- coding: utf-8 -*-
import numpy as np
import math
from python.MFCF_new.MFCF import g_table as gtab

def first_clique(C, min_size):
    r, c = np.nonzero(C <= C.mean())
    C1 = C.copy()
    C1[r, c] = 0
    sums = C1.sum(axis=0)
    cand = np.argsort(-sums)
    clq = np.nonzero(cand < min_size)
    return clq[0]


def mfcf(C, ctl, gain_function):
    cliques = []
    separators = []
    peo = []
    gt = gtab.gain_table()
    q, p = C.shape
    outstanding_nodes = list(range(0, p))

    first_cl = first_clique(C, ctl['min_clique_size'])
    first_cl = frozenset(first_cl)
    cliques.append(first_cl)

    outstanding_nodes = [node for node in outstanding_nodes if node not in first_cl]
    peo = [node for node in first_cl]

    gt.nodes, gt.gains, gt.separators = gain_function(C, outstanding_nodes, first_cl, ctl)
    gt.cliques = [first_cl] * len(gt.gains)

    while len(outstanding_nodes) > 0:
        # get maximum gain
        the_gain = np.nanmax(gt.gains)
        idx = gt.gains.index(the_gain)
        clique_extension = False

        # case isolated clique, pick up outstanding node and add isolated clq
        if 0 == max(0, the_gain):
            # pick up first vertex
            the_node = outstanding_nodes[0]
            the_sep = []
            parent_clique = []
            parent_clique_id = float('nan')
        # case gain with a clique
        else:
            the_node = gt.nodes[idx]
            the_sep = gt.separators[idx]
            parent_clique = gt.cliques[idx]
            parent_clique_id = cliques.index(parent_clique)
            clique_extension = True

        #print("node: ", the_node)
        clq_as_set = set(the_sep)
        clq_as_set.add(the_node)
        new_clique = frozenset(clq_as_set)
        peo.append(the_node)

        outstanding_nodes = [x for x in outstanding_nodes if x != the_node]

        if 0 == max(0, the_gain):
            cliques.append(new_clique)
        elif len(new_clique) <= len(parent_clique):
            cliques.append(new_clique)
            separators.append(the_sep)
        else:
            cliques.append(new_clique)
            cliques.remove(parent_clique)

        if len(outstanding_nodes) == 0:
            break

        nodes, gains, seps = gain_function(C, outstanding_nodes, new_clique, ctl)
        add_cliques = [new_clique] * len(gains)

        gt.gains.extend(gains)
        gt.cliques.extend(add_cliques)
        gt.separators.extend(seps)
        gt.nodes.extend(nodes)

        gt.gains = [gt.gains[i] if gt.nodes[i] != the_node else math.nan for i in range(0, len(gt.gains))]

        if ctl['drop_sep'] == True:
            gt.gains = [gt.gains[i] if gt.separators[i] != the_sep else math.nan for i in range(0, len(gt.gains))]

        if clique_extension:
            # In case of clique extension remove the old clique
            gt.gains = [gt.gains[i] if gt.cliques[i] != parent_clique else math.nan for i in range(0, len(gt.gains))]

    return cliques, separators, peo, gt


def mfcf_control():
    ctl = {'min_clique_size': 4,
           'max_clique_size': 4,
           'coordination_number': 1,
           'cachesize': 3,
           'threshold': 0.01,
           'drop_sep': True}
    return ctl


def logo(C, cliques, separators):
    J = np.zeros(C.shape)

    for clq in cliques:
        clqt = tuple(clq)
        J[np.ix_(clqt, clqt)] += np.linalg.inv(C[np.ix_(clqt, clqt)])

    for sep in separators:
        sept = tuple(sep)
        J[np.ix_(sept, sept)] -= np.linalg.inv(C[np.ix_(sept, sept)])

    return J
