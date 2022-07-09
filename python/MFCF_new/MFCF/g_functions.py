# -*- coding: utf-8 -*-
"""
Created on Sun Jun  6 11:02:30 2021
@author: Guido Previde Massara
"""

import itertools as itertools
import numpy as np


def sumsquares_gen(M, v, clq, ct_control):
    """
    M similarity matrix
    v vector of outstanding nodes
    clq clique
    ct_control parameters for the clique expansion algorithm
    """
    nodes = []
    gains = []
    seps = []

    clq = tuple(clq)
    csz = len(clq)
    vn = len(v)
    W = M * M

    if 'threshold' in ct_control:
        threshold = ct_control['threshold']
    else:
        threshold = 0.0

    if 'min_clique_size' in ct_control:
        min_clique_size = ct_control['min_clique_size']
    else:
        min_clique_size = 4

    if 'max_clique_size' in ct_control:
        max_clique_size = ct_control['max_clique_size']
    else:
        max_clique_size = 4

    if 'cachesize' in ct_control:
        cachesize = ct_control['cachesize']
    else:
        cachesize = min(4, min_clique_size)

    if csz < max_clique_size:
        facets = list()
        facets.append(clq)
    else:
        facets = list(itertools.combinations(clq, csz - 1))

    block_rows = len(facets)
    ncol = len(facets[0])

    the_vs = np.sort(np.tile(v, block_rows))  # nodes in order
    the_fs = facets * vn  # facets as they are generated

    ranked_values, ranked_seps = greedy_sortsep_v(the_vs, the_fs, W)
    ranked_values_thr, ranked_seps_thr = apply_threshold_v(ranked_values, ranked_seps, min_clique_size, threshold)

    gains = list(map(sum, ranked_values_thr))

    selector = np.tile(list(range(1, block_rows + 1)), vn)

    the_table = list(zip(the_vs, gains, ranked_seps_thr))

    idx = np.where(selector <= cachesize)[0].tolist()

    the_table = [the_table[x] for x in idx]

    nodes = [x[0] for x in the_table]
    gains = [x[1] for x in the_table]
    seps = [frozenset(x[2].tolist()) for x in the_table]

    return nodes, gains, seps


def greedy_sortsep(vtx, sep, W):
    w = W[vtx, sep]
    sepv = np.array(sep)
    sep_ranked = np.argsort(w)[::-1]
    values = w[sep_ranked]
    sep_ranked = sepv[sep_ranked]

    return values, sep_ranked


def greedy_sortsep_v(vertices, sets, W):
    local_fun = lambda x, y: greedy_sortsep(x, y, W)
    retval = list(map(local_fun, vertices, sets))
    ranked_values = [x[0] for x in retval]
    ranked_seps = [x[1] for x in retval]

    return ranked_values, ranked_seps


def apply_threshold(val, sep, mincsize, threshold):
    idx = val >= threshold
    idx[0:(mincsize - 1)] = True
    val = val[idx]
    sep = sep[idx]

    return val, sep


def apply_threshold_v(ranked_values, ranked_seps, mincsize, threshold):
    local_fun = lambda x, y: apply_threshold(x, y, mincsize, threshold)
    retval = list(map(local_fun, ranked_values, ranked_seps))
    ranked_values = [x[0] for x in retval]
    ranked_seps = [x[1] for x in retval]

    return ranked_values, ranked_seps