from skimage import exposure
import numpy as np
import itertools
from typing import List, Dict, Tuple, Callable
from PythonCodes.library.thresholding_base import *
    
def cluster_mean(prob: List[float], clust: List[int], start: int) -> float:
    """Compute the mean of a given cluster 
    Arguments:
    prob the probability list of gray levels (list of elements of class float, value from 0 to 1)
    clust a cluster of the gray levels (list of elements of class int)
    start 0 or 1 (int) for gray levels 0,...,L-1 or 1,...,L, respectively 
    Value:
    cluster_mean returns an object with class float"""
    
    term = list()
    clusterProb = 0

    # Compute the expected value of each element in the cluster
    # and compute the cluster probability
    for grayLevel in clust:
        term.append(grayLevel * prob[grayLevel - start]) 
        clusterProb += prob[grayLevel - start]

    # Compute the cluster mean
    clusterMean = sum(term) / clusterProb

    return clusterMean

def cluster_var(prob: List[float], clust: List[int], start: int) -> float:
    """Compute the variance of a given cluster 
    Arguments:
    prob the probability list of gray levels (list of elements of class float, value from 0 to 1)
    clust a cluster of the gray levels (list of elements of class int)
    start 0 or 1 (int) for gray levels 0,...,L-1 or 1,...,L, respectively 
    Value:
    cluster_mean returns an object with class float"""
    
    term = list()
    clusterProb = 0
    
    clusterMean = cluster_mean(prob, clust, start)

    # Compute the variance of each element in the cluster
    # and compute the cluster probability
    for grayLevel in clust:
        term.append(((grayLevel - clusterMean) ** 2)* prob[grayLevel - start]) 
        clusterProb += prob[grayLevel - start]

    clusterVariance = sum(term) / clusterProb

    return clusterVariance