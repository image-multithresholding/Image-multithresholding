from skimage import exposure
import numpy as np
import itertools
from typing import List, Dict, Tuple, Callable
from PythonCodes.library.thresholding_base import *

def total_correlation(prob, candidate):
    return 100
    # TODO: Implement this

def total_entropy(prob, candidate):
    return 100
    # TODO: Implement this

def threshold_candidates(gray_levels: List[int], k: int) -> itertools.combinations:
    """Returns a generator of all combinations of k elements from gray_levels, never taking the first or last element"""
    return itertools.combinations(gray_levels[1:-1], k)

def threshold_candidate_generic(img: np.ndarray, k: int, candidate_function: Callable[[Dict[int, float], List[int]], Tuple]):
    prob = image_probabilities(img)

    max_generic = 0
    max_generic_candidate = 0

    # Get the candidate with the highest total value for the candidate_function
    for candidate in threshold_candidates(len(prob), k):
        value = candidate_function(prob, candidate)
        if value > max_generic:
            max_generic = value
            max_generic_candidate = candidate
    
    return max_generic_candidate

def threshold_mcc(img: np.ndarray, k: int):
    return threshold_candidate_generic(img, k, total_correlation)

def threshold_mec(img: np.ndarray, k: int):
    return threshold_candidate_generic(img, k, total_entropy)