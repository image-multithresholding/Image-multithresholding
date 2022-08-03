from skimage import exposure, io
import numpy as np
import itertools
from typing import List, Dict, Tuple, Callable
from math import sqrt, pi, exp, ceil
from numba import njit, jit


def load_image(path: str) -> np.ndarray:
    return io.imread(path)


def _thresholded_image(img: np.ndarray, thr: List[int]) -> np.ndarray:
    """thresholdedImage function
    Arguments:
    img a numpy.ndarray object representing an image
    thr a list of thresholds
    Value:
    thresholdingImage returns an object with class "numpy.ndarray" and possible values 0,1,...,255"""

    image = np.copy(img)
    thr = np.array(thr, dtype=int)
    _, counts = np.unique(thr, return_counts=True)
    for count in counts:
        if count > 1:
            raise Exception(
                "[Error!]: there are repetead values in 'thr' (list of thresholds) argument")
    _, grays = exposure.histogram(img)
    L = grays.size
    K = thr.size
    if K == 0:
        raise Exception("[Error!]: 'thr', list of thresholds, is empty")
    if K > L:
        raise Exception(
            "[Error!]: 'thr', list of thresholds, contains more values than the amount of gray levels")

    # Compute the grayscale mean in each class
    avg = np.zeros(1)
    avg[0] = np.mean(grays[0:(thr[0]+1)])
    if(K != 1):
        for i in range(1, K):
            avg = np.append(avg, np.mean(grays[(thr[i-1]+1):(thr[i]+1)]))
    avg = np.append(avg, np.mean(grays[(thr[K-1]+1):]))
    avg = np.round(avg)

    # Replace each gray value in the image by the mean of its class

    image[(image >= grays[0]) & (image <= grays[thr[0]])] = avg[0]
    if(K != 1):
        for i in range(1, K):
            image[(image >= grays[thr[i-1]+1]) &
                  (image <= grays[thr[i]])] = avg[i]
    image[image > grays[thr[K-1]]] = avg[-1]

    return image


def PSNR(img: np.ndarray, thImg: np.ndarray) -> np.float64:
    """Compute the peak signal to noise ratio (PSNR) measured in decibel (dB) of a thersholded 
    image
    Arguments:
    img a "numpy.ndarray" object representing an image
    thImg a thresholded image of img, a "numpy.ndarray" object
    Value:
    PSNR returns an object with type class 'numpy.float64'"""

    image = np.asarray(np.copy(img), dtype=np.int32)
    thresholded_image = np.asarray(np.copy(thImg), dtype=np.int32)

    if image.shape != thresholded_image.shape:
        raise Exception(
            "[Error!]: the shape of 'img' and 'thImg' does not match")

    # Compute the root mean-squared error (RMSE)
    rmse = np.sqrt(np.mean((image - thresholded_image) ** 2))

    # Compute the peak signal to noise ratio (PSNR)
    psnr = 20 * np.log10(np.max(img) / rmse)

    return psnr


def _image_probabilities(img: np.ndarray) -> List[float]:
    probabilities = [0 for _ in range(256)]
    histogram = _image_histogram(img)

    for grayLevel in range(256):
        probabilities[grayLevel] = histogram[grayLevel] / img.size

    return probabilities


def _image_histogram(img: np.ndarray) -> List[int]:
    """Build the histogram of the gray levels of a given image 
    Arguments:
    img a "numpy.ndarray" object 
    Value:
    image_histogram returns a dictionary with gray levels frequencies"""

    image = np.copy(img)

    # Find and sort gray levels and frequency
    _, freq = np.unique(image.flatten(), return_counts=True)

    return freq


def _prob_up_to_level(prob: List[float], levels: List[int]) -> float:
    """
    Compute a vector of probabilities up to a list of gray levels 

    Arguments:
    prob the probability list of gray levels (list of elements of class float, value from 0 to 1)
    levels a list of gray levels which give the breaks 

    Value:
    prob_up_to_level returns an object with class 'list', a float elements list
    """

    # Find the amount of levels

    amountOfLevels = len(levels)

    if(amountOfLevels == 0):
        raise Exception("list of breaks is empty")
    if(amountOfLevels > len(prob)):
        raise Exception("more levels than len of prob")
    for i, level in enumerate(levels):
        # if type(level) != int:
        #     raise Exception(f"non-integer level: {level} {type(level)}")
        # Replace -1 with 0 to mimic R
        if level == -1:
            levels[i] = 0

    # Initialize probUpToLevel list

    probUpToLevel = list()

    probUpToLevel.append(sum(prob[:levels[0] + 1]))

    if (amountOfLevels == 1):
        # Find both probabilities
        probUpToLevel.append(1 - probUpToLevel[0])
    else:
        # Find probabilities up to each level
        for i in range(1, amountOfLevels):
            probUpToLevel.append(
                sum(prob[(levels[i - 1] + 1): (levels[i] + 1)]))

        probUpToLevel.append(1 - sum(probUpToLevel))

    return probUpToLevel


def _cluster_nth_moment(n: int, prob: List[float], clust: List[int]) -> float:
    """
Compute the nth moment of a given cluster of gray levels.

Arguments:
n order moment
prob the probability vector of gray levels 0,...,L-1
clust a cluster of the gray levels

Value:
cluster_nth_moment returns an object with class float
"""

    clustMean = _cluster_mean(prob, clust, 1)

    length = len(clust)

    numerator = list()
    denominator = list()

    # Find nth moment factors

    for i in range(0, length):
        numerator.append((clust[i] - clustMean) ** n * prob[clust[i] - 1])
        denominator.append(prob[clust[i] - 1])

    clustMom = sum(numerator) / sum(denominator)

    return clustMom


def _skewness(prob: List[float], clust: List[int]) -> float:
    """
    Compute the skewness of a given cluster of gray levels.

    Arguments:
    prob the probability vector of gray levels 0,...,L-1
    clust a cluster of the gray levels

    Value:
    skewness returns an object with class float
    """

    mom2 = _cluster_nth_moment(2, prob, clust)
    mom3 = _cluster_nth_moment(3, prob, clust)

    skewness = mom3 / sqrt(mom2 ** 3)

    return skewness

def _between_class_var(prob: List[float], levels: List[int]) -> float:
    """Compute the variance between classes"""
    probUpToLevel = _prob_up_to_level(prob, levels)
    amountOfProbs = len(prob)
    clusters = _gray_clustering(amountOfProbs, levels)

    mu = list()

    for cluster in clusters:
        mu.append(_cluster_mean(prob, cluster, 0))
    totalMean = _cluster_mean(prob, [x for x in range(len(prob))], 0)

    term = list()
    for i, m in enumerate(mu):
        term.append(probUpToLevel[i] * (m-totalMean)**2)

    return sum(term)

def _optimal_window(prob: List[float], w: np.ndarray) -> List[int]:
    if w[0][1] - w[0][0] != 1:
        skew = list()
        for wj in w:
            skew.append(_skewness(prob, list(range(wj[0], wj[1] + 1))))
        skew = [abs(x) for x in skew]

    for wi in w:
        # Do nothing if w is of the type a,a+1
        if wi[1] - wi[0] == 1:
            bounds = wi.tolist()
        else:
            minSkew = skew[0]
            minSkewIndex = 0
            for i, s in enumerate(skew):
                if s < minSkew:
                    minSkew = s
                    minSkewIndex = i
            optimalWindow = w[minSkewIndex]
            bounds = [optimalWindow[0], optimalWindow[1]]

    return bounds


def _cluster_estimations(prob: List[float], clust: List[int]) -> List[float]:
    meanEstimation = _cluster_mean(prob, clust, 1)
    varianceEstimation = _cluster_var(prob, clust, 1)
    proportionEstimation = sum(prob[i - 1] for i in clust)

    return (
        meanEstimation,
        varianceEstimation,
        proportionEstimation
    )


def _weighted_gaussian(x: float, mu: float, sigma: float, p: float) -> float:
    """
Apply gaussian normal function with a given weigth.

Arguments:
x real value in the domain
mu mean parameter
sigma standard deviation parameter
p weight of proportion

Value:
weighted_gaussian returns an object with class float
"""

    # Compute the gaussian value

    gV = p / (sqrt(2 * pi) * sigma) * exp(-0.5 * ((x-mu) / sigma) ** 2)

    return gV


def _cluster_mean(prob: List[float], clust: List[int], start: int) -> float:
    """
Compute the mean of a given cluster 

Arguments:
prob the probability list of gray levels (list of elements of class float, value from 0 to 1)
clust a cluster of the gray levels (list of elements of class int)
start 0 or 1 (int) for gray levels 0,...,L-1 or 1,...,L, respectively 

Value:
cluster_mean returns an object with class float
"""
    # Initialize term list and cluster prbability accumulator

    term = list()
    clusterProb = 0

    # Compute the expected value of each element in the cluster
    # and compute the cluster probability

    for grayLevel in clust:
        term.append((grayLevel) * prob[grayLevel - start])
        clusterProb += prob[grayLevel - start]

    clusterMean = sum(term) / clusterProb

    return clusterMean


def _cluster_var(prob: List[float], clust: List[int], start: int) -> float:
    """
Compute the variance of a given cluster 

Arguments:
prob the probability list of gray levels (list of elements of class float, value from 0 to 1)
clust a cluster of the gray levels (list of elements of class int)
start 0 or 1 (int) for gray levels 0,...,L-1 or 1,...,L, respectively 

Value:
cluster_mean returns an object with class float
"""
    # Initialize term list and cluster prbability accumulator

    term = list()
    clusterProb = 0

    # Compute the cluster mean
    clusterMean = _cluster_mean(prob, clust, start)

    # Compute the variance of each element in the cluster
    # and compute the cluster probability

    for grayLevel in clust:
        term.append(((grayLevel - clusterMean - 1 + start) ** 2)
                    * prob[grayLevel - start])
        clusterProb += prob[grayLevel - start]

    # Compute the variance of the cluster

    clusterVariance = sum(term) / clusterProb

    return clusterVariance


def _gray_clustering(levels: int, breakPositions: List[int], levelsOffset: int = 0) -> List[List[int]]:
    clusters = [[]]

    # Iterate over every level.
    for x in range(levelsOffset, levelsOffset + levels):
        # If we have to break at this level, start a new list.
        if x - 1 in breakPositions:
            clusters.append([x])
        # If not, just add this level to the last created cluster.
        else:
            clusters[-1].append(x)

    return clusters


def _total_correlation(prob: List[float], levels: List[int]) -> float:
    """Compute the total correlation according to a list of breaking gray levels
    Arguments:
    prob the probability list of gray levels (list of elements of class float, value from 0 to 1)
    levels a list of gray levels which give the breaks
    Value:
    total_correlation returns an object with class 'float'"""

    probUpToLevel = _prob_up_to_level(prob, levels)

    amountOfProbabilities = len(prob)
    levels = [0] + levels + [amountOfProbabilities]
    amountOfLevels = len(levels)

    # Initialize correlations list
    correlations = list()

    # Find the correlation of each interval
    #correlations.append(-np.log(sum(prob[:levels[0]]**2)/probUpToLevel[0]**2))
    for i in range(0, amountOfLevels-1):
        correlations.append(-np.log(
            sum(np.square(prob[(levels[i]): (levels[i+1])])) / probUpToLevel[i] ** 2))

    return sum(correlations)


def _threshold_candidates(gray_levels: List[int], k: int) -> itertools.combinations:
    """Returns a generator of all combinations of k elements from gray_levels, never taking the first or last element"""
    return itertools.combinations([x for x in range(1, len(gray_levels) - 1)], k)


def _threshold_candidate_generic(img: np.ndarray, k: int, candidate_function: Callable[[List[float], List[int]], float], is_max=True):
    prob = _image_probabilities(img)

    max_generic = 0 if is_max else np.inf
    max_generic_candidate = 0

    # Get the candidate with the highest total value for the candidate_function
    for candidate in _threshold_candidates(prob, k):
        value = candidate_function(prob, list(candidate))
        if (value > max_generic and is_max) or (value < max_generic and not is_max):
            max_generic = value
            max_generic_candidate = candidate

    return max_generic_candidate


def _cell_histogram(freq: List[float], size: int) -> List[float]:
    n = ceil(len(freq)/size)

    val = []
    for i in range(n):
        val.append(sum(freq[i*size:min((i+1)*size, len(freq))]))
    return val


def _arrow_direction(freq: List[int]) -> List[int]:
    """Returns a list with the arrow direction at each
    cell acording to Tsai and Chen algorithm (1992)

    A value is 1 if arrow is going up, -1 if it's
    going down and 0 otherwise"""

    direction = list()

    # First value special case
    if freq[0] != 0 and freq[0] <= freq[1]:
        direction.append(-1)
    else:
        direction.append(0)

    for i, f in enumerate(freq):
        # Ignore first and last value since they are special cases
        if i == 0 or i == len(freq) - 1:
            continue

        # Previous and next frequencies
        fp = freq[i-1]
        fn = freq[i+1]

        if f != 0 and fp > fn and fp >= f:
            direction.append(1)  # Going down
        elif f != 0 and fn > fp and fn >= f:
            direction.append(-1)  # Going up
        else:
            direction.append(0)  # Peak or valley

    #  Last value special case
    if freq[-1] != 0 and freq[-2] >= freq[-1]:
        direction.append(1)
    else:
        direction.append(0)

    return direction


def _peak_identification(freq: List[int]):
    direction = _arrow_direction(freq)
    peaks = list()

    for i, d in enumerate(direction):
        # Ignore first and last values
        if i == 0 or i == len(direction) - 1:
            continue

        # Previous and next values
        dp = direction[i-1]
        dn = direction[i+1]

        if d == 0 and dp == -1 and dn == 1:
            peaks.append(i)

    return peaks


def _hill_identification(freq: List[float], k: int) -> Tuple[List[int], int]:
    size = 0
    peaks = []
    while len(peaks) > k+1 or size == 0:
        size += 1
        cells = _cell_histogram(freq, size)
        peaks = _peak_identification(cells)

    return peaks, size


def _uncell(L: int, size: int, start=1) -> List[List[int]]:
    cells = []

    currCells = []
    for x in range(start, L + start):
        currCells.append(x)
        if len(currCells) == size:
            cells.append(currCells)
            currCells = []
    if currCells:
        cells.append(currCells)
    return cells


def _discrete_local_min(prob: List[float]) -> float:
    result = []
    slope = []
    for i, p in enumerate(prob[:-1]):
        slope.append(prob[i+1] - p)
    for i, s in enumerate(slope[:-1]):
        if s < 0 and slope[i+1] > 0:
            result.append(i+1)
    return result
