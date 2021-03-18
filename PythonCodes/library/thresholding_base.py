from skimage import exposure, io
import numpy as np
import itertools
from typing import List, Dict, Tuple, Callable


def load_image(path: str) -> np.ndarray:
    return io.imread(path)


def thresholded_image(img: np.ndarray, thr: List[int]) -> np.ndarray:
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


def image_probabilities(img: np.ndarray) -> List[float]:
    probabilities = [0 for _ in range(256)]
    histogram = image_histogram(img)

    for grayLevel in range(256):
        probabilities[grayLevel] = histogram[grayLevel] / img.size

    return probabilities


def gray_clustering(levels: int, breakPositions: List[int], levelsOffset: int = 0) -> List[List[int]]:
    clusters = [[]]

    # Iterate over every level.
    for x in range(levelsOffset, levelsOffset + levels):
        # If we have to break at this level, start a new list.
        if x in breakPositions:
            clusters.append([x])
        # If not, just add this level to the last created cluster.
        else:
            clusters[-1].append(x)

    return clusters


def image_histogram(img: np.ndarray) -> List[int]:
    """Build the histogram of the gray levels of a given image 
    Arguments:
    img a "numpy.ndarray" object 
    Value:
    image_histogram returns a dictionary with gray levels frequencies"""

    image = np.copy(img)

    # Find and sort gray levels and frequency
    _, freq = np.unique(image.flatten(), return_counts=True)

    return freq


def prob_up_to_level(prob: List[float], levels: List[int]) -> float:
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
        if type(level) != int:
            raise Exception(f"non-integer level: {level}")
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
