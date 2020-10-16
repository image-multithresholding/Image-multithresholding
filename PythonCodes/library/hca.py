import math
from typing import List, Tuple
from PythonCodes.library.thresholding_analysis import arrow_direction, peak_identification

def cell_histogram(freq: List[float], size: int) -> List[float]:
    n = math.ceil(len(freq)/size)

    val = []
    for i in range(n):
        val.append(sum(freq[i*size:min((i+1)*size, len(freq))]))
    return val

def hill_identification(freq: List[float], k: int) -> Tuple[int, int]:
    size = 0
    peaks = []
    while len(peaks) > k or size == 0:
        size += 1
        cells = cell_histogram(freq, size)
        peaks = peak_identification(cells)

    return peaks, size

def valley_location(freq: List[float], size: int) -> List[float]:
    cells = cell_histogram(freq, size)
    direction = arrow_direction(cells)
    
    leftCell = list()
    rightCell = list()

    # Get all positions where direction is 1, avoiding first and last position
    ones = [i for i, x in enumerate(direction[1::-2]) if x == 1]

    for i in ones:
        nextDirections = direction[i+1:] # TODO: This probably breaks at i = len(ones) - 1
        if nextDirections:
            h = i + 1 # TODO: which(aux !=0)[[1]]+i ???
            if direction[h] == -1:
                leftCell.append(i)
                rightCell.append(h)
    
    return leftCell + rightCell # TODO: cbind(leftCell, rightCell) ?