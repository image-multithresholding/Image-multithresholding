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
    while len(peaks) > k+1 or size == 0:
        size += 1
        cells = cell_histogram(freq, size)
        peaks = peak_identification(cells)

    return peaks, size

def valley_location(freq: List[float], size: int) -> List[float]:
    cells = cell_histogram(freq, size)
    direction = arrow_direction(cells)
    
    limits = list()

    # Get all positions where direction is 1, avoiding first and last position
    ones = [i for i, x in enumerate(direction[:-1]) if x == 1]

    for i in ones:
        nextDirections = [dir for dir in direction[i+1:]]
        if nextDirections:
            aux = []
            if 1 in nextDirections:
                aux.append(nextDirections.index(1))
            if -1 in nextDirections:
                aux.append(nextDirections.index(-1))
            h = min(aux)
            if direction[h + i + 1] == -1:
                limits.append((i, h + i + 1))
    
    return limits


def uncell(L: int, size: int) -> List[List[int]]:
    cells = []

    currCells = []
    for x in range(1, L + 1):
        currCells.append(x)
        if len(currCells) == size:
            cells.append(currCells)
            currCells = []
    if currCells:
        cells.append(currCells)
    return cells

