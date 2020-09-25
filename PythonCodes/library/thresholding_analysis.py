from typing import List

def arrow_direction(freq: List[int]) -> List[int]:
    """Returns a list with the arrow direction at each
    cell acording to Tsai and Chen algorithm (1992)

    A value is 1 if arrow is going up, -1 if it's
    going down and 0 otherwise"""

    direction = list()

    # First value special case
    if freq[0] != 0 and freq[0] <= freq[1]: # TODO: Ask about this simplification (see original code)
        direction.append(-1)
    else:
        direction.append(0)

    for i, f in enumerate(freq):
        # Ignore first and last value since they are special cases
        if i == 1 or i == len(freq):
            continue

        # Previous and next frequencies
        fp = freq[i-1]
        fn = freq[1+1]

        if f != 0 and fp>fn and fp >= f:
            direction.append(1) # Going up
        elif f!= 0 and fn>fp and fn>= f:
            direction.append(-1) # Going down
        else:
            direction.append(0) # Peak or valley
    
    #  Last value special case
    if freq[-1] != 0 and freq[-2] >= freq[-1]:
        direction.append(1)
    else:
        direction.append(0)

    return direction

# TODO: Can we compute arrow direction inside of peak_identification instead of passing it?
def peak_identification(direction: List[int]):
    peaks = list()

    for i, d in enumerate(direction):
        # Previous and next values
        dp = direction[i-1]
        dn = direction[i+1]

        # Ignore first and last values
        if i == 0 or i == len(direction):
            continue

        if d == 0 and dp == -1 and dn == 1: # TODO: Shouldn't they both be -1?
            peaks.append(i)
        
    return peaks