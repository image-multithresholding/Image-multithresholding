import numpy as np
from dataclasses import dataclass
from image_multi_thresholding.base import _between_class_var, _image_probabilities
from random import random, randrange


def _is_valid_ant(ant, L):
    return (len(set(ant)) == len(ant) and ant[0] != 0 and ant[-1] != L-1)


def _random_selection(lb, ub, pheromone_intensity, intensity_control):
    n = len(pheromone_intensity)
    p = [phin**intensity_control /
         sum([phins**intensity_control for phins in pheromone_intensity]) for phin in pheromone_intensity]
    # print(f'{p=}')
    partial_sum = [sum(p[:i]) for i in range(1, n + 1)]
    # print(f'{partial_sum=}')
    r = random()
    #r = 0.5
    i = len(partial_sum) - 1
    for index, v in enumerate(partial_sum):
        if v >= r:
            i = index
            #print(f'{i=} {v=}')
            break
    return [x for x in range(lb, ub)][i]


@dataclass()
class ACOOptions:
    pheromone_intensity: float = 1.0
    break_str: float = 0.68
    pheromone_trail: float = 0.01
    pheromone_evaporation: float = 0.94
    pheromone_persistence: float = 0.9
    pheromone_contribution: float = 0.01*pheromone_trail
    population_size: int = 40


def threshold_aco(img: np.ndarray, k: int, iter: int = 100, options: ACOOptions = ACOOptions()):
    prob = _image_probabilities(img)
    #prob = [0.1, 0.1, 0.2, 0.05, 0.05, 0.1, 0.3, 0.05, 0.05]
    L = len(prob)
    # print(f'{L=}')

    hsr = np.array([[0, L-1] for _ in range(k)], dtype=int)
    # print(f'{hsr=}')
    tau = np.array([[options.pheromone_trail for _ in range(L)]
                   for _ in range(k)], dtype=float)
    # print(f'{tau=}')
    ants = np.array([sorted([randrange(0, L) for _ in range(k)])
                    for _ in range(options.population_size)], dtype=int)
    #ants = np.array([[4], [5], [0], [7], [6], [5], [7], [5], [7], [4]], dtype=int)
    # print(f'{ants=}')
    bcv = np.array([_between_class_var(prob, ant)
                   if _is_valid_ant(ant, L) else 0 for ant in ants])
    # print(f'{bcv=}')

    best_bcv = max(bcv)
    best_ant = ants[np.argmax(bcv)]
    # print(f'{best_bcv=}')
    # print(f'{best_ant=}')

    counter = 0
    while counter < iter:
        #print(f'{counter=} {ants=}')
        for h in range(options.population_size):
            for i in range(k):
                # print(f'{hsr[i][0]=}')
                # print(f'{hsr[i][1]=}')
                # print(f'{tau[i][hsr[i][0]:hsr[i][1]]=}')
                # print(f'{options.pheromone_intensity=}')
                q = random()
                #q = 0.5
                # print(f'{[hsr[i][0], hsr[i][1]]=}')
                if q < options.break_str:
                    if hsr[i][0] == hsr[i][1]:
                        max_tau = tau[i][hsr[i][0]
                                         ]**options.pheromone_persistence
                    else:
                        max_tau = max(
                            [t**options.pheromone_intensity for t in tau[i][hsr[i][0]:hsr[i][1]]])
                    # print(f'{max_tau=}')
                    # print(max_tau)
                    # print(tau[i])
                    arg_max_tau = np.argmax(max_tau)
                    # print(f'{arg_max_tau=}')
                    ants[h][i] = arg_max_tau + hsr[i][0]
                    # print(f'{ants[h][i]=}')
                else:
                    ants[h][i] = _random_selection(
                        hsr[i][0],
                        hsr[i][1],
                        tau[i][hsr[i][0]:hsr[i][1]],
                        options.pheromone_intensity
                    )
                    tau[i] = (1-options.pheromone_evaporation)*tau[i]
                    # print(f'{tau[i]=}')
                if i < k-1:
                    hsr[i+1] = np.array([ants[h][i]+1, L-1])

        ants = np.sort(ants)
        bcv = np.array([_between_class_var(prob, ant)
                       if _is_valid_ant(ant, L) else 0 for ant in ants])
        if max(bcv) > best_bcv:
            best_bcv = max(bcv)
            best_ant = ants[np.argmax(bcv)]
        #print(f'{counter=} {best_ant=}')

        tau = options.pheromone_persistence*tau + \
            (1-options.pheromone_persistence) * \
            options.pheromone_contribution*best_bcv**2
        counter += 1

    return tuple(best_ant)
