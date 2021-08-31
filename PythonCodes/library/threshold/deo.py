import math
from typing import List, Literal
import numpy as np
import random

from PythonCodes.library.base import _between_class_var, _image_probabilities

def _objective_error(prob: List[float], priori_prob: List[float], means: List[float], vars: List[float], penalty: List[float]):
    normal_comb = np.ndarray([
        _normal_sum(i, priori_prob, means, vars)
        for i, _ in enumerate(prob)
    ])

    return sum((normal_comb-prob)**2)/len(prob) + penalty*(sum(priori_prob)-1)**2

def _normal_sum(x, priori_prob, means, vars):
    return np.sum(
        (p/math.sqrt(2*math.pi*vars[i]) *
        math.exp(-(x-means[i]**2/(2*vars[i])))
        for i, p in enumerate(priori_prob))
    )

def threshold_deo(img, ..., fintess_function):
    if fitness_function = A:
        a()
    elif fitness_function = B:
        b()

def threshold_deo(img: np.ndarray, k: int, iter: int = 1000, number_pop: int = 40,
    mutation_factor: float = 0.5, crossover_constant: float = 0.1, penalty: float = 1.5, fitness_function: Literal['BCV', 'GE'] = 'BCV'):
    
    # Variaciones entre BCV y GE:
    # Variables iniciales
    # valor único para cada umbral versus priority_prob/mean/var
    # Cambia la fitness function
    # Fitness function mejor = max/min
    # GE tiene un paso extra al final para devolver umbrales en lugar de tuplas de 3 elementos (quadOptimizaion.r)

    prob = _image_probabilities(img)
    L = len(prob)
    if fitness_function == 'BCV':
        # Between class var
        min_thr = 0
        max_thr = L-1

        solutions = np.ndarray(
            [[random.randint(min_thr, max_thr) for _ in range(number_pop)]
                for _ in range(k)]
        )
        solutions.sort()

    if fitness_function == 'GE':
        # Gaussian error
        min_priority_prob = 0
        max_priority_prob = 1
        min_mean = 0
        max_mean = L-1
        min_var = 0
        max_var = 100

        prior_prob = np.ndarray(
            [[min_priority_prob + random.random()*(max_priority_prob - min_priority_prob)
                for _ in range(number_pop)] for _ in range(k+1)]
                # k+1 porque quadOptimization devuelve k soluciones
        )
        mean = np.ndarray(
            [[min_mean + random.random()*(max_mean - min_mean)
                for _ in range(number_pop)] for _ in range(k+1)]
        )
        var = np.ndarray(
            [[min_var + random.random()*(max_var - min_var)
                for _ in range(number_pop)] for _ in range(k+1)]
        )
    
    counter = 0
    while counter < iter:
        fitting_error = [
            _between_class_var(prob, sorted(s))
            if len(set(s)) == k-1 and s[0] != 0 and s[-1] != L-1
            else 0
            for s in solutions
        ]
        best_solution = solutions[np.argmin(fitting_error)]
        pos1 = []
        pos2 = []
        for i in range(number_pop):
            positions = [x for x in range(number_pop) if x != i]
            pos1.append(random.randrange(len(positions)))
            pos2.append(random.randrange(len(positions)))
        
        mutant_vectors = np.ndarray([
            np.round(best_solution + mutation_factor * (solutions[pos1[i]]-solutions[pos2[i]])) for i in range(number_pop)
            ])
        mutant_vectors.sort()

        for i in range(number_pop):
            #if mutant_vectors[i].any()
            pass

