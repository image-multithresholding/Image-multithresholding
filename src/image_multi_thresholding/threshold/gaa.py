from typing import List, Literal
import numpy as np
import random
import math
from image_multi_thresholding.base import _between_class_var, _cluster_mean, _cluster_var, _image_histogram, _image_probabilities, _prob_up_to_level, _gray_clustering


def _within_class_var(prob: List[float], levels: List[int]) -> int:
    w = _prob_up_to_level(prob, levels)
    clusters = _gray_clustering(len(prob), [l + 1 for l in levels])

    var = [_cluster_var(prob, c, 0) for c in clusters]

    total_mean = _cluster_mean(
        prob, [i for i, _ in enumerate(prob)], 0)
    total_var = sum([(i-total_mean)**2*prob[i] for i, _ in enumerate(prob)])

    return sum(var)/total_var


def _fitness_function(prob: List[float], threshold: List[Literal[0, 1]]) -> List[float]:
    return _between_class_var(prob, threshold) / _within_class_var(prob, threshold)


def _bit_vector(size: int) -> List[Literal[0, 1]]:
    return [random.choice([0, 1]) for _ in range(size)]


def _binary_to_decimal(chromo: List[Literal[0, 1]], K: int) -> List[int]:
    number_length = len(chromo) // K
    result = []
    for x in range(K):
        current_value = 0
        for off in range(number_length):
            current_value += chromo[x*number_length +
                                    off]*2**(number_length-off-1)
        result.append(current_value)

    return sorted(result)


def _RWS(prob: List[float], pop: List[List[Literal[0, 1]]], K: int) -> List[int]:
    thr = [_binary_to_decimal(p, K) for p in pop]

    fit_pop = [_fitness_function(prob, t) for t in thr]
    s = sum(fit_pop)
    r = random.random()*s
    previous_hypothesis = [sum(fit_pop[:i]) for i in range(1, len(fit_pop)+1)]
    # print(fit_pop)
    # print(previous_hypothesis)
    # print(r)

    m = list(filter(lambda x: x > r, previous_hypothesis))[0]
    return pop[previous_hypothesis.index(m)]


def _mutation(chromosome: List[Literal[0, 1]]):
    position = random.randrange(0, len(chromosome))
    return chromosome[:position] + [1 - chromosome[position]] + chromosome[position + 1:]


def _crossover(chr_a: List[Literal[0, 1]], chr_b: List[Literal[0, 1]]):
    position = random.randrange(1, len(chr_a)-1)
    return chr_a[:position] + chr_b[position:], chr_b[:position] + chr_a[position:]


def _has_unique_thresholds(chr: List[Literal[0, 1]], k: int) -> bool:
    return len(set(_binary_to_decimal(chr, k))) == k


def _is_new_chromosome(prob: List[float], new_chr: List[Literal[0, 1]], others_fit: List[float], k: int) -> bool:
    #new_chr_dec = _binary_to_decimal(new_chr, k)
    if _has_unique_thresholds(new_chr, k):
        fit = _fitness_function(prob, new_chr)
        return fit > max(others_fit)
    return False


def threshold_gaa(img: np.ndarray, k: int, iter: int = 200, pop_size: int = 10, crossover_rate: float = 0.95, mutation_rate: float = 0.05):
    hist = _image_histogram(img)
    prob = _image_probabilities(img)

    bits = int(math.log(len(hist)) / math.log(2))

    initial_pop = [_bit_vector(bits*k) for _ in range(pop_size)]

    best_hip = []

    for j in range(iter):
        new_pop = []
        new_size = 0
        while new_size < pop_size:
            while True:
                chr_a = _RWS(prob, initial_pop, k)
                if _has_unique_thresholds(chr_a, k):
                    new_pop.append(chr_a)
                    break

            while True:
                chr_b = _RWS(prob, initial_pop, k)
                if _has_unique_thresholds(chr_b, k):
                    new_pop.append(chr_b)
                    break

            r = random.random()
            new_chr_a, new_chr_b = chr_a, chr_b
            if r < crossover_rate:
                new_chr_a, new_chr_b = _crossover(new_chr_a, new_chr_b)

            if r < mutation_rate:
                new_chr_a, new_chr_b = _mutation(
                    new_chr_a), _mutation(new_chr_b)

            fit_a = _fitness_function(prob, _binary_to_decimal(chr_a, k))
            fit_b = _fitness_function(prob, _binary_to_decimal(chr_b, k))

            # Calculate new_pop?
            if _is_new_chromosome(prob, new_chr_a, [fit_a, fit_b], k):
                new_pop.append(new_chr_a)

            if _is_new_chromosome(prob, new_chr_b, [fit_a, fit_b], k):
                new_pop.append(new_chr_b)

            new_size = len(new_pop)

        fit_new_pop = [_fitness_function(
            prob, _binary_to_decimal(p, k)) for p in new_pop]
        max_fit = max(fit_new_pop)
        for i, pop in enumerate(new_pop):
            if fit_new_pop[i] == max_fit:
                best_hip.append(pop)
                break

    fit_best = [_fitness_function(
        prob, _binary_to_decimal(p, k)) for p in best_hip]
    max_fit_best = max(fit_best)
    for i, pop in enumerate(best_hip):
        if fit_best[i] == max_fit_best:
            return _binary_to_decimal(pop, k)
