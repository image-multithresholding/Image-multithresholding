import math
from typing import List, Literal
import numpy as np
import random

from image_multi_thresholding.base import _between_class_var, _image_probabilities


def _objective_error(prob: List[float], priori_prob: List[float], means: List[float], vars: List[float], penalty: float):
    normal_comb = np.array([
        _normal_sum(i, priori_prob, means, vars)
        for i, _ in enumerate(prob)
    ])

    return sum((normal_comb-prob)**2)/len(prob) + penalty*(sum(priori_prob)-1)**2


def _normal_sum(x, priori_prob, means, vars):
    return sum(
        (p/math.sqrt(2*math.pi*vars[i]) *
         math.exp(-(x-means[i])**2/(2*vars[i]))
         for i, p in enumerate(priori_prob))
    )


def BetweenClassVar():
    class BCV:
        def __init__(self, prob, k, number_pop):
            self.prob = prob
            self.k = k
            self.number_pop = number_pop
            self.min_thr = 0
            self.max_thr = len(prob)-1

        def generate_initial_solutions(self):
            self.solutions = np.array(
                [[random.randint(self.min_thr, self.max_thr) for _ in range(self.k)]
                    for _ in range(self.number_pop)]
            )
            self.solutions.sort()

        def calculate_best_solution(self):
            self.fitting_error = [
                _between_class_var(self.prob, sorted(s))
                if self._is_valid_solution_vector(s)
                else 0
                for s in self.solutions
            ]
            self.best_solution = self.solutions[np.argmax(self.fitting_error)]

        def generate_mutant_vectors(self, mutation_factor, pos1, pos2):
            self.mutant_vectors = np.array([
                np.round(self.best_solution + mutation_factor * (self.solutions[pos1[i]]-self.solutions[pos2[i]])) for i in range(self.number_pop)
            ])
            self.mutant_vectors.sort()
            for i, mv in enumerate(self.mutant_vectors):
                if any([x > self.max_thr or x < self.min_thr for x in mv]):
                    self.mutant_vectors[i] = self.best_solution

        def crossover(self, crossover_prob):
            self.crossed_vectors = np.ndarray(
                shape=(self.number_pop, self.k), dtype=int)
            for i, mr in enumerate(self.mutant_vectors):
                if random.random() < crossover_prob or i == random.randint(0, self.number_pop - 1):
                    self.crossed_vectors[i] = mr
                else:
                    self.crossed_vectors[i] = self.solutions[i]

        def new_fitting_error(self):
            self.nfe = [_between_class_var(self.prob, np.sort(
                cv)) if self._is_valid_solution_vector(cv) else 0 for cv in self.crossed_vectors]
            for i, cv in enumerate(self.crossed_vectors):
                if self.nfe[i] > self.fitting_error[i]:
                    self.solutions[i] = cv

        def get_result(self) -> List[int]:
            ffv = np.array([_between_class_var(self.prob, s)
                           for s in self.solutions])
            return [r for r in np.sort(self.solutions[np.argmax(ffv)])]

        def _is_valid_solution_vector(self, vector):
            return len(set(vector)) == self.k and vector[0] != 0 and vector[-1] != len(self.prob)-1

    return BCV


def GaussianError(penalty: float = 1.5):
    class GE:
        def __init__(self, prob, k, number_pop):
            self.k = k
            self.number_pop = number_pop
            self.prob = prob
            self.penalty = penalty
            self.min_priority_prob = 0.001
            self.max_priority_prob = 1
            self.min_mean = 0
            self.max_mean = len(prob)-1
            self.min_var = 1
            self.max_var = 100

        def generate_initial_solutions(self):
            self.prior_prob = np.array(
                [[self.min_priority_prob + random.random()*(self.max_priority_prob - self.min_priority_prob)
                    for _ in range(self.k+1)] for _ in range(self.number_pop)], dtype=float
            )
            self.mean = np.array(
                [[self.min_mean + random.random()*(self.max_mean - self.min_mean)
                    for _ in range(self.k+1)] for _ in range(self.number_pop)], dtype=int
            )
            self.var = np.array(
                [[self.min_var + random.random()*(self.max_var - self.min_var)
                    for _ in range(self.k+1)] for _ in range(self.number_pop)], dtype=int
            )

        def calculate_best_solution(self):
            self.fitting_error = [
                _objective_error(
                    self.prob, self.prior_prob[i], self.mean[i], self.var[i], self.penalty)
                for i, _ in enumerate(self.prior_prob)
            ]
            i = np.argmin(self.fitting_error)
            self.best_solution = (
                self.prior_prob[i],
                self.mean[i],
                self.var[i]
            )

        def generate_mutant_vectors(self, mutation_factor, pos1, pos2):
            self.mutant_vectors_prior_prob = np.array([
                np.round(self.best_solution[0] + mutation_factor * (self.prior_prob[pos1[i]]-self.prior_prob[pos2[i]])) for i in range(self.number_pop)
            ])
            self.mutant_vectors_prior_prob.sort()
            self.mutant_vectors_mean = np.array([
                self.best_solution[1] + mutation_factor * (self.mean[pos1[i]]-self.mean[pos2[i]]) for i in range(self.number_pop)
            ])
            self.mutant_vectors_mean.sort()
            self.mutant_vectors_var = np.array([
                np.round(self.best_solution[2] + mutation_factor * (self.var[pos1[i]]-self.var[pos2[i]])) for i in range(self.number_pop)
            ])
            self.mutant_vectors_var.sort()

        def crossover(self, crossover_prob):
            self.crossed_vectors_prior_prob = np.ndarray(
                shape=(self.number_pop, self.k+1), dtype=float)
            self.crossed_vectors_mean = np.ndarray(
                shape=(self.number_pop, self.k+1), dtype=int)
            self.crossed_vectors_var = np.ndarray(
                shape=(self.number_pop, self.k+1), dtype=int)

            for i, _ in enumerate(self.mutant_vectors_prior_prob):
                if (random.random() < crossover_prob) and self._is_solution_in_range(self.mutant_vectors_prior_prob[i], self.mutant_vectors_mean[i], self.mutant_vectors_var[i]):
                    self.crossed_vectors_prior_prob[i] = self.mutant_vectors_prior_prob[i]
                    self.crossed_vectors_mean[i] = self.mutant_vectors_mean[i]
                    self.crossed_vectors_var[i] = self.mutant_vectors_var[i]
                else:
                    self.crossed_vectors_prior_prob[i] = self.prior_prob[i]
                    self.crossed_vectors_mean[i] = self.mean[i]
                    self.crossed_vectors_var[i] = self.var[i]

        def new_fitting_error(self):
            self.nfe = [_objective_error(self.prob, self.crossed_vectors_prior_prob[i], self.crossed_vectors_mean[i],
                                         self.crossed_vectors_var[i], self.penalty) for i, _ in enumerate(self.crossed_vectors_prior_prob)]
            for i, nfei in enumerate(self.nfe):
                if nfei < self.fitting_error[i]:
                    self.prior_prob[i] = self.crossed_vectors_prior_prob[i]
                    self.mean[i] = self.crossed_vectors_mean[i]
                    self.var[i] = self.crossed_vectors_var[i]

        def get_result(self) -> List[int]:
            prior_prob = self.best_solution[0]
            mean = self.best_solution[1]
            var = self.best_solution[2]

            A = [v**2-var[i+1]**2 for i, v in enumerate(var[:-1])]
            B = [2*(mean[i]*var[i+1]**2-mean[i+1]*v**2)
                 for i, v in enumerate(var[:-1])]
            C = [(v*mean[i+1])**2-(var[i+1]*mean[i])**2+2*(v*var[i+1])**2*np.log((var[i+1]
                                                                                  * prior_prob[i])/(v*prior_prob[i+1])) for i, v in enumerate(var[:-1])]

            solutions = []
            for i in range(len(A)):
                solutions.extend(np.roots([A[i], B[i], C[i]]))
            return sorted([int(s) for s in solutions if type(s) != np.complex128 and s > self.min_mean and s < self.max_mean])

        def _is_solution_in_range(self, m_prior_prob: List[int], m_mean: List[int], m_var: List[int]):
            return (
                m_prior_prob[0] > self.min_priority_prob and
                m_prior_prob[-1] <= self.max_priority_prob and
                m_mean[0] > self.min_mean and
                m_mean[-1] <= self.max_mean and
                min(m_var) > self.min_var and
                m_var[-1] <= self.max_var
            )

    return GE


def threshold_deo(img: np.ndarray, k: int, iter: int = 10, number_pop: int = 40,
                  mutation_factor: float = 0.5, crossover_constant: float = 0.1, fitness_function: any = BetweenClassVar()):

    prob = _image_probabilities(img)
    ff = fitness_function(prob, k, number_pop)
    ff.generate_initial_solutions()

    counter = 0
    while counter < iter:
        ff.calculate_best_solution()

        pos1 = []
        pos2 = []
        for i in range(number_pop):
            positions = [x for x in range(number_pop) if x != i]
            pos1.append(positions.pop(random.randint(0, len(positions)-1)))
            pos2.append(positions.pop(random.randint(0, len(positions)-1)))

        ff.generate_mutant_vectors(mutation_factor, pos1, pos2)

        for i in range(number_pop):
            ff.crossover(crossover_constant)
            ff.new_fitting_error()

        counter += 1

    return ff.get_result()
