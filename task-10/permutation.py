import numpy as np
from collections import Counter


class Permutation:
    @staticmethod
    def identity(size):
        return Permutation(np.fromiter(range(size), dtype=np.int))

    def __init__(self, permutation):
        if len(Counter(permutation)) < permutation.shape[0]:
            raise ValueError('Elements of the permutation are not unique')
        self._p = np.array(permutation)

    def __mul__(self, right):
        result = np.zeros(self._p.shape, dtype=np.int)
        for x in range(self._p.shape[0]):
            result[x] = self[right[x]]
        return Permutation(result)

    def __call__(self, elements, *args, **kwargs):
        result = np.zeros(elements.shape, dtype=elements.dtype)
        for idx, element in enumerate(elements):
            result[self._p[idx]] = element
        return result

    def __eq__(self, other):
        return np.all(self._p == other.get())

    def __getitem__(self, item):
        return self._p[item]

    def is_identity(self):
        return self.identity(len(self._p)) == self

    def get(self):
        return self._p

    def invert(self):
        result = np.zeros(self._p.shape, dtype=self._p.dtype)
        for x in range(self._p.shape[0]):
            result[self._p[x]] = x
        return Permutation(result)

    def calc_conjugate(self, other):
        result = np.zeros(self._p.shape, dtype=self._p.dtype)
        x_from, x_to = 0, 0
        for i in range(self._p.shape[0]):
            if i > 0:
                x_from, x_to = self._p[x_from], other[x_to]
            result[x_from] = x_to

        assert other == Permutation(result) * self * Permutation(result).invert()
        return Permutation(result)
