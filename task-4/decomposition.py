import numpy as np
from functools import partial


class SquareMatrixMath:
    @staticmethod
    def sum(a, b, modulus=None):
        if a.shape != b.shape:
            raise ValueError('Shapes of matrices are not equal')
        result = np.zeros(a.shape)
        for row in range(a.shape[0]):
            for col in range(a.shape[1]):
                result[row, col] = a[row, col] + b[row, col]
                if modulus is not None:
                    result[row, col] %= modulus
        return result

    @staticmethod
    def negate(a, modulus=None):
        result = np.zeros(a.shape)
        for row in range(a.shape[0]):
            for col in range(a.shape[1]):
                result[row, col] = -a[row, col]
                if modulus is not None:
                    result[row, col] += modulus
        return result

    @staticmethod
    def _calc_nearest_greater_power_of_2(number):
        return 1 << (number - 1).bit_length()

    @staticmethod
    def _pad(matrix, target_size):
        result = np.zeros((target_size, target_size))
        result[:matrix.shape[0], :matrix.shape[0]] = matrix
        return result

    @staticmethod
    def _split_into_submatrices(matrix):
        size = matrix.shape[0] // 2
        return matrix[:size, :size], matrix[:size, size:], matrix[size:, :size], matrix[size:, size:]

    @staticmethod
    def _collect_matrix(upper_left, upper_right, lower_left, lower_right):
        submatrix_size = upper_left.shape[0]
        result = np.zeros((submatrix_size * 2, submatrix_size * 2))
        result[:submatrix_size, :submatrix_size] = upper_left
        result[:submatrix_size, submatrix_size:] = upper_right
        result[submatrix_size:, :submatrix_size] = lower_left
        result[submatrix_size:, submatrix_size:] = lower_right
        return result

    @staticmethod
    def _multiply(a, b, modulus=None):
        sum = partial(SquareMatrixMath.sum, modulus=modulus)
        negate = partial(SquareMatrixMath.negate, modulus=modulus)
        if a.shape[0] == 1:
            result = a[0, 0] * b[0, 0]
            if modulus is not None:
                result %= modulus
            return np.array([[result]])
        a11, a12, a21, a22 = SquareMatrixMath._split_into_submatrices(a)
        b11, b12, b21, b22 = SquareMatrixMath._split_into_submatrices(b)
        aux_products = list(map(lambda matrices: SquareMatrixMath._multiply(*matrices), [
            (sum(a12, negate(a22)), sum(b21, b22)),
            (sum(a11, a22), sum(b11, b22)),
            (sum(a11, negate(a21)), sum(b11, b12)),
            (sum(a11, a12), b22),
            (a11, sum(b12, negate(b22))),
            (a22, sum(b21, negate(b11))),
            (sum(a21, a22), b11)
        ]))

        result_parts = [
            sum(sum(aux_products[0], aux_products[1]), sum(negate(aux_products[3]), aux_products[5])),
            sum(aux_products[3], aux_products[4]),
            sum(aux_products[5], aux_products[6]),
            sum(sum(aux_products[1], negate(aux_products[2])), sum(aux_products[4], negate(aux_products[6])))
        ]

        return SquareMatrixMath._collect_matrix(*result_parts)

    @staticmethod
    def multiply(a, b, modulus=None):
        if a.shape[0] != a.shape[1]:
            raise ValueError('One of matrices is not square')
        if a.shape != b.shape:
            raise ValueError('Shapes of matrices are not equal')

        size = a.shape[0]
        size_padded = SquareMatrixMath._calc_nearest_greater_power_of_2(size)
        a, b = SquareMatrixMath._pad(a, size_padded), SquareMatrixMath._pad(b, size_padded)
        result = SquareMatrixMath._multiply(a, b, modulus)
        return result[:size, :size]


if __name__ == '__main__':
    a = np.array([[-1, -2], [3, 4]], dtype=np.int)
    b = np.array([[1, -1], [2, 1]], dtype=np.int)
    res = SquareMatrixMath.multiply(a, b)
    print(res)
