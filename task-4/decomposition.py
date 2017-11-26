import numpy as np
from functools import partial


class MatrixMath:
    @staticmethod
    def sum(a, b, modulus=None):
        if a.shape != b.shape:
            raise ValueError('Shapes of matrices are not equal')
        result = np.zeros(a.shape, dtype=a.dtype)
        for row in range(a.shape[0]):
            for col in range(a.shape[1]):
                result[row, col] = a[row, col] + b[row, col]
                if modulus is not None:
                    result[row, col] %= modulus
        return result

    @staticmethod
    def negate(a, modulus=None):
        result = np.zeros(a.shape, dtype=a.dtype)
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
        result = np.zeros((target_size, target_size), dtype=matrix.dtype)
        result[:matrix.shape[0], :matrix.shape[0]] = matrix
        return result

    @staticmethod
    def _split_into_submatrices(matrix):
        size = matrix.shape[0] // 2
        return matrix[:size, :size], matrix[:size, size:], matrix[size:, :size], matrix[size:, size:]

    @staticmethod
    def _collect_matrix(upper_left, upper_right, lower_left, lower_right):
        height = upper_left.shape[0] + lower_left.shape[0]
        width = upper_left.shape[1] + upper_right.shape[1]
        result = np.zeros((width, height), dtype=upper_left.dtype)
        result[:upper_left.shape[0], :upper_left.shape[1]] = upper_left
        result[:upper_left.shape[0], upper_left.shape[1]:] = upper_right
        result[upper_left.shape[0]:, :upper_left.shape[1]] = lower_left
        result[upper_left.shape[0]:, upper_left.shape[1]:] = lower_right
        return result

    @staticmethod
    def _multiply(a, b, modulus=None):
        sum = partial(MatrixMath.sum, modulus=modulus)
        negate = partial(MatrixMath.negate, modulus=modulus)
        if a.shape[0] == 1:
            result = a[0, 0] * b[0, 0]
            if modulus is not None:
                result %= modulus
            return np.array([[result]])
        a11, a12, a21, a22 = MatrixMath._split_into_submatrices(a)
        b11, b12, b21, b22 = MatrixMath._split_into_submatrices(b)
        aux_products = list(map(lambda matrices: MatrixMath._multiply(*matrices), [
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

        return MatrixMath._collect_matrix(*result_parts)

    @staticmethod
    def is_zero_matrix(matrix):
        return all(x == 0 for row in matrix for x in row)

    @staticmethod
    def multiply(a, b, modulus=None):
        if a.shape[0] != a.shape[1]:
            raise ValueError('One of matrices is not square')
        if a.shape != b.shape:
            raise ValueError('Shapes of matrices are not equal')

        size = a.shape[0]
        size_padded = MatrixMath._calc_nearest_greater_power_of_2(size)
        a, b = MatrixMath._pad(a, size_padded), MatrixMath._pad(b, size_padded)
        result = MatrixMath._multiply(a, b, modulus)
        return result[:size, :size]

    @staticmethod
    def _invert(a, modulus=None):
        if a.shape[0] == 1:
            result = 1 / a[0, 0]
            if modulus is not None:
                result = pow(a[0, 0].item(), modulus - 2, modulus)
            return np.array([[result]])

        negate = partial(MatrixMath.negate, modulus=modulus)
        mult = partial(MatrixMath.multiply, modulus=modulus)

        a11, a12, a21, a22 = MatrixMath._split_into_submatrices(a)
        a11_inverse = MatrixMath._invert(a11, modulus)
        a22_inverse = MatrixMath._invert(a22, modulus)
        is_upper_triangular = MatrixMath.is_zero_matrix(a21)

        result_parts = [
            a11_inverse,
            negate(mult(mult(a11_inverse, a12), a22_inverse)) if is_upper_triangular else a12,
            negate(mult(mult(a22_inverse, a21), a11_inverse)) if not is_upper_triangular else a21,
            a22_inverse
        ]

        return MatrixMath._collect_matrix(*result_parts)

    @staticmethod
    def invert(matrix, modulus=None):
        """Works only for lower/upper-triangular matrices"""
        size = matrix.shape[0]
        size_padded = MatrixMath._calc_nearest_greater_power_of_2(size)
        size_eye = size_padded - size
        matrix_embedded = MatrixMath._collect_matrix(matrix,
                                                     np.zeros((size, size_eye), dtype=matrix.dtype),
                                                     np.zeros((size_eye, size), dtype=matrix.dtype),
                                                     np.eye(size_eye, dtype=matrix.dtype))
        result = MatrixMath._invert(matrix_embedded, modulus)
        return result[:size, :size]

    # @staticmethod
    # def _decompose_lup(matrix_a, modulus=None):
    #     size = matrix_a.shape[0]
    #     if size == 1:
    #         lower = np.array([[1]])
    #         permutation = np.eye(matrix_a.shape[1])
    #         col_non_zero = next((idx for idx, number in enumerate(matrix_a[0]) if number != 0), 0)
    #         permutation[[0, col_non_zero]] = permutation[[col_non_zero, 0]]
    #         upper = matrix_a.copy()
    #         upper[:, [0, col_non_zero]] = upper[:, [col_non_zero, 0]]
    #     else:
    #         matrix_b, matrix_c = matrix_a[:size // 2, :], matrix_a[size // 2:, :]
    #         lower_b, upper_b, permutation_b = SquareMatrixMath._decompose_lup(matrix_b, modulus)
    #         matrix_d = SquareMatrixMath.multiply()
    #
    #         # lower = matrix_a
    #         # permutation = []
    #         # upper = []
    #     return lower, upper, permutation

    # @staticmethod
    # def decompose_lup(matrix, modulus=None):
    #     # pad
    #     return SquareMatrixMath._decompose_lup(matrix, modulo)


def stringify_matrix(matrix):
    return '\n'.join(' '.join(map(str, row)) for row in matrix)


if __name__ == '__main__':
    first_row = list(map(int, input().split(' ')))
    matrix_size = len(first_row)
    matrix = [first_row] + [list(map(int, input().split(' '))) for _ in range(matrix_size - 1)]
    matrix = np.array(matrix)
    decomposition = MatrixMath.decompose_lup(matrix)
    for factor in decomposition:
        print(stringify_matrix(factor))
