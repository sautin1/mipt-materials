from decomposition import MatrixMath
import numpy as np


def make_random_invertible_matrix(size, bounds=(1, 10), modulus=None, is_triangular=False):
    bounds = (bounds[0], modulus or bounds[1])
    while True:
        a = np.random.randint(bounds[0], bounds[1], (size, size))
        if is_triangular:
            # make either lower triangular or upper triangular matrix
            a = np.tril(a) if np.random.rand() < 0.5 else np.triu(a)
        det = np.linalg.det(a).astype(np.int).item()
        if modulus is not None:
            det %= modulus
        if det != 0:
            break
    return a


class Tester:
    def __init__(self, test_count):
        self.test_count = test_count

    def run(self, test, test_case_name):
        is_passed = True
        for test_idx in range(self.test_count):
            is_passed, inputs = test()
            if not is_passed:
                print(f'{test_case_name}: test {test_idx} failed!')
                print('Inputs:')
                print(inputs)
                break
        if is_passed:
            print(f'{test_case_name}: all tests passed!')


def test_multiplication():
    matrix_size = np.random.randint(1, 10)
    modulus = None
    dtype = np.float
    if np.random.rand() < 0.5:
        modulus = np.random.choice([3, 5, 7, 11])
        dtype = np.int

    a = make_random_invertible_matrix(matrix_size, modulus=modulus).astype(dtype)
    b = make_random_invertible_matrix(matrix_size, modulus=modulus).astype(dtype)
    result_correct = np.dot(a, b)
    if modulus is not None:
        result_correct %= modulus
    result_tested = MatrixMath.multiply(a, b, modulus)
    return np.allclose(result_correct, result_tested), {'a': a, 'b': b, 'modulus': modulus}


def test_inversion():
    matrix_size = np.random.randint(1, 10)
    modulus = None
    dtype = np.float
    if np.random.rand() < 0.5:
        modulus = np.random.choice([2, 3, 5, 7])
        dtype = np.int

    a = make_random_invertible_matrix(matrix_size, modulus=modulus, is_triangular=True).astype(dtype)

    a_inverse = MatrixMath.invert(a, modulus)
    product = np.dot(a, a_inverse)
    if modulus is not None:
        product %= modulus
    return np.allclose(product, np.eye(matrix_size)), {'a': a, 'modulus': modulus}


if __name__ == '__main__':
    tester = Tester(20)
    for test, test_name in zip([test_multiplication, test_inversion],
                               ['Multiplication', 'Inversion']):
        tester.run(test, test_name)
