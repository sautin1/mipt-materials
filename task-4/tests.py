from decomposition import MatrixMath
import numpy as np


def make_random_matrix(size, bounds=(1, 10)):
    return np.random.randint(bounds[0], bounds[1], (size, size), dtype=np.int)


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
    a, b = make_random_matrix(matrix_size), make_random_matrix(matrix_size)
    result_correct = np.dot(a, b)
    result_tested = MatrixMath.multiply(a, b)
    return np.allclose(result_correct, result_tested), {'a': a, 'b': b}


def test_inversion():
    matrix_size = np.random.randint(1, 10)
    a = make_random_matrix(matrix_size)
    # make either lower triangular or upper triangular matrix
    a = np.tril(a) if np.random.rand() < 0.5 else np.triu(a)

    a_inverse = MatrixMath.invert(a)
    return np.allclose(np.dot(a, a_inverse), np.eye(matrix_size)), {'a': a}


def test_lup_decomposition(test_count=10):
    for test in range(test_count):
        matrix_size = np.random.randint(1, 10)
        matrix = make_random_matrix(matrix_size)
        decomposed = MatrixMath.decompose_lup(matrix)
        result = np.linalg.multi_dot(decomposed)
        if np.all(matrix == result):
            print(f'Test {test + 1} PASSED!')
        else:
            print(f'Test {test + 1} FAILED:')
            print(matrix)
            break

if __name__ == '__main__':
    tester = Tester(10)
    for test, test_name in zip([test_multiplication, test_inversion],
                               ['Multiplication', 'Inversion']):
        tester.run(test, test_name)
