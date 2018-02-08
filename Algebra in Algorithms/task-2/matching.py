import numpy as np


def has_perfect_matching(incidence_matrix, randint_max=int(10 ** 9) + 7):
    weights = np.random.randint(1, randint_max, size=incidence_matrix.shape)
    matrix = incidence_matrix * weights
    return calculate_determinant(matrix) != 0


def calculate_determinant(matrix, modulo=int(10 ** 9) + 7):
    det = 1
    matrix %= modulo
    for pivot_col in range(matrix.shape[1]):
        pivot_row = np.argmax(matrix[pivot_col:, pivot_col]) + pivot_col
        pivot = int(matrix[pivot_row, pivot_col])
        if pivot == 0:
            det = 0
            break
        if pivot_row != pivot_col:
            matrix[[pivot_row, pivot_col]] = matrix[[pivot_col, pivot_row]]
            det = modulo - det
            pivot_row = pivot_col
        pivot_inverse = pow(pivot, modulo - 2, modulo)
        det = (det * pivot) % modulo
        matrix[pivot_row] = (matrix[pivot_row] * pivot_inverse) % modulo
        for row in range(pivot_row + 1, matrix.shape[0]):
            matrix[row] += matrix[pivot_row] * (modulo - matrix[row, pivot_col])
            matrix[row] %= modulo
    return det


def build_incidence_matrix(edges):
    node_count = max(max(edge) for edge in edges) + 1
    matrix = np.zeros(shape=(node_count, node_count), dtype=np.int)
    for edge in edges:
        matrix[edge[0], edge[1]] = 1
    return matrix


if __name__ == '__main__':
    CHECK_COUNT = 100

    edge_count = int(input())
    edges = [tuple(map(int, input().split())) for _ in range(edge_count)]
    incidence_matrix = build_incidence_matrix(edges)

    answer = 'no'
    if any(has_perfect_matching(incidence_matrix) for _ in range(CHECK_COUNT)):
        answer = 'yes'
    print(answer)
