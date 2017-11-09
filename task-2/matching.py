import numpy as np


def has_perfect_matching(incidence_matrix, randint_max=int(10 ** 9) + 7):
    weights = np.random.randint(1, randint_max, size=incidence_matrix.shape)
    matrix = incidence_matrix * weights
    sign, _ = np.linalg.slogdet(matrix)
    return sign != 0


def build_incidence_matrix(edges):
    node_count = max(max(edge) for edge in edges) + 1
    matrix = np.zeros(shape=(node_count, node_count), dtype=np.int)
    for edge in edges:
        matrix[edge[0], edge[1]] = 1
    return matrix


if __name__ == '__main__':
    CHECK_COUNT = 1

    edge_count = int(input())
    edges = [tuple(map(int, input().split())) for _ in range(edge_count)]
    incidence_matrix = build_incidence_matrix(edges)

    answer = 'no'
    if any([has_perfect_matching(incidence_matrix) for _ in range(CHECK_COUNT)]):
        answer = 'yes'
    print(answer)
