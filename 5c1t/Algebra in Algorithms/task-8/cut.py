import numpy as np


def build_adjacency_sets(edges, node_count):
    neighbors = [set() for _ in range(node_count)]
    for edge in edges:
        neighbors[edge[0]].add(edge[1])
        neighbors[edge[1]].add(edge[0])
    return neighbors


class SparsestCut:
    @staticmethod
    def approximate(edges, node_count):
        graph = build_adjacency_sets(edges, node_count)
        laplace_matrix = SparsestCut._build_laplace_matrix(graph)
        _, eigenvectors = np.linalg.eigh(laplace_matrix)
        eigenvector = eigenvectors[:, 1]
        permutation = np.argsort(eigenvector)[::-1]
        node_set = set()
        node_set_optimal_lengths, density_optimal = [], len(edges) * 2
        for node in permutation[:-1]:
            node_set.add(node)
            edges_cut_count = sum(1 for edge in edges if (edge[0] in node_set) != (edge[1] in node_set))
            density = edges_cut_count / len(node_set) * (node_count / (node_count - len(node_set)))
            if density < density_optimal:
                node_set_optimal_lengths, density_optimal = [len(node_set)], density
            elif density == density_optimal:
                node_set_optimal_lengths.append(len(node_set))
        return [set(permutation[:length]) for length in node_set_optimal_lengths]

    @staticmethod
    def _build_laplace_matrix(graph):
        node_count = len(graph)
        matrix = np.zeros((node_count, node_count))
        for node in range(node_count):
            matrix[node, node] = len(graph[node])
        for node_from in range(node_count):
            for node_to in graph[node_from]:
                matrix[node_from, node_to] = -1
        return matrix


def get_set_complement(elements_all, elements_part):
    return set(elements_all) - set(elements_part)


def choose_prettiest_answer(answers, node_idx_to_id):
    answers_smallest_parts = []
    node_count = len(node_idx_to_id)
    for answer in answers:
        if len(answer) * 2 <= node_count:
            answers_smallest_parts.append(answer)
        if len(answer) * 2 >= node_count:
            answers_smallest_parts.append(get_set_complement(range(node_count), answer))
    answers_with_node_ids = (set(map(lambda x: node_idx_to_id[x], answer)) for answer in answers_smallest_parts)
    answers_sorted = (sorted(list(answer)) for answer in answers_with_node_ids)
    return min(answers_sorted)


if __name__ == '__main__':
    edge_count = int(input())
    edges = [tuple(map(int, input().split())) for _ in range(edge_count)]
    node_ids = set(node for edge in edges for node in edge)
    node_id_to_idx = {node_id: node_idx for node_idx, node_id in enumerate(node_ids)}
    node_idx_to_id = {node_idx: node_id for node_id, node_idx in node_id_to_idx.items()}
    node_count = len(node_id_to_idx)
    edges_with_node_idx = [tuple(map(lambda node_id: node_id_to_idx[node_id], edge)) for edge in edges]
    cuts = SparsestCut().approximate(edges_with_node_idx, node_count)
    result = choose_prettiest_answer(cuts, node_idx_to_id)
    print(' '.join(map(str, result)))
