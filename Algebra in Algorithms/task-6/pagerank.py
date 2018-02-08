import numpy as np


def build_adjacency_list(edges, node_count):
    adjacency_list = [[] for _ in range(node_count)]
    for node_from, node_to in edges:
        adjacency_list[node_from].append(node_to)
    return adjacency_list


class PageRank:
    @staticmethod
    def calc(graph, damping_factor):
        matrix = PageRank._build_google_matrix(graph, damping_factor)
        eigenvalues, eigenvectors = np.linalg.eig(matrix)
        page_rank = np.real(eigenvectors[:, np.argmax(eigenvalues)])
        return page_rank / page_rank.sum()

    @staticmethod
    def _build_google_matrix(graph, damping_factor):
        transition_matrix = np.zeros((len(graph), len(graph)), dtype=np.float)
        for node_from, neighbors in enumerate(graph):
            if neighbors:
                transition_matrix[neighbors, node_from] = 1 / len(neighbors)
            else:
                transition_matrix[:, node_from] = 1 / len(graph)
        return (1 - damping_factor) * transition_matrix + damping_factor / len(graph) * np.ones(transition_matrix.shape)


if __name__ == '__main__':
    damping_factor = float(input())
    edge_count = int(input())
    edges = [input().split(' ') for idx in range(edge_count)]
    node_ids = set(node for edge in edges for node in edge)
    node_str_to_idx = {node_str: node_idx for node_idx, node_str in enumerate(node_ids)}
    node_idx_to_str = {node_idx: node_str for node_str, node_idx in node_str_to_idx.items()}
    edges = [(node_str_to_idx[node_from], node_str_to_idx[node_to]) for node_from, node_to in edges]
    graph = build_adjacency_list(edges, len(node_str_to_idx))
    page_rank = PageRank.calc(graph, damping_factor)
    for node, rank in enumerate(page_rank):
        print(f'{node_idx_to_str[node]} {rank}')
