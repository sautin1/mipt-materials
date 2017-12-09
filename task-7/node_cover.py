class NodeCover:
    @staticmethod
    def approximate(edges, weights):
        node_count = len(weights)
        pays = [0] * node_count
        dual = [0] * len(edges)
        for edge_idx, (node_from, node_to) in enumerate(edges):
            if pays[node_from] < weights[node_from] and pays[node_to] < weights[node_to]:
                dual[edge_idx] = min(weights[node_from] - pays[node_from], weights[node_to] - pays[node_to])
                pays[node_from] += dual[edge_idx]
                pays[node_to] += dual[edge_idx]
        return [node for node in range(node_count) if pays[node] >= weights[node]], dual


if __name__ == '__main__':
    node_count = int(input())
    weights = [int(input()) for _ in range(node_count)]
    edge_count = int(input())
    edges = [tuple(map(int, input().split())) for _ in range(edge_count)]

    cover, _ = NodeCover.approximate(edges, weights)
    print(' '.join(map(str, cover)))
