import numpy as np
import math


def calc_regular_polygon_coordinates(vertice_count, radius=1):
    angles = np.linspace(0, 2 * math.pi, vertice_count + 1)[:-1]
    xs = radius * np.cos(angles)
    ys = radius * np.sin(angles)
    return np.array(list(zip(xs, ys)))


class TutteEmbedding:
    @staticmethod
    def embed(graph):
        external_face_length = find_external_face_length(neighbors)
        external_face_coordinates = calc_regular_polygon_coordinates(external_face_length)
        coordinates = {node: coord for node, coord in enumerate(external_face_coordinates)}
        if external_face_length < len(graph):
            matrix, answers = TutteEmbedding.init_equation_system(graph,
                                                                  external_face_length,
                                                                  coordinates)
            internal_nodes_coordinates = np.linalg.solve(matrix, answers)
            coordinates.update({idx + external_face_length: coord
                                for idx, coord in enumerate(internal_nodes_coordinates)})
        return coordinates

    @staticmethod
    def init_equation_system(graph, external_face_length, coordinates):
        internal_nodes_count = len(graph) - external_face_length
        matrix = np.zeros((internal_nodes_count, internal_nodes_count))
        answers = np.zeros((internal_nodes_count, 2))
        for idx in range(internal_nodes_count):
            node = external_face_length + idx
            matrix[idx, idx] = len(graph[node])
            for neighbor in graph[node]:
                neighbor_coordinate = coordinates.get(neighbor, None)
                if neighbor_coordinate is not None:
                    answers[idx] += coordinates[neighbor]
                else:
                    neighbor_idx = neighbor - external_face_length
                    matrix[idx, neighbor_idx] = -1
        return matrix, answers


def find_external_face_length(graph):
    # find first node that is not connected to (node + 1) or is connected to 0
    # nodes 0, 1, 2 have to be in the external face
    last_face_node = next(node for node in range(2, len(graph)) if 0 in graph[node])
    return last_face_node + 1


def build_adjacency_sets(edges, node_count):
    neighbors = [set() for _ in range(node_count)]
    for edge in edges:
        neighbors[edge[0]].add(edge[1])
        neighbors[edge[1]].add(edge[0])
    return neighbors


if __name__ == '__main__':
    edge_count = int(input())
    edges = [tuple(map(int, input().split())) for _ in range(edge_count)]
    node_count = max((max(edge) for edge in edges)) + 1
    neighbors = build_adjacency_sets(edges, node_count)

    coordinates = TutteEmbedding().embed(neighbors)
    for node in range(node_count):
        node_coordinates = coordinates[node]
        print('{} {} {}'.format(node, *node_coordinates))

    from visualization import plot_planar_graph
    plot_planar_graph(coordinates, neighbors)
