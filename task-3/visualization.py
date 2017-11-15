from matplotlib import pyplot as plt


def draw_node(x, y):
    plt.plot(x, y, 'ro')


def draw_edge(point_start, point_end):
    xs, ys = tuple(zip(point_start, point_end))
    plt.plot(xs, ys, 'b-')


def plot_planar_graph(coordinates, graph):
    for node in range(len(graph)):
        for neighbor in graph[node]:
            draw_edge(coordinates[node], coordinates[neighbor])
    for node in range(len(graph)):
        draw_node(*coordinates[node])
    plt.show()
