#include <iostream>
#include <vector>

struct GraphEdge {
	int neighbour;
	int edge;
	GraphEdge() = default;
	GraphEdge(int _neighbour, int _edge)
		: neighbour(_neighbour), edge(_edge) {}
};

typedef std::vector< std::vector<GraphEdge> > Graph;

int countSpragueGrandyFunction(const Graph& graph, int node, int parent, std::vector<int>& sprague_grundy) {
	sprague_grundy[node] = 0;
	for (size_t neighbour_index = 0; neighbour_index < graph[node].size(); ++neighbour_index) {
		int neighbour = graph[node][neighbour_index].neighbour;
		if (neighbour == parent) {
			continue;
		}
		sprague_grundy[node] ^= 1 + countSpragueGrandyFunction(graph, neighbour, node, sprague_grundy);
	}
	return sprague_grundy[node];
}

int getCorrectFirstStep(const Graph& graph, int node, int parent, int root, std::vector<int>& sprague_grundy,
						int tree_part_sprague_grundy) {
	int result_edge = -1;
	for (size_t neighbour_index = 0; neighbour_index < graph[node].size(); ++neighbour_index) {
		int neighbour = graph[node][neighbour_index].neighbour;
		if (neighbour == parent) {
			continue;
		}
		int grundy_tree_no_neighbour = (sprague_grundy[node] ^ (sprague_grundy[neighbour] + 1));
		grundy_tree_no_neighbour ^= tree_part_sprague_grundy;
		if (grundy_tree_no_neighbour == 0) {
			result_edge = graph[node][neighbour_index].edge;
		} else {
			result_edge = getCorrectFirstStep(graph, neighbour, node, root, sprague_grundy, grundy_tree_no_neighbour - 1); // WHY -1 AT LAST ARG???
		}
		if (result_edge >= 0) {
			break;
		}
	}
	return result_edge;
}

int main() {
	int node_quantity;
	int root;
	std::cin >> node_quantity >> root;
	--root;
	Graph graph(node_quantity, std::vector<GraphEdge>());
	for (int edge_index = 0; edge_index < node_quantity-1; ++edge_index) {
		int from, to;
		std::cin >> from >> to;
		graph[from-1].push_back(GraphEdge(to-1, edge_index));
		graph[to-1].push_back(GraphEdge(from-1, edge_index));
	}

	std::vector<int> sprague_grundy(node_quantity, 0);
	int game_result = countSpragueGrandyFunction(graph, root, -1, sprague_grundy);
	if (game_result == 0) {
		std::cout << 2 << "\n";
	} else {
		std::cout << 1 << "\n";
		std::cout << getCorrectFirstStep(graph, root, -1, root, sprague_grundy, 0) + 1 << "\n";
	}
	return 0;
}
