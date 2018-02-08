#include "weighted_graph.h"

weighted_graph::weighted_graph(){}

weighted_graph::weighted_graph(size_t node_quantity, const std::vector<edge_t>& edges)
	:matrix_(node_quantity, std::vector<weight_t>(node_quantity, INF)), node_quantity_(node_quantity)
{
	for (size_t edge_index = 0; edge_index < edges.size(); ++edge_index){
		node_t from = edges[edge_index].first;
		node_t to = edges[edge_index].second;
		matrix_[from][to] = edges[edge_index].weight;
	}
	for (node_t node = 0; node < node_quantity_; ++node){
		matrix_[node][node] = 0;
	}
}

weighted_graph::weighted_graph(size_t node_quantity, const std::vector< std::vector<weight_t> >& matrix)
	:matrix_(matrix), node_quantity_(node_quantity)
{}

size_t weighted_graph::size() const
{
	return node_quantity_;
}

void weighted_graph::print_shortest_path(node_t from, node_t to, const std::vector< std::vector<ssize_t> >& reference, std::vector<node_t>& path) const
{
	if (reference[from][to] == -1){
		path.push_back(from);
	} else {
		print_shortest_path(from, reference[from][to], reference, path);
		print_shortest_path(reference[from][to], to, reference, path);
	}
}

bool weighted_graph::negative_cycle(std::vector<node_t>& cycle) const
{
	//Warshall-Floyd
	std::vector< std::vector<weight_t> > distances(matrix_);
	std::vector< std::vector<ssize_t> > reference(node_quantity_, std::vector<ssize_t>(node_quantity_, -1));
	cycle.clear();
	bool is_cycle = false;
	node_t cycle_node;
	for (node_t through = 0; through < node_quantity_; ++through){
		for (node_t from = 0; from < node_quantity_; ++from){
			for (node_t to = 0; to < node_quantity_; ++to){
				if (distances[from][through] < INF && distances[through][to] < INF){
					if (distances[from][through] + distances[through][to] < distances[from][to] - EPS){
						distances[from][to] = std::max(-INF, distances[from][through] + distances[through][to]);
						reference[from][to] = through;
						if (from == to && distances[from][to] < 0){
							is_cycle = true;
							cycle_node = from;
							break;
						}
					}
				}
			}
			if (is_cycle) break;
		}
		if (is_cycle) break;
	}
	if (is_cycle){
		print_shortest_path(cycle_node, cycle_node, reference, cycle);
		cycle.push_back(cycle_node);
	}
	return is_cycle;
}

void preprocess(size_t node_quantity, std::vector< std::vector<weight_t> >& matrix)
{
	for (node_t node1 = 0; node1 < node_quantity; ++node1){
		for (node_t node2 = 0; node2 < node_quantity; ++node2){
			weight_t weight = matrix[node1][node2];
			matrix[node1][node2] = -std::log(weight);
		}
	}
}
