#include "weighted_graph.h"

weighted_graph::weighted_graph(){}

weighted_graph::weighted_graph(size_t node_quantity, const std::vector<edge_t>& edges)
	:edges_(edges), node_quantity_(node_quantity)
{}

weighted_graph::weighted_graph(size_t node_quantity, const std::vector< std::vector<weight_t> >& matrix)
	:edges_(), node_quantity_(node_quantity)
{
	for (node_t node1 = 0; node1 < node_quantity; ++node1){
		for (node_t node2 = 0; node2 < node_quantity; ++node2){
			if (matrix[node1][node2] < INF){
				edges_.push_back(edge_t(node1, node2, matrix[node1][node2]));
			}
		}
	}
}

size_t weighted_graph::size() const
{
	return node_quantity_;
}

bool weighted_graph::negative_cycle(std::vector<node_t>& cycle) const
{
	//Ford-Bellman
	std::vector<weight_t> distances(node_quantity_, 0);
	std::vector<node_t> reference(node_quantity_, 0);
	cycle.clear();
	bool is_changed;
	node_t node_changed;
	for (size_t iteration_number = 0; iteration_number < node_quantity_; ++iteration_number){
		is_changed = false;
		for (size_t edge_index = 0; edge_index < edges_.size(); ++edge_index){
			node_t from = edges_[edge_index].first;
			node_t to = edges_[edge_index].second;
			if (distances[from] + edges_[edge_index].weight < distances[to]){
				distances[edges_[edge_index].second] = std::max(-INF, distances[edges_[edge_index].first] + edges_[edge_index].weight);
				is_changed = true;
				node_changed = to;
				reference[to] = from;
			}
		}
		if (!is_changed){
			break;
		}
	}
	if (is_changed){
		//cycle exists
		node_t cycle_node = node_changed;
		for (size_t iteration_number = 0; iteration_number < node_quantity_; ++iteration_number){
			cycle_node = reference[cycle_node];
		}
		node_t current_node = cycle_node;
		do {
			cycle.push_back(current_node);
			current_node = reference[current_node];
		} while (current_node != cycle_node);
		cycle.push_back(cycle_node);
		std::reverse(cycle.begin(), cycle.end());
	}
	return is_changed;
}

void preprocess(size_t node_quantity, std::vector< std::vector<weight_t> >& matrix)
{
	for (node_t node1 = 0; node1 < node_quantity; ++node1){
		for (node_t node2 = 0; node2 < node_quantity; ++node2){
			weight_t weight = matrix[node1][node2];
			if (node1 != node2 || weight != 1){
				matrix[node1][node2] = -std::log(weight);
			} else {
				matrix[node1][node2] = INF;
			}
		}
	}
}
