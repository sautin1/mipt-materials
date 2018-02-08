#include <iostream>
#include <fstream>
#include <vector>
#include <stdexcept>
#include <limits>
#include <algorithm>

typedef size_t node_t;
typedef long weight_t;
struct edge_t{
	edge_t(node_t first_, node_t second_, weight_t weight_): first(first_), second(second_), weight(weight_){}
	node_t first;
	node_t second;
	weight_t weight;
};
const long INF = (std::numeric_limits<long>().max() / 2) - 1;

class weighted_graph
{
	std::vector< edge_t > edges_;
	size_t node_quantity_;
public:
	weighted_graph();
	weighted_graph(size_t node_quantity, const std::vector<edge_t>& edges);
	weighted_graph(size_t node_quantity, const std::vector< std::vector<weight_t> >& matrix);
	size_t size() const;
	bool min_distance(node_t start_node, std::vector<weight_t>& distances) const; //Ford-Bellman
	weight_t node_path(node_t start_node, node_t end_node, std::vector<node_t>& path) const; //Ford-Bellman
	bool negative_cycle(node_t start_node, std::vector<node_t>& cycle) const;
};

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

bool weighted_graph::min_distance(node_t start_node, std::vector<weight_t>& distances) const
{
	//Ford-Bellman
	distances.assign(node_quantity_, INF);
	distances[start_node] = 0;
	bool is_changed;
	for (size_t iteration_number = 0; iteration_number < node_quantity_; ++iteration_number){
		is_changed = false;
		for (size_t edge_index = 0; edge_index < edges_.size(); ++edge_index){
			node_t from = edges_[edge_index].first;
			node_t to = edges_[edge_index].second;
			if (distances[from] < INF && distances[from] + edges_[edge_index].weight < distances[to]){
				distances[edges_[edge_index].second] = std::max(-INF, distances[edges_[edge_index].first] + edges_[edge_index].weight);
				is_changed = true;
			}
		}
		if (!is_changed){
			break;
		}
	}
	return is_changed;
}

weight_t weighted_graph::node_path(node_t start_node, node_t end_node, std::vector<node_t>& path) const
{
	//Ford-Bellman
	std::vector<weight_t> distances(node_quantity_, INF);
	distances[start_node] = 0;
	std::vector<node_t> reference(node_quantity_, 0);
	path.clear();
	bool is_changed;
	for (size_t iteration_number = 0; iteration_number < node_quantity_; ++iteration_number){
		is_changed = false;
		for (size_t edge_index = 0; edge_index < edges_.size(); ++edge_index){
			node_t from = edges_[edge_index].first;
			node_t to = edges_[edge_index].second;
			if (distances[from] < INF && distances[from] + edges_[edge_index].weight < distances[to]){
				distances[edges_[edge_index].second] = std::max(-INF, distances[edges_[edge_index].first] + edges_[edge_index].weight);
				is_changed = true;
				reference[to] = from;
			}
		}
		if (!is_changed){
			break;
		}
	}
	if (!is_changed && distances[end_node] < INF){
		size_t current_node = end_node;
		while (current_node != start_node){
			path.push_back(current_node);
			current_node = reference[current_node];
		}
		path.push_back(start_node);
		std::reverse(path.begin(), path.end());
	}
	return distances[end_node];
}

bool weighted_graph::negative_cycle(node_t start_node, std::vector<node_t>& cycle) const
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

const std::string INPUT_FILE  = "input.txt";
const std::string OUTPUT_FILE = "output.txt";

int main()
{
	std::ifstream fin(INPUT_FILE.c_str(), std::ifstream::in);
	size_t node_quantity;
	fin >> node_quantity;
	
	std::vector< std::vector<weight_t> > matrix(node_quantity, std::vector<weight_t>(node_quantity, INF));
	for (node_t node1 = 0; node1 < node_quantity; ++node1){
		for (node_t node2 = 0; node2 < node_quantity; ++node2){
			weight_t weight;
			fin >> weight;
			if (weight < 100000 && (node1 != node2 || weight != 0)){
				matrix[node1][node2] = weight;
			}
		}
	}
	fin.close();

	weighted_graph graph(node_quantity, matrix);
	std::vector<node_t> cycle;
	bool is_cycle = graph.negative_cycle(0, cycle);
	std::ofstream fout(OUTPUT_FILE.c_str(), std::ofstream::out);
	if (is_cycle){
		fout << "YES\n";
		fout << cycle.size() << "\n";
		for (size_t node_index = 0; node_index < cycle.size(); ++node_index){
			fout << cycle[node_index] + 1 << " ";
		}
	} else {
		fout << "NO\n";
	}
	fout.close();
}
