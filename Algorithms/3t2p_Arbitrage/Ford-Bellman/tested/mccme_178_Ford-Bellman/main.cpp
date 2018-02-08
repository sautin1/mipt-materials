#include <iostream>
#include <fstream>
#include <vector>
#include <stdexcept>
#include <limits>

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

const std::string INPUT_FILE  = "input.txt";
const std::string OUTPUT_FILE = "output.txt";

int main()
{
	std::ifstream fin(INPUT_FILE.c_str(), std::ifstream::in);
	size_t node_quantity, edge_quantity;
	fin >> node_quantity >> edge_quantity;
	std::vector<edge_t> edges;
   for (size_t edge_index = 0; edge_index < edge_quantity; ++edge_index){
		node_t from, to;
		weight_t weight;
		fin >> from >> to >> weight;
		edges.push_back(edge_t(from-1, to-1, weight));
	}
	fin.close();

	weighted_graph graph(node_quantity, edges);
	std::vector<weight_t> distances;
	graph.min_distance(0, distances);
	std::ofstream fout(OUTPUT_FILE.c_str(), std::ofstream::out);
	for (node_t node = 0; node < node_quantity; ++node){
		if (distances[node] < INF){
			fout << distances[node];
		} else {
			fout << 30000;
		}
		fout << " ";
	}
	fout.close();
}

