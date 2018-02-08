#include <fstream>
#include <vector>
#include <iostream>
#include <stdexcept>
#include <limits>
#include <algorithm>
#include <cmath>

typedef size_t node_t;
typedef long weight_t;
struct edge_t{
	edge_t(node_t first_, node_t second_, weight_t weight_): first(first_), second(second_), weight(weight_){}
	node_t first;
	node_t second;
	weight_t weight;
};
const weight_t INF = (std::numeric_limits<weight_t>().max() / 2) - 1;
//const weight_t EPS = 1.0E-14;

class weighted_graph
{
	std::vector< std::vector<weight_t> > matrix_;
	size_t node_quantity_;
	void getPath(node_t from, node_t to, std::vector< std::vector<node_t> >& reference, std::vector<node_t>& path);
public:
	weighted_graph();
	weighted_graph(size_t node_quantity, const std::vector<edge_t>& edges);
	weighted_graph(size_t node_quantity, const std::vector< std::vector<weight_t> >& matrix);
	size_t size() const;
	void shortest_paths(std::vector< std::vector<weight_t> >& paths) const;
};

void preprocess(size_t node_quantity, std::vector< std::vector<weight_t> >& matrix);

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

void weighted_graph::shortest_paths(std::vector< std::vector<weight_t> >& paths) const
{
	//Warshall-Floyd
	std::vector< std::vector<weight_t> > distances(matrix_);
	for (node_t through = 0; through < node_quantity_; ++through){
		for (node_t from = 0; from < node_quantity_; ++from){
			for (node_t to = 0; to < node_quantity_; ++to){
				if (distances[from][through] < INF && distances[through][to] < INF){
					if (distances[from][through] + distances[through][to] < distances[from][to]){
						distances[from][to] = std::max(-INF, distances[from][through] + distances[through][to]);
					}
				}
			}
		}
	}
	std::swap(paths, distances);
}


const std::string INPUT_FILE  = "input.txt";
const std::string OUTPUT_FILE = "output.txt";

int main()
{
	//auto-tests
	//testArbitrage(1000);

	//manual tests
	std::ifstream fin(INPUT_FILE.c_str(), std::ifstream::in);
	if (!fin.good()){
		throw std::runtime_error(std::string("Cannot open input file: ") + INPUT_FILE);
	}
	size_t node_quantity;
	fin >> node_quantity;
	std::vector< std::vector<weight_t> > matrix(node_quantity, std::vector<weight_t>(node_quantity, INF));
	for (node_t node1 = 0; node1 < node_quantity; ++node1){
		for (node_t node2 = 0; node2 < node_quantity; ++node2){
			weight_t weight;
			fin >> weight;
			matrix[node1][node2] = weight;
		}
	}
	fin.close();

	weighted_graph graph(node_quantity, matrix);
	std::vector< std::vector<weight_t> > paths;
	graph.shortest_paths(paths);
	std::ofstream fout(OUTPUT_FILE.c_str(), std::ofstream::out);
	if (!fout.good()){
		throw std::runtime_error(std::string("Cannot open/create output file: ") + OUTPUT_FILE);
	}
	for (node_t node1 = 0; node1 < node_quantity; ++node1){
		for (node_t node2 = 0; node2 < node_quantity; ++node2){
			fout << paths[node1][node2] << " ";
		}
		fout << "\n";
	}
	fout.close();
}
