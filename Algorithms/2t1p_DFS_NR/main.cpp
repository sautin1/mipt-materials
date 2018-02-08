#include <fstream>
#include "directed_graph.h"

const std::string INPUT_FILE  = "toposort.in";
const std::string OUTPUT_FILE = "toposort.out";

int main()
{
	std::ifstream fin;
	fin.open(INPUT_FILE.c_str(), std::ifstream::in);
	if (!fin.good()){
		throw std::runtime_error(std::string("Cannot open input file: ") + INPUT_FILE);
	}
	size_t node_quantity, edge_quantity;
	std::vector<edge_t> edges;
	fin >> node_quantity >> edge_quantity;
	for (size_t edge_index = 0; edge_index < edge_quantity; ++edge_index){
		size_t node1, node2;
		fin >> node1 >> node2;
		edges.push_back(std::make_pair<node_t>(node1, node2));
	}
	fin.close();
	directed_graph graph(node_quantity, edges);
	std::vector<node_t> node_toposort;
	bool success = graph.toposort(node_toposort);
	//bool success = graph.dfs_caller();

	std::ofstream fout;
	fout.open(OUTPUT_FILE.c_str(), std::ofstream::out);
	if (success){
		for (size_t node_index = 0; node_index < node_toposort.size(); ++node_index){
			fout << node_toposort[node_index] << " ";
		}
	} else {
		fout << "The graph has loops";
	}
	fout.close();

	return 0;
}
