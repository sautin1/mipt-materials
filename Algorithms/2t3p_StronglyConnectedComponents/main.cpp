#include <fstream>
#include "directed_graph.h"

const std::string INPUT_FILE  = "scc.in";
const std::string OUTPUT_FILE = "ssc.out";
const std::string OUTPUT_FILE2 = "ssc2.out";

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
		edges.push_back(std::make_pair(node1, node2));
	}
	fin.close();
	directed_graph graph(node_quantity, edges);
	std::vector<size_t> scc;
	size_t scc_quantity = graph.scc_tarjan(scc);

	std::ofstream fout;
	fout.open(OUTPUT_FILE.c_str(), std::ofstream::out);
	fout << scc_quantity << "\n";
	for (size_t node_index = 0; node_index < node_quantity; ++node_index){
		fout << scc[node_index] << " ";
	}
	fout.close();

	size_t scc_quantity = graph.scc_kosaraju(scc);
	fout.open(OUTPUT_FILE2.c_str(), std::ofstream::out);
	fout << scc_quantity << "\n";
	for (size_t node_index = 0; node_index < node_quantity; ++node_index){
		fout << scc[node_index] << " ";
	}
	fout.close();

	return 0;
}
