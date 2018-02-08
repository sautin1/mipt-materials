#include <iostream>
#include <fstream>
#include <vector>
#include "weighted_graph.h"
#include "test_system.h"

const std::string INPUT_FILE  = "arbitrage.in";
const std::string OUTPUT_FILE = "arbitrage.out";

int main()
{
	//auto-tests
	testArbitrage();

	//manual tests
	/*std::ifstream fin(INPUT_FILE.c_str(), std::ifstream::in);
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
	preprocess(node_quantity, matrix);

	weighted_graph graph(node_quantity, matrix);
	std::vector<node_t> cycle;
	bool is_cycle = graph.negative_cycle(cycle);
	std::ofstream fout(OUTPUT_FILE.c_str(), std::ofstream::out);
	if (!fout.good()){
		throw std::runtime_error(std::string("Cannot open/create output file: ") + OUTPUT_FILE);
	}
	if (is_cycle){
		fout << "YES\n";
		for (size_t node_index = 0; node_index < cycle.size(); ++node_index){
			fout << cycle[node_index] << " ";
		}
	} else {
		fout << "NO\n";
	}
	fout.close();*/
}
